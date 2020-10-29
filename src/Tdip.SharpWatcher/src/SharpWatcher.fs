namespace Tdip.SharpWatcher

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO

module SharpWatcher =

    type EventsHandler = {
        OnEvent : WatchEvent -> unit
    }

    type ISubscription =
        abstract ConsistentHash : int with get

        abstract Start : EventsHandler -> IDisposable

    type Subscription = {
        Folder : string
        Attributes : seq<WatchAttribute>
    }
    with
        interface ISubscription with
            member x.ConsistentHash
                with get() = x.Folder.GetHashCode()

            member x.Start(handler) =
                let dirs =
                    Directory.EnumerateDirectories x.Folder
                    |> Seq.map WatchEvent.FromExistingFile

                let files =
                    Directory.EnumerateFiles x.Folder
                    |> Seq.map WatchEvent.FromExistingFile

                let initialEvents =
                    Seq.concat [dirs; files]
                    |> Seq.map FileSystem

                let cliEventHandler (args: FileSystemEventArgs) =
                    args
                    |> WatchEvent.FromEvent
                    |> handler.OnEvent

                let watcher = new FileSystemWatcher()
                watcher.Path <- x.Folder
                watcher.Changed.Add(cliEventHandler)
                watcher.Created.Add(cliEventHandler)
                watcher.Deleted.Add(cliEventHandler)
                watcher.Renamed.Add(cliEventHandler)
                watcher.EnableRaisingEvents <- true

                initialEvents
                |> Seq.iter handler.OnEvent

                {
                    new IDisposable with
                        member _.Dispose() = watcher.Dispose()
                }

    type State = seq<Subscription>
   
    type Context = {
        Subscriptions : seq<ISubscription>
    } with
        static member Empty with get() =
            {
                Subscriptions = Seq.empty
            }

    type Monad<'a> = Context -> Context*'a

    type Args = {
        Root : string
        Update : Notification -> State -> Monad<State>
        Interval : TimeSpan
    }

    type Builder() =

        member _.Return(v) ctx = (ctx, v)

        member _.Bind(m : Monad<'a>, f : 'a -> Monad<'b>) ctx =
            let (ctx', a') = m ctx
            f a' ctx'

        member monad.For(values: seq<'a>, f : 'a -> Monad<Option<Subscription>>) : Monad<seq<Subscription>> =
            match Seq.tryHead values with
            | None -> monad.Return(Seq.empty)
            | Some a ->
                let rest = Seq.skip 1 values
                let aggregator (state: Monad<seq<Subscription>>) (item: 'a) : Monad<seq<Subscription>> =
                    monad {
                        let! rest = state
                        let! next = f item
                        return
                            match next with
                            | Some next' ->
                                Seq.concat [rest; Seq.singleton next']
                            | None -> rest
                    }
                let initial =
                    monad {
                        let! value = f a
                        return
                            match value with
                            | Some value' -> Seq.singleton value'
                            | None -> Seq.empty
                    }
                Seq.fold aggregator initial rest

        member _.Zero() : Monad<Option<Subscription>> = fun ctx -> (ctx, None)

        member monad.Yield(x : 'a) : Monad<Option<'a>> when 'a :> ISubscription =
            monad.Yield(Some x)

        member _.Yield(x: Option<'a>) : Monad<Option<'a>> when 'a :> ISubscription =
            fun ctx ->
                match x with
                | Some subscription ->
                    let subscriptions = Seq.append [subscription :> ISubscription] ctx.Subscriptions
                    (
                        { ctx with Subscriptions = subscriptions },
                        x
                    )
                | _-> (ctx, None)

        member monad.Combine(a, b) =
            monad {
                let! _ = a
                let! b' = b
                return b'
            }

    let builder = Builder()
    type private SubscriptionEntry = {
        Subscription : IDisposable
    }

    type private AttributesEntry = {
        Subscription : Subscription
    } with
        member x.MatchesAttributes(other : Subscription) =
            x.Subscription.Attributes = other.Attributes

        member x.Match(entry: FileSystemEventEntry) =
            if entry.HasAttribute(FileAttributes.Directory) |> not
            then
                x.Subscription.Attributes
                |> Seq.exists (WatchAttribute.Match entry)
            else
                false

    type Manager(args: Args) as self =

        let mutable state = Seq.empty
        let attributes = Dictionary<string, AttributesEntry>()
        let subscriptions = Dictionary<int, SubscriptionEntry>()
        let eventsCache = ConcurrentBag<WatchEvent>()
        let onFilesystemEvent = new Event<obj*seq<FileSystemEventArgs>>()

        let shouldRaiseEvent (e : FileSystemEventEntry) =
            let targets =
                [
                    e.EventArgs.FullPath
                    Path.GetDirectoryName(e.EventArgs.FullPath)
                ]

            let checkTarget path =
                let attrs = attributes
                printfn "%A" attrs
                match Dictionary.tryGet path attributes with
                | Some attrs -> attrs.Match(e)
                | _ -> false

            targets |> Seq.exists checkTarget

        let removeSubscription i =
            match Dictionary.tryRemove i subscriptions with
            | Some entry ->
                entry.Subscription.Dispose()
            | None ->
                failwithf "The index %i was expected to be in the dictionary" i

        let updateState (newState: State) =

            // This is communism but sometimes it has to
            // be that way
            let currentKeys =
                newState
                |> Seq.map
                    (
                        fun entry ->
                            match Dictionary.tryGet entry.Folder attributes with
                            | Some entry' when entry'.MatchesAttributes(entry) |> not ->
                                attributes.[entry.Folder] <- { Subscription = entry }
                            | None ->
                                attributes.[entry.Folder] <- { Subscription = entry }
                            | _ -> ()

                            entry.Folder
                    )
                |> Set.ofSeq
            
            let dictKeys = attributes.Keys |> Set.ofSeq

            Set.difference dictKeys currentKeys
            |> Seq.iter
                (
                    fun k ->
                        match Dictionary.tryRemove k attributes with
                        | Some _ -> ()
                        | None ->
                            failwithf "Attributes expected to have key %s" k
                )

        let rec step (events: Notification) =
            let (newContext, newState) = args.Update events state Context.Empty

            updateState(newState)
            
            let newSubscriptions =
                newContext.Subscriptions
                |> Seq.map (fun s -> s.ConsistentHash)
                |> Set.ofSeq
            let existingSubscriptions =
                subscriptions.Keys
                |> Set.ofSeq

            let shouldBeAdded = Set.difference newSubscriptions existingSubscriptions
            let shouldBeDeleted = Set.difference existingSubscriptions newSubscriptions

            Seq.iter removeSubscription shouldBeDeleted
            
            newContext.Subscriptions
            |> Seq.filter (fun c -> Set.contains c.ConsistentHash shouldBeAdded)
            |> Seq.iter addSubscription

        and addSubscription (subscription: ISubscription) =
            let onEvent event =
                eventsCache.Add event
                dispatchScheduler.Schedule()
            subscriptions.[subscription.ConsistentHash] <-
                {
                    Subscription = subscription.Start({ OnEvent = onEvent })
                }

        and dispatcher () =
            let nextEvents = Concurrent.tryTakeMany eventsCache
            let filesystemEvents =
                nextEvents
                |> Seq.filterMap (fun e -> e.AsFileSystem)
                |> Seq.filter shouldRaiseEvent
                |> Seq.map (fun entry -> entry.EventArgs)

            do
                if Seq.isEmpty filesystemEvents |> not
                then
                    onFilesystemEvent.Trigger(self :> obj, filesystemEvents)

            do (attributes, nextEvents) |> Notification.FromEvents |> step

        and dispatchScheduler : Concurrent.ScheduleConcurrent =
            Concurrent.createScheduler args.Interval dispatcher

        do
            args.Root
            |> Init
            |> eventsCache.Add

        do
            dispatchScheduler.Schedule()


        // todo: add the directory as the first event and dispatch

        [<CLIEvent>]
        member _.OnFileSystemEvent = onFilesystemEvent.Publish

    type Api =
        static member Watch(args : Args) = Manager(args)

    let watch root update =
        Api.Watch { Root = root; Update = update; Interval = TimeSpan.FromMilliseconds(500.0) }

[<AutoOpen>]
module SharpWatcherNonQualified =
    let watch = SharpWatcher.builder

    module Watch =
        let subscribe folder attributes : SharpWatcher.Subscription =
            {
                Folder = folder
                Attributes = attributes
            }