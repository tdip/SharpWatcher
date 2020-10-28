namespace Tdip.SharpWatcher

open System
open System.Collections.Generic
open System.IO

module SharpWatcher =

    type WatchAttribute = ByExtension of string

    type Events = {
        Created : Set<string>
        Deleted : Set<string>
    } with
        static member FromCreated(events: seq<string>) =
            {
                Created = Set.ofSeq events
                Deleted = Set.empty
            }

    type EventsHandler = {
        OnCreated : FileSystemEventArgs -> unit
        OnDeleted : FileSystemEventArgs -> unit
    }

    type ISubscription =
        abstract ConsistentHash : int with get

        abstract Start : EventsHandler -> IDisposable

    type Subscription = {
        Folder : string
        Attributes : Set<WatchAttribute>
    }
    with
        interface ISubscription with
            member x.ConsistentHash
                with get() = x.Folder.GetHashCode()

            member x.Start(handler) =
                {
                    new IDisposable with
                        member _.Dispose() = ()
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
        Update : Events -> State -> Monad<State>
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

    type Manager(args: Args) =

        let init = Events.FromCreated [args.Root]
        let mutable state = Seq.empty
        let attributes = Dictionary<string, AttributesEntry>()
        let subscriptions = Dictionary<int, SubscriptionEntry>()

        let removeSubscription i =
            match Dictionary.tryRemove i subscriptions with
            | Some entry ->
                entry.Subscription.Dispose()
            | None ->
                failwithf "The index %i was expected to be in the dictionary" i

        let addSubscription (subscription: ISubscription) =
            ()

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
                                attributes.[entry.Folder] = { Subscription = entry }
                                |> ignore
                            | None ->
                                attributes.[entry.Folder] = { Subscription = entry }
                                |> ignore
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

        let step (events: Events) =
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


            ()

        do (args.Update init Seq.empty Context.Empty).Deconstruct(&context, &state)

    type Api =
        static member Watch(args : Args) = failwith ""

    let watch root update = Api.Watch { Root = root; Update = update }

[<AutoOpen>]
module SharpWatcherNonQualified =
    let watch = SharpWatcher.builder

    module Watch =
        let subscribe _ _ : SharpWatcher.Subscription = failwith ""

type WatchAttribute = ByExtension of string

type SharpWatcherEvent = {
    Event : FileSystemEventArgs
    Attributes : Option<FileAttributes>
} with
    static member FromEvent(e : FileSystemEventArgs) =
        {
            Event = e
            Attributes =
                try
                    File.GetAttributes(e.FullPath)
                    |> Some
                with
                    :? FileNotFoundException -> None
        }

type SharpWatcherUpdateContext = {
    Events : list<SharpWatcherEvent>
}

type SharpWatcherContext = obj

type SharpWatcherMonad<'a> = SharpWatcherContext -> SharpWatcherContext*'a

type SharpWatcherFileSystem = FileSystem<list<WatchAttribute>>

type private WatchEntry = {
    Location : string
    Watcher : FileSystemWatcher
    mutable WatchAttributes : list<WatchAttribute>
} with

    interface IDisposable with
        member x.Dispose() = x.Watcher.Dispose()

type SharpWatcher(
    initial: SharpWatcherFileSystem,
    update: SharpWatcherUpdateContext -> SharpWatcherMonad<SharpWatcherFileSystem>) =

    let watchers = Dictionary<string, WatchEntry>()

    let onEvent (fse: SharpWatcherEvent) = ()

    let removeIfExisting (path: string) =
        match Dictionary.tryGet path watchers with
        | Some entry ->
            (entry :> IDisposable).Dispose()
            watchers.Remove(path)
            |> ignore
        | None -> ()

    let onDeleted (event: SharpWatcherEvent) =
        removeIfExisting event.Event.FullPath
        onEvent event

    let onCreated (event: SharpWatcherEvent) =
        removeIfExisting event.Event.FullPath


    let updateRegistrations (path: string, attributes: list<WatchAttribute>) =
        match Dictionary.tryGet path watchers with
        | Some watchEntry ->
            watchEntry.WatchAttributes <- attributes
        | None ->
            let watcher = new FileSystemWatcher(path)

            (*
            watcher.Deleted.Add(SharpWatcherEvent.FromEvent >> onEvent)
            watcher.Created.Add(onEvent)
            watcher.Changed.Add(onEvent)
            watcher.Renamed.Add(onEvent)

            watcher.EnableRaisingEvents <- true
            *)

            watchers.[path] = { Location = path; Watcher = watcher; WatchAttributes = attributes }
            |> ignore


    let rec listMonitoredFolders fs =
        match fs with
        | Directory(path, attributes, items) ->
            (path, attributes) :: List.collect listMonitoredFolders (failwith "")