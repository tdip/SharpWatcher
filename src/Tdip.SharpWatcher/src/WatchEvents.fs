namespace Tdip.SharpWatcher

open System.Collections.Generic
open System.IO

[<AutoOpen>]
module WatchEvents =

    type FileSystemEventEntry = {
        EventArgs : FileSystemEventArgs
        Attributes : Option<FileAttributes>
    } with
        member x.Extension with get() =
            Path.GetExtension(x.EventArgs.FullPath)

        member x.HasAttribute(attribute : FileAttributes) =
            match x.Attributes with
            | Some attributes -> attributes.HasFlag(attribute)
            | None -> false

        member x.IsChangeType(change: WatcherChangeTypes) =
            x.EventArgs.ChangeType.HasFlag(change)

    type WatchEvent =
        | FileSystem of FileSystemEventEntry
        | Init of string
    with

        static member FromEvent(event: FileSystemEventArgs) =
            let attributes =
                if event.ChangeType.HasFlag(WatcherChangeTypes.Deleted)
                then None
                else
                    File.GetAttributes(event.FullPath)
                    |> Some

            FileSystem { EventArgs = event; Attributes = attributes }

        static member FromExistingFile(path: string) =
            let attributes =
                File.GetAttributes path
                |> Some
            let name = Path.GetFileName path
            let directory = Path.GetDirectoryName path
            let eventArgs =
                FileSystemEventArgs(WatcherChangeTypes.Created, directory, name)
            { EventArgs = eventArgs; Attributes = attributes }

        member x.AsFileSystem with get() =
            match x with
            | FileSystem e -> Some e
            | _ -> None

    let (|Created|Deleted|Ignored|) (index, watch : WatchEvent) : Choice<string, string, unit> =
        match watch with
        | FileSystem e when e.EventArgs.ChangeType.HasFlag(WatcherChangeTypes.Deleted) ->
            match Dictionary.tryGet e.EventArgs.FullPath index with
            | Some _ -> Choice2Of3(e.EventArgs.FullPath)
            | _ -> Choice3Of3 ()
        | FileSystem e when e.IsChangeType(WatcherChangeTypes.Created) && e.HasAttribute(FileAttributes.Directory) ->
            Choice1Of3 e.EventArgs.FullPath
        | Init s ->
            Choice1Of3 s
        | _ -> Choice3Of3 ()

    type Notification = {
        Created : Set<string>
        Deleted : Set<string>
    } with

        static member FromCreated(events: seq<string>) =
            {
                Created = Set.ofSeq events
                Deleted = Set.empty
            }

        static member FromEvents(index, evs: seq<WatchEvent>) : Notification =

            let aggregator (created, deleted) event =
                match index, event with
                | Created f ->
                    (Set.add f created, deleted)
                | Deleted f ->
                    (created, Set.add f deleted)
                | _ ->
                    (created, deleted)

            let (created, deleted) =
                evs
                |> Seq.fold aggregator (Set.empty, Set.empty)

            {
                Created = created
                Deleted = deleted
            }

