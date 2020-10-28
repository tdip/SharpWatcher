namespace Tdip.SharpWatcher

open System.IO

[<AutoOpen>]
module WatchEvents =

    type FileSystemEventEntry = {
        EventArgs : FileSystemEventArgs
        IsDirectory : bool
    } with
        member x.Extension with get() =
            Path.GetExtension(x.EventArgs.FullPath)

        member x.Directory with get() =
            if x.IsDirectory
            then x.EventArgs.FullPath
            else
                Path.GetDirectoryName(x.EventArgs.FullPath)

    type WatchEvent =
        | FileSystem of FileSystemEventEntry
    with
        member x.AsFileSystem with get() =
            match x with
            | FileSystem e -> Some e

    type Notification = {
        Created : Set<string>
        Deleted : Set<string>
    } with

        static member FromCreated(events: seq<string>) =
            {
                Created = Set.ofSeq events
                Deleted = Set.empty
            }

        static member FromEvents(evs: seq<WatchEvent>) : Notification = failwith ""

    type WatchAttribute =
        | ByExtension of string
    with
        static member Match (e: FileSystemEventEntry) (self: WatchAttribute) =
            match self with
            | ByExtension ext -> ext = e.Extension

