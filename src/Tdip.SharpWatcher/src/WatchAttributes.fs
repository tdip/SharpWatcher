namespace Tdip.SharpWatcher

open System.Collections.Generic
open System.Runtime.CompilerServices

[<AutoOpen>]
module WatchAttributes =

    type WatchAttribute =
        | ByExtension of string
    with
        static member Match (e: FileSystemEventEntry) (self: WatchAttribute) =
            match self with
            | ByExtension ext -> ext = e.Extension


[<AutoOpen>]
module FolderIndex =

    let makeCanonical (key : string) =
        if key.EndsWith("/")
        then key
        else key + "/"

    [<Extension>]
    type FolderIndexExtensions =

        [<Extension>]
        static member AddFolder(index : IDictionary<string, 'v>, key: string, value: 'v) =

            index.[makeCanonical key]  <- value

        [<Extension>]
        static member TryGetFolder(index : IDictionary<string, 'v>, key : string) =
            None