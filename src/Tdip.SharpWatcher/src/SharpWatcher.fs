namespace Tdip.SharpWatcher

type SharpWatcherEventDescriptor =
    | Create
    | Modify
    | Rename
    | Delete

type SharpWatcherPath =
    | AllDirectories
    | ByDirectory of string
    | ByExtension of string
    | AllElements

type SharpWatcherSemantics =
    | Scope of List<SharpWatcherPath>*List<SharpWatcherSemantics>
    | Ignore of List<SharpWatcherPath>
    | Raise of List<SharpWatcherPath>*List<SharpWatcherEventDescriptor>

[<AutoOpen>]
module SharpWatcherOperations =

    let (</>) p1 p2 = List.concat [p1; p2]