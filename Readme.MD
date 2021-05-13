# SharpWatcher

A very sharp filesystem event watcher for F# and .Net.

## Usage

This project attempts to apply the ideas from functional reactive programming to the problem of monitoring filesystem events. This allows to easily update the rules being applied to a filesystem event watcher depending on the events and changes that happen in the monitored files.

### Example: Monitoring *.json files in a directory

```
open System
open Tdip.SharpWatcher

[<EntryPoint>]
let main args =
    let allFolders  (events: Notification) (state : SharpWatcher.State) : SharpWatcher.Monad<SharpWatcher.State> =
        watch {
            let! newWatchers =
                watch {
                    for folder in events.Created do
                        // if Set.contains folder ignored |> not
                        // then
                        yield Watch.subscribe folder [ByExtension "json"]
                }
            let! remainingWatchers =
                watch {
                    let remaining =
                        Seq.filter
                            (fun (i : SharpWatcher.Subscription) -> Set.contains i.Folder events.Deleted |> not)
                            state
                    for subscription in remaining do
                        yield subscription
                }
            return Seq.concat [newWatchers; remainingWatchers]

    let watcher = SharpWatcher.watch testsDirectory allFolders

    watcher.OnFileSystemEvents.Add(
        fun (_, e) -> printfn "An event! %A" e
    )

    Console.ReadLine()
    0
```

