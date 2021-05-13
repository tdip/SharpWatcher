namespace Tdip.SharpWatcher

open System
open System.IO
open System.Threading.Tasks
open Xunit

module Tests =

    let createTestingDirectory() =
        let folderName = Guid.NewGuid()
        let tmp = Path.GetTempPath()
        Directory.CreateDirectory(
            Path.Combine(tmp, folderName.ToString())
        )

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
        }

    [<Fact>]
    let ``test create new file event`` () =

        let testsDirectory = createTestingDirectory().FullName
        let newFileName = "file.json"

        let tcs = TaskCompletionSource<bool>()
        let watcher = SharpWatcher.watch testsDirectory allFolders
        watcher.OnFileSystemEvents.Add(
            fun (e : seq<FileSystemEventArgs>) ->
                try
                    let event = Seq.head e
                    let expected = Path.Combine(testsDirectory, newFileName)
                    Xunit.Assert.Equal(expected, event.FullPath)
                    tcs.SetResult(true)
                with
                    e -> tcs.SetException(e) 
        )

        File.Create(Path.Combine(testsDirectory, newFileName))
        |> ignore

        tcs.Task

    [<Fact>]
    let ``test create new directory`` () =
        let testsDirectory = createTestingDirectory().FullName
        let newFolderName = "folder1"
        let newFileName = "file.json"

        let tcs = TaskCompletionSource<bool>()
        let watcher = SharpWatcher.watch testsDirectory allFolders

        watcher.OnFileSystemEvents.Add(
            fun (e : seq<FileSystemEventArgs>) ->
                try
                    let event = Seq.head e
                    let expected = Path.Combine(testsDirectory, newFolderName, newFileName)
                    Xunit.Assert.Equal(expected, event.FullPath)
                    tcs.SetResult(true)
                with
                    e -> tcs.SetException(e) 
            )

        let newDirectory =
            Directory.CreateDirectory(
                Path.Combine(testsDirectory, newFolderName)
            ).FullName

        File.Create(Path.Combine(newDirectory, newFileName))
        |> ignore

        tcs.Task

        