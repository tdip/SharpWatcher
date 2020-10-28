namespace Tdip.SharpWatcher

open System
open Xunit

module Tests =

    [<Fact>]
    let ``test watch directory`` () =

        let ignored = failwith ""

        let update (events: SharpWatcher.Events) (state : SharpWatcher.State) : SharpWatcher.Monad<SharpWatcher.State> =
            watch {
                let! newWatchers =
                    watch {
                        for folder in events.Created do
                            if Set.contains folder ignored |> not
                            then
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

        Assert.True(true)
