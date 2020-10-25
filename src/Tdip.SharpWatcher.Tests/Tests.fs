namespace Tdip.SharpWatcher

open System
open Xunit

module Tests =

    [<Fact>]
    let ``test watch directory`` () =

        (*
        let watch =

            Root (
                "/home/user/workspace",
                [
                    Scope(
                        AllDirectories </> ByDirectory "node-modules" </> AllDirectories,
                        [
                            Ignore AnyFilesystemElement
                        ]
                    ),
                    Raise (AllDirectories </> ByExtension "json") [Create, Modify, Rename, Delete]
                ]
            )
        *)

        let watch =
            Scope (
                [ByDirectory "home"; ByDirectory "neto"],
                [
                    Scope (
                        [ByDirectory "folder"],
                        [
                            Ignore [AllDirectories]
                        ]
                    )
                    Scope (
                        [ByDirectory "folder2"],
                        [
                            Ignore [AllDirectories]
                        ]
                    )
                ]
            )

        let result =
            Evaluator.setScopeAttribute watch Evaluator.empty

        printfn "kaiser %A" result

        Assert.True(true)
