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

        let (result, _) =
            Evaluator.setScopeAttribute watch Evaluator.empty

        let listAttributes =
            let scopeSem _ _ = sharpFold.Return ()
            let ignoreSem _ =
                sharpFold {
                    let! scopeAttribute = SharpFold.getAttribute<list<SharpWatcherPath>>()
                    return printfn "attribute %A" scopeAttribute
                }
            let raiseSem _ _ = sharpFold.Return ()

            SharpFold.foldM scopeSem ignoreSem raiseSem


        result.Pipe() |> listAttributes watch
        |> printfn "kaiser %A"

        Assert.True(true)
