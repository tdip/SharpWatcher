module Tests

open System
open Xunit

[<Fact>]
let ``test watch directory`` () =

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

    Assert.True(true)
