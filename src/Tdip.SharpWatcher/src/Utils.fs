namespace Tdip.SharpWatcher

module Option =

    type Builder() =

        member _.Return(value) = Some(value)

        member _.Bind(m, f) =
            match m with
            | Some v -> f v
            | None -> None

        member _.ReturnFrom(m) = m

    let builder = Builder()

    let verify cond =
        if cond
        then Some()
        else None

[<AutoOpen>]
module OptionUtilsUnQualified =
    let maybe = Option.builder

module Indexed =

    let lookup i (arr : 'a[]) =
        if i < arr.Length
        then Some(arr.[i])
        else None
