namespace Tdip.SharpWatcher

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

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

module Dictionary =
    let tryGet k (dictionary : Dictionary<'k, 'v>) =
        let mutable result = Unchecked.defaultof<'v>
        if dictionary.TryGetValue(k, &result)
        then Some result
        else None

    let tryRemove k (dictionary : Dictionary<'k, 'v>) =
        let result = tryGet k dictionary
        do dictionary.Remove(k) |> ignore
        result

module Concurrent =

    type ScheduleConcurrent(time : TimeSpan, action : unit -> unit) =
        let mutable ``lock`` = 0

        member _.Schedule() =
            if Interlocked.Increment(&``lock``) = 1
            then
                async {
                    do!
                        Task.Delay time
                        |> Async.AwaitTask
                    do
                        Interlocked.Decrement(&``lock``)
                        |> ignore
                    do action()
                }
                |> Async.StartAsTask
                |> ignore
            else
                Interlocked.Decrement(&``lock``)
                |> ignore

    let createScheduler time action = ScheduleConcurrent(time, action)

    let tryTakeMany (concurrentBag : ConcurrentBag<'a>) =
        let mutable results = []
        let mutable result = Unchecked.defaultof<'a>

        while concurrentBag.TryTake(&result) do
            results <- result :: results

        results :> seq<_>

module Seq =
    let filterMap mapper collection =
        collection
        |> Seq.collect(
            fun value ->
                match mapper value with
                | Some v -> [v]
                | None -> []
                :> seq<_>

        )

