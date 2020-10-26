namespace Tdip.SharpWatcher

open System.IO

module Matcher =

    type MatchContext = {
        WatchPath : SharpWatcherPath[]
        WatchPathPosition : int
        MatchPath : string[]
        MatchPathPosition : int
    } with

        member x.WatchPathLookup(i) =
            Indexed.lookup (x.WatchPathPosition + i) x.WatchPath

        member x.MatchPathLookup(i) =
            Indexed.lookup (x.MatchPathPosition + 1) x.MatchPath

        member x.RemainingPathItems
            with get() = x.MatchPath.Length - x.MatchPathPosition

    let (|LookAhead2|_|) (context : MatchContext) =
        maybe {
            let! currentWatch = context.WatchPathLookup(0)
            let! nextWatch = context.WatchPathLookup(1)
            let! currentMatch = context.MatchPathLookup(0)
            let! nextMatch = context.MatchPathLookup(1)
            return (currentWatch, nextWatch, currentMatch, nextMatch)
        }

    let (|LookAhead1|_|) (context : MatchContext) =
        maybe {
            let! currentWatch = context.WatchPathLookup(0)
            let! currentMatch = context.MatchPathLookup(0)
            return (currentWatch, currentMatch)
        }

    let step (state: MatchContext) =
        match state with
        | LookAhead2(AllDirectories, ByDirectory directory, current, _) when current = directory ->
            {
                state with
                    WatchPathPosition = state.WatchPathPosition + 1
            }
        | LookAhead2(AllDirectories, _, _, _) when state.RemainingPathItems = 2 ->
            {
                state with
                    WatchPathPosition = state.WatchPathPosition + 1
                    MatchPathPosition = state.MatchPathPosition + 1
            }
    


    