namespace Tdip.SharpWatcher

open System

module Evaluator =

    type AttributeKey = AttributeKey of Type
        with
            interface IComparable with
                member x.CompareTo(y) =
                    let (AttributeKey ownType) = x
                    let (AttributeKey otherType) = y :?> AttributeKey

                    ownType.GUID.CompareTo(otherType.GUID)

    type AttributesCollection = Map<AttributeKey, obj>

    type EvaluatorAttributes = {
        Attributes : Map<int, obj>
    } with
        member x.UpdateAttribute i mapper =
            match Map.tryFind i x.Attributes with
            | Some attr ->
                {
                    x with
                        Attributes = Map.add i (mapper attr) x.Attributes
                }
            | _ -> x

    type EvaluatorContext = {
        Attributes : EvaluatorAttributes
        Current : int
        AttributesHierarchy : Map<int, seq<int>>
    } with
        member x.MapChildAttributes i mapping =
            match Map.tryFind i x.AttributesHierarchy with
            | Some is ->
                let attributes =
                    Seq.fold
                        (fun (stalin : EvaluatorAttributes) i' -> stalin.UpdateAttribute i' mapping)
                        x.Attributes
                        is
                { x with Attributes = attributes }
            | _ -> x


        member x.MapChildAttributesRec i mapping =
            let updatedAttributes = x.MapChildAttributes i mapping
            match  Map.tryFind i x.AttributesHierarchy with
            | Some is ->
                Seq.fold
                    (fun (stalin : EvaluatorContext) i' -> stalin.MapChildAttributesRec i' mapping)
                    updatedAttributes
                    is
            | _ -> updatedAttributes

    type Monad<'t> = EvaluatorContext -> EvaluatorContext*'t

    let incrementCurrent () context =
        ({context with Current = context.Current + 1}, ())

    let getCurrentId () context =
        (context, context.Current)

    let updateHierarchy ids (context : EvaluatorContext) =
        let context' =
            {
                context with
                    AttributesHierarchy = Map.add context.Current ids context.AttributesHierarchy
            }
        (context', ())

    type Builder() = 

        member _.Return(value) (context: EvaluatorContext) = (context, value)

        member x.Yield(a) = x.Return a

        member _.Bind(m : Monad<'a>, f: 'a -> Monad<'b>) (context: EvaluatorContext) =
            let (context', value) = m context
            f value context'

        member monad.For(items, f) =

            let aggregator stalin value =
                monad {
                    let! values = stalin
                    let! next = f value
                    return Seq.append values [next]
                }

            Seq.fold aggregator (monad.Return Seq.empty) items

    let builder = Builder()

    let iMapChildAttributes i mapper (context: EvaluatorContext) =
        (context.MapChildAttributes i mapper, ())

    let iMapChildAttributesRec i mapper (context: EvaluatorContext) =
        (context.MapChildAttributesRec i mapper, ())

    let mapChildAttributes mapper =
        builder {
            let! i = getCurrentId()
            do! iMapChildAttributes i mapper
        }

    let mapChildAttributesRec mapper =
        builder {
            let! i = getCurrentId()
            do! iMapChildAttributesRec i mapper
        }

    let rec foldM value scope ignoreFn raiseFn =
        match value with
        | Scope (path, items) ->
            builder{
                let! previous = builder {
                    for item in items do
                        let! value = foldM item scope ignoreFn raiseFn
                        let! current = getCurrentId()
                        yield (value, current)
                }
                do! incrementCurrent()
                let childIds = previous |> Seq.map (fun (_, i) -> i)
                let childs = previous |> Seq.map (fun (child, _) -> child)
                do! updateHierarchy childIds
                let! next = scope path childs
                return next
            }
        | Ignore path ->
            builder {
                do! incrementCurrent()
                let! next = ignoreFn path
                return next
            }
        | Raise (path, events) ->
            builder {
                do! incrementCurrent()
                let! next = raiseFn path events
                return next
            }

    let setScopeAttribute value =

        let scopeSem path state =
            builder {

                do! 
                return Scope(path, state)
            }

        failwith ""