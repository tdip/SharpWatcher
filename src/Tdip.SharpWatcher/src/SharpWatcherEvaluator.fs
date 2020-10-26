namespace Tdip.SharpWatcher

open System

module Evaluator =

    [<CustomEquality>]
    [<CustomComparison>]
    type AttributeKey = AttributeKey of Type
        with
            override x.Equals(y : obj) =
                let (AttributeKey ty) = x
                let (AttributeKey ty') = y :?> AttributeKey
                ty.Equals(ty')

            override x.GetHashCode() =
                let (AttributeKey ty) = x
                ty.GetHashCode()

            interface IComparable with
                member x.CompareTo(y) =
                    let (AttributeKey ownType) = x
                    let (AttributeKey otherType) = y :?> AttributeKey

                    ownType.GUID.CompareTo(otherType.GUID)

    type AttributesCollection = Map<AttributeKey, obj>

    type EvaluatorAttributes = {
        Attributes : Map<int, AttributesCollection>
    } with
        member x.UpdateAttribute i mapper =
            let attr =
                match Map.tryFind i x.Attributes with
                | Some attr -> attr
                | _ -> Map.empty
            {
                x with
                    Attributes = Map.add i (mapper attr) x.Attributes
            }


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

        member x.Pipe() =
            {
                x with
                    Current = 0
            }

    let empty =
        {
            Attributes = { Attributes = Map.empty }
            Current = 0
            AttributesHierarchy = Map.empty
        }

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

    let iGetAttribute<'t> i (context: EvaluatorContext) =
        let key = AttributeKey typeof<'t>
        let attribute =
            match Map.tryFind i context.Attributes.Attributes with
            | Some attributes ->
                Map.tryFind key attributes
            | _ -> None
        (context, Option.map (fun (v: obj) -> v :?> 't) attribute)

    let getAttribute<'t> () =
        builder {
            let! i = getCurrentId()
            let! attribute = iGetAttribute<'t> i
            return attribute
        }

    let rec foldM scope ignoreFn raiseFn value =
        match value with
        | Scope (path, items) ->
            builder{
                let! previous = builder {
                    for item in items do
                        let! value = foldM scope ignoreFn raiseFn item
                        let! current = getCurrentId()
                        yield (value, current)
                }
                do! incrementCurrent()
                let childIds = previous |> Seq.map (fun (_, i) -> i)
                let childs =
                    previous
                    |> Seq.map (fun (child, _) -> child)
                    |> Seq.toList
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

    let attributeMapper (empty : 't) (mapper : 't -> 't) (attributes: AttributesCollection) =
        let key = AttributeKey typeof<'t>
        let newValue =
            match Map.tryFind key attributes with
            | Some attribute ->
                mapper (attribute :?> 't) :> obj
            | _ -> empty :> obj
        Map.add key newValue attributes

    let setScopeAttribute =

        let scopeSem path state =
            builder {

                do! mapChildAttributesRec (attributeMapper path (fun prev -> path </> prev))
                return Scope(path, state)
            }

        let ignoreSem path = builder.Return (Ignore path)

        let raiseSem path events = builder.Return (Raise(path, events))

        foldM scopeSem ignoreSem raiseSem

[<AutoOpen>]
module EvaluatorUnqualified =

    module SharpFold =

        let foldM = Evaluator.foldM

        let getAttribute<'t> = Evaluator.getAttribute<'t>

    let sharpFold = Evaluator.builder