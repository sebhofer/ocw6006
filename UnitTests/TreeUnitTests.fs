module TreeUnitTests

open FsCheck
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsCheck.NUnit

module ImmutableBinarySearchTree =

    open Trees

    let createInputList (inputs: int[]) =
        let rand = System.Random()
        inputs |> Array.map (fun e -> rand.Next(), e) |> List.ofArray

    [<Property>]
    let createAndRetrieveSortedList (NonEmptyArray input: NonEmptyArray<int * int>) =
        let inputList = List.ofArray input
        let sortedByKeys = List.sortByDescending fst inputList |> List.map snd

        ImmutableBinarySearchTree.ofList inputList
        |> ImmutableBinarySearchTree.inorderAccumulation
        |> (=) sortedByKeys

    [<Property>]
    let tryFind (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input
        let tree = inputList |> ImmutableBinarySearchTree.ofList

        for key, value in inputList do
            match ImmutableBinarySearchTree.tryFind key tree with
            | Some v -> v.Value = value
            | None -> false
            |> Assert.True

    [<Property>]
    let first (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input

        inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.first
        |> ImmutableBinarySearchTree.value
        |> (=) (List.minBy fst inputList |> snd)

    [<Property>]
    let last (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input

        inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.last
        |> ImmutableBinarySearchTree.value
        |> (=) (List.maxBy fst inputList |> snd)

    [<Test>]
    let trySuccessorOnSingleton () =
        ImmutableBinarySearchTree.createRoot (0, 0)
        |> ImmutableBinarySearchTree.trySuccessor
        |> Option.isNone
        |> Assert.True

    [<Property>]
    let trySuccessorOnLast (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input

        let tree = inputList |> ImmutableBinarySearchTree.ofList
        let lastNode = tree |> ImmutableBinarySearchTree.last
        let successor = lastNode |> ImmutableBinarySearchTree.trySuccessor

        successor |> Option.isNone |> Assert.True

    [<Test>]
    let tryPredecessorOnSingleton () =
        ImmutableBinarySearchTree.createRoot (0, 0)
        |> ImmutableBinarySearchTree.tryPredecessor
        |> Option.isNone
        |> Assert.True

    [<Property>]
    let tryPredecessorOnFirst (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input

        inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.first
        |> ImmutableBinarySearchTree.tryPredecessor
        |> Option.isNone

    [<Property>]
    let tryPredecessor (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input
        let sortedInputs = List.sort inputList

        let predecessor =
            let index = sortedInputs |> List.findIndex ((=) inputList[0])

            match index with
            | 0 -> None
            | i -> Some(snd sortedInputs[i - 1])

        inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.tryPredecessor
        |> Option.map ImmutableBinarySearchTree.value
        |> (=) predecessor
        |> Assert.True

    [<Property>]
    let trySuccessor (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = createInputList input
        let sortedInputs = List.sort inputList

        let predecessor =
            let index = sortedInputs |> List.findIndex ((=) inputList[0])

            if index = inputList.Length - 1 then
                None
            else
                Some(snd sortedInputs[index + 1])

        inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.trySuccessor
        |> Option.map ImmutableBinarySearchTree.value
        |> (=) predecessor
        |> Assert.True

    [<Property>]
    let delete (NonEmptyArray input: NonEmptyArray<int>) =
        let rand = System.Random(666)
        let inputList = (rand.Next(), rand.Next()) :: createInputList input
        let sortedInputs = List.sort inputList

        let tree = inputList |> ImmutableBinarySearchTree.ofList

        for i in 0 .. sortedInputs.Length - 1 do
            let key, _ = sortedInputs[i]
            let shortenedList = List.removeAt i sortedInputs |> List.map snd

            let prunedTreeList =
                tree
                |> ImmutableBinarySearchTree.delete key
                |> ImmutableBinarySearchTree.inorderAccumulation

            shortenedList = prunedTreeList |> Assert.True
