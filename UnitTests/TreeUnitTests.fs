module TreeUnitTests

open System
open FsCheck
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsCheck.NUnit
open OCW6006

module ImmutableBinarySearchTree =

    open Trees

    [<Property>]
    let createAndRetrieveSortedList (NonEmptyArray input: NonEmptyArray<int * int>) =
        let inputList = List.ofArray input
        let sortedByKeys = List.sortByDescending fst inputList |> List.map snd

        ImmutableBinarySearchTree.ofList inputList
        |> ImmutableBinarySearchTree.inorderAccumulation
        |> (=) sortedByKeys

    [<Property>]
    let tryFind (NonEmptyArray input: NonEmptyArray<int * int>) =
        let inputList = List.ofArray input |> List.distinctBy fst

        let tree = inputList |> ImmutableBinarySearchTree.ofList

        for key, value in inputList do
            match ImmutableBinarySearchTree.tryFind key tree with
            | Some v -> v = value
            | None -> false
            |> Assert.True

    [<Property>]
    let first (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = List.ofArray input

        List.zip inputList inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.first
        |> ImmutableBinarySearchTree.value
        |> (=) (List.min inputList)

    [<Property>]
    let last (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = List.ofArray input

        List.zip inputList inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.last
        |> ImmutableBinarySearchTree.value
        |> (=) (List.max inputList)

    [<Test>]
    let trySuccessorOnSingleton () =
        ImmutableBinarySearchTree.createRoot (0, 0)
        |> ImmutableBinarySearchTree.trySuccessor
        |> Option.isNone
        |> Assert.True

    [<Property>]
    let trySuccessorOnLast (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = List.ofArray input

        let tree = List.zip inputList inputList |> ImmutableBinarySearchTree.ofList
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
        let inputList = List.ofArray input

        List.zip inputList inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.first
        |> ImmutableBinarySearchTree.tryPredecessor
        |> Option.isNone

    [<Property>]
    let tryPredecessor (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = List.ofArray input |> List.distinct

        let sortedInputs = List.sort inputList

        let predecessor =
            let index = sortedInputs |> List.findIndex ((=) inputList[0])

            match index with
            | 0 -> None
            | i -> Some sortedInputs[i - 1]

        List.zip inputList inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.tryPredecessor
        |> Option.map ImmutableBinarySearchTree.value
        |> (=) predecessor
        |> Assert.True

    [<Property>]
    let trySuccessor (NonEmptyArray input: NonEmptyArray<int>) =
        let inputList = List.ofArray input |> List.distinct
        let sortedInputs = List.sort inputList

        let predecessor =
            let index = sortedInputs |> List.findIndex ((=) inputList[0])

            if index = inputList.Length - 1 then
                None
            else
                Some sortedInputs[index + 1]

        List.zip inputList inputList
        |> ImmutableBinarySearchTree.ofList
        |> ImmutableBinarySearchTree.trySuccessor
        |> Option.map ImmutableBinarySearchTree.value
        |> (=) predecessor
        |> Assert.True
