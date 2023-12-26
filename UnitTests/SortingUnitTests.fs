module SortingUnitTests

open System
open FsCheck
open NUnit.Framework
open FsCheck.NUnit
open OCW6006

module CountingSort =

    [<Test>]
    let countingSortIsCorrect () =
        let A = [| 5; 3; 4; 1; 10; 11; 9 |]
        printfn $"input array %A{A}"
        let sorted = Array.sort A
        let B = CountingSort.sort (Array.max A) id A
        printfn $"output array %A{B}"
        B = sorted |> Assert.True

    [<Test>]
    let countingSortIsStable () =
        for m in [ 1; 2; 3 ] do
            let A = [| 5; 3; 4; 1; 10; 11; 9; 8 |]
            printfn $"input array %A{A}"
            let B1 = CountingSort.sort 2 (fun v -> v % m) A
            let B2 = CountingSort.sort 2 (fun v -> v % m) B1
            printfn $"output array 1 %A{B1}"
            printfn $"output array 2 %A{B2}"
            B1 = B2 |> Assert.True


module MergeSort =

    [<Property>]
    let TopDownMergeSortedRuns (left: int[]) (right: int[]) =
        Array.sortInPlace left
        Array.sortInPlace right
        let B = Array.zeroCreate (left.Length + right.Length)
        MergeSort.TopDown.topDownMergeSortedRuns (ReadOnlySpan left) (ReadOnlySpan right) (Span B)
        printfn $"left %A{left}"
        printfn $"right %A{right}"
        printfn $"output array %A{B}"
        B = Array.sort (Array.concat [| left; right |])

    [<Property>]
    let TopDownMergeLengthTwo a b =
        let A = [| a; b |]
        printfn $"input array %A{A}"
        let sorted = Array.sort A
        MergeSort.TopDown.sortInPlace A
        printfn $"output array %A{A}"
        A = sorted

    [<Test>]
    let TopDownMergeLengthThree () =
        let A = [| 3; 2; 1 |]
        let sorted = Array.sort A
        MergeSort.TopDown.sortInPlace A
        printfn $"output array %A{A}"
        A = sorted |> Assert.True

    [<Property>]
    let TopDownMerge (A: int[]) =
        printfn $"input array %A{A}"
        let sorted = Array.sort A
        MergeSort.TopDown.sortInPlace A
        printfn $"output array %A{A}"
        A = sorted

    [<Property>]
    let BottomUpMerge (A: int[]) =
        printfn $"input array %A{A}"
        let sorted = Array.sort A
        MergeSort.BottomUp.sortInPlace A
        printfn $"output array %A{A}"
        A = sorted
