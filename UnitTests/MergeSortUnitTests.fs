module MergeSortUnitTests

open System
open NUnit.Framework
open FsCheck.NUnit
open OCW6006

[<SetUp>]
let Setup () = ()

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
