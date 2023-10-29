module MergeSortUnitTests

open NUnit.Framework
open FsCheck.NUnit

[<SetUp>]
let Setup () = ()

[<Property>]
let TopDownMergeSortedRuns (left: int[]) (right: int[]) =
    Array.sortInPlace left
    Array.sortInPlace right
    let A = Array.concat [| left; right |]
    let B = Array.zeroCreate A.Length
    MergeSort.TopDown.topDownMergeSortedRuns A B 0 left.Length A.Length
    printfn $"left %A{left}"
    printfn $"right %A{right}"
    printfn $"input array %A{A}"
    printfn $"output array %A{B}"
    B = Array.sort A

[<Property>]
let TopDownMergeLengthTwo a b =
    let A = [| a; b |]
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
