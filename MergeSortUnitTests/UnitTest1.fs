module MergeSortUnitTests

open NUnit.Framework
open FsCheck.NUnit

[<SetUp>]
let Setup () =
    ()

[<Property>]
let BottomUpMerge (A: int[]) =
    printfn $"input array %A{A}"
    let sorted = Array.sort A
    MergeSort.BottomUp.sortInPlace A
    printfn $"output array %A{A}"
    A = sorted
