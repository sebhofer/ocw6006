namespace MergeSort

open System

module TopDown =

    /// Merge two sorted runs from source into target
    let topDownMergeSortedRuns (left: ReadOnlySpan<int>) (right: ReadOnlySpan<int>) (target: Span<int>) =
        let mutable i = left.Length - 1 // index left run
        let mutable j = right.Length - 1 // index right run
        let mutable k = target.Length - 1 // index in A

        while k >= 0 do
            if i >= 0 && (j < 0 || left[i] > right[j]) then
                target[k] <- left[i]
                i <- i - 1
            else
                target[k] <- right[j]
                j <- j - 1

            k <- k - 1

    /// target and aux must be called with the same contents. target will be sorted in-place.
    let rec private topDownSplitMergeInPlace (aux: Span<int>) (target: Span<int>) =
        let len = aux.Length
        if len <= 1 then
            ()
        else
            let delta = len / 2
            // create sorted left and right runs in source
            topDownSplitMergeInPlace (target.Slice(0, delta)) (aux.Slice(0, delta))
            topDownSplitMergeInPlace (target.Slice(delta, len - delta)) (aux.Slice(delta, len - delta))
            // merge sorted runs into target. target is fixed by top-level call!
            topDownMergeSortedRuns (aux.Slice(0, delta)) (aux.Slice(delta, len - delta)) target

    let sortInPlace (A: int[]) =
        let B = Array.copy A
        topDownSplitMergeInPlace (Span B) (Span A)

module BottomUp =

    /// Merge runs in A into B
    let bottomUpMerge (A: int[]) (B: int[]) startInclusive middle endExclusive =
        let mutable i = startInclusive
        let mutable j = middle
        let mutable k = startInclusive

        while k < endExclusive do
            if i < middle && (j >= endExclusive || A[i] <= A[j]) then
                B[k] <- A[i]
                i <- i + 1
            else
                B[k] <- A[j]
                j <- j + 1

            k <- k + 1

    let private bottomUpMergeSort (A: int[]) (B: int[]) n =
        let mutable width = 1
        let mutable source = A
        let mutable target = B

        while width < n do
            let mutable i = 0

            while i < n do
                bottomUpMerge source target i (min (i + width) n) (min (i + 2 * width) n)
                i <- i + 2 * width

            width <- 2 * width

            // swap source and target to avoid copying
            let source' = source
            source <- target
            target <- source'

        // at this point source is the sorted array
        if not (LanguagePrimitives.PhysicalEquality source A) then
            Array.blit source 0 target 0 source.Length

    let sortInPlace (A: int[]) =
        let B = Array.zeroCreate A.Length
        bottomUpMergeSort A B A.Length

module Program =

    [<EntryPoint>]
    let main _ =
        System.Collections.Generic.Dictionary()
        for A in [ [| 1; 0 |]; [| 2; 1; 0 |]; [| 3; 2; 1; 0 |]; [| 10..-1..0 |] ] do
            let B = Array.copy A
            BottomUp.sortInPlace A
            TopDown.sortInPlace B
            printfn $"bottom up result %A{A}"
            printfn $"top down result %A{B}"

        0
