module MergeSort =

    /// Merge two sorted runs from B into A
    let topDownMerge (A: int[]) (B: int[]) startInclusive middle endExclusive =
        let mutable i = middle - 1// index left run
        let mutable j = endExclusive - 1 // index right run
        let mutable k = endExclusive - 1 // index in A

        while k >= startInclusive do
            if i >= 0 && (j <= middle || B[i] > B[j]) then
                A[k] <- B[i]
                i <- i - 1
            elif j >= 0 || i < 0 then
                A[k] <- B[j]
                j <- j - 1
            k <- k - 1

    /// Split A[] into 2 runs, sort both runs into B[], merge both runs from B[] to A[]
    let rec private topDownSplitMergeInPlace (B: int[]) (A: int[]) startInclusive endExclusive =
        if endExclusive - startInclusive <= 1 then
            ()
        else
            let middle = (startInclusive + endExclusive) / 2
            topDownSplitMergeInPlace A B startInclusive middle
            topDownSplitMergeInPlace A B middle endExclusive
            topDownMerge B A startInclusive middle endExclusive

    let sortInPlace (A: int[]) =
        let B = Array.copy A
        topDownSplitMergeInPlace A B 0 A.Length

[<EntryPoint>]
let main _ =

    let B = [| 1; 0 |]
    let A = Array.zeroCreate B.Length
    MergeSort.topDownMerge B A 0 1 B.Length
    printfn $"A %A{A}"
    
    let A = [| 3 .. -1 .. 0 |]
    MergeSort.sortInPlace A
    printfn $"array %A{A}"
    
    0
