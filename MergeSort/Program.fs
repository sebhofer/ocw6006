module MergeSort =

    module TopDown =

        /// Merge two sorted runs from B into A
        let topDownMerge (B: int[]) (A: int[]) startInclusive middle endExclusive =
            let mutable i = middle - 1 // index left run
            let mutable j = endExclusive - 1 // index right run
            let mutable k = endExclusive - 1 // index in A

            while k >= startInclusive do
                if i >= 0 && (j < middle || B[i] > B[j]) then
                    A[k] <- B[i]
                    i <- i - 1
                elif j >= 0 then
                    A[k] <- B[j]
                    j <- j - 1

                k <- k - 1

        /// Split A[] into 2 runs, sort both runs into B[], merge both runs from B[] to A[]
        let rec topDownSplitMergeInPlace (B: int[]) (A: int[]) startInclusive endExclusive =
            if endExclusive - startInclusive <= 1 then
                ()
            else
                let middle = (startInclusive + endExclusive) / 2
                topDownSplitMergeInPlace B A startInclusive middle
                topDownSplitMergeInPlace B A middle endExclusive
                topDownMerge A B startInclusive middle endExclusive

        let sortInPlace (A: int[]) =
            let B = Array.copy A
            topDownSplitMergeInPlace A B 0 A.Length

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

        let bottomUpMergeSort (A: int[]) (B: int[]) n =
            let mutable width = 1
            let mutable source = A
            let mutable target = B

            while width < n do
                let mutable i = 0

                while i < n do
                    bottomUpMerge source target i (min (i + width) n) (min (i + 2 * width) n)
                    i <- i + 2 * width

                width <- 2 * width
                Array.blit B 0 A 0 B.Length

[<EntryPoint>]
let main _ =

    // let B = [| 0; 1 |]
    // let A = Array.zeroCreate B.Length
    // MergeSort.BottomUp.bottomUpMerge B A 0 1 B.Length
    // printfn $"A %A{A}"
    //
    // let A = [| 2; 3; 0; 1 |]
    // let B = Array.zeroCreate A.Length
    // MergeSort.BottomUp.bottomUpMerge A B 0 2 A.Length
    // printfn $"A %A{A}"
    //
    // let A = [| 0; 1; 2; 3 |]
    // let B = Array.zeroCreate A.Length
    // MergeSort.BottomUp.bottomUpMerge A B 0 2 A.Length
    // printfn $"A %A{A}"
    //
    // let A = [| 0; 1; 2; 3; 4; 5 |]
    // let B = Array.zeroCreate A.Length
    // MergeSort.BottomUp.bottomUpMerge A B 0 3 A.Length
    // printfn $"A %A{A}"
    //
    // let A = [| 3; 4; 5; 0; 1; 2 |]
    // let B = Array.zeroCreate A.Length
    // MergeSort.BottomUp.bottomUpMerge A B 0 3 A.Length
    // printfn $"A %A{A}"

    for A in [ [| 1; 0 |]; [| 2; 1; 0 |]; [| 3; 2; 1; 0 |]; [| 10 .. -1 .. 0 |] ] do
        let B = Array.zeroCreate A.Length
        MergeSort.BottomUp.bottomUpMergeSort A B A.Length
        printfn $"array A %A{A}"
        printfn $"array B %A{B}"

    0
