module Program =
    
    open OCW6006.MergeSort

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
