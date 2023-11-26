namespace OCW6006

module CountingSort =
    
    let sort (keySize: int) (hash: int -> int) (input: int[]) =
        
        let output = Array.zeroCreate input.Length
        let counts = Array.zeroCreate (keySize + 1)
        
        // build histogram
        for i in 0 .. input.Length - 1 do
            let key = hash input[i]
            counts[key] <- counts[key] + 1
            
        // accumulate counts = compute key collisions = compute offsets for final array
        for i in 1 .. counts.Length - 1 do
            counts[i] <- counts[i] + counts[i - 1]
            
        for i in input.Length - 1 .. -1 .. 0 do
            let key = hash input[i]
            counts[key] <- counts[key] - 1
            output[counts[key]] <- input[i]
    
        output