module ISA.RISCV.Utils.Bits

type System.Int32 with
    member x.flip i = // change bit
        (x ^^^ (1 <<< i))         
    member x.isSet i = // test if bit set at a specified position
        x &&& (1 <<< i) <> 0   
    member x.reverse =                              
        let rec go b i acc =
            if i = 32 then acc else go (b>>>1) (i+1) ((acc <<< 1) ||| (b &&& 1))
        go x 0 0
    member x.rotateLeft r =
        (x<<<r) ||| (x>>>(32-r)) 
    member x.rotateRight r =
        (x>>>r) ||| (x<<<(32-r))

    (* bit coersion methods *)
    member x.toHex = sprintf "0x%x" x           // to hexadecimal
    member x.toBits = System.Convert.ToString(x, 2).PadLeft(32, '0') // to binary   
    member x.toResizeArray = // to Resizable array of positions set to 1
        let array = ResizeArray()
        for i=0 to 31 do
            if x.isSet i then array.Add(i)
        array        
    member x.toArray = // to array of positions set to 1
        let res = x.toResizeArray
        res.ToArray()
    member x.toList = // to list of positions set to 1
        let res = x.toResizeArray
        Array.toList (res.ToArray())
    member x.toSeq = // to seq of positions set to 1
        let res = x.toResizeArray
        Array.toSeq (res.ToArray())
  
    (* bit print methods *)
    member x.print = printf "%A" x 
    member x.display = // helper to show bits
        x.toArray |> Seq.iter(fun i -> printf "%A " i)
        
    (* misc methods *)
    member x.abs = // fast math.abs
        (x ^^^ (x >>> 31)) - (x >>> 31)