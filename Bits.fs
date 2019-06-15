module ISA.RISCV.Utils.Bits

type System.Int32 with
    member x.bitSlice endBit startBit = // get Bit slice from range
        (x >>> startBit) &&& ~~~(-1 <<< (endBit - startBit + 1))
    member x.signExtend n = // Sign extend bits for x32
        let bitOffset = 32 - n
        (x <<< bitOffset) >>> bitOffset
    member x.align32bitMask = // get x32 mask with all `1` bits
        -1
    member x.flip i = // change bit
        (x ^^^ (1 <<< i))
    member x.isSet i = // test if bit set at a specified position
        x &&& (1 <<< i) <> 0
    member x.rotateLeft r =
        (x<<<r) ||| (x>>>(32-r))
    member x.rotateRight r =
        (x>>>r) ||| (x<<<(32-r))

    (* bit coersion methods *)
    member x.toHex = sprintf "0x%x" x   // to hexadecimal
    member x.toBin = // to binary string
        System.Convert.ToString(x, 2).PadLeft(32, '0')
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

let combineBytes (x : int8 array) : int64 =
    let xz = Array.zip [|0..x.Length-1|] x
    Array.fold (fun acc (x : (int*int8)) -> acc ||| (int64(snd x) <<< (fst x)*8 )) 0L xz

// Load from Memory 1 byte
let loadByte (mem : int8 array) (addr : int32) : int8 =
    mem.[addr]

// Load from Memory 2 bytes
let loadHalf (mem : int8 array) (addr : int32) : int16 =
    int16(combineBytes mem.[addr..addr+1])

// Load from Memory 4 bytes
let loadWord (mem : int8 array) (addr : int32) : int32 =
    int32(combineBytes mem.[addr..addr+3])

// Load from Memory 8 bytes
let loadDouble (mem : int8 array) (addr : int32) : int64 =
    combineBytes mem.[addr..addr+7]
