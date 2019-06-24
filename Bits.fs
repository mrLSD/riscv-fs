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

let combineBytes (x : byte array) : int64 =
    let xz = Array.zip [|0..x.Length-1|] x
    Array.fold (fun acc (x : (int*byte)) -> acc ||| (int64(snd x) <<< (fst x)*8 )) 0L xz

// Load from Memory 1 byte
let loadByte (mem : Map<int32, byte>) (addr : int32) : byte option =
    if Map.containsKey addr mem then
        Some(mem.[addr])
    else
        None

// Load from Memory 2 bytes
let loadHalfWord (mem : Map<int32, byte>) (addr : int32) : int16 option =
    if Map.containsKey addr mem &&
       Map.containsKey (addr+1) mem then
        let halfWord = [| mem.[addr]; mem.[addr+1] |]
        Some(int16(combineBytes halfWord))
    else
        None

// Load from Memory 4 bytes
let loadWord (mem : Map<int32, byte>) (addr : int32) : int32 option =
    if Map.containsKey addr mem &&
       Map.containsKey (addr+1) mem &&
       Map.containsKey (addr+2) mem &&
       Map.containsKey (addr+3) mem then
        let word = [| mem.[addr]; mem.[addr+1]; mem.[addr+2]; mem.[addr+3] |]
        Some(int32(combineBytes word))
    else
        None

// Load from Memory 8 bytes
let loadDouble (mem : Map<int32, byte>) (addr : int32) : int64 option =
    if Map.containsKey addr mem &&
       Map.containsKey (addr+1) mem &&
       Map.containsKey (addr+2) mem &&
       Map.containsKey (addr+3) mem &&
       Map.containsKey (addr+4) mem &&
       Map.containsKey (addr+5) mem &&
       Map.containsKey (addr+6) mem &&
       Map.containsKey (addr+7) mem then
        let dWord = [| mem.[addr]; mem.[addr+1]; mem.[addr+2]; mem.[addr+3]; mem.[addr+4]; mem.[addr+5]; mem.[addr+6]; mem.[addr+7] |]
        Some(int64(combineBytes dWord))
    else
        None
