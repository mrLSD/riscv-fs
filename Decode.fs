/// Decode instructions set
 
module ISA.RISCV.Decode

type MachineInt = uint64
type Register = MachineInt

//==============================
// Decode Instructions

/// I-instructions set
type InstructionI =
    | Lb of  {| rd: Register; rs1: Register; oimm12: MachineInt |}
    | Lh of  {| rd: Register; rs1: Register; oimm12: MachineInt |}
    | Lw of  {| rd: Register; rs1: Register; oimm12: MachineInt |}
    | Lbu of {| rd: Register; rs1: Register; oimm12: MachineInt |}
    
    | Fence of {| pred: MachineInt; succ: MachineInt  |}
    | Fence_i
    
    | Addi of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Slli of  {| rd: Register; rs1: Register; shamt6: int  |}
    | Slti of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Sltiu of {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Xori of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Ori of   {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Andi of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Srli of  {| rd: Register; rs1: Register; shamt6: int  |}
    | Srai of  {| rd: Register; rs1: Register; shamt6: int  |}
    
    | Auipc
    | Lui
    
    | Sb
    | Sh
    | Sw
    
    | Add
    | Sub
    | Sll
    | Slt
    | Sltu
    | Xor
    | Srl
    | Sra
    | Or
    | And
    
    | Beq
    | Bne
    | Blt
    | Bge
    | Bltu
    | Bgeu
    
    | Jalr of {| rd: Register; rs: Register; oimm12: MachineInt  |}
    | Jal of  {| rd: Register; jimm20: MachineInt  |}
    
let decode_execution =
    printfn "Decode instructions"