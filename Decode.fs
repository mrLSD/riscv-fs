/// Decode instructions set
 
module ISA.RISCV.Decode

type MachineInt = uint64
type Register = MachineInt

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
    
    | Auipc of {| rd: Register; oimm20: MachineInt |}
    | Lui of   {| rd: Register; imm20: MachineInt |}
    
    | Sb of {| rs1: Register; rs2: Register; simm12: MachineInt |}
    | Sh of {| rs1: Register; rs2: Register; simm12: MachineInt |}
    | Sw of {| rs1: Register; rs2: Register; simm12: MachineInt |}
    
    | Add of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sub of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sll of  {| rd: Register; rs1: Register; rs2: Register |}
    | Slt of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sltu of {| rd: Register; rs1: Register; rs2: Register |}
    | Xor of  {| rd: Register; rs1: Register; rs2: Register |}
    | Srl of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sra of  {| rd: Register; rs1: Register; rs2: Register |}
    | Or of   {| rd: Register; rs1: Register; rs2: Register |}
    | And of  {| rd: Register; rs1: Register; rs2: Register |}
    
    | Beq of  {| rs1: Register; rs2: Register; sbimm12: MachineInt |}
    | Bne of  {| rs1: Register; rs2: Register; sbimm12: MachineInt |}
    | Blt of  {| rs1: Register; rs2: Register; sbimm12: MachineInt |}
    | Bge of  {| rs1: Register; rs2: Register; sbimm12: MachineInt |}
    | Bltu of {| rs1: Register; rs2: Register; sbimm12: MachineInt |}
    | Bgeu of {| rs1: Register; rs2: Register; sbimm12: MachineInt |}
    
    | Jalr of {| rd: Register; rs: Register; oimm12: MachineInt  |}
    | Jal of  {| rd: Register; jimm20: MachineInt  |}

/// M-instructions set    
type InstructionM =
    | Mul of    {| rd: Register; rs1: Register; rs2: Register |}
    | Mulh of   {| rd: Register; rs1: Register; rs2: Register |}
    | Mulhsu of {| rd: Register; rs1: Register; rs2: Register |}
    | Mulhu of  {| rd: Register; rs1: Register; rs2: Register |}
    | Div of    {| rd: Register; rs1: Register; rs2: Register |}
    | Divu of   {| rd: Register; rs1: Register; rs2: Register |}
    | Rem of    {| rd: Register; rs1: Register; rs2: Register |}
    | Remu of   {| rd: Register; rs1: Register; rs2: Register |}

type Instruction =
    | IInsruction of InstructionI
    | MInsruction of {| mInsruction: InstructionM |} 
    
let decode_execution =
    printfn "Decode instructions: %d" 10
    
    