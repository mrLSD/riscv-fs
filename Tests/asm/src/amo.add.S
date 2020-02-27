.section ".text"
  .globl main
main:
  li        a0, 0xffffffff80000000
  li        a1, 0xfffffffffffff800
  la        a3, amo_operand
  sw        a0, 0(a3)
  amoadd.w	a4, a1, 0(a3)
  ret
  
  .data
    .bss
    .align 3
amo_operand:
    .dword 0    
