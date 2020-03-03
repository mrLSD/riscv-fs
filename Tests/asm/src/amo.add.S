.section ".text"
  .globl main
main:
  li        a0, 0xffffffff80000000
  li        a1, 0xfffffffffffff800
  la        a3, amo_operand
  sw        a0, 0(a3)
  amoadd.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0x000000007ffff800
  
  # try again after a cache miss
  li        a1, 0xffffffff80000000
  amoadd.w  a6, a1, 0(a3);      a6 = 0x000000007ffff800
  lw        a7, 0(a3);          a7 = 0xfffffffffffff800
  
  ret
  
.data
  .bss
  .align 3
amo_operand:
  .dword 0    
