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
  li        a2, 0xffffffff80000000
  amoadd.w  a6, a2, 0(a3);      a6 = 0x000000007ffff800
  lw        a7, 0(a3);          a7 = 0xfffffffffffff800
  
  #===================================
  # AMO.AND.W
  amoand.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xffffffff80000000

  # try again after a cache miss
  li        a2, 0x0000000080000000
  amoand.w  a6, a2, 0(a3);      a6 = 0xffffffff80000000
  lw        a7, 0(a3);          a7 = 0xffffffff80000000

  #===================================
  # AMO.MAX.W
  amomax.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xfffffffffffff800
  
  li        a2, 1
  sw        x0, 0(a3)
  amomax.w  a6, a2, 0(a3);      a6 = 0
  lw        a7, 0(a3);          a7 = 1
  
  ret
  
.data
  .bss
  .align 3
amo_operand:
  .dword 0    
