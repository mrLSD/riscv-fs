.section ".text"
  .globl main
main:
  li        a0, 0xffffffff80000000
  li        a1, 0xfffffffffffff800
  la        a3, amo_operand
  sw        a0, 0(a3)
  
  #===================================
  # AMOADD.W
  amoadd.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0x000000007ffff800
  
  # try again after a cache miss
  li        a2, 0xffffffff80000000
  amoadd.w  a6, a2, 0(a3);      a6 = 0x000000007ffff800
  lw        a7, 0(a3);          a7 = 0xfffffffffffff800
  
  #===================================
  # AMOAND.W
  amoand.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xffffffff80000000

  # try again after a cache miss
  li        a2, 0x0000000080000000
  amoand.w  a6, a2, 0(a3);      a6 = 0xffffffff80000000
  lw        a7, 0(a3);          a7 = 0xffffffff80000000

  #===================================
  # AMOOR.W
  amoor.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xfffffffffffff800

  # try again after a cache miss
  li        a2, 1
  amoor.w  a6, a2, 0(a3);       a6 = 0xfffffffffffff800
  lw        a7, 0(a3);          a7 = 0xfffffffffffff801

  #===================================
  # AMOXOR.W
  amoxor.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0x7ffff800

  # try again after a cache miss
  li        a2, 0xc0000001
  amoxor.w  a6, a2, 0(a3);      a6 = 0x7ffff800
  lw        a7, 0(a3);          a7 = 0xffffffffbffff801

  #===================================
  # AMOSWAP.W
  amoswap.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xfffffffffffff800

  # try again after a cache miss
  li        a2, 0x0000000080000000
  amoswap.w  a6, a2, 0(a3);     a6 = 0xfffffffffffff800
  lw        a7, 0(a3);          a7 = 0xffffffff80000000

  #===================================
  # AMOMAX.W
  amomax.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xfffffffffffff800
  
  li        a2, 1
  sw        x0, 0(a3)
  amomax.w  a6, a2, 0(a3);      a6 = 0
  lw        a7, 0(a3);          a7 = 1

  #===================================
  # AMOMAXU.W
  amomaxu.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xfffffffffffff800
  
  li        a2, 0xffffffffffffffff
  sw        x0, 0(a3)
  amomaxu.w a6, a2, 0(a3);      a6 = 0
  lw        a7, 0(a3);          a7 = 0xffffffffffffffff

  #===================================
  # AMOMIN.W
  amomin.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xffffffff80000000
  
  li        a2, 0xffffffffffffffff
  sw        x0, 0(a3)
  amomin.w a6, a2, 0(a3);       a6 = 0
  lw        a7, 0(a3);          a7 = 0xffffffffffffffff

  #===================================
  # AMOMINU.W
  amominu.w	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  lw        a5, 0(a3);          a5 = 0xffffffff80000000
  
  li        a2, 0xffffffffffffffff
  sw        x0, 0(a3)
  amominu.w a6, a2, 0(a3);      a6 = 0
  lw        a7, 0(a3);          a7 = 0
    
  ret
  
.data
  .bss
  .align 3
amo_operand:
  .dword 0    
