.section ".text"
  .globl main
main:
  li        a0, 0xffffffff80000000
  li        a1, 0xfffffffffffff800
  la        a3, amo_operand
  sw        a0, 0(a3)
  
  #===================================
  # AMOADD.D
  amoadd.d	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  ld        a5, 0(a3);          a5 = 0xffffffff7ffff800

  # try again after a cache miss
  amoadd.d  a6, a1, 0(a3);      a6 = 0xffffffff7ffff800
  ld        a7, 0(a3);          a7 = 0xffffffff7ffff000

  #===================================
  # AMOAND.D
  amoand.d	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  ld        a5, 0(a3);          a5 = 0xffffffff80000000

  # try again after a cache miss
  li        a1, 0x0000000080000000; 
  amoand.d  a6, a1, 0(a3);      a6 = 0xffffffff80000000
  ld        a7, 0(a3);          a7 = 0x0000000080000000
   
  #===================================
  # AMOOR.D
  amoor.d	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  ld        a5, 0(a3);          a5 = 0xfffffffffffff800

  # try again after a cache miss
  li        a1, 1; 
  amoor.d   a6, a1, 0(a3);      a6 = 0xfffffffffffff800 
  ld        a7, 0(a3);          a7 = 0xfffffffffffff801  
  
  #===================================
  # AMOXOR.D
  amoxor.d	a4, a1, 0(a3);      a4 = 0xffffffff80000000
  ld        a5, 0(a3);          a5 = 0x000000007ffff800
  
  # try again after a cache miss
  li        a1, 1; 
  amoxor.d  a6, a1, 0(a3);      a6 = 0x000000007ffff800
  ld        a7, 0(a3);          a7 = 0x000000007ffff801 

  #===================================
  # AMOSWAP.D
  # try again after a cache miss
  
  #===================================
  # AMOMAX.D
  
  #===================================
  # AMOMAXU.D
  
  #===================================
  # AMOMIN.D
  
  #===================================
  # AMOMINU.D
  
  ret
  
.data
  .bss
  .align 3
amo_operand:
  .dword 0    
