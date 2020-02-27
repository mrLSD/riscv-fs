.section ".text"
  .globl main
main:
  lb    x3, 10(x2)
  lb    x3, -10(x2)
  lh    x3, 10(x2)
  lh    x3, -10(x2)
  lw    x3, 10(x2)
  lw    x3, -10(x2)
#ifdef x64
  ld    x3, 10(x2)
  ld    x3, -10(x2)
#endif

  lbu   x3, 10(x2)
  lbu   x3, -10(x2)
  lhu   x3, 10(x2)
  lhu   x3, -10(x2)
#ifdef x64
  lwu   x3, 10(x2)
  lwu   x3, -10(x2)
#endif

  sb    x3, 10(x2)
  sb    x3, -10(x2)
  sh    x3, 10(x2)
  sh    x3, -10(x2)
  sw    x3, 10(x2)
  sw    x3, -10(x2)
  
  auipc	a3,0x1
  addi	a3,a3,-8
  sw	a0, 10(a3)
  addi	a1,a1,-8
  
#ifdef x64
  sd    x3, 10(x2)
  sd    x3, -10(x2)
#endif
  ret
