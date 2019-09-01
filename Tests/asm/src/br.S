.section ".text"
  .globl main
main:
  beq    x1, x2, r
  beq    x1, x2, main
  bne    x1, x2, r
  bne    x1, x2, main
  blt    x1, x2, r
  blt    x1, x2, main
  bge    x1, x2, r
  bge    x1, x2, main
  bltu   x1, x2, r
  bltu   x1, x2, main
  bgeu   x1, x2, r
  bgeu   x1, x2, main
r:
  ret
