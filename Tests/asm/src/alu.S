.section ".text"
  .globl main
main:
  add   x3, x2, x1
  sub   x3, x2, x1
  sll   x3, x2, x1
  slt   x3, x2, x1
  sltu  x3, x2, x1
  xor   x3, x2, x1
  srl   x3, x2, x1
  sra   x3, x2, x1
  or    x3, x2, x1
  and   x3, x2, x1
  ret