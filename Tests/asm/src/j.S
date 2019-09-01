.section ".text"
  .globl main
main:
  jal    x3, r
  jal    x3, main
  jalr   x3, x2, 10
  jalr   x3, x2, -10
r:
  ret
