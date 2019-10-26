.section ".text"
  .globl main
main:
  lui    x0, 0
  lui    x0, 0
  jal    x3, r
  jal    x3, main
  jalr   x3, 16(x2)
  jalr   x3, -16(x2)
  lui    x0, 0
  lui    x0, 0
r:
  ret
