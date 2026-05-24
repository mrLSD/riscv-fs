.section ".text"
  .globl main
main:
  li   x0, 5
  li   x2, 3
  li   x4, -5
  addi x3, x2, 5
  addi x3, x4, 10
  addi x3, x4, 5
  addi x3, x4, -10
  ret
