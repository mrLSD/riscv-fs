.section ".text"
  .globl main
main:
  lui   x3, 10
  lui   x3, 0
  lui   x3, 1048575
  auipc x3, 10
  auipc x3, 0
  auipc x3, 1048575
  ret
