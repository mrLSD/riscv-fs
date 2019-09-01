.section ".text"
  .globl main
main:
  lui   x3, 10
  auipc x3, 10
  ret
