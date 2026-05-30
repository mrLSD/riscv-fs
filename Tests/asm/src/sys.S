.section ".text"
  .globl main
main:
  lui   x3, 10
  lui   x3, 0
  fence
  ecall
  ebreak
  ret
