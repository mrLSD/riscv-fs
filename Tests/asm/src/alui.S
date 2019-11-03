.section ".text"
  .globl main
main:
  addi  x3, x2, 5
  slti  x3, x2, 5
  sltiu x3, x2, 5
  xori  x3, x2, 5
  ori   x3, x2, 5
  andi  x3, x2, 5

  slli  x3, x2, 5
  srli  x3, x2, 5
  srai  x3, x2, 5
  # x64 only
  addiw  x3, x2, 5
  ret
