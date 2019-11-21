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
#ifdef x64
  addiw  x3, x2, 5
  slliw x3, x2, 5
  srliw x3, x2, 5
  sraiw x3, x2, 5
#endif
  ret
