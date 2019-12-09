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
#ifdef x64
  addw  x3, x2, x1
  subw  x3, x2, x1
  sllw  x3, x2, x1
  srlw  x3, x2, x1
  sraw  x3, x2, x1
#endif

  # M-extension
  mul    x3, x2, x1
  mulh   x3, x2, x1
  mulhu  x3, x2, x1
  mulhsu x3, x2, x1

  div    x3, x2, x1
  divu   x3, x2, x1
  rem    x3, x2, x1
  remu   x3, x2, x1

#ifdef x64
  mulw   x3, x2, x1
  divw    x3, x2, x1
  divuw  x3, x2, x1
  remw   x3, x2, x1
  remuw  x3, x2, x1
#endif
  ret
