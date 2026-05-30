.section ".text.init"
  .globl _start
_start:
  call main
1:
  j 1b
