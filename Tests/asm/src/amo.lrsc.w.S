.section ".text"
  .globl main
#define LOG_ITERATIONS 10
  
main:
    la      a0, foo
    li      a5, 0xdeadbeef
    sc.w    a4, a5, (a0);       a4 = 1
    
    lw      a4, foo;            a4 = 0
    
    la      a0, foo
    la      a1, fooTest3
    lr.w    a1, (a1)
    sc.w    a4, a1, (a0);       a4 = 1

    lw      a0, foo
    slli    a1, a3, LOG_ITERATIONS-1
1:  sub     a0, a0, a1
    addi    a3, a3, -1
    bgez    a3, 1b;             a0 = 0

    la      a0, foo
1:  lr.w    a1, (a0)
    sc.w    a1, x0, (a0)
    bnez    a1, 1b
    sc.w    a1, x0, (a0);       a1 = 1

    ret

.data
coreid: .word 0
barrier: .word 0
foo: .word 0
.skip 1024
fooTest3: .word 0
