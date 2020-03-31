.section ".text"
  .globl main
#define LOG_ITERATIONS 10
  
main:
    la a0, coreid
    li a1, 1
    amoadd.w a2, a1, (a0)

    1:li a3, 1
    bgeu a2, a3, 1b

    1: lw a1, (a0)
    bltu a1, a3, 1b

    # ======================================
    la      a0, foo
    li      a5, 0xdeadbeef
    sc.w    a4, a5, (a0);       a4 = 1
    
    lw      a5, foo;            a5 = 0
    
    la      a0, foo
    la      a1, fooTest3
    lr.w    a1, (a1)
    sc.w    a6, a1, (a0);       a6 = 1

    lw      a0, foo
    slli    a1, a3, LOG_ITERATIONS-1
1:  sub     a0, a0, a1
    addi    a3, a3, -1
    bgez    a3, 1b

    la      a0, foo
1:  lr.w    a1, (a0)
    sc.w    a1, x0, (a0)
    bnez    a1, 1b
    sc.w    a7, x0, (a0);       a7 = 1

    ret

.data
coreid: .word 0
barrier: .word 0
foo: .word 0
.skip 1024
fooTest3: .word 0
