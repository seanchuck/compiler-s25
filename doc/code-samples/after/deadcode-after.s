.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
main0:
    movq %rbp, %rsp
    popq %rbp
    ret
