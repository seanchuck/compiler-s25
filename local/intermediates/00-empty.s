
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
main0:
    movl $0, -4(%rbp)
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

