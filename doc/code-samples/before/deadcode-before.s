
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
main0:
    movl $42, -4(%rbp)
    movl $1, -16(%rbp)
    movl -4(%rbp), %eax
    addl -16(%rbp), %eax
    movl %eax, -8(%rbp)
    movq %rbp, %rsp
    popq %rbp
    ret

