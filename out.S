
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
main0:
    movl $3, %ebx
    movq -12(%rbp), %rax
    movq %rax, -4(%rbp)
    movl $4, %ebx
    movq -16(%rbp), %rax
    movq %rax, -8(%rbp)
    movl -4(%rbp), %eax
    addl -8(%rbp), %eax
    movl %eax, -20(%rbp)
    movq -20(%rbp), %rax
    movq %rax, -4(%rbp)
    movl $0, %ebx
    movl -24(%rbp), %rax
    movl %rbp, %rsp
    popq %rbp
    ret

