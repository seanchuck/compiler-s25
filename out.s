
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $48, %rsp
main0:
    movl $0, -8(%rbp)
    movl -8(%rbp), %eax
    movl %eax, -4(%rbp)
    jmp main1
main1:
    movl $3, -12(%rbp)
    movl -4(%rbp), %eax
    cmpl -12(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movq %rax, -16(%rbp)
    movl -16(%rbp), %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    movl $0, -24(%rbp)
    movl -24(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp main5
main3:
    movl $1, -40(%rbp)
    movl -4(%rbp), %eax
    addl -40(%rbp), %eax
    movl %eax, -4(%rbp)
    jmp main1
main4:
    movl $0, -44(%rbp)
    movq -44(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
main5:
    movl $3, -28(%rbp)
    movl -20(%rbp), %eax
    cmpl -28(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movq %rax, -32(%rbp)
    movl -32(%rbp), %eax
    cmpl $0, %eax
    jne main6

    jmp main8
main6:
    jmp main7
main7:
    movl $1, -36(%rbp)
    movl -20(%rbp), %eax
    addl -36(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp main5
main8:
    jmp main3

