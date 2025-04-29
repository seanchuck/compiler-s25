
str0:
    .string "Hello Decaf\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
main0:
    leaq str0(%rip), %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -12(%rbp)
    movl -12(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

