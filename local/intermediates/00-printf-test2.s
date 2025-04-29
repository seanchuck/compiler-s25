
str0:
    .string "Hello Decaf\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
main0:
    leaq str0(%rip), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -20(%rbp)
    movl -20(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

