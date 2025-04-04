
str0:
    .string "Hello Decaf %d\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
main0:
    movq $10, %rdi
    movq $1, %rsi
    movq $20, %rdx
    movq $0, %rcx
    movq $30, %r8
    movq $1, %r9
    pushq $40
    call do_print
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
do_print0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq %rsi, %rax
    movq %rax, -16(%rbp)
    movq %rdx, %rax
    movq %rax, -24(%rbp)
    movq %rcx, %rax
    movq %rax, -32(%rbp)
    movq %r8, %rax
    movq %rax, -40(%rbp)
    movq %r9, %rax
    movq %rax, -48(%rbp)
    movq 16(%rbp), %rax
    movq %rax, -56(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rdi
    movq -56(%rbp), %rsi
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

