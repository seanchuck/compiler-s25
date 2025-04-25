
.comm a, 8, 8
.comm b, 32, 8
str0:
    .string "%d, %d\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $48, %rsp
    movq $3, %rax
    movq %rax, b
main0:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -8(%rbp)
    movq -8(%rbp), %rax
    movq %rax, a
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -16(%rbp)
    movq a, %r10
    addq $1, %r10
    movq -16(%rbp), %rax
    movq %rax, b(, %r10, 8)
    leaq str0(%rip), %rax
    movq %rax, -24(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq -40(%rbp), %r10
    addq $1, %r10
    movq b(, %r10, 8), %rax
    movq %rax, -32(%rbp)
    movq -24(%rbp), %rdi
    movq a, %rsi
    movq -32(%rbp), %rdx
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

