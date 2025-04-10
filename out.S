
str0:
    .string "Hello Decaf %d, %d, %d, %d\n"
do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
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
    movq 24(%rbp), %rax
    movq %rax, -64(%rbp)
    movq 32(%rbp), %rax
    movq %rax, -72(%rbp)
    movq 40(%rbp), %rax
    movq %rax, -80(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq -56(%rbp), %rsi
    movq -64(%rbp), %rdx
    movq -72(%rbp), %rcx
    movq -80(%rbp), %r8
    movq $0, %rax
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
main0:
    movq $10, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -8(%rbp)
    movq $20, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -16(%rbp)
    movq $30, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -24(%rbp)
    movq $40, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -32(%rbp)
    movq $50, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq $60, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq $70, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -8(%rbp), %rdi
    movq $1, %rsi
    movq -16(%rbp), %rdx
    movq $0, %rcx
    movq -24(%rbp), %r8
    movq $1, %r9
    movq -32(%rbp), %rax
    movq %rax, 0(%rsp)
    movq -40(%rbp), %rax
    movq %rax, 8(%rsp)
    movq -48(%rbp), %rax
    movq %rax, 16(%rsp)
    movq -56(%rbp), %rax
    movq %rax, 24(%rsp)
    movq $0, %rax
    call do_print
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -64(%rbp)
    movq -64(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

