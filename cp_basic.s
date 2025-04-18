
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
    movq %rbx, -40(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -8(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -32(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -16(%rbp), %rax
    addq -48(%rbp), %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rax
    addq -32(%rbp), %rax
    movq %rax, -64(%rbp)
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -72(%rbp)
    movq -64(%rbp), %rax
    addq -72(%rbp), %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rax
    movq %rax, -24(%rbp)
    movq %rbp, %rsp
    popq %rbp
    ret

