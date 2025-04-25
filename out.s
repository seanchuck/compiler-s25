

.comm a, 32, 8
str0:
    .string "should be 17: %d\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $128, %rsp
    movq $3, %rax
    movq %rax, a
main0:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -8(%rbp)
    movq $0, %rax
    call foo
    movq %rax, -16(%rbp)
    movq -16(%rbp), %r10
    addq $1, %r10
    movq -8(%rbp), %rax
    movq %rax, a(, %r10, 8)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -24(%rbp)
    movq $0, %rax
    call foo
    movq %rax, -40(%rbp)
    movq -40(%rbp), %r10
    addq $1, %r10
    movq a(, %r10, 8), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %r10
    addq $1, %r10
    movq -24(%rbp), %rax
    movq %rax, a(, %r10, 8)
    movq $17, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -72(%rbp)
    movq -72(%rbp), %r10
    addq $1, %r10
    movq a(, %r10, 8), %rax
    movq %rax, -64(%rbp)
    movq $0, %rax
    call foo
    movq %rax, -88(%rbp)
    movq -88(%rbp), %r10
    addq $1, %r10
    movq a(, %r10, 8), %rax
    movq %rax, -80(%rbp)
    movq -64(%rbp), %rax
    subq -80(%rbp), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %r10
    addq $1, %r10
    movq a(, %r10, 8), %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %r10
    addq $1, %r10
    movq -48(%rbp), %rax
    movq %rax, a(, %r10, 8)
    leaq str0(%rip), %rax
    movq %rax, -104(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -120(%rbp)
    movq -120(%rbp), %r10
    addq $1, %r10
    movq a(, %r10, 8), %rax
    movq %rax, -112(%rbp)
    movq -104(%rbp), %rdi
    movq -112(%rbp), %rsi
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -128(%rbp)
    movq -128(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

foo:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
foo0:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -8(%rbp)
    movq -8(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

