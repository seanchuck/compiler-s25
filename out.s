whoooop
whoooop

.comm b, 24, 8
.comm a, 8, 8
str0:
    .string "%ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $288, %rsp
    movq $2, %rax
    movq %rax, b
main0:
    movq $3, %rax
    movq %rax, -40(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rax, a
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -64(%rbp), %r10
    addq $1, %r10
    movq -56(%rbp), %rax
    movq %rax, b(, %r10, 8)
    movq $3, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -72(%rbp)
    movq -80(%rbp), %r10
    addq $1, %r10
    movq -72(%rbp), %rax
    movq %rax, b(, %r10, 8)
    movq $4, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -88(%rbp)
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -96(%rbp)
    leaq -40(%rbp), %r11
    movq -104(%rbp), %r10
    addq $1, %r10
    movq -96(%rbp), %rax
    movq %rax, (%r11, %r10, 8)
    movq $6, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -112(%rbp)
    leaq -40(%rbp), %r11
    movq -120(%rbp), %r10
    addq $1, %r10
    movq -112(%rbp), %rax
    movq %rax, (%r11, %r10, 8)
    movq $7, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -128(%rbp)
    leaq -40(%rbp), %r11
    movq -136(%rbp), %r10
    addq $1, %r10
    movq -128(%rbp), %rax
    movq %rax, (%r11, %r10, 8)
    leaq str0(%rip), %rax
    movq %rax, -144(%rbp)
    movq b, %rax
    movq %rax, -152(%rbp)
    movq -168(%rbp), %r10
    addq $1, %r10
    movq b(, %r10, 8), %rax
    movq %rax, -160(%rbp)
    movq -184(%rbp), %r10
    addq $1, %r10
    movq b(, %r10, 8), %rax
    movq %rax, -176(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -192(%rbp)
    leaq -40(%rbp), %r11
    movq -208(%rbp), %r10
    addq $1, %r10
    movq (%r11, %r10, 8), %rax
    movq %rax, -200(%rbp)
    leaq -40(%rbp), %r11
    movq -224(%rbp), %r10
    addq $1, %r10
    movq (%r11, %r10, 8), %rax
    movq %rax, -216(%rbp)
    leaq -40(%rbp), %r11
    movq -240(%rbp), %r10
    addq $1, %r10
    movq (%r11, %r10, 8), %rax
    movq %rax, -232(%rbp)
    movq -144(%rbp), %rdi
    movq a, %rsi
    movq -152(%rbp), %rdx
    movq -160(%rbp), %rcx
    movq -176(%rbp), %r8
    movq -88(%rbp), %r9
    movq -192(%rbp), %rax
    movq %rax, 0(%rsp)
    movq -200(%rbp), %rax
    movq %rax, 8(%rsp)
    movq -216(%rbp), %rax
    movq %rax, 16(%rsp)
    movq -232(%rbp), %rax
    movq %rax, 24(%rsp)
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -248(%rbp)
    movq -248(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

