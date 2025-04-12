Replacing _t0 with _t5
Replacing _t2 with _t5
Replacing _t1 with _t7
Replacing _t2 with _t5
Replacing _t3 with _t8

str0:
    .string "%d\n"
str1:
    .string "%d\n"
get_int:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
get_int0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
main0:
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq -40(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rax, -8(%rbp)
    movq $3, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -56(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -48(%rbp), %rax
    movq %rax, -24(%rbp)
    movq -48(%rbp), %rax
    addq -64(%rbp), %rax
    movq %rax, -72(%rbp)
    movq -72(%rbp), %rax
    movq %rax, -32(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rdi
    movq -48(%rbp), %rsi
    movq $0, %rax
    call printf
    leaq str1(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq -72(%rbp), %rsi
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -96(%rbp)
    movq -96(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

