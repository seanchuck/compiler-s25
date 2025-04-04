
str0:
    .string "%d\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
main0:
    leaq -32(%rbp), %r11
    movq $0, %r10
    movq $1, %rax
    movq %rax, (%r11, %r10, 8)
    leaq -32(%rbp), %r11
    movq $1, %r10
    movq $2, %rax
    movq %rax, (%r11, %r10, 8)
    leaq -32(%rbp), %r11
    movq $2, %r10
    movq $3, %rax
    movq %rax, (%r11, %r10, 8)
    movq $0, %rax
    movq %rax, -48(%rbp)
    movq $0, %rax
    movq %rax, -40(%rbp)
    jmp main1
main1:
    cmpq $3, -40(%rbp)
    setl %al
    movzbq %al, %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rax
    cmpq $0, %rax
    jne main2

    jmp main4
main2:
    leaq -32(%rbp), %r11
    movq -40(%rbp), %r10
    movq (%r11, %r10, 8), %rax
    movq %rax, -64(%rbp)
    movq -48(%rbp), %rax
    addq -64(%rbp), %rax
    movq %rax, -48(%rbp)
    jmp main3
main3:
    movq -40(%rbp), %rax
    addq $1, %rax
    movq %rax, -40(%rbp)
    jmp main1
main4:
    leaq str0(%rip), %rax
    movq %rax, -72(%rbp)
    movq -72(%rbp), %rdi
    movq -48(%rbp), %rsi
    call printf
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

