
.comm d, 8, 8
str0:
    .string "i should be 20, actual is %d\n"
str1:
    .string "j should be 100, actual is %d\n"
bar:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
bar0:
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -16(%rbp)
    movq d, %rax
    cmpq -16(%rbp), %rax
    setne %al
    movzbq %al, %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

f:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
f0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -16(%rbp)
    movq -8(%rbp), %rax
    imul -16(%rbp), %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
main0:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -24(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -16(%rbp)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -32(%rbp)
    movq -32(%rbp), %rax
    movq %rax, -8(%rbp)
    jmp main1
main1:
    movq $0, %rax
    call bar
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rax
    cmpq $0, %rax
    jne main2

    jmp main4
main2:
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -8(%rbp), %rax
    addq -48(%rbp), %rax
    movq %rax, -8(%rbp)
    jmp main3
main3:
    movq -8(%rbp), %rdi
    movq $0, %rax
    call f
    movq %rax, -56(%rbp)
    movq -16(%rbp), %rax
    addq -56(%rbp), %rax
    movq %rax, -16(%rbp)
    jmp main1
main4:
    leaq str0(%rip), %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rdi
    movq -8(%rbp), %rsi
    movq $0, %rax
    call printf
    leaq str1(%rip), %rax
    movq %rax, -72(%rbp)
    movq -72(%rbp), %rdi
    movq -16(%rbp), %rsi
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -80(%rbp)
    movq -80(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

