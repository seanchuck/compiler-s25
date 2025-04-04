
str0:
    .string "hi"
str1:
    .string "low"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
main0:
    movq $3, %rax
    movq %rax, -8(%rbp)
    cmpq $0, -8(%rbp)
    setg %al
    movzbq %al, %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rax
    cmpq $0, %rax
    jne main1

    jmp main2
main1:
    leaq str0(%rip), %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rdi
    call printf
    jmp main3
main2:
    leaq str1(%rip), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %rdi
    call printf
    jmp main3
main3:
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

