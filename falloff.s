
str0:
    .string "%d\n"
get_3:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
get_30:
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $48, %rsp
main0:
    call get_5
    movq %rax, -16(%rbp)
    call get_3
    movq %rax, -24(%rbp)
    movq -16(%rbp), %rax
    addq -24(%rbp), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %rax
    movq %rax, -8(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rdi
    movq -8(%rbp), %rsi
    call printf
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

get_5:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
get_50:
    movq $5, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

