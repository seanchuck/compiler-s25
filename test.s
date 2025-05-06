
.comm a, 4, 4
str0:
    .string "expecting an error next:\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
main0:
    leaq str0(%rip), %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %rdi
    xor %rax, %rax
    call printf
    xor %rax, %rax
    call foo
    movl %eax, -12(%rbp)
    movl -12(%rbp), %eax
    movl %eax, a
    movl $0, -16(%rbp)
    movl -16(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

foo:
    pushq %rbp
    movq %rsp, %rbp
    subq $48, %rsp
foo0:
    movq $-1, %rdi
    call exit
    movq %rbp, %rsp
    popq %rbp
    ret

