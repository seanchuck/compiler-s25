
str0:
    .string "Hello Decaf\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $0, %rsp
main0:
    call do_print
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $8, %rsp
do_print0:
    leaq str0(%rip), %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %rdi
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

