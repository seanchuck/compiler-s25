looking at temp _t0 of type Long
going to allocate 16 bytes for _t0
looking at temp _t1 of type Long
going to allocate 8 bytes for _t1
A

bouttta assign between Const(
    1,
) and LocalVar(
    "_t0",
)
LONG
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
main0:
    movq $1, %rax
    movq %rax, -16(%rbp)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -24(%rbp)
    movl -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

