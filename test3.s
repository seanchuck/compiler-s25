# import printf;
# int a;
# int c[3];
# void main() {
#     int b;
#     int d[5];
#
#     a = 2;
#     b = 5;
#     c[1] = 4;
#     d[2] = 3;
#
#     printf("%d, %d, %d, %d\n", a, b, c[1], d[2]);
# }

str0:
    .string  "%d, %d, %d, %d\n"

.comm a, 8, 16
.comm c, 24, 16

.globl main
main:
    pushq %rbp
    movq  %rsp, %rbp
    subq   $48, %rsp

    movq $2, a
    movq $5, -8(%rbp)
    movq $4, c + 8
    movq $3, -32(%rbp)

    leaq str0(%rip), %rdi
    movq a, %rsi
    movq -8(%rbp), %rdx
    movq c + 8, %rcx
    movq -32(%rbp), %r8
    call printf

    movq %rbp, %rsp
    popq %rbp
    ret