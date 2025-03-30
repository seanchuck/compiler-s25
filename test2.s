# import printf;
# void main() {
#     int a[5]; // stack offset: -48
#     int b; // stack offset: -56
#
#     b = 2;
#     a[b] = 4;
#
#     printf("%d\n", a[2]);
# }

str0:
    .string  "%d\n"

.globl main
main:
    pushq %rbp
    movq  %rsp, %rbp
    subq   $64, %rsp          # must be a multiple of 16; 48 bytes for a, 8 bytes for b

    movq $5, -48(%rbp)        # store the length of a

    movq $2, -56(%rbp)        # b = 2

    leaq -40(%rbp), %rax      # base address of a
    movq -56(%rbp), %r10      # b
    movq $4, (%rax, %r10, 8)  # a[b] = 4

    leaq str0(%rip), %rdi
    movq -24(%rbp), %rsi
    call printf

    movq %rbp, %rsp
    popq %rbp
    ret