
str0:
    .string "Hello Decaf\n"
do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
do_print0:
    movl $1, -28(%rbp)
    movl $0, -32(%rbp)
    leaq -24(%rbp), %r11
    movl -32(%rbp), %r10d
    addl $1, %r10d
    movl -28(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $2, -36(%rbp)
    movl $1, -40(%rbp)
    leaq -24(%rbp), %r11
    movl -40(%rbp), %r10d
    addl $1, %r10d
    movl -36(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $3, -44(%rbp)
    movl $2, -48(%rbp)
    leaq -24(%rbp), %r11
    movl -48(%rbp), %r10d
    addl $1, %r10d
    movl -44(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $4, -52(%rbp)
    movl $3, -56(%rbp)
    leaq -24(%rbp), %r11
    movl -56(%rbp), %r10d
    addl $1, %r10d
    movl -52(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $5, -60(%rbp)
    movl $4, -64(%rbp)
    leaq -24(%rbp), %r11
    movl -64(%rbp), %r10d
    addl $1, %r10d
    movl -60(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    leaq str0(%rip), %rax
    movq %rax, -72(%rbp)
    movq -72(%rbp), %rdi
    movq $0, %rax
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
main0:
    movq $0, %rax
    call do_print
    movl $0, -4(%rbp)
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

