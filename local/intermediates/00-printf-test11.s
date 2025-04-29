
str0:
    .string "Hello Decaf\n"
do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
do_print0:
    movl $1, -32(%rbp)
    movl $0, -36(%rbp)
    leaq -28(%rbp), %r11
    movl -36(%rbp), %r10d
    addl $1, %r10d
    movl -32(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $2, -40(%rbp)
    movl $1, -44(%rbp)
    leaq -28(%rbp), %r11
    movl -44(%rbp), %r10d
    addl $1, %r10d
    movl -40(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $3, -48(%rbp)
    movl $2, -52(%rbp)
    leaq -28(%rbp), %r11
    movl -52(%rbp), %r10d
    addl $1, %r10d
    movl -48(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $4, -56(%rbp)
    movl $3, -60(%rbp)
    leaq -28(%rbp), %r11
    movl -60(%rbp), %r10d
    addl $1, %r10d
    movl -56(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $5, -64(%rbp)
    movl $4, -68(%rbp)
    leaq -28(%rbp), %r11
    movl -68(%rbp), %r10d
    addl $1, %r10d
    movl -64(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    movl $6, -72(%rbp)
    movl $5, -76(%rbp)
    leaq -28(%rbp), %r11
    movl -76(%rbp), %r10d
    addl $1, %r10d
    movl -72(%rbp), %eax
    movl %eax, (%r11, %r10, 4)
    leaq str0(%rip), %rax
    movq %rax, -84(%rbp)
    movq -84(%rbp), %rdi
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

