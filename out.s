
str0:
    .string "Hello Decaf\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
main0:
    movl $0, %eax
    call do_print
    movl $0, %ebx
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
do_print0:
    movq $6, %rax
    movq %rax, -28(%rbp)
    movl $1, %ebx
    movl $0, %ebx
    leaq -28(%rbp), %r11
    movq -36(%rbp), %r10
    addq $1, %r10
    movq -32(%rbp), %rax
    movq %rax, (%r11, %r10, 4)
    movl $2, %ebx
    movl $1, %ebx
    leaq -28(%rbp), %r11
    movq -44(%rbp), %r10
    addq $1, %r10
    movq -40(%rbp), %rax
    movq %rax, (%r11, %r10, 4)
    movl $3, %ebx
    movl $2, %ebx
    leaq -28(%rbp), %r11
    movq -52(%rbp), %r10
    addq $1, %r10
    movq -48(%rbp), %rax
    movq %rax, (%r11, %r10, 4)
    movl $4, %ebx
    movl $3, %ebx
    leaq -28(%rbp), %r11
    movq -60(%rbp), %r10
    addq $1, %r10
    movq -56(%rbp), %rax
    movq %rax, (%r11, %r10, 4)
    movl $5, %ebx
    movl $4, %ebx
    leaq -28(%rbp), %r11
    movq -68(%rbp), %r10
    addq $1, %r10
    movq -64(%rbp), %rax
    movq %rax, (%r11, %r10, 4)
    movl $6, %ebx
    movl $5, %ebx
    leaq -28(%rbp), %r11
    movq -76(%rbp), %r10
    addq $1, %r10
    movq -72(%rbp), %rax
    movq %rax, (%r11, %r10, 4)
    leaq str0(%rip), %rax
    movq %rax, -84(%rbp)
    movq -84(%rbp), %rdi
    movl $0, %eax
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

