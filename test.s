
str0:
    .string "%d\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
main0:
    movq $0, %rax
    call foo
    movl %eax, -8(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rdi
    movl -8(%rbp), %esi
    movq $0, %rax
    call printf
    movl $0, -20(%rbp)
    movl -20(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

get_int:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
get_int0:
    movl %edi, %eax
    movl %eax, -4(%rbp)
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

foo:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
foo0:
    movl $7, -36(%rbp)
    movl -36(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -40(%rbp)
    movl $2, -44(%rbp)
    movl -44(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -48(%rbp)
    movl $1, %edi
    movq $0, %rax
    call get_bool
    movl %eax, -52(%rbp)
    movl $0, -56(%rbp)
    movl -56(%rbp), %eax
    movl %eax, -12(%rbp)
    movl -40(%rbp), %eax
    addl -48(%rbp), %eax
    movl %eax, -60(%rbp)
    movl -60(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp foo1
foo1:
    movl $2, -64(%rbp)
    movl -64(%rbp), %eax
    imul -60(%rbp), %eax
    movl %eax, -68(%rbp)
    movl -20(%rbp), %eax
    cmpl -68(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -72(%rbp)
    movl -72(%rbp), %eax
    cmpl $0, %eax
    jne foo2

    jmp foo3
foo2:
    movl -20(%rbp), %eax
    imul -20(%rbp), %eax
    movl %eax, -76(%rbp)
    movl -76(%rbp), %eax
    imul -60(%rbp), %eax
    movl %eax, -80(%rbp)
    movl -12(%rbp), %eax
    addl -80(%rbp), %eax
    movl %eax, -84(%rbp)
    movl -84(%rbp), %eax
    movl %eax, -12(%rbp)
    movl $1, -88(%rbp)
    movl -20(%rbp), %eax
    addl -88(%rbp), %eax
    movl %eax, -92(%rbp)
    movl -92(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp foo1
foo3:
    movl -20(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

get_bool:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
get_bool0:
    movl %edi, %eax
    movl %eax, -4(%rbp)
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

