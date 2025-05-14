.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
main0:
    xorq %rax, %rax
    call example
    movl $0, -4(%rbp)
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

example:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $88, %rsp
example0:
    movl $0, %ecx
    movl $2, %ebx
    movl %ebx, %esi
    addl %ecx, %esi
    imul $8, %ecx
    movl $5, %ebx
    cmpl %ebx, %ecx
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne example1

    jmp example2
example1:
    jmp example3
example2:
    movl $4, -36(%rbp)
    movl -36(%rbp), %esi
    jmp example3
example3:
    movl %esi, %eax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret
