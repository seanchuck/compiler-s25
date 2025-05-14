.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
main0:
    xorq %rax, %rax
    call example
    xorl %eax, %eax
    movq %rbp, %rsp
    popq %rbp
    ret

example:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $88, %rsp
example0:
    xorl %ecx, %ecx
    movl $2, %esi
    addl %ecx, %esi
    shl $3, %ecx
    movl $5, %ebx
    cmpl %ebx, %ecx
    setl %al
    movzbq %al, %rax
    cmpl $0, %eax
    jne example1

    jmp example2
example1:
    jmp example3
example2:
    movl $4, %esi
example3:
    movl %esi, %eax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret
