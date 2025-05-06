
.comm x, 4004, 4
.comm y, 4004, 4
str0:
    .string "%d "
str1:
    .string "\n"
.globl main
main:
    pushq %r12
    pushq %rbp
    movq %rsp, %rbp
    subq $200, %rsp
    movq $1000, %rax
    movq %rax, x
    movq $1000, %rax
    movq %rax, y
main0:
    movl $0, -24(%rbp)
    movl -24(%rbp), %eax
    movl %eax, %esi
    jmp main1
main1:
    movl $1000, %ecx
    cmpl %ecx, %esi
    setl %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    movl %esi, %r10d
    addl $1, %r10d
    movl %esi, %eax
    movl %eax, x(, %r10, 4)
    movl $0, %ecx
    movl %esi, %r10d
    addl $1, %r10d
    movl %ecx, %eax
    movl %eax, y(, %r10, 4)
    jmp main3
main3:
    movl $1, %ecx
    movl %esi, %esi
    addl %ecx, %esi
    jmp main1
main4:
    movl $0, -44(%rbp)
    movl -44(%rbp), %eax
    movl %eax, %esi
    jmp main5
main5:
    movl $998, %ecx
    cmpl %ecx, %esi
    setl %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne main6

    jmp main8
main6:
    movl $998, %ecx
    movl %ecx, %eax
    subl %esi, %eax
    movl %eax, %r8d
    movl %r8d, %eax
    movl %eax, %ecx
    movl $1, %edi
    movl %r8d, %edi
    addl %edi, %edi
    movl %edi, %r10d
    addl $1, %r10d
    movl x(, %r10, 4), %eax
    movl %eax, %r12d
    movl %ecx, %r10d
    addl $1, %r10d
    movl x(, %r10, 4), %eax
    movl %eax, %r9d
    movl $1, %edi
    movl %r8d, %eax
    subl %edi, %eax
    movl %eax, %edi
    movl %edi, %r10d
    addl $1, %r10d
    movl x(, %r10, 4), %eax
    movl %eax, %r8d
    movl $2, %edi
    movl %r9d, %eax
    imul %edi, %eax
    movl %eax, %edi
    movl %edi, %edi
    addl %r12d, %edi
    movl %edi, %edi
    addl %r8d, %edi
    movl %ecx, %r10d
    addl $1, %r10d
    movl %edi, %eax
    movl %eax, y(, %r10, 4)
    jmp main7
main7:
    movl $1, %ecx
    movl %esi, %esi
    addl %ecx, %esi
    jmp main5
main8:
    movl $0, -116(%rbp)
    movl -116(%rbp), %eax
    movl %eax, %edi
    jmp main9
main9:
    movl $1000, %ecx
    cmpl %ecx, %edi
    setl %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne main10

    jmp main12
main10:
    leaq str0(%rip), %rax
    movq %rax, %rsi
    movl %edi, %r10d
    addl $1, %r10d
    movl y(, %r10, 4), %eax
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl 24(%rsp), %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp main11
main11:
    movl $1, %ecx
    movl %edi, %edi
    addl %ecx, %edi
    jmp main9
main12:
    leaq str1(%rip), %rax
    movq %rax, -148(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq -148(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $0, -152(%rbp)
    movl -152(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %r12
    ret

