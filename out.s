
str0:
    .string "%d %d\n"
str1:
    .string "%d %d\n"
.globl main
main:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $152, %rsp
main0:
    movl $10, %esi
    addl $20, %esi
    movl $30, %ebx
    imul $3, %ebx
    movl %ebx, %ebx
    imul %esi, %ebx
    movl %ebx, %ecx
    subl $100, %ecx
    leaq str0(%rip), %rax
    movq %rax, %rbx
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %rbx, %rdi
    movl 8(%rsp), %esi
    movl 24(%rsp), %edx
    xorq %rax, %rax
    call printf
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %esi, %eax
    cdq
    movl $16, %r11d
    idivl %r11d
    movl %edx, %esi
    pushq %rdx
    movl %ecx, %r11d
    movabs $184467440737095517, %rax
    mul %r11
    movl %edx, %ecx
    popq %rdx
    leaq str1(%rip), %rax
    movq %rax, %rbx
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %rbx, %rdi
    movl 8(%rsp), %esi
    movl 24(%rsp), %edx
    xorq %rax, %rax
    call printf
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl $0, -100(%rbp)
    movl -100(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

