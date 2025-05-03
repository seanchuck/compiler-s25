main
applying assignments:

 1: Reg(
    R14,
)
 2: Reg(
    R12,
)
 3: Reg(
    R12,
)
 5: Reg(
    R13,
)
 6: Reg(
    R12,
)
 10: Reg(
    R13,
)
 11: Reg(
    R12,
)
 12: Reg(
    R12,
)
 13: Reg(
    R14,
)
 14: Reg(
    R12,
)

.comm x, 4004, 4
.comm y, 4004, 4
str0:
    .string "%d\n"
str1:
    .string "eeee"
str2:
    .string "%d "
str3:
    .string "done"
str4:
    .string "\n"
.globl main
main:
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rsp, %rbp
    subq $96, %rsp
    movq $1000, %rax
    movq %rax, x
    movq $1000, %rax
    movq %rax, y
main0:
    movl $0, -16(%rbp)
    movl -16(%rbp), %eax
    movl %eax, %r14d
    jmp main1
main1:
    movl $1000, %r12d
    cmpl %r12d, %r14d
    setl %al
    movzbq %al, %rax
    movl %eax, %r12d
    movl %r12d, %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    movl %r14d, %r10d
    addl $1, %r10d
    movl %r14d, %eax
    movl %eax, x(, %r10, 4)
    leaq str0(%rip), %rax
    movq %rax, %r13
    movl %r14d, %r10d
    addl $1, %r10d
    movl x(, %r10, 4), %eax
    movl %eax, %r12d
    movq %r13, %rdi
    movl %r12d, %esi
    movq $0, %rax
    call printf
    leaq str1(%rip), %rax
    movq %rax, -44(%rbp)
    movq -44(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main3
main3:
    movl $1, -48(%rbp)
    movl %r14d, %eax
    addl -48(%rbp), %eax
    movl %eax, %r14d
    jmp main1
main4:
    movl $0, -52(%rbp)
    movl -52(%rbp), %eax
    movl %eax, %r13d
    jmp main5
main5:
    movl $1000, %r12d
    cmpl %r12d, %r13d
    setl %al
    movzbq %al, %rax
    movl %eax, %r12d
    movl %r12d, %eax
    cmpl $0, %eax
    jne main6

    jmp main8
main6:
    leaq str2(%rip), %rax
    movq %rax, %r14
    movl %r13d, %r10d
    addl $1, %r10d
    movl y(, %r10, 4), %eax
    movl %eax, %r12d
    movq %r14, %rdi
    movl %r12d, %esi
    movq $0, %rax
    call printf
    jmp main7
main7:
    movl $1, -76(%rbp)
    movl %r13d, %eax
    addl -76(%rbp), %eax
    movl %eax, %r13d
    jmp main5
main8:
    leaq str3(%rip), %rax
    movq %rax, -84(%rbp)
    movq -84(%rbp), %rdi
    movq $0, %rax
    call printf
    leaq str4(%rip), %rax
    movq %rax, -92(%rbp)
    movq -92(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -96(%rbp)
    movl -96(%rbp), %eax
    movq %rbp, %rsp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    ret

