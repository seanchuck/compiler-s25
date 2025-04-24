
str0:
    .string "ERROR: for loop is bad (1)\n"
str1:
    .string "ERROR: for loop is bad (2)\n"
str2:
    .string "%d\n"
str3:
    .string "ERROR: true branch is bad (2)\n"
str4:
    .string "ERROR: else branch is bad (2)\n"
str5:
    .string "control flow OK if no previous output\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $160, %rsp
main0:
    movl $0, -16(%rbp)
    movl -16(%rbp), %eax
    movl %eax, -8(%rbp)
    movl $0, -20(%rbp)
    movl -20(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main1
main1:
    movl $10, -24(%rbp)
    movl -20(%rbp), %eax
    cmpl -24(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -28(%rbp)
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    movl -16(%rbp), %eax
    addl -20(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main3
main3:
    movl $1, -32(%rbp)
    movl -20(%rbp), %eax
    addl -32(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main1
main4:
    movl $45, -36(%rbp)
    movl -16(%rbp), %eax
    cmpl -36(%rbp), %eax
    setne %al
    movzbq %al, %rax
    movl %eax, -40(%rbp)
    movl -40(%rbp), %eax
    cmpl $0, %eax
    jne main5

    jmp main6
main5:
    leaq str0(%rip), %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main6
main6:
    movl $11, -52(%rbp)
    movl -52(%rbp), %eax
    movl %eax, -4(%rbp)
    movl $10, -56(%rbp)
    movl -56(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main7
main7:
    movl $0, -60(%rbp)
    movl -56(%rbp), %eax
    cmpl -60(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -64(%rbp)
    movl -64(%rbp), %eax
    cmpl $0, %eax
    jne main8

    jmp main10
main8:
    movl $1, -68(%rbp)
    movl -52(%rbp), %eax
    addl -68(%rbp), %eax
    movl %eax, -4(%rbp)
    jmp main9
main9:
    movl $1, -72(%rbp)
    movl -56(%rbp), %eax
    addl -72(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main7
main10:
    movl $11, -76(%rbp)
    movl -52(%rbp), %eax
    cmpl -76(%rbp), %eax
    setne %al
    movzbq %al, %rax
    movl %eax, -80(%rbp)
    movl -80(%rbp), %eax
    cmpl $0, %eax
    jne main11

    jmp main12
main11:
    leaq str1(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq $0, %rax
    call printf
    leaq str2(%rip), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    movl -52(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main12
main12:
    movl $1, -100(%rbp)
    movl $2, -104(%rbp)
    movl -100(%rbp), %eax
    cmpl -104(%rbp), %eax
    setg %al
    movzbq %al, %rax
    movl %eax, -108(%rbp)
    movl -108(%rbp), %eax
    cmpl $0, %eax
    jne main13

    jmp main14
main13:
    leaq str3(%rip), %rax
    movq %rax, -116(%rbp)
    movq -116(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main15
main14:
    jmp main15
main15:
    movl $1, -120(%rbp)
    movl $2, -124(%rbp)
    movl -120(%rbp), %eax
    cmpl -124(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -128(%rbp)
    movl -128(%rbp), %eax
    cmpl $0, %eax
    jne main16

    jmp main17
main16:
    jmp main18
main17:
    leaq str4(%rip), %rax
    movq %rax, -136(%rbp)
    movq -136(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main18
main18:
    leaq str5(%rip), %rax
    movq %rax, -144(%rbp)
    movq -144(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -148(%rbp)
    movl -148(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

