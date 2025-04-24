Replacing j with _t1
Replacing j with _t1
Replacing sum with _t0
Replacing j with _t1
Replacing sum with _t0
Replacing j with _t1
Replacing sum with _t0
Replacing j with _t11
Replacing i with _t10
Replacing j with _t11
Replacing i with _t10
Replacing i with _t10

str0:
    .string "j is %d\n"
str1:
    .string "sum is %d\n"
str2:
    .string "ERROR: for loop is bad (1)\n"
str3:
    .string "hi\n"
str4:
    .string "ERROR: for loop is bad (2)\n"
str5:
    .string "%d\n"
str6:
    .string "ERROR: true branch is bad (2)\n"
str7:
    .string "ERROR: else branch is bad (2)\n"
str8:
    .string "control flow OK if no previous output\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $176, %rsp
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
    leaq str0(%rip), %rax
    movq %rax, -36(%rbp)
    movq -36(%rbp), %rdi
    movl -20(%rbp), %esi
    movq $0, %rax
    call printf
    movl -16(%rbp), %eax
    addl -20(%rbp), %eax
    movl %eax, -8(%rbp)
    leaq str1(%rip), %rax
    movq %rax, -44(%rbp)
    movq -44(%rbp), %rdi
    movl -16(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main3
main3:
    movl $1, -48(%rbp)
    movl -20(%rbp), %eax
    addl -48(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main1
main4:
    movl $45, -52(%rbp)
    movl -16(%rbp), %eax
    cmpl -52(%rbp), %eax
    setne %al
    movzbq %al, %rax
    movl %eax, -56(%rbp)
    movl -56(%rbp), %eax
    cmpl $0, %eax
    jne main5

    jmp main6
main5:
    leaq str2(%rip), %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main6
main6:
    movl $11, -68(%rbp)
    movl -68(%rbp), %eax
    movl %eax, -4(%rbp)
    movl $10, -72(%rbp)
    movl -72(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main7
main7:
    movl $0, -76(%rbp)
    movl -72(%rbp), %eax
    cmpl -76(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -80(%rbp)
    movl -80(%rbp), %eax
    cmpl $0, %eax
    jne main8

    jmp main10
main8:
    leaq str3(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $1, -92(%rbp)
    movl -68(%rbp), %eax
    addl -92(%rbp), %eax
    movl %eax, -4(%rbp)
    jmp main9
main9:
    movl $1, -96(%rbp)
    movl -72(%rbp), %eax
    addl -96(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main7
main10:
    movl $11, -100(%rbp)
    movl -68(%rbp), %eax
    cmpl -100(%rbp), %eax
    setne %al
    movzbq %al, %rax
    movl %eax, -104(%rbp)
    movl -104(%rbp), %eax
    cmpl $0, %eax
    jne main11

    jmp main12
main11:
    leaq str4(%rip), %rax
    movq %rax, -112(%rbp)
    movq -112(%rbp), %rdi
    movq $0, %rax
    call printf
    leaq str5(%rip), %rax
    movq %rax, -120(%rbp)
    movq -120(%rbp), %rdi
    movl -68(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main12
main12:
    movl $1, -124(%rbp)
    movl $2, -128(%rbp)
    movl -124(%rbp), %eax
    cmpl -128(%rbp), %eax
    setg %al
    movzbq %al, %rax
    movl %eax, -132(%rbp)
    movl -132(%rbp), %eax
    cmpl $0, %eax
    jne main13

    jmp main14
main13:
    leaq str6(%rip), %rax
    movq %rax, -140(%rbp)
    movq -140(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main15
main14:
    jmp main15
main15:
    movl $1, -144(%rbp)
    movl $2, -148(%rbp)
    movl -144(%rbp), %eax
    cmpl -148(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -152(%rbp)
    movl -152(%rbp), %eax
    cmpl $0, %eax
    jne main16

    jmp main17
main16:
    jmp main18
main17:
    leaq str7(%rip), %rax
    movq %rax, -160(%rbp)
    movq -160(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main18
main18:
    leaq str8(%rip), %rax
    movq %rax, -168(%rbp)
    movq -168(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -172(%rbp)
    movl -172(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

