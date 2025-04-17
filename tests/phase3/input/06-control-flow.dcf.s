
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
    subq $256, %rsp
main0:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -32(%rbp)
    movq -32(%rbp), %rax
    movq %rax, -16(%rbp)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -24(%rbp)
    jmp main1
main1:
    movq $10, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -40(%rbp), %rax
    cmpq -48(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rax
    cmpq $0, %rax
    jne main2

    jmp main4
main2:
    movq -32(%rbp), %rax
    addq -40(%rbp), %rax
    movq %rax, -16(%rbp)
    jmp main3
main3:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -64(%rbp)
    movq -40(%rbp), %rax
    addq -64(%rbp), %rax
    movq %rax, -24(%rbp)
    jmp main1
main4:
    movq $45, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -72(%rbp)
    movq -32(%rbp), %rax
    cmpq -72(%rbp), %rax
    setne %al
    movzbq %al, %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rax
    cmpq $0, %rax
    jne main5

    jmp main6
main5:
    leaq str0(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main6
main6:
    movq $11, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -96(%rbp)
    movq -96(%rbp), %rax
    movq %rax, -8(%rbp)
    movq $10, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -104(%rbp)
    movq -104(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main7
main7:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -112(%rbp)
    movq -24(%rbp), %rax
    cmpq -112(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -120(%rbp)
    movq -120(%rbp), %rax
    cmpq $0, %rax
    jne main8

    jmp main10
main8:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -128(%rbp)
    movq -96(%rbp), %rax
    addq -128(%rbp), %rax
    movq %rax, -8(%rbp)
    jmp main9
main9:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -136(%rbp)
    movq -24(%rbp), %rax
    addq -136(%rbp), %rax
    movq %rax, -24(%rbp)
    jmp main7
main10:
    movq $11, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -144(%rbp)
    movq -96(%rbp), %rax
    cmpq -144(%rbp), %rax
    setne %al
    movzbq %al, %rax
    movq %rax, -152(%rbp)
    movq -152(%rbp), %rax
    cmpq $0, %rax
    jne main11

    jmp main12
main11:
    leaq str1(%rip), %rax
    movq %rax, -160(%rbp)
    movq -160(%rbp), %rdi
    movq $0, %rax
    call printf
    leaq str2(%rip), %rax
    movq %rax, -168(%rbp)
    movq -168(%rbp), %rdi
    movq -96(%rbp), %rsi
    movq $0, %rax
    call printf
    jmp main12
main12:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -176(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -184(%rbp)
    movq -176(%rbp), %rax
    cmpq -184(%rbp), %rax
    setg %al
    movzbq %al, %rax
    movq %rax, -192(%rbp)
    movq -192(%rbp), %rax
    cmpq $0, %rax
    jne main13

    jmp main14
main13:
    leaq str3(%rip), %rax
    movq %rax, -200(%rbp)
    movq -200(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main15
main14:
    jmp main15
main15:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -208(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -216(%rbp)
    movq -208(%rbp), %rax
    cmpq -216(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -224(%rbp)
    movq -224(%rbp), %rax
    cmpq $0, %rax
    jne main16

    jmp main17
main16:
    jmp main18
main17:
    leaq str4(%rip), %rax
    movq %rax, -232(%rbp)
    movq -232(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main18
main18:
    leaq str5(%rip), %rax
    movq %rax, -240(%rbp)
    movq -240(%rbp), %rdi
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -248(%rbp)
    movq -248(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

