
.comm x, 8008, 8
.comm y, 8008, 8
str0:
    .string "%d "
str1:
    .string "\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $288, %rsp
    movq $1000, %rax
    movq %rax, x
    movq $1000, %rax
    movq %rax, y
main0:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main1
main1:
    movq $1000, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -40(%rbp), %rax
    cmpq -56(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    cmpq $0, %rax
    jne main2

    jmp main4
main2:
    movq -40(%rbp), %r10
    addq $1, %r10
    movq -48(%rbp), %rax
    movq %rax, x(, %r10, 8)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -72(%rbp)
    movq -40(%rbp), %r10
    addq $1, %r10
    movq -72(%rbp), %rax
    movq %rax, y(, %r10, 8)
    jmp main3
main3:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -80(%rbp)
    movq -40(%rbp), %rax
    addq -80(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main1
main4:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -88(%rbp)
    movq -88(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main5
main5:
    movq $998, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -96(%rbp)
    movq -40(%rbp), %rax
    cmpq -96(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -104(%rbp)
    movq -104(%rbp), %rax
    cmpq $0, %rax
    jne main6

    jmp main8
main6:
    movq $998, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -120(%rbp)
    movq -120(%rbp), %rax
    subq -40(%rbp), %rax
    movq %rax, -128(%rbp)
    movq -128(%rbp), %rax
    movq %rax, -112(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -144(%rbp)
    movq -112(%rbp), %rax
    addq -144(%rbp), %rax
    movq %rax, -152(%rbp)
    movq -152(%rbp), %r10
    addq $1, %r10
    movq x(, %r10, 8), %rax
    movq %rax, -136(%rbp)
    movq -136(%rbp), %rax
    movq %rax, -8(%rbp)
    movq -128(%rbp), %r10
    addq $1, %r10
    movq x(, %r10, 8), %rax
    movq %rax, -160(%rbp)
    movq -160(%rbp), %rax
    movq %rax, -16(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -176(%rbp)
    movq -112(%rbp), %rax
    subq -176(%rbp), %rax
    movq %rax, -184(%rbp)
    movq -184(%rbp), %r10
    addq $1, %r10
    movq x(, %r10, 8), %rax
    movq %rax, -168(%rbp)
    movq -168(%rbp), %rax
    movq %rax, -24(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -192(%rbp)
    movq -16(%rbp), %rax
    imul -192(%rbp), %rax
    movq %rax, -200(%rbp)
    movq -200(%rbp), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %rax
    addq -8(%rbp), %rax
    movq %rax, -208(%rbp)
    movq -208(%rbp), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %rax
    addq -24(%rbp), %rax
    movq %rax, -216(%rbp)
    movq -216(%rbp), %rax
    movq %rax, -32(%rbp)
    movq -112(%rbp), %r10
    addq $1, %r10
    movq -216(%rbp), %rax
    movq %rax, y(, %r10, 8)
    jmp main7
main7:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -224(%rbp)
    movq -40(%rbp), %rax
    addq -224(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main5
main8:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -232(%rbp)
    movq -232(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main9
main9:
    movq $1000, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -240(%rbp)
    movq -40(%rbp), %rax
    cmpq -240(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -248(%rbp)
    movq -248(%rbp), %rax
    cmpq $0, %rax
    jne main10

    jmp main12
main10:
    leaq str0(%rip), %rax
    movq %rax, -256(%rbp)
    movq -232(%rbp), %r10
    addq $1, %r10
    movq y(, %r10, 8), %rax
    movq %rax, -264(%rbp)
    movq -256(%rbp), %rdi
    movq -264(%rbp), %rsi
    movq $0, %rax
    call printf
    jmp main11
main11:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -272(%rbp)
    movq -40(%rbp), %rax
    addq -272(%rbp), %rax
    movq %rax, -40(%rbp)
    jmp main9
main12:
    leaq str1(%rip), %rax
    movq %rax, -280(%rbp)
    movq -280(%rbp), %rdi
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -288(%rbp)
    movq -288(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

