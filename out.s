
.comm x, 8, 8
str0:
    .string "x should be 0; x = %d\n"
str1:
    .string "Sum should be 10; x + y = %d\n"
str2:
    .string "This shouldnt print\n"
str3:
    .string "We have mastered shadowing\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $112, %rsp
main0:
    movq $7, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -16(%rbp)
    movq -16(%rbp), %rax
    movq %rax, x
    movq $1, %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %rax
    cmpq $0, %rax
    jne main1

    jmp main2
main1:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -32(%rbp)
    movq -32(%rbp), %rax
    movq %rax, -24(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rdi
    movq -32(%rbp), %rsi
    movq $0, %rax
    call printf
    jmp main2
main2:
    movq -8(%rbp), %rax
    cmpq $0, %rax
    jne main3

    jmp main4
main3:
    movq $3, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    leaq str1(%rip), %rax
    movq %rax, -64(%rbp)
    movq x, %rax
    addq -56(%rbp), %rax
    movq %rax, -72(%rbp)
    movq -64(%rbp), %rdi
    movq -72(%rbp), %rsi
    movq $0, %rax
    call printf
    jmp main4
main4:
    movq -8(%rbp), %rax
    cmpq $0, %rax
    jne main5

    jmp main6
main5:
    movq $0, %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rax
    cmpq $0, %rax
    jne main7

    jmp main8
main6:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -104(%rbp)
    movq -104(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
main7:
    leaq str2(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main9
main8:
    leaq str3(%rip), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main9
main9:
    jmp main6

