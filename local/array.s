
.comm array, 88, 8
str0:
    .string "below output should be 18 16 14 12 10 8 6 4 2 0\n"
str1:
    .string "length of array: %d\n"
str2:
    .string "%d "
str3:
    .string "\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
main0:
    leaq str0(%rip), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rdi
    call printf
    leaq str1(%rip), %rax
    movq %rax, -24(%rbp)
    movq array, %rax
    movq %rax, -32(%rbp)
    movq -24(%rbp), %rdi
    movq -32(%rbp), %rsi
    call printf
    movq $0, %rax
    movq %rax, -8(%rbp)
    jmp main1
main1:
    movq array, %rax
    movq %rax, -40(%rbp)
    movq -8(%rbp), %rax
    cmpq -40(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rax
    cmpq $0, %rax
    jne main2

    jmp main4
main2:
    movq -8(%rbp), %rax
    imul $2, %rax
    movq %rax, -56(%rbp)
    movq -8(%rbp), %r10
    movq -56(%rbp), %rax
    movq %rax, array(, %r10, 8)
    jmp main3
main3:
    movq -8(%rbp), %rax
    addq $1, %rax
    movq %rax, -8(%rbp)
    jmp main1
main4:
    movq $0, %rax
    movq %rax, -8(%rbp)
    jmp main5
main5:
    cmpq $10, -8(%rbp)
    setl %al
    movzbq %al, %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    cmpq $0, %rax
    jne main6

    jmp main8
main6:
    leaq str2(%rip), %rax
    movq %rax, -72(%rbp)
    movq $9, %rax
    subq -8(%rbp), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %r10
    movq array(, %r10, 8), %rax
    movq %rax, -80(%rbp)
    movq -72(%rbp), %rdi
    movq -80(%rbp), %rsi
    call printf
    jmp main7
main7:
    movq -8(%rbp), %rax
    addq $1, %rax
    movq %rax, -8(%rbp)
    jmp main5
main8:
    leaq str3(%rip), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    call printf
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

