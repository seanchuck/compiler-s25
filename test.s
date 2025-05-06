
str0:
    .string "H"
str1:
    .string "e"
str2:
    .string "l"
str3:
    .string "l"
str4:
    .string "*"
str5:
    .string "o"
str6:
    .string "*"
str7:
    .string "*"
str8:
    .string "*"
str9:
    .string "*"
str10:
    .string " "
str11:
    .string "*"
str12:
    .string "W"
str13:
    .string "o"
str14:
    .string "r"
str15:
    .string "*"
str16:
    .string "l"
str17:
    .string "d"
str18:
    .string "["
str19:
    .string "]"
str20:
    .string "\n"
str21:
    .string "should be here\n"
str22:
    .string "should not be here\n"
str23:
    .string "should be here\n"
str24:
    .string "should not be here\n"
not2:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
not20:
    movl %edi, %eax
    cmpl $0, %eax
    jne not21

    jmp not22
not21:
    movl $0, %eax
    movl %eax, -8(%rbp)
    jmp not23
not22:
    movl $1, %eax
    movl %eax, -8(%rbp)
    jmp not23
not23:
    movl -8(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

phw:
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    pushq %rbp
    movq %rsp, %rbp
    subq $272, %rsp
phw0:
    movl 48(%rbp), %eax
    movl %eax, -28(%rbp)
    movl 56(%rbp), %eax
    movl %eax, %r15d
    movl 64(%rbp), %eax
    movl %eax, %r14d
    movl 72(%rbp), %eax
    movl %eax, %r13d
    movl %edi, %eax
    cmpl $0, %eax
    jne phw1

    jmp phw2
phw1:
    leaq str0(%rip), %rax
    movq %rax, %r12
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %esi, %eax
    cmpl $0, %eax
    jne phw3

    jmp phw4
phw2:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 0(%rsp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw19

    jmp phw20
phw3:
    leaq str1(%rip), %rax
    movq %rax, %r12
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %edx, %eax
    cmpl $0, %eax
    jne phw5

    jmp phw6
phw4:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 8(%rsp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw17

    jmp phw18
phw5:
    leaq str2(%rip), %rax
    movq %rax, %r12
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw7

    jmp phw8
phw6:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 16(%rsp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw15

    jmp phw16
phw7:
    leaq str3(%rip), %rax
    movq %rax, %r12
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %r8d, %eax
    cmpl $0, %eax
    jne phw9

    jmp phw10
phw8:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 24(%rsp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw13

    jmp phw14
phw9:
    leaq str4(%rip), %rax
    movq %rax, %r12
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw10
phw10:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 32(%rsp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %r8d
    movl %r8d, %eax
    cmpl $0, %eax
    jne phw11

    jmp phw12
phw11:
    leaq str5(%rip), %rax
    movq %rax, %r8
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 32(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw12
phw12:
    jmp phw8
phw13:
    leaq str6(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw14
phw14:
    jmp phw6
phw15:
    leaq str7(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw16
phw16:
    jmp phw4
phw17:
    leaq str8(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw18
phw18:
    jmp phw2
phw19:
    leaq str9(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw20
phw20:
    leaq str10(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 40(%rsp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw21

    jmp phw22
phw21:
    leaq str11(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw22
phw22:
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne phw23

    jmp phw24
phw23:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl %r15d, %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne phw25

    jmp phw26
phw24:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl -28(%rbp), %edi
    xor %rax, %rax
    call not2
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, -192(%rbp)
    movl -192(%rbp), %eax
    cmpl $0, %eax
    jne phw31

    jmp phw32
phw25:
    leaq str12(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %r14d, %eax
    cmpl $0, %eax
    jne phw27

    jmp phw28
phw26:
    jmp phw24
phw27:
    leaq str13(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %r13d, %eax
    cmpl $0, %eax
    jne phw29

    jmp phw30
phw28:
    jmp phw26
phw29:
    leaq str14(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw30
phw30:
    jmp phw28
phw31:
    leaq str15(%rip), %rax
    movq %rax, -200(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq -200(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp phw32
phw32:
    leaq str16(%rip), %rax
    movq %rax, -208(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq -208(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str17(%rip), %rax
    movq %rax, -216(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq -216(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movq %rbp, %rsp
    popq %rbp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $192, %rsp
main0:
    leaq str18(%rip), %rax
    movq %rax, -8(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -8(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl $0, %eax
    movq %rax, 8(%rsp)
    movl $1, %eax
    movq %rax, 16(%rsp)
    movl $1, %eax
    movq %rax, 24(%rsp)
    movq %rdi, 32(%rsp)
    movq %rsi, 40(%rsp)
    movq %rcx, 56(%rsp)
    movl $1, %edi
    movl $1, %esi
    movl $1, %edx
    movl $1, %ecx
    movl $0, %r8d
    movl $1, %r9d
    xor %rax, %rax
    call phw
    movq 32(%rsp), %rdi
    movq 40(%rsp), %rsi
    movq 56(%rsp), %rcx
    leaq str19(%rip), %rax
    movq %rax, -16(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -16(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    leaq str20(%rip), %rax
    movq %rax, -24(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -24(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl $2, %edi
    movl $3, %esi
    movl $5, %ecx
    movl %esi, %eax
    subl %ecx, %eax
    movl %eax, %esi
    movl $2, %ecx
    movl %esi, %eax
    imul %ecx, %eax
    movl %eax, %ecx
    cmpl %ecx, %edi
    setg %al
    movzbq %al, %rax
    movl %eax, -52(%rbp)
    movl -52(%rbp), %eax
    cmpl $0, %eax
    jne main1

    jmp main2
main1:
    leaq str21(%rip), %rax
    movq %rax, -60(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -60(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl $2, %esi
    movl $5, %ecx
    cmpl %ecx, %esi
    setg %al
    movzbq %al, %rax
    movl %eax, -72(%rbp)
    movl -72(%rbp), %eax
    cmpl $0, %eax
    jne main4

    jmp main5
main2:
    leaq str24(%rip), %rax
    movq %rax, -96(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -96(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    jmp main3
main3:
    movl $0, -100(%rbp)
    movl -100(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret
main4:
    leaq str22(%rip), %rax
    movq %rax, -80(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -80(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    jmp main6
main5:
    leaq str23(%rip), %rax
    movq %rax, -88(%rbp)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq -88(%rbp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    jmp main6
main6:
    jmp main3

