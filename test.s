
str0:
    .string "Top of loop\n"
str1:
    .string "done z gurp\n"
str2:
    .string "%d"
str3:
    .string "\n"
str4:
    .string "done 0z gurp\n"
str5:
    .string "done y gurp\n"
str6:
    .string "done a gurp\n"
str7:
    .string "done b gurp\n"
str8:
    .string "After gurps\n"
str9:
    .string "%d"
str10:
    .string " "
str11:
    .string "%d"
str12:
    .string " "
str13:
    .string "%d"
str14:
    .string " "
str15:
    .string "%d"
str16:
    .string "\n"
str17:
    .string "top of gurp\n"
str18:
    .string "done j\n"
str19:
    .string "done i\n"
str20:
    .string "done j\n"
str21:
    .string "done k\n"
str22:
    .string "done k\n"
str23:
    .string "done m\n%d %d\n"
str24:
    .string "after n\n"
str25:
    .string "after first if\n"
str26:
    .string "after second if\n"
str27:
    .string "after third if\n"
str28:
    .string "%d"
str29:
    .string " "
str30:
    .string "%d"
str31:
    .string " "
str32:
    .string "%d"
str33:
    .string " "
str34:
    .string "%d"
str35:
    .string "%d"
str36:
    .string "%d"
str37:
    .string "after fourth if\n"
.globl main
main:
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    pushq %rbp
    movq %rsp, %rbp
    subq $496, %rsp
main0:
    movl $3, -24(%rbp)
    movl -24(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp main1
main1:
    movl $5, %ecx
    cmpl %ecx, -20(%rbp)
    setl %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    leaq str0(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %ecx
    movl %ecx, %eax
    addl -20(%rbp), %eax
    movl %eax, %edi
    movl $0, %ecx
    movl %ecx, %eax
    subl -20(%rbp), %eax
    movl %eax, %esi
    movl $3, %ecx
    movl $2, %r8d
    movl %r8d, %eax
    addl -20(%rbp), %eax
    movl %eax, %r12d
    movl $1, %r8d
    movl %r8d, %eax
    subl -20(%rbp), %eax
    movl %eax, %r9d
    movl $4, %r8d
    movl %r8d, %eax
    subl -20(%rbp), %eax
    movl %eax, %r8d
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl %r8d, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl 16(%rsp), %edi
    movl 24(%rsp), %esi
    movl $1, %edx
    movl 40(%rsp), %ecx
    movl %r12d, %r8d
    movl 56(%rsp), %r9d
    xor %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, -88(%rbp)
    leaq str1(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str2(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    movl -20(%rbp), %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str3(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %ecx
    movl %ecx, %eax
    addl -20(%rbp), %eax
    movl %eax, %ecx
    movl $0, %esi
    movl %esi, %eax
    subl -20(%rbp), %eax
    movl %eax, %r12d
    movl $3, %r9d
    movl $2, %esi
    movl %esi, %eax
    addl -20(%rbp), %eax
    movl %eax, %r8d
    movl $1, %esi
    movl -20(%rbp), %eax
    addl %esi, %eax
    movl %eax, %edi
    movl $0, %esi
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl %esi, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl 40(%rsp), %edi
    movl %r12d, %esi
    movl $1, %edx
    movl 56(%rsp), %ecx
    movl 48(%rsp), %r8d
    movl 16(%rsp), %r9d
    xor %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, -156(%rbp)
    leaq str4(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $3, %ecx
    movl %ecx, %eax
    subl -20(%rbp), %eax
    movl %eax, %edi
    movl $-8, %ecx
    movl %ecx, %eax
    addl -20(%rbp), %eax
    movl %eax, %esi
    movl $12, %r8d
    movl $3, %ecx
    movl %ecx, %eax
    imul -20(%rbp), %eax
    movl %eax, %ecx
    movl %r8d, %eax
    subl %ecx, %eax
    movl %eax, %ecx
    movl $16, %r8d
    movl %r8d, %eax
    addl -20(%rbp), %eax
    movl %eax, %r12d
    movl $1, %r9d
    movl $8, %r8d
    movl %r8d, %eax
    subl -20(%rbp), %eax
    movl %eax, %r8d
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl %r8d, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl 16(%rsp), %edi
    movl 24(%rsp), %esi
    movl $0, %edx
    movl 40(%rsp), %ecx
    movl %r12d, %r8d
    movl 56(%rsp), %r9d
    xor %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %r15d
    leaq str5(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $2, %ecx
    movl %ecx, %eax
    subl -20(%rbp), %eax
    movl %eax, %r9d
    movl $6, %ecx
    movl %ecx, %eax
    addl -20(%rbp), %eax
    movl %eax, %r8d
    movl $-3, %ecx
    movl -20(%rbp), %eax
    imul %ecx, %eax
    movl %eax, %edi
    movl $1, %esi
    movl $3, %ecx
    movl $2, %r12d
    movl -20(%rbp), %eax
    imul %r12d, %eax
    movl %eax, %r12d
    movl %ecx, %eax
    subl %r12d, %eax
    movl %eax, %ecx
    movl $5, %r12d
    movl %r12d, %eax
    subl -20(%rbp), %eax
    movl %eax, %r12d
    movl $0, %eax
    movq %rax, 0(%rsp)
    movl %r12d, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl 56(%rsp), %edi
    movl 48(%rsp), %esi
    movl $0, %edx
    movl 16(%rsp), %ecx
    movl 24(%rsp), %r8d
    movl 40(%rsp), %r9d
    xor %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %r14d
    leaq str6(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $-3, %r13d
    movl $8, %r12d
    movl $7, %ecx
    movl %ecx, %eax
    subl -20(%rbp), %eax
    movl %eax, %r9d
    movl $4, %ecx
    movl -20(%rbp), %eax
    subl %ecx, %eax
    movl %eax, %r8d
    movl $2, %edi
    movl $6, %esi
    movl $9, %ecx
    movl -20(%rbp), %eax
    imul %ecx, %eax
    movl %eax, %ecx
    movl %esi, %eax
    subl %ecx, %eax
    movl %eax, %ecx
    movl $0, %eax
    movq %rax, 0(%rsp)
    movl %ecx, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl %r13d, %edi
    movl %r12d, %esi
    movl $1, %edx
    movl 56(%rsp), %ecx
    movl 48(%rsp), %r8d
    movl 16(%rsp), %r9d
    xor %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %ecx
    leaq str7(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str8(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str9(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl %r15d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str10(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str11(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl -156(%rbp), %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str12(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str13(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl %r14d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str14(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str15(%rip), %rax
    movq %rax, %rsi
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
    leaq str16(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp main3
main3:
    movl $1, %ecx
    movl -20(%rbp), %eax
    addl %ecx, %eax
    movl %eax, -20(%rbp)
    jmp main1
main4:
    movl $0, -428(%rbp)
    movl -428(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    ret

gurp:
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    pushq %rbp
    movq %rsp, %rbp
    subq $928, %rsp
gurp0:
    movl %edi, %eax
    movl %eax, %edi
    movl %esi, %eax
    movl %eax, %esi
    movl %edx, %eax
    movl %eax, %edx
    movl %ecx, %eax
    movl %eax, %ecx
    movl %r8d, %eax
    movl %eax, %r8d
    movl %r9d, %eax
    movl %eax, %r9d
    movl 48(%rbp), %eax
    movl %eax, -28(%rbp)
    movl 56(%rbp), %eax
    movl %eax, -32(%rbp)
    leaq str17(%rip), %rax
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
    movl %edi, %eax
    imul %esi, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r8d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %ecx, %eax
    movl %eax, %r13d
    leaq str18(%rip), %rax
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
    movl $1, %r12d
    movl %r13d, %eax
    subl %r12d, %eax
    movl %eax, -156(%rbp)
    leaq str19(%rip), %rax
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
    movl $3, %r12d
    movl %r12d, %eax
    addl -156(%rbp), %eax
    movl %eax, %r14d
    leaq str20(%rip), %rax
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
    movl -156(%rbp), %eax
    addl %r14d, %eax
    movl %eax, %r13d
    movl $2, %r12d
    movl %r13d, %eax
    subl %r12d, %eax
    movl %eax, -192(%rbp)
    leaq str21(%rip), %rax
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
    movl -192(%rbp), %eax
    subl %r14d, %eax
    movl %eax, %r13d
    movl %r13d, %eax
    movl %eax, -48(%rbp)
    leaq str22(%rip), %rax
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
    movl -156(%rbp), %eax
    addl -192(%rbp), %eax
    movl %eax, -216(%rbp)
    leaq str23(%rip), %rax
    movq %rax, %r12
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl -192(%rbp), %esi
    movl -216(%rbp), %edx
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl -192(%rbp), %eax
    subl -216(%rbp), %eax
    movl %eax, %r12d
    movl %r14d, %eax
    subl %r12d, %eax
    movl %eax, %r12d
    movl -156(%rbp), %eax
    addl %r12d, %eax
    movl %eax, -236(%rbp)
    leaq str24(%rip), %rax
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
    movl $50, %r12d
    cmpl %r12d, %edi
    setg %al
    movzbq %al, %rax
    movl %eax, %r12d
    movl %r12d, %eax
    cmpl $0, %eax
    jne gurp1

    jmp gurp2
gurp1:
    movl %edi, %eax
    addl %esi, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %r13d, %eax
    movl %eax, %r14d
    movl %r14d, %eax
    movl %eax, -60(%rbp)
    movl %r13d, %eax
    subl -216(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -40(%rbp)
    movl %edx, %eax
    cmpl $0, %eax
    jne gurp4

    jmp gurp5
gurp2:
    movl %esi, %eax
    subl %edi, %eax
    movl %eax, %r15d
    movl %r15d, %eax
    movl %eax, -60(%rbp)
    movl %r13d, %eax
    addl -216(%rbp), %eax
    movl %eax, %r14d
    movl %r14d, %eax
    movl %eax, -40(%rbp)
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp10

    jmp gurp11
gurp3:
    leaq str25(%rip), %rax
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
    addl -48(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -156(%rbp), %eax
    movl %eax, -392(%rbp)
    movl -392(%rbp), %eax
    addl -40(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -60(%rbp), %eax
    movl %eax, -400(%rbp)
    movl -392(%rbp), %eax
    subl -400(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -216(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -40(%rbp), %eax
    movl %eax, -412(%rbp)
    movl -48(%rbp), %eax
    addl -400(%rbp), %eax
    movl %eax, %r13d
    movl -68(%rbp), %eax
    addl %r9d, %eax
    movl %eax, %r12d
    movl -392(%rbp), %eax
    subl %r12d, %eax
    movl %eax, %r12d
    movl %r13d, %eax
    addl %r12d, %eax
    movl %eax, -428(%rbp)
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp13

    jmp gurp14
gurp4:
    movl %r8d, %eax
    addl %r9d, %eax
    movl %eax, %r13d
    movl -32(%rbp), %eax
    addl -236(%rbp), %eax
    movl %eax, %r12d
    movl %r13d, %eax
    subl %r12d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -48(%rbp)
    movl $1, %r12d
    movl %r9d, %eax
    subl %r12d, %eax
    movl %eax, %r9d
    movl %r9d, %eax
    movl %eax, %r9d
    movl $3, %r12d
    movl %r14d, %eax
    subl %r12d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -68(%rbp)
    jmp gurp6
gurp5:
    movl $3, %r12d
    movl %r14d, %eax
    addl %r12d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -68(%rbp)
    jmp gurp6
gurp6:
    movl %edx, %eax
    cmpl $0, %eax
    jne gurp7

    jmp gurp9
gurp7:
    movl %r9d, %eax
    addl %r8d, %eax
    movl %eax, %r13d
    movl -32(%rbp), %eax
    subl %esi, %eax
    movl %eax, %r12d
    movl %r13d, %eax
    addl %r12d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -76(%rbp)
    jmp gurp8
gurp8:
    jmp gurp3
gurp9:
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp7

    jmp gurp8
gurp10:
    movl $3, %r12d
    movl %edi, %eax
    subl %r15d, %eax
    movl %eax, %r9d
    movl %r12d, %eax
    addl %r9d, %eax
    movl %eax, %r13d
    movl %r13d, %eax
    movl %eax, %r9d
    movl $3, %r12d
    movl %r12d, %eax
    subl %r15d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -68(%rbp)
    movl %r13d, %eax
    addl %r12d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -76(%rbp)
    jmp gurp12
gurp11:
    movl $3, %r13d
    movl %r15d, %eax
    subl %edi, %eax
    movl %eax, %r12d
    movl $2, %r9d
    movl %r12d, %eax
    subl %r9d, %eax
    movl %eax, %r9d
    movl %r13d, %eax
    addl %r9d, %eax
    movl %eax, %r9d
    movl %r9d, %eax
    movl %eax, -76(%rbp)
    movl %r9d, %eax
    addl %r14d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -68(%rbp)
    movl %r9d, %eax
    addl %r12d, %eax
    movl %eax, %r9d
    movl %r9d, %eax
    subl %r12d, %eax
    movl %eax, %r9d
    movl %r9d, %eax
    movl %eax, %r9d
    jmp gurp12
gurp12:
    jmp gurp3
gurp13:
    movl $3, %r12d
    movl %r12d, %eax
    movl %eax, -92(%rbp)
    jmp gurp15
gurp14:
    movl $-1, %r12d
    movl %r12d, %eax
    movl %eax, -92(%rbp)
    jmp gurp15
gurp15:
    leaq str26(%rip), %rax
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
    movl $99, %r12d
    movl %r12d, %eax
    addl -236(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r8d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -392(%rbp), %eax
    movl %eax, -460(%rbp)
    movl %r8d, %eax
    subl -76(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -236(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -60(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -216(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r8d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %esi, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %esi, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %r8d, %eax
    movl %eax, -492(%rbp)
    movl -400(%rbp), %eax
    addl -392(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -236(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -392(%rbp), %eax
    movl %eax, -504(%rbp)
    movl -32(%rbp), %eax
    addl %r9d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %ecx, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r8d, %eax
    movl %eax, -516(%rbp)
    movl $-1, %r12d
    cmpl %r12d, -92(%rbp)
    sete %al
    movzbq %al, %rax
    movl %eax, %r12d
    movl %r12d, %eax
    cmpl $0, %eax
    jne gurp16

    jmp gurp17
gurp16:
    movl -504(%rbp), %eax
    addl -516(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -116(%rbp)
    jmp gurp18
gurp17:
    movl -516(%rbp), %eax
    subl -504(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    movl %eax, -116(%rbp)
    jmp gurp18
gurp18:
    movl -460(%rbp), %eax
    subl -428(%rbp), %eax
    movl %eax, -536(%rbp)
    movl -536(%rbp), %eax
    addl -92(%rbp), %eax
    movl %eax, -540(%rbp)
    movl $42, %r12d
    movl %r12d, %eax
    subl -540(%rbp), %eax
    movl %eax, %r15d
    movl $0, %r14d
    leaq str27(%rip), %rax
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
    leaq str28(%rip), %rax
    movq %rax, %r13
    movl %edi, %eax
    subl -156(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %ecx, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -40(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r8d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r9d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -48(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -216(%rbp), %eax
    movl %eax, %r12d
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r13, %rdi
    movl %r12d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str29(%rip), %rax
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
    leaq str30(%rip), %rax
    movq %rax, %r13
    movl %esi, %eax
    subl -192(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -32(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -236(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -60(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -68(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -412(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -504(%rbp), %eax
    movl %eax, %r12d
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r13, %rdi
    movl %r12d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str31(%rip), %rax
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
    leaq str32(%rip), %rax
    movq %rax, %r13
    movl -516(%rbp), %eax
    addl -76(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -392(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -540(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -492(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %r14d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -92(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -116(%rbp), %eax
    movl %eax, %r12d
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r13, %rdi
    movl %r12d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str33(%rip), %rax
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
    jne gurp19

    jmp gurp20
gurp19:
    leaq str34(%rip), %rax
    movq %rax, %r13
    movl %r15d, %eax
    addl -460(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -428(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl -400(%rbp), %eax
    movl %eax, %r12d
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r13, %rdi
    movl %r12d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp gurp21
gurp20:
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp22

    jmp gurp23
gurp21:
    leaq str37(%rip), %rax
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
    movl %edi, %eax
    subl %esi, %eax
    movl %eax, %esi
    movl %esi, %eax
    addl %ecx, %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl %r8d, %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl %r9d, %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -32(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -156(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -40(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -192(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -48(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -216(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl -236(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -60(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl %r15d, %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -68(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl -412(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -76(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl -392(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -460(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl -428(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -92(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl %r14d, %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -492(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl -540(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -400(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -504(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    addl -116(%rbp), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    subl -516(%rbp), %eax
    movl %eax, -868(%rbp)
    movl -868(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    ret
gurp22:
    leaq str35(%rip), %rax
    movq %rax, %r13
    movl -536(%rbp), %eax
    addl -400(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl %r15d, %eax
    movl %eax, %r12d
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r13, %rdi
    movl %r12d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp gurp24
gurp23:
    leaq str36(%rip), %rax
    movq %rax, %r13
    movl -400(%rbp), %eax
    addl -428(%rbp), %eax
    movl %eax, %r12d
    movl %r12d, %eax
    addl %r15d, %eax
    movl %eax, %r12d
    movl %r12d, %eax
    subl -460(%rbp), %eax
    movl %eax, %r12d
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r13, %rdi
    movl %r12d, %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp gurp24
gurp24:
    jmp gurp21

