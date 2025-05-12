
str0:
    .string "top of gurp\n"
str1:
    .string "done j\n"
str2:
    .string "done i\n"
str3:
    .string "done j\n"
str4:
    .string "done k\n"
str5:
    .string "done k\n"
str6:
    .string "done m\n%d %d\n"
str7:
    .string "after n\n"
str8:
    .string "after first if\n"
str9:
    .string "after second if\n"
str10:
    .string "after third if\n"
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
    .string " "
str17:
    .string "%d"
str18:
    .string "%d"
str19:
    .string "%d"
str20:
    .string "after fourth if\n"
str21:
    .string "Top of loop\n"
str22:
    .string "done z gurp\n"
str23:
    .string "%d"
str24:
    .string "\n"
str25:
    .string "done 0z gurp\n"
str26:
    .string "done y gurp\n"
str27:
    .string "done a gurp\n"
str28:
    .string "done b gurp\n"
str29:
    .string "After gurps\n"
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
    .string " "
str36:
    .string "%d"
str37:
    .string "\n"
.globl main
main:
    pushq %rbx
    pushq %r12
    pushq %r13
    pushq %rbp
    movq %rsp, %rbp
    subq $504, %rsp
main0:
    movl $3, %r8d
main1:
    cmpl $5, %r8d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    leaq str21(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %r9d
    addl %r8d, %r9d
    movl $0, %edi
    subl %r8d, %edi
    movl $2, %esi
    addl %r8d, %esi
    movl $1, %ecx
    subl %r8d, %ecx
    movl $4, %ebx
    subl %r8d, %ebx
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl %ebx, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl 56(%rsp), %edi
    movl 16(%rsp), %esi
    movl $1, %edx
    movl $3, %ecx
    movl 24(%rsp), %r8d
    movl 40(%rsp), %r9d
    xorq %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, -88(%rbp)
    leaq str22(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str23(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    movl 32(%rsp), %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str24(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %edi
    addl %r8d, %edi
    movl $0, %esi
    subl %r8d, %esi
    movl $2, %ecx
    addl %r8d, %ecx
    movl %r8d, %ebx
    addl $1, %ebx
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl $0, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl 16(%rsp), %edi
    movl 24(%rsp), %esi
    movl $1, %edx
    movl $3, %ecx
    movl 40(%rsp), %r8d
    movl %ebx, %r9d
    xorq %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %edi
    leaq str25(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $3, %r12d
    subl %r8d, %r12d
    movl $-8, %r9d
    addl %r8d, %r9d
    movl $3, %ebx
    imul %r8d, %ebx
    movl $12, %esi
    subl %ebx, %esi
    movl $16, %ecx
    addl %r8d, %ecx
    movl $8, %ebx
    subl %r8d, %ebx
    movl $1, %eax
    movq %rax, 0(%rsp)
    movl %ebx, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl %r12d, %edi
    movl 56(%rsp), %esi
    movl $0, %edx
    movl 24(%rsp), %ecx
    movl 40(%rsp), %r8d
    movl $1, %r9d
    xorq %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %esi
    leaq str26(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $2, %r13d
    subl %r8d, %r13d
    movl $6, %r12d
    addl %r8d, %r12d
    movl %r8d, %r9d
    imul $-3, %r9d
    movl %r8d, %ebx
    imul $2, %ebx
    movl $3, %ecx
    subl %ebx, %ecx
    movl $5, %ebx
    subl %r8d, %ebx
    movl $0, %eax
    movq %rax, 0(%rsp)
    movl %ebx, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl %r13d, %edi
    movl %r12d, %esi
    movl $0, %edx
    movl 56(%rsp), %ecx
    movl $1, %r8d
    movl 40(%rsp), %r9d
    xorq %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %ecx
    leaq str27(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $7, %r12d
    subl %r8d, %r12d
    movl %r8d, %r9d
    subl $4, %r9d
    movl %r8d, %ebx
    imul $9, %ebx
    movl $6, %eax
    subl %ebx, %eax
    movl %eax, %ebx
    movl $0, %eax
    movq %rax, 0(%rsp)
    movl %ebx, %eax
    movq %rax, 8(%rsp)
    movq %rdi, 16(%rsp)
    movq %rsi, 24(%rsp)
    movq %rcx, 40(%rsp)
    movq %r8, 48(%rsp)
    movq %r9, 56(%rsp)
    movl $-3, %edi
    movl $8, %esi
    movl $1, %edx
    movl %r12d, %ecx
    movl 56(%rsp), %r8d
    movl $2, %r9d
    xorq %rax, %rax
    call gurp
    movq 16(%rsp), %rdi
    movq 24(%rsp), %rsi
    movq 40(%rsp), %rcx
    movq 48(%rsp), %r8
    movq 56(%rsp), %r9
    movl %eax, %ebx
    leaq str28(%rip), %rax
    movq %rax, %r9
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 40(%rsp), %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str29(%rip), %rax
    movq %rax, %r9
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 40(%rsp), %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str30(%rip), %rax
    movq %rax, %r9
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 40(%rsp), %rdi
    movl 8(%rsp), %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str31(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str32(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl 0(%rsp), %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str33(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str34(%rip), %rax
    movq %rax, %rsi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl 24(%rsp), %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str35(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str36(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str37(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
main3:
    movl %r8d, %r8d
    addl $1, %r8d
    jmp main1
main4:
    movl $0, -428(%rbp)
    movl -428(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %r13
    popq %r12
    popq %rbx
    ret

gurp:
    pushq %rbx
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    pushq %rbp
    movq %rsp, %rbp
    subq $920, %rsp
gurp0:
    movl 56(%rbp), %eax
    movl %eax, -28(%rbp)
    movl 64(%rbp), %eax
    movl %eax, -32(%rbp)
    leaq str0(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %edi, %ebx
    imul %esi, %ebx
    movl %ebx, %ebx
    subl %r8d, %ebx
    movl %ebx, %r12d
    addl %ecx, %r12d
    leaq str1(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %r12d, %eax
    subl $1, %eax
    movl %eax, -156(%rbp)
    leaq str2(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $3, %r13d
    addl -156(%rbp), %r13d
    leaq str3(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl -156(%rbp), %ebx
    addl %r13d, %ebx
    movl %ebx, %eax
    subl $2, %eax
    movl %eax, -192(%rbp)
    leaq str4(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl -192(%rbp), %r12d
    subl %r13d, %r12d
    movl %r12d, -48(%rbp)
    leaq str5(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
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
    leaq str6(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    movl -192(%rbp), %esi
    movl -216(%rbp), %edx
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl -192(%rbp), %ebx
    subl -216(%rbp), %ebx
    movl %r13d, %eax
    subl %ebx, %eax
    movl %eax, %ebx
    movl -156(%rbp), %eax
    addl %ebx, %eax
    movl %eax, -236(%rbp)
    leaq str7(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    cmpl $50, %edi
    setg %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne gurp1

    jmp gurp2
gurp1:
    movl %edi, %ebx
    addl %esi, %ebx
    movl %ebx, %r13d
    addl %r12d, %r13d
    movl %r13d, -60(%rbp)
    movl %r12d, %ebx
    subl -216(%rbp), %ebx
    movl %ebx, -40(%rbp)
    movl %edx, %eax
    cmpl $0, %eax
    jne gurp4

    jmp gurp5
gurp4:
    movl %r8d, %r12d
    addl %r9d, %r12d
    movl -32(%rbp), %ebx
    addl -236(%rbp), %ebx
    movl %r12d, %eax
    subl %ebx, %eax
    movl %eax, %ebx
    movl %ebx, -48(%rbp)
    movl %r9d, %ebx
    subl $1, %ebx
    movl %ebx, %r9d
    movl %r13d, %ebx
    subl $3, %ebx
    movl %ebx, -68(%rbp)
    jmp gurp6
gurp5:
    movl %r13d, %ebx
    addl $3, %ebx
    movl %ebx, -68(%rbp)
gurp6:
    movl %edx, %eax
    cmpl $0, %eax
    jne gurp7

gurp9:
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp7

    jmp gurp8
gurp7:
    movl %r9d, %r12d
    addl %r8d, %r12d
    movl -32(%rbp), %ebx
    subl %esi, %ebx
    movl %ebx, %ebx
    addl %r12d, %ebx
    movl %ebx, -76(%rbp)
gurp8:
    jmp gurp3
gurp2:
    movl %esi, %r13d
    subl %edi, %r13d
    movl %r13d, -60(%rbp)
    movl %r12d, %r9d
    addl -216(%rbp), %r9d
    movl %r9d, -40(%rbp)
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp10

    jmp gurp11
gurp10:
    movl %edi, %ebx
    subl %r13d, %ebx
    movl $3, %r12d
    addl %ebx, %r12d
    movl %r12d, %r9d
    movl $3, %ebx
    subl %r13d, %ebx
    movl %ebx, -68(%rbp)
    movl %ebx, %ebx
    addl %r12d, %ebx
    movl %ebx, -76(%rbp)
    jmp gurp12
gurp11:
    movl %r13d, %ebx
    subl %edi, %ebx
    movl %ebx, %ebx
    subl $2, %ebx
    movl %ebx, %ebx
    addl $3, %ebx
    movl %ebx, -76(%rbp)
    movl %r9d, %r9d
    addl %ebx, %r9d
    movl %r9d, -68(%rbp)
    movl %ebx, %ebx
    addl %r9d, %ebx
    movl %ebx, %ebx
    subl %r9d, %ebx
    movl %ebx, %r9d
gurp12:
gurp3:
    leaq str8(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %r8d, %ebx
    addl -48(%rbp), %ebx
    movl %ebx, %eax
    subl -156(%rbp), %eax
    movl %eax, -392(%rbp)
    movl -392(%rbp), %ebx
    addl -40(%rbp), %ebx
    movl %ebx, %eax
    addl -60(%rbp), %eax
    movl %eax, -400(%rbp)
    movl -392(%rbp), %ebx
    subl -400(%rbp), %ebx
    movl %ebx, %ebx
    addl -216(%rbp), %ebx
    movl %ebx, %eax
    subl -40(%rbp), %eax
    movl %eax, -412(%rbp)
    movl -48(%rbp), %r12d
    addl -400(%rbp), %r12d
    movl -68(%rbp), %ebx
    addl %r9d, %ebx
    movl -392(%rbp), %eax
    subl %ebx, %eax
    movl %eax, %ebx
    movl %r12d, %eax
    addl %ebx, %eax
    movl %eax, -428(%rbp)
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne gurp13

    jmp gurp14
gurp13:
    movl $3, %eax
    movl %eax, -92(%rbp)
    jmp gurp15
gurp14:
    movl $-1, %eax
    movl %eax, -92(%rbp)
gurp15:
    leaq str9(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $99, %ebx
    addl -236(%rbp), %ebx
    movl %ebx, %ebx
    subl %r8d, %ebx
    movl %ebx, %eax
    addl -392(%rbp), %eax
    movl %eax, -460(%rbp)
    movl %r8d, %ebx
    subl -76(%rbp), %ebx
    movl %ebx, %ebx
    addl -236(%rbp), %ebx
    movl %ebx, %ebx
    addl -60(%rbp), %ebx
    movl %ebx, %ebx
    addl -216(%rbp), %ebx
    movl %ebx, %ebx
    subl %r8d, %ebx
    movl %ebx, %ebx
    addl %esi, %ebx
    movl %ebx, %ebx
    subl %esi, %ebx
    movl %ebx, %eax
    addl %r8d, %eax
    movl %eax, -492(%rbp)
    movl -400(%rbp), %ebx
    addl -392(%rbp), %ebx
    movl %ebx, %ebx
    subl -236(%rbp), %ebx
    movl %ebx, %eax
    subl -392(%rbp), %eax
    movl %eax, -504(%rbp)
    movl -32(%rbp), %ebx
    addl %r9d, %ebx
    movl %ebx, %ebx
    addl %ecx, %ebx
    movl %ebx, %eax
    subl %r8d, %eax
    movl %eax, -516(%rbp)
    cmpl $-1, -92(%rbp)
    sete %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne gurp16

    jmp gurp17
gurp16:
    movl -504(%rbp), %ebx
    addl -516(%rbp), %ebx
    movl %ebx, -116(%rbp)
    jmp gurp18
gurp17:
    movl -516(%rbp), %ebx
    subl -504(%rbp), %ebx
    movl %ebx, -116(%rbp)
gurp18:
    movl -460(%rbp), %r15d
    subl -428(%rbp), %r15d
    movl %r15d, %r14d
    addl -92(%rbp), %r14d
    movl $42, %r13d
    subl %r14d, %r13d
    leaq str10(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str11(%rip), %rax
    movq %rax, %r12
    movl %edi, %ebx
    subl -156(%rbp), %ebx
    movl %ebx, %ebx
    addl %ecx, %ebx
    movl %ebx, %ebx
    addl -40(%rbp), %ebx
    movl %ebx, %ebx
    subl %r8d, %ebx
    movl %ebx, %ebx
    subl %r9d, %ebx
    movl %ebx, %ebx
    addl -48(%rbp), %ebx
    movl %ebx, %ebx
    addl -216(%rbp), %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str12(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str13(%rip), %rax
    movq %rax, %r12
    movl %esi, %ebx
    subl -192(%rbp), %ebx
    movl %ebx, %ebx
    subl -32(%rbp), %ebx
    movl %ebx, %ebx
    addl -236(%rbp), %ebx
    movl %ebx, %ebx
    addl -60(%rbp), %ebx
    movl %ebx, %ebx
    addl -68(%rbp), %ebx
    movl %ebx, %ebx
    subl -412(%rbp), %ebx
    movl %ebx, %ebx
    subl -504(%rbp), %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str14(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str15(%rip), %rax
    movq %rax, %r12
    movl -516(%rbp), %ebx
    addl -76(%rbp), %ebx
    movl %ebx, %ebx
    subl -392(%rbp), %ebx
    movl %ebx, %ebx
    addl %r14d, %ebx
    movl %ebx, %ebx
    subl -492(%rbp), %ebx
    movl %ebx, %ebx
    addl $0, %ebx
    movl %ebx, %ebx
    addl -92(%rbp), %ebx
    movl %ebx, %ebx
    subl -116(%rbp), %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    leaq str16(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
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
    leaq str17(%rip), %rax
    movq %rax, %r12
    movl %r13d, %ebx
    addl -460(%rbp), %ebx
    movl %ebx, %ebx
    subl -428(%rbp), %ebx
    movl %ebx, %ebx
    addl -400(%rbp), %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl %ebx, %esi
    xorq %rax, %rax
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
gurp22:
    leaq str18(%rip), %rax
    movq %rax, %r12
    movl %r15d, %ebx
    addl -400(%rbp), %ebx
    movl %ebx, %ebx
    subl %r13d, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    jmp gurp24
gurp23:
    leaq str19(%rip), %rax
    movq %rax, %r12
    movl -400(%rbp), %ebx
    addl -428(%rbp), %ebx
    movl %ebx, %ebx
    addl %r13d, %ebx
    movl %ebx, %ebx
    subl -460(%rbp), %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r12, %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
gurp24:
gurp21:
    leaq str20(%rip), %rax
    movq %rax, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %edi, %ebx
    subl %esi, %ebx
    movl %ebx, %ebx
    addl %ecx, %ebx
    movl %ebx, %ebx
    addl %r8d, %ebx
    movl %ebx, %ebx
    addl %r9d, %ebx
    movl %ebx, %ebx
    addl -32(%rbp), %ebx
    movl %ebx, %ebx
    addl -156(%rbp), %ebx
    movl %ebx, %ebx
    addl -40(%rbp), %ebx
    movl %ebx, %ebx
    addl -192(%rbp), %ebx
    movl %ebx, %ebx
    addl -48(%rbp), %ebx
    movl %ebx, %ebx
    addl -216(%rbp), %ebx
    movl %ebx, %ebx
    subl -236(%rbp), %ebx
    movl %ebx, %ebx
    addl -60(%rbp), %ebx
    movl %ebx, %ebx
    subl %r13d, %ebx
    movl %ebx, %ebx
    addl -68(%rbp), %ebx
    movl %ebx, %ebx
    subl -412(%rbp), %ebx
    movl %ebx, %ebx
    addl -76(%rbp), %ebx
    movl %ebx, %ebx
    subl -392(%rbp), %ebx
    movl %ebx, %ebx
    addl -460(%rbp), %ebx
    movl %ebx, %ebx
    subl -428(%rbp), %ebx
    movl %ebx, %ebx
    addl -92(%rbp), %ebx
    movl %ebx, %ebx
    subl $0, %ebx
    movl %ebx, %ebx
    addl -492(%rbp), %ebx
    movl %ebx, %ebx
    subl %r14d, %ebx
    movl %ebx, %ebx
    addl -400(%rbp), %ebx
    movl %ebx, %ebx
    addl -504(%rbp), %ebx
    movl %ebx, %ebx
    addl -116(%rbp), %ebx
    movl %ebx, %eax
    subl -516(%rbp), %eax
    movl %eax, -868(%rbp)
    movl -868(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbx
    ret

