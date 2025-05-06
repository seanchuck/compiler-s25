
.comm base_cases, 12, 4
.comm is_memoized, 404, 4
.comm memoization, 404, 4
str0:
    .string "recursive: %d\n"
str1:
    .string "memoized: %d\n"
str2:
    .string "iterative: %d\n"
fib_recursive:
    pushq %rbp
    pushq %rcx
    pushq %rsi
    pushq %rdi
    movq %rsp, %rbp
    subq $112, %rsp
fib_recursive0:
    movl %edi, %eax
    movl %eax, %edi
    movl $0, %ecx
    cmpl %ecx, %edi
    sete %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne fib_recursive1

    jmp fib_recursive4
fib_recursive1:
    movl %edi, %r10d
    addl $1, %r10d
    movl base_cases(, %r10, 4), %eax
    movl %eax, -24(%rbp)
    movl -24(%rbp), %eax
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret
fib_recursive2:
    movl $1, %ecx
    movl %edi, %eax
    subl %ecx, %eax
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 24(%rsp), %edi
    xor %rax, %rax
    call fib_recursive
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %esi
    movl $2, %ecx
    movl %edi, %eax
    subl %ecx, %eax
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 24(%rsp), %edi
    xor %rax, %rax
    call fib_recursive
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %ecx
    movl %esi, %eax
    addl %ecx, %eax
    movl %eax, -60(%rbp)
    movl -60(%rbp), %eax
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret
fib_recursive3:
    movq $-1, %rdi
    call exit
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret
fib_recursive4:
    movl $1, %ecx
    cmpl %ecx, %edi
    sete %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne fib_recursive1

    jmp fib_recursive2

fib_iterative:
    pushq %rbp
    pushq %rcx
    pushq %rsi
    pushq %rdi
    pushq %r8
    pushq %r9
    movq %rsp, %rbp
    subq $96, %rsp
fib_iterative0:
    movl %edi, %eax
    movl %eax, %edi
    movl $0, %ecx
    movl %ecx, %eax
    movl %eax, %r9d
    movl $1, %ecx
    movl %ecx, %eax
    movl %eax, %r8d
    movl $2, %ecx
    movl %ecx, %eax
    movl %eax, %esi
    jmp fib_iterative1
fib_iterative1:
    cmpl %edi, %esi
    setle %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne fib_iterative2

    jmp fib_iterative4
fib_iterative2:
    movl %r9d, %eax
    addl %r8d, %eax
    movl %eax, %ecx
    movl %r8d, %eax
    movl %eax, %r9d
    movl %ecx, %eax
    movl %eax, %r8d
    jmp fib_iterative3
fib_iterative3:
    movl $1, %ecx
    movl %esi, %eax
    addl %ecx, %eax
    movl %eax, %esi
    jmp fib_iterative1
fib_iterative4:
    movl %r8d, %eax
    movq %rbp, %rsp
    popq %r9
    popq %r8
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret

fib_memoized:
    pushq %rbp
    pushq %rcx
    pushq %rsi
    pushq %rdi
    movq %rsp, %rbp
    subq $128, %rsp
fib_memoized0:
    movl %edi, %eax
    movl %eax, %edi
    movl %edi, %r10d
    addl $1, %r10d
    movl is_memoized(, %r10, 4), %eax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne fib_memoized1

    jmp fib_memoized2
fib_memoized1:
    movl %edi, %r10d
    addl $1, %r10d
    movl memoization(, %r10, 4), %eax
    movl %eax, -12(%rbp)
    movl -12(%rbp), %eax
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret
fib_memoized2:
    movl $0, %ecx
    cmpl %ecx, %edi
    sete %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne fib_memoized4

    jmp fib_memoized7
fib_memoized3:
    movq $-1, %rdi
    call exit
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret
fib_memoized4:
    movl %edi, %eax
    movl %eax, %ecx
    jmp fib_memoized6
fib_memoized5:
    movl $1, %ecx
    movl %edi, %eax
    subl %ecx, %eax
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 24(%rsp), %edi
    xor %rax, %rax
    call fib_memoized
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %esi
    movl $2, %ecx
    movl %edi, %eax
    subl %ecx, %eax
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 24(%rsp), %edi
    xor %rax, %rax
    call fib_memoized
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %ecx
    movl %esi, %eax
    addl %ecx, %eax
    movl %eax, %ecx
    movl %ecx, %eax
    movl %eax, %ecx
    jmp fib_memoized6
fib_memoized6:
    movl %edi, %r10d
    addl $1, %r10d
    movl %ecx, %eax
    movl %eax, memoization(, %r10, 4)
    movl %edi, %r10d
    addl $1, %r10d
    movl $1, %eax
    movl %eax, is_memoized(, %r10, 4)
    movl %ecx, %eax
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret
fib_memoized7:
    movl $1, %ecx
    cmpl %ecx, %edi
    sete %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne fib_memoized4

    jmp fib_memoized5

.globl main
main:
    pushq %rbp
    pushq %rcx
    pushq %rsi
    pushq %rdi
    movq %rsp, %rbp
    subq $128, %rsp
    movq $2, %rax
    movq %rax, base_cases
    movq $100, %rax
    movq %rax, is_memoized
    movq $100, %rax
    movq %rax, memoization
main0:
    movl $8, %edi
    movl $0, %esi
    movl $0, %ecx
    movl %ecx, %r10d
    addl $1, %r10d
    movl %esi, %eax
    movl %eax, base_cases(, %r10, 4)
    movl $1, %esi
    movl $1, %ecx
    movl %ecx, %r10d
    addl $1, %r10d
    movl %esi, %eax
    movl %eax, base_cases(, %r10, 4)
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 0(%rsp), %edi
    xor %rax, %rax
    call fib_recursive
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %esi
    leaq str0(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq 24(%rsp), %rdi
    movl 8(%rsp), %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 0(%rsp), %edi
    xor %rax, %rax
    call fib_memoized
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %esi
    leaq str1(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq 24(%rsp), %rdi
    movl 8(%rsp), %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movl 0(%rsp), %edi
    xor %rax, %rax
    call fib_iterative
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %esi
    leaq str2(%rip), %rax
    movq %rax, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq 24(%rsp), %rdi
    movl 8(%rsp), %esi
    xor %rax, %rax
    call printf
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl $0, -72(%rbp)
    movl -72(%rbp), %eax
    movq %rbp, %rsp
    popq %rdi
    popq %rsi
    popq %rcx
    popq %rbp
    ret

