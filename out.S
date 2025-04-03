
.comm memoization, 808, 8
.comm is_memoized, 808, 8
.comm base_cases, 24, 8
str0:
    .string "recursive: %d\n"
str1:
    .string "memoized: %d\n"
str2:
    .string "iterative: %d\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
main0:
    movq $8, %rax
    movq %rax, -8(%rbp)
    movq $0, %rax
    movq %rax, -16(%rbp)
    movq $0, %r10
    movq $0, %rax
    movq %rax, base_cases(, %r10, 8)
    movq $1, %r10
    movq $1, %rax
    movq %rax, base_cases(, %r10, 8)
    movq -8(%rbp), %rdi
    call fib_recursive
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -16(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %rdi
    movq -16(%rbp), %rsi
    call printf
    movq -8(%rbp), %rdi
    call fib_memoized
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -16(%rbp)
    leaq str1(%rip), %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rdi
    movq -16(%rbp), %rsi
    call printf
    movq -8(%rbp), %rdi
    call fib_iterative
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rax
    movq %rax, -16(%rbp)
    leaq str2(%rip), %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rdi
    movq -16(%rbp), %rsi
    call printf
    movq $0, %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

fib_recursive:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
fib_recursive0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    cmpq $0, -8(%rbp)
    sete %al
    movzbq %al, %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rax
    cmpq $0, %rax
    jne fib_recursive1

    jmp fib_recursive4
fib_recursive1:
    movq -8(%rbp), %r10
    movq base_cases(, %r10, 8), %rax
    movq %rax, -32(%rbp)
    movq -32(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_recursive2:
    movq -8(%rbp), %rax
    subq $1, %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rdi
    call fib_recursive
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    movq %rax, -40(%rbp)
    movq -8(%rbp), %rax
    subq $2, %rax
    movq %rax, -72(%rbp)
    movq -72(%rbp), %rdi
    call fib_recursive
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rax
    movq %rax, -48(%rbp)
    movq -40(%rbp), %rax
    addq -48(%rbp), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_recursive3:
fib_recursive4:
    cmpq $1, -8(%rbp)
    sete %al
    movzbq %al, %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    cmpq $0, %rax
    jne fib_recursive1

    jmp fib_recursive2
    movq %rbp, %rsp
    popq %rbp
    ret

fib_iterative:
    pushq %rbp
    movq %rsp, %rbp
    subq $64, %rsp
fib_iterative0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq $0, %rax
    movq %rax, -16(%rbp)
    movq $1, %rax
    movq %rax, -24(%rbp)
    movq $2, %rax
    movq %rax, -32(%rbp)
    jmp fib_iterative1
fib_iterative1:
    movq %rax, -32(%rbp)
    cmpq -8(%rbp), %rax
    setle %al
    movzbq %al, %rax
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rax
    cmpq $0, %rax
    jne fib_iterative2

    jmp fib_iterative3
fib_iterative2:
    movq -16(%rbp), %rax
    addq -24(%rbp), %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rax
    movq %rax, -48(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -48(%rbp), %rax
    movq %rax, -24(%rbp)
    movq -32(%rbp), %rax
    addq $1, %rax
    movq %rax, -32(%rbp)
    jmp fib_iterative1
fib_iterative3:
    movq -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
    movq %rbp, %rsp
    popq %rbp
    ret

fib_memoized:
    pushq %rbp
    movq %rsp, %rbp
    subq $112, %rsp
fib_memoized0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %r10
    movq is_memoized(, %r10, 8), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rax
    cmpq $0, %rax
    jne fib_memoized1

    jmp fib_memoized2
fib_memoized1:
    movq -8(%rbp), %r10
    movq memoization(, %r10, 8), %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_memoized2:
    cmpq $0, -8(%rbp)
    sete %al
    movzbq %al, %rax
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rax
    cmpq $0, %rax
    jne fib_memoized4

    jmp fib_memoized7
fib_memoized3:
fib_memoized4:
    movq -8(%rbp), %rax
    movq %rax, -32(%rbp)
    jmp fib_memoized6
fib_memoized5:
    movq -8(%rbp), %rax
    subq $1, %rax
    movq %rax, -72(%rbp)
    movq -72(%rbp), %rdi
    call fib_memoized
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rax
    movq %rax, -56(%rbp)
    movq -8(%rbp), %rax
    subq $2, %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    call fib_memoized
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rax
    movq %rax, -64(%rbp)
    movq -56(%rbp), %rax
    addq -64(%rbp), %rax
    movq %rax, -104(%rbp)
    movq -104(%rbp), %rax
    movq %rax, -32(%rbp)
    jmp fib_memoized6
fib_memoized6:
    movq -8(%rbp), %r10
    movq -32(%rbp), %rax
    movq %rax, memoization(, %r10, 8)
    movq -8(%rbp), %r10
    movq $1, %rax
    movq %rax, is_memoized(, %r10, 8)
    movq -32(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_memoized7:
    cmpq $1, -8(%rbp)
    sete %al
    movzbq %al, %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rax
    cmpq $0, %rax
    jne fib_memoized4

    jmp fib_memoized5
    movq %rbp, %rsp
    popq %rbp
    ret

