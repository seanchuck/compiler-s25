succewsors are {
    1: {
        4,
        2,
    },
    3: {
        1,
    },
    2: {
        3,
    },
    0: {
        1,
    },
}
in_map: {
    1: {
        "_t4",
        "b",
        "i",
        "_t6",
        "_t5",
        "_t0",
    },
    2: {
        "_t0",
        "_t6",
        "i",
        "_t5",
        "_t4",
    },
    4: {
        "b",
    },
    3: {
        "_t5",
        "b",
        "_t6",
        "_t4",
        "_t0",
        "i",
    },
    0: {
        "_t2",
        "_t5",
        "_t1",
        "_t6",
        "_t3",
        "_t4",
    },
}, out map {
    4: {},
    3: {
        "_t6",
        "_t0",
        "b",
        "i",
        "_t4",
        "_t5",
    },
    1: {
        "_t0",
        "b",
        "_t4",
        "_t6",
        "i",
        "_t5",
    },
    2: {
        "_t4",
        "_t5",
        "b",
        "_t6",
        "_t0",
        "i",
    },
    0: {
        "_t6",
        "_t4",
        "i",
        "_t5",
        "_t0",
        "b",
    },
}
succewsors are {
    0: {
        4,
        1,
    },
    4: {
        2,
        1,
    },
}
in_map: {
    0: {
        "_t5",
        "_t0",
        "_t7",
        "_t3",
        "base_cases",
        "_t11",
        "_t9",
        "_t1",
        "_t10",
        "_t4",
        "_t12",
        "_t8",
        "_t6",
        "_t2",
    },
    2: {
        "_t10",
        "_t7",
        "_t11",
        "_t12",
        "_t8",
        "_t6",
        "_t9",
        "_t0",
    },
    3: {},
    1: {
        "_t5",
        "base_cases",
        "_t0",
    },
    4: {
        "_t0",
        "_t3",
        "_t6",
        "_t8",
        "_t10",
        "base_cases",
        "_t7",
        "_t11",
        "_t4",
        "_t9",
        "_t12",
        "_t5",
    },
}, out map {
    1: {},
    0: {
        "_t4",
        "_t12",
        "_t10",
        "_t9",
        "_t5",
        "_t11",
        "_t3",
        "_t8",
        "_t7",
        "_t6",
        "_t0",
        "base_cases",
    },
    3: {},
    4: {
        "_t11",
        "_t10",
        "_t0",
        "_t5",
        "base_cases",
        "_t9",
        "_t7",
        "_t12",
        "_t6",
        "_t8",
    },
    2: {},
}
succewsors are {}
in_map: {
    0: {
        "_t6",
        "_t4",
        "_t3",
        "_t8",
        "_t11",
        "_t12",
        "_t10",
        "_t5",
        "_t1",
        "_t2",
        "_t9",
        "_t0",
        "_t7",
    },
}, out map {
    0: {},
}
succewsors are {
    2: {
        4,
        7,
    },
    4: {
        6,
    },
    0: {
        2,
        1,
    },
    5: {
        6,
    },
    7: {
        5,
        4,
    },
}
in_map: {
    4: {
        "_t0",
    },
    0: {
        "_t13",
        "_t0",
        "_t10",
        "memoization",
        "_t12",
        "_t9",
        "_t2",
        "_t4",
        "_t7",
        "_t6",
        "is_memoized",
        "_t8",
        "_t5",
        "_t1",
        "_t11",
        "_t3",
    },
    7: {
        "_t7",
        "_t5",
        "_t6",
        "_t10",
        "_t8",
        "_t12",
        "_t9",
        "_t11",
        "_t0",
        "_t13",
    },
    6: {
        "_res1",
        "_t0",
    },
    3: {},
    2: {
        "_t11",
        "_t9",
        "_t7",
        "_t5",
        "_t6",
        "_t8",
        "_t13",
        "_t10",
        "_t12",
        "_t0",
        "_t4",
        "_t3",
    },
    5: {
        "_t13",
        "_t10",
        "_t8",
        "_t0",
        "_t11",
        "_t9",
        "_t12",
        "_t7",
    },
    1: {
        "_t0",
        "_t2",
        "memoization",
    },
}, out map {
    5: {
        "_res1",
        "_t0",
    },
    0: {
        "_t6",
        "_t13",
        "_t4",
        "_t10",
        "_t9",
        "_t7",
        "_t8",
        "_t12",
        "_t5",
        "_t2",
        "memoization",
        "_t0",
        "_t3",
        "_t11",
    },
    6: {},
    1: {},
    7: {
        "_t8",
        "_t13",
        "_t7",
        "_t10",
        "_t0",
        "_t12",
        "_t9",
        "_t11",
    },
    3: {},
    2: {
        "_t8",
        "_t10",
        "_t9",
        "_t13",
        "_t11",
        "_t0",
        "_t7",
        "_t5",
        "_t12",
        "_t6",
    },
    4: {
        "_t0",
        "_res1",
    },
}

.comm memoization, 808, 8
.comm is_memoized, 808, 8
.comm base_cases, 24, 8
str0:
    .string "recursive: %d\n"
str1:
    .string "memoized: %d\n"
str2:
    .string "iterative: %d\n"
fib_iterative:
    pushq %rbp
    movq %rsp, %rbp
    subq $96, %rsp
fib_iterative0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rax, -24(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -56(%rbp), %rax
    movq %rax, -32(%rbp)
    jmp fib_iterative1
fib_iterative1:
    movq -32(%rbp), %rax
    cmpq -8(%rbp), %rax
    setle %al
    movzbq %al, %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    cmpq $0, %rax
    jne fib_iterative2

    jmp fib_iterative4
fib_iterative2:
    movq -16(%rbp), %rax
    addq -24(%rbp), %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rax
    movq %rax, -24(%rbp)
    jmp fib_iterative3
fib_iterative3:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -88(%rbp)
    movq -32(%rbp), %rax
    addq -88(%rbp), %rax
    movq %rax, -32(%rbp)
    jmp fib_iterative1
fib_iterative4:
    movq -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $128, %rsp
    movq $100, %rax
    movq %rax, memoization
    movq $100, %rax
    movq %rax, is_memoized
    movq $2, %rax
    movq %rax, base_cases
main0:
    movq $8, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -24(%rbp)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -48(%rbp), %r10
    addq $1, %r10
    movq -40(%rbp), %rax
    movq %rax, base_cases(, %r10, 8)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -64(%rbp)
    movq -64(%rbp), %r10
    addq $1, %r10
    movq -56(%rbp), %rax
    movq %rax, base_cases(, %r10, 8)
    movq -24(%rbp), %rdi
    movq $0, %rax
    call fib_recursive
    movq %rax, -72(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rdi
    movq -72(%rbp), %rsi
    movq $0, %rax
    call printf
    movq -24(%rbp), %rdi
    movq $0, %rax
    call fib_memoized
    movq %rax, -88(%rbp)
    leaq str1(%rip), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    movq -88(%rbp), %rsi
    movq $0, %rax
    call printf
    movq -24(%rbp), %rdi
    movq $0, %rax
    call fib_iterative
    movq %rax, -104(%rbp)
    leaq str2(%rip), %rax
    movq %rax, -112(%rbp)
    movq -112(%rbp), %rdi
    movq -104(%rbp), %rsi
    movq $0, %rax
    call printf
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -120(%rbp)
    movq -120(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

fib_recursive:
    pushq %rbp
    movq %rsp, %rbp
    subq $128, %rsp
fib_recursive0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -16(%rbp)
    movq -8(%rbp), %rax
    cmpq -16(%rbp), %rax
    sete %al
    movzbq %al, %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    cmpq $0, %rax
    jne fib_recursive1

    jmp fib_recursive4
fib_recursive1:
    movq -8(%rbp), %r10
    addq $1, %r10
    movq base_cases(, %r10, 8), %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_recursive2:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -72(%rbp)
    movq -8(%rbp), %rax
    subq -72(%rbp), %rax
    movq %rax, -80(%rbp)
    movq -80(%rbp), %rdi
    movq $0, %rax
    call fib_recursive
    movq %rax, -88(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -96(%rbp)
    movq -8(%rbp), %rax
    subq -96(%rbp), %rax
    movq %rax, -104(%rbp)
    movq -104(%rbp), %rdi
    movq $0, %rax
    call fib_recursive
    movq %rax, -112(%rbp)
    movq -88(%rbp), %rax
    addq -112(%rbp), %rax
    movq %rax, -120(%rbp)
    movq -120(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_recursive3:
    movq $-1, %rax
    call exit
    movq %rbp, %rsp
    popq %rbp
    ret
fib_recursive4:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -32(%rbp)
    movq -8(%rbp), %rax
    cmpq -32(%rbp), %rax
    sete %al
    movzbq %al, %rax
    movq %rax, -40(%rbp)
    movq -40(%rbp), %rax
    cmpq $0, %rax
    jne fib_recursive1

    jmp fib_recursive2

fib_memoized:
    pushq %rbp
    movq %rsp, %rbp
    subq $144, %rsp
fib_memoized0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    movq -8(%rbp), %r10
    addq $1, %r10
    movq is_memoized(, %r10, 8), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rax
    cmpq $0, %rax
    jne fib_memoized1

    jmp fib_memoized2
fib_memoized1:
    movq -8(%rbp), %r10
    addq $1, %r10
    movq memoization(, %r10, 8), %rax
    movq %rax, -24(%rbp)
    movq -24(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_memoized2:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq -8(%rbp), %rax
    cmpq -40(%rbp), %rax
    sete %al
    movzbq %al, %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rax
    cmpq $0, %rax
    jne fib_memoized4

    jmp fib_memoized7
fib_memoized3:
    movq $-1, %rax
    call exit
    movq %rbp, %rsp
    popq %rbp
    ret
fib_memoized4:
    movq -8(%rbp), %rax
    movq %rax, -32(%rbp)
    jmp fib_memoized6
fib_memoized5:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -88(%rbp)
    movq -8(%rbp), %rax
    subq -88(%rbp), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    movq $0, %rax
    call fib_memoized
    movq %rax, -104(%rbp)
    movq $2, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -112(%rbp)
    movq -8(%rbp), %rax
    subq -112(%rbp), %rax
    movq %rax, -120(%rbp)
    movq -120(%rbp), %rdi
    movq $0, %rax
    call fib_memoized
    movq %rax, -128(%rbp)
    movq -104(%rbp), %rax
    addq -128(%rbp), %rax
    movq %rax, -136(%rbp)
    movq -136(%rbp), %rax
    movq %rax, -32(%rbp)
    jmp fib_memoized6
fib_memoized6:
    movq -8(%rbp), %r10
    addq $1, %r10
    movq -32(%rbp), %rax
    movq %rax, memoization(, %r10, 8)
    movq -8(%rbp), %r10
    addq $1, %r10
    movq $1, %rax
    movq %rax, is_memoized(, %r10, 8)
    movq -32(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret
fib_memoized7:
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -8(%rbp), %rax
    cmpq -56(%rbp), %rax
    sete %al
    movzbq %al, %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    cmpq $0, %rax
    jne fib_memoized4

    jmp fib_memoized5

