
str0:
    .string "%d\n"
str1:
    .string "%d\n"
str2:
    .string "%ld\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $128, %rsp
main0:
    movl $2147483647, -16(%rbp)
    movl -16(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -20(%rbp)
    movl -20(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -36(%rbp)
    movl -36(%rbp), %eax
    movsxd %eax, %rax
    movq %rax, -32(%rbp)
    movq $1, %rbx
    movq $4294967295, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -44(%rbp)
    movq -44(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -52(%rbp)
    movq -32(%rbp), %rax
    addq -52(%rbp), %rax
    movq %rax, -60(%rbp)
    movl -60(%rbp), %eax
    movl %eax, -24(%rbp)
    movl -24(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -64(%rbp)
    movl $-2147483648, -68(%rbp)
    movl -68(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -72(%rbp)
    movl -72(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -88(%rbp)
    movl -88(%rbp), %eax
    movsxd %eax, %rax
    movq %rax, -84(%rbp)
    movq $4294967295, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -96(%rbp)
    movq -96(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -104(%rbp)
    movq -84(%rbp), %rax
    addq -104(%rbp), %rax
    movq %rax, -112(%rbp)
    movl -112(%rbp), %eax
    movl %eax, -76(%rbp)
    movl -76(%rbp), %edi
    movq $0, %rax
    call get_int
    movl %eax, -116(%rbp)
    movl $0, -120(%rbp)
    movl -120(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

get_int:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
get_int0:
    movl %edi, %eax
    movl %eax, -4(%rbp)
    leaq str0(%rip), %rax
    movq %rax, -12(%rbp)
    movq -12(%rbp), %rdi
    movl -4(%rbp), %esi
    movq $0, %rax
    call printf
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

get_long:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
get_long0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    leaq str2(%rip), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rdi
    movq -8(%rbp), %rsi
    movq $0, %rax
    call printf
    movq -8(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

get_bool:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
get_bool0:
    movl %edi, %eax
    movl %eax, -4(%rbp)
    movl -4(%rbp), %eax
    cmpl $0, %eax
    jne get_bool1

    jmp get_bool2
get_bool1:
    movl $1, -16(%rbp)
    movl -16(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp get_bool3
get_bool2:
    movl $2, -20(%rbp)
    movl -20(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp get_bool3
get_bool3:
    leaq str1(%rip), %rax
    movq %rax, -28(%rbp)
    movq -28(%rbp), %rdi
    movl -12(%rbp), %esi
    movq $0, %rax
    call printf
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

