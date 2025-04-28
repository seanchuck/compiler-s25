
.comm A, 404, 4
.comm length, 4, 4
str0:
    .string "creating random array of %d elements\n"
str1:
    .string "\nbefore sort:\n"
str2:
    .string "%d\n"
str3:
    .string "\nafter sort\n"
str4:
    .string "%d\n"
partition:
    pushq %rbp
    movq %rsp, %rbp
    subq $144, %rsp
partition0:
    movl %edi, %eax
    movl %eax, -4(%rbp)
    movl %esi, %eax
    movl %eax, -8(%rbp)
    movl -4(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -32(%rbp)
    movl -32(%rbp), %eax
    movl %eax, -12(%rbp)
    movl $1, -36(%rbp)
    movl -4(%rbp), %eax
    subl -36(%rbp), %eax
    movl %eax, -40(%rbp)
    movl -40(%rbp), %eax
    movl %eax, -16(%rbp)
    movl $1, -44(%rbp)
    movl -8(%rbp), %eax
    addl -44(%rbp), %eax
    movl %eax, -48(%rbp)
    movl -48(%rbp), %eax
    movl %eax, -20(%rbp)
    movl $0, -52(%rbp)
    movl -52(%rbp), %eax
    movl %eax, -28(%rbp)
    jmp partition1
partition1:
    movl length, %eax
    imul length, %eax
    movl %eax, -56(%rbp)
    movl -28(%rbp), %eax
    cmpl -56(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -60(%rbp)
    movl -60(%rbp), %eax
    cmpl $0, %eax
    jne partition2

    jmp partition4
partition2:
    movl $1, -68(%rbp)
    movl -20(%rbp), %eax
    subl -68(%rbp), %eax
    movl %eax, -72(%rbp)
    movl -72(%rbp), %eax
    movl %eax, -20(%rbp)
    movl $0, -76(%rbp)
    movl -76(%rbp), %eax
    movl %eax, -64(%rbp)
    jmp partition5
partition3:
    movl $1, -140(%rbp)
    movl -28(%rbp), %eax
    addl -140(%rbp), %eax
    movl %eax, -28(%rbp)
    jmp partition1
partition4:
    movl $-1, -144(%rbp)
    movl -144(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret
partition5:
    movl -64(%rbp), %eax
    cmpl length, %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -80(%rbp)
    movl -80(%rbp), %eax
    cmpl $0, %eax
    jne partition6

    jmp partition8
partition6:
    movl -20(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -84(%rbp)
    movl -84(%rbp), %eax
    cmpl -12(%rbp), %eax
    setle %al
    movzbq %al, %rax
    movl %eax, -88(%rbp)
    movl -88(%rbp), %eax
    cmpl $0, %eax
    jne partition9

    jmp partition10
partition7:
    movl $1, -100(%rbp)
    movl -64(%rbp), %eax
    addl -100(%rbp), %eax
    movl %eax, -64(%rbp)
    jmp partition5
partition8:
    movl $1, -104(%rbp)
    movl -16(%rbp), %eax
    addl -104(%rbp), %eax
    movl %eax, -108(%rbp)
    movl -108(%rbp), %eax
    movl %eax, -64(%rbp)
    jmp partition11
partition9:
    jmp partition8
partition10:
    movl $1, -92(%rbp)
    movl -20(%rbp), %eax
    subl -92(%rbp), %eax
    movl %eax, -96(%rbp)
    movl -96(%rbp), %eax
    movl %eax, -20(%rbp)
    jmp partition7
partition11:
    movl -64(%rbp), %eax
    cmpl length, %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -112(%rbp)
    movl -112(%rbp), %eax
    cmpl $0, %eax
    jne partition12

    jmp partition14
partition12:
    movl -64(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -116(%rbp)
    movl -116(%rbp), %eax
    cmpl -12(%rbp), %eax
    setge %al
    movzbq %al, %rax
    movl %eax, -120(%rbp)
    movl -120(%rbp), %eax
    cmpl $0, %eax
    jne partition15

    jmp partition16
partition13:
    movl $1, -124(%rbp)
    movl -64(%rbp), %eax
    addl -124(%rbp), %eax
    movl %eax, -64(%rbp)
    jmp partition11
partition14:
    movl -16(%rbp), %eax
    cmpl -20(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -128(%rbp)
    movl -128(%rbp), %eax
    cmpl $0, %eax
    jne partition17

    jmp partition18
partition15:
    movl -64(%rbp), %eax
    movl %eax, -16(%rbp)
    jmp partition14
partition16:
    jmp partition13
partition17:
    movl -16(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -132(%rbp)
    movl -20(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -136(%rbp)
    movl -16(%rbp), %r10d
    addl $1, %r10d
    movl -136(%rbp), %eax
    movl %eax, A(, %r10, 4)
    movl -20(%rbp), %r10d
    addl $1, %r10d
    movl -132(%rbp), %eax
    movl %eax, A(, %r10, 4)
    jmp partition19
partition18:
    movl -20(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret
partition19:
    jmp partition3

quicksort:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
quicksort0:
    movl %edi, %eax
    movl %eax, -4(%rbp)
    movl %esi, %eax
    movl %eax, -8(%rbp)
    movl -4(%rbp), %eax
    cmpl -8(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -16(%rbp)
    movl -16(%rbp), %eax
    cmpl $0, %eax
    jne quicksort1

    jmp quicksort2
quicksort1:
    movl -4(%rbp), %edi
    movl -8(%rbp), %esi
    movq $0, %rax
    call partition
    movl %eax, -20(%rbp)
    movl -4(%rbp), %edi
    movl -20(%rbp), %esi
    movq $0, %rax
    call quicksort
    movl $1, -24(%rbp)
    movl -20(%rbp), %eax
    addl -24(%rbp), %eax
    movl %eax, -28(%rbp)
    movl -28(%rbp), %edi
    movl -8(%rbp), %esi
    movq $0, %rax
    call quicksort
    jmp quicksort2
quicksort2:
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $128, %rsp
    movq $100, %rax
    movq %rax, A
main0:
    movl $10, -12(%rbp)
    movl -12(%rbp), %eax
    movl %eax, length
    leaq str0(%rip), %rax
    movq %rax, -20(%rbp)
    movq -20(%rbp), %rdi
    movl length, %esi
    movq $0, %rax
    call printf
    movl $17, -24(%rbp)
    movl -24(%rbp), %edi
    movq $0, %rax
    call srandom
    movl $0, -28(%rbp)
    movl -28(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main1
main1:
    movl -8(%rbp), %eax
    cmpl length, %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -32(%rbp)
    movl -32(%rbp), %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    movq $0, %rax
    call random
    movl %eax, -36(%rbp)
    movl -8(%rbp), %r10d
    addl $1, %r10d
    movl -36(%rbp), %eax
    movl %eax, A(, %r10, 4)
    jmp main3
main3:
    movl $1, -40(%rbp)
    movl -8(%rbp), %eax
    addl -40(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main1
main4:
    leaq str1(%rip), %rax
    movq %rax, -48(%rbp)
    movq -48(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -52(%rbp)
    movl -52(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main5
main5:
    movl -8(%rbp), %eax
    cmpl length, %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -56(%rbp)
    movl -56(%rbp), %eax
    cmpl $0, %eax
    jne main6

    jmp main8
main6:
    leaq str2(%rip), %rax
    movq %rax, -64(%rbp)
    movl -8(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -68(%rbp)
    movq -64(%rbp), %rdi
    movl -68(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main7
main7:
    movl $1, -72(%rbp)
    movl -8(%rbp), %eax
    addl -72(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main5
main8:
    movl $0, -76(%rbp)
    movl $1, -80(%rbp)
    movl length, %eax
    subl -80(%rbp), %eax
    movl %eax, -84(%rbp)
    movl -76(%rbp), %edi
    movl -84(%rbp), %esi
    movq $0, %rax
    call quicksort
    leaq str3(%rip), %rax
    movq %rax, -92(%rbp)
    movq -92(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -96(%rbp)
    movl -96(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main9
main9:
    movl -8(%rbp), %eax
    cmpl length, %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -100(%rbp)
    movl -100(%rbp), %eax
    cmpl $0, %eax
    jne main10

    jmp main12
main10:
    leaq str4(%rip), %rax
    movq %rax, -108(%rbp)
    movl -8(%rbp), %r10d
    addl $1, %r10d
    movl A(, %r10, 4), %eax
    movl %eax, -112(%rbp)
    movq -108(%rbp), %rdi
    movl -112(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main11
main11:
    movl $1, -116(%rbp)
    movl -8(%rbp), %eax
    addl -116(%rbp), %eax
    movl %eax, -8(%rbp)
    jmp main9
main12:
    movl $0, -120(%rbp)
    movl -120(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

