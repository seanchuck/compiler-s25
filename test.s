
.comm ARR_SIZE, 4, 4
.comm SAMPLED_INTS, 1004, 4
str0:
    .string "tests/phase5/output/mergesort.txt"
merge_sort:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $104, %rsp
merge_sort0:
    cmpl %edx, %esi
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge_sort1

    jmp merge_sort2
merge_sort1:
    movl %edx, %ecx
    subl %esi, %ecx
    movl $2, %ebx
    pushq %rdx
    movl %ecx, %eax
    cdq
    idivl %ebx
    movl %eax, %ebx
    popq %rdx
    movl %esi, %ecx
    addl %ebx, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq 0(%rsp), %rdi
    movl 8(%rsp), %esi
    movl 24(%rsp), %edx
    xorq %rax, %rax
    call merge_sort
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movl $1, %ebx
    movl %ebx, %ebx
    addl %ecx, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq 0(%rsp), %rdi
    movl %ebx, %esi
    movl 16(%rsp), %edx
    xorq %rax, %rax
    call merge_sort
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq 0(%rsp), %rdi
    movl 8(%rsp), %esi
    movl 24(%rsp), %edx
    movl 16(%rsp), %ecx
    xorq %rax, %rax
    call merge
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
merge_sort2:
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

sample_output:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $136, %rsp
sample_output0:
    movl $0, %ebx
    movl %ebx, %ebx
sample_output1:
    cmpl ARR_SIZE, %ebx
    setl %al
    movzbq %al, %rax
    movl %eax, %ecx
    movl %ecx, %eax
    cmpl $0, %eax
    jne sample_output2

    jmp sample_output4
sample_output2:
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rsi
    movsxd %ebx, %rax
    movq %rax, %rcx
    movq %rcx, %rcx
    imul %rsi, %rcx
    movq %rcx, %rcx
    addq %rdi, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq 24(%rsp), %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movl %eax, %esi
    movl $10000, %ecx
    pushq %rdx
    movl %ebx, %eax
    cdq
    idivl %ecx
    movl %eax, %ecx
    popq %rdx
    movl %ecx, %r10d
    addl $1, %r10d
    movl %esi, SAMPLED_INTS(, %r10, 4)
sample_output3:
    movl $10000, %ecx
    movl %ebx, %ebx
    addl %ecx, %ebx
    jmp sample_output1
sample_output4:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq 0(%rsp), %rdi
    xorq %rax, %rax
    call ptr_free
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

fix_neg:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $120, %rsp
fix_neg0:
    movl $0, %ebx
    cmpl %ebx, %edi
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne fix_neg1

    jmp fix_neg2
fix_neg1:
    movsxd %edi, %rax
    movq %rax, %rdi
    movsxd %esi, %rax
    movq %rax, %rcx
    movsxd %esi, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, %rax
    addq %rbx, %rax
    movq %rax, -56(%rbp)
    movq -56(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret
fix_neg2:
    movsxd %edi, %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret
fix_neg3:
    movq $-1, %rdi
    call exit
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

write_array:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $88, %rsp
write_array0:
    leaq str0(%rip), %rax
    movq %rax, -12(%rbp)
    movq %rcx, 24(%rsp)
    movq -12(%rbp), %rdi
    xorq %rax, %rax
    call num_open_file_write
    movq 24(%rsp), %rcx
    movl $0, -16(%rbp)
    movl -16(%rbp), %ecx
write_array1:
    movl $250, %ebx
    cmpl %ebx, %ecx
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne write_array2

    jmp write_array4
write_array2:
    movl %ecx, %r10d
    addl $1, %r10d
    movl SAMPLED_INTS(, %r10, 4), %ebx
    movq %rcx, 24(%rsp)
    movl %ebx, %edi
    xorq %rax, %rax
    call num_write_num_int
    movq 24(%rsp), %rcx
write_array3:
    movl $1, %ebx
    movl %ecx, %ecx
    addl %ebx, %ecx
    jmp write_array1
write_array4:
    movq %rcx, 24(%rsp)
    xorq %rax, %rax
    call num_close_file
    movq 24(%rsp), %rcx
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

allocate:
    pushq %rbx
    pushq %r12
    pushq %rbp
    movq %rsp, %rbp
    subq $288, %rsp
allocate0:
    movl $65536, %ebx
    movl ARR_SIZE, %eax
    movsxd %eax, %rax
    movq %rax, %rsi
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movq %rcx, %rcx
    imul %rsi, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xorq %rax, %rax
    call ptr_alloc
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    xorq %rax, %rax
    call ptr_get_lower
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 24(%rsp), %edi
    movl %ebx, %esi
    xorq %rax, %rax
    call fix_neg
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movq %rax, %rcx
    movq %rcx, %rdi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    xorq %rax, %rax
    call ptr_get_upper
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl 24(%rsp), %edi
    xorq %rax, %rax
    call abs
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movsxd %ecx, %rax
    movq %rax, %rsi
    movsxd %ebx, %rax
    movq %rax, %rcx
    movsxd %ebx, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rbx, %rbx
    imul %rsi, %rbx
    movq %rdi, %rdi
    addq %rbx, %rdi
    movq %rdi, %rsi
    movl $0, %ebx
    movl %ebx, %ecx
allocate1:
    cmpl ARR_SIZE, %ecx
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne allocate2

    jmp allocate4
allocate2:
    movl $3559, %ebx
    movl %ecx, %eax
    cdq
    idivl %ebx
    movl %edx, %r8d
    movl $5581, %ebx
    movl %ebx, %ebx
    imul %r8d, %ebx
    movl $1777, %r8d
    movl %ecx, %eax
    cdq
    idivl %r8d
    movl %edx, %r12d
    movl $8693, %r9d
    movl $9151, %r8d
    movl %ecx, %eax
    cdq
    idivl %r8d
    movl %edx, %r8d
    movl %r9d, %eax
    subl %r8d, %eax
    movl %eax, %r8d
    movl %r8d, %r8d
    imul %r12d, %r8d
    movl %ebx, %r9d
    subl %r8d, %r9d
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %r8
    movl ARR_SIZE, %ebx
    subl %ecx, %ebx
    movsxd %ebx, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %r8, %rbx
    movq %rbx, %rbx
    addq %rdi, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    movl 40(%rsp), %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
allocate3:
    movl $1, %ebx
    movl %ecx, %ecx
    addl %ebx, %ecx
    jmp allocate1
allocate4:
    movq %rsi, %rax
    movq %rbp, %rsp
    popq %rbp
    popq %r12
    popq %rbx
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $80, %rsp
    movq $250, %rax
    movq %rax, SAMPLED_INTS
main0:
    movl $2500000, -12(%rbp)
    movl -12(%rbp), %eax
    movl %eax, ARR_SIZE
    xorq %rax, %rax
    call allocate
    movq %rax, -20(%rbp)
    xorq %rax, %rax
    call start_timer
    movq -20(%rbp), %rdi
    movl ARR_SIZE, %esi
    xorq %rax, %rax
    call perform_sort_and_print
    xorq %rax, %rax
    call end_timer
    xorq %rax, %rax
    call timer_print
    movq -20(%rbp), %rdi
    xorq %rax, %rax
    call sample_output
    xorq %rax, %rax
    call write_array
    xorq %rax, %rax
    call timer_write
    movl $0, -24(%rbp)
    movl -24(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

perform_sort_and_print:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $72, %rsp
perform_sort_and_print0:
    movl $0, %ecx
    movl $1, %ebx
    movl %esi, %eax
    subl %ebx, %eax
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rcx, 24(%rsp)
    movq 0(%rsp), %rdi
    movl 24(%rsp), %esi
    movl %ebx, %edx
    xorq %rax, %rax
    call merge_sort
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 24(%rsp), %rcx
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

abs:
    pushq %rbx
    pushq %rbp
    movq %rsp, %rbp
    subq $72, %rsp
abs0:
    movl $0, %ebx
    cmpl %ebx, %edi
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne abs1

    jmp abs2
abs1:
    movl $-1, %ebx
    movl %edi, %eax
    imul %ebx, %eax
    movl %eax, -20(%rbp)
    movl -20(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret
abs2:
    movl %edi, %eax
    movq %rbp, %rsp
    popq %rbp
    popq %rbx
    ret

merge:
    pushq %rbx
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    pushq %rbp
    movq %rsp, %rbp
    subq $920, %rsp
merge0:
    movl $65536, %r9d
    movl %edx, %r8d
    subl %esi, %r8d
    movl $1, %ebx
    movl %r8d, %eax
    addl %ebx, %eax
    movl %eax, -84(%rbp)
    movl -84(%rbp), %eax
    movl %eax, -36(%rbp)
    movl %ecx, %eax
    subl %edx, %eax
    movl %eax, -88(%rbp)
    movl -88(%rbp), %eax
    movl %eax, -40(%rbp)
    movl -84(%rbp), %eax
    movsxd %eax, %rax
    movq %rax, %rcx
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call ptr_alloc
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
    xorq %rax, %rax
    call ptr_get_lower
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl %ebx, %edi
    movl 40(%rsp), %esi
    xorq %rax, %rax
    call fix_neg
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movq %rax, %rbx
    movq %rbx, %r8
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    xorq %rax, %rax
    call ptr_get_upper
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl %ebx, %edi
    xorq %rax, %rax
    call abs
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movsxd %ebx, %rax
    movq %rax, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rcx, %rcx
    imul %rbx, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %r8, %r13
    addq %rbx, %r13
    movq %r13, %r15
    movl -88(%rbp), %eax
    movsxd %eax, %rax
    movq %rax, %rcx
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call ptr_alloc
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
    xorq %rax, %rax
    call ptr_get_lower
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl %ebx, %edi
    movl 40(%rsp), %esi
    xorq %rax, %rax
    call fix_neg
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movq %rax, %rbx
    movq %rbx, %r8
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    xorq %rax, %rax
    call ptr_get_upper
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movl %ebx, %edi
    xorq %rax, %rax
    call abs
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movsxd %ebx, %rax
    movq %rax, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rcx, %rcx
    imul %rbx, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %r8, %r12
    addq %rbx, %r12
    movq %r12, %r14
    movl $0, %ebx
    movl %ebx, %r9d
merge1:
    cmpl -84(%rbp), %r9d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge2

    jmp merge4
merge2:
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movl %esi, %ebx
    addl %r9d, %ebx
    movsxd %ebx, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, %r8
    addq %rbx, %r8
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %r13, %rcx
    addq %rbx, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 32(%rsp), %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
merge3:
    movl $1, %ebx
    movl %r9d, %r9d
    addl %ebx, %r9d
    jmp merge1
merge4:
    movl $0, %ebx
    movl %ebx, %r9d
merge5:
    cmpl -88(%rbp), %r9d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge6

    jmp merge8
merge6:
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movl $1, %ebx
    movl %ebx, %ebx
    addl %edx, %ebx
    movl %ebx, %ebx
    addl %r9d, %ebx
    movsxd %ebx, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, %r8
    addq %rbx, %r8
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %r12, %rcx
    addq %rbx, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 32(%rsp), %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
merge7:
    movl $1, %ebx
    movl %r9d, %r9d
    addl %ebx, %r9d
    jmp merge5
merge8:
    movl $0, %ebx
    movl %ebx, %r13d
    movl $0, %ebx
    movl %ebx, %r12d
    movl %esi, %r9d
merge9:
    cmpl -36(%rbp), %r13d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge12

    jmp merge11
merge12:
    cmpl -40(%rbp), %r12d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge10

    jmp merge11
merge10:
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, %r8
    addq %rbx, %r8
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r13d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %r15, %rsi
    addq %rbx, %rsi
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r12d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rbx, %rbx
    addq %r14, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %esi
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    cmpl %ecx, %esi
    setle %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge13

    jmp merge14
merge13:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 32(%rsp), %rdi
    movl 8(%rsp), %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %ebx
    movl %r13d, %r13d
    addl %ebx, %r13d
    jmp merge15
merge14:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 32(%rsp), %rdi
    movl 24(%rsp), %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %ebx
    movl %r12d, %r12d
    addl %ebx, %r12d
merge15:
    movl $1, %ebx
    movl %r9d, %r9d
    addl %ebx, %r9d
    jmp merge9
merge11:
merge16:
    cmpl -36(%rbp), %r13d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge17

    jmp merge18
merge17:
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rdi, %rsi
    addq %rbx, %rsi
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r13d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rbx, %rbx
    addq %r15, %rbx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ebx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 8(%rsp), %rdi
    movl %ebx, %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %ebx
    movl %r13d, %r13d
    addl %ebx, %r13d
    movl $1, %ebx
    movl %r9d, %r9d
    addl %ebx, %r9d
    jmp merge16
merge18:
merge19:
    cmpl -40(%rbp), %r12d
    setl %al
    movzbq %al, %rax
    movl %eax, %ebx
    movl %ebx, %eax
    cmpl $0, %eax
    jne merge20

    jmp merge21
merge20:
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rcx
    movsxd %r9d, %rax
    movq %rax, %rbx
    movq %rbx, %rbx
    imul %rcx, %rbx
    movq %rbx, %rbx
    addq %rdi, %rbx
    movq $4, %r10
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %r10
    movq %r10, %rsi
    movsxd %r12d, %rax
    movq %rax, %rcx
    movq %rcx, %rcx
    imul %rsi, %rcx
    movq %rcx, %rcx
    addq %r14, %rcx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq 24(%rsp), %rdi
    xorq %rax, %rax
    call ptr_read_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl %eax, %ecx
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %rbx, %rdi
    movl 24(%rsp), %esi
    xorq %rax, %rax
    call ptr_write_int
    movq 0(%rsp), %rdi
    movq 8(%rsp), %rsi
    movq 16(%rsp), %rdx
    movq 24(%rsp), %rcx
    movq 32(%rsp), %r8
    movq 40(%rsp), %r9
    movl $1, %ebx
    movl %r12d, %r12d
    addl %ebx, %r12d
    movl $1, %ebx
    movl %r9d, %r9d
    addl %ebx, %r9d
    jmp merge19
merge21:
    movq %rdi, 0(%rsp)
    movq %rsi, 8(%rsp)
    movq %rdx, 16(%rsp)
    movq %rcx, 24(%rsp)
    movq %r8, 32(%rsp)
    movq %r9, 40(%rsp)
    movq %r15, %rdi
    xorq %rax, %rax
    call ptr_free
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
    movq %r14, %rdi
    xorq %rax, %rax
    call ptr_free
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
    popq %rbx
    ret

