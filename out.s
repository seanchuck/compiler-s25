<<<<<<< HEAD
Replacing j with _t1
Replacing j with _t1
Replacing sum with _t0
Replacing j with _t1
Replacing sum with _t0
Replacing j with _t1
Replacing sum with _t0
Replacing j with _t11
Replacing i with _t10
Replacing j with _t11
Replacing i with _t10
Replacing i with _t10

str0:
    .string "j is %d\n"
str1:
    .string "sum is %d\n"
str2:
    .string "ERROR: for loop is bad (1)\n"
str3:
    .string "hi\n"
str4:
    .string "ERROR: for loop is bad (2)\n"
str5:
    .string "%d\n"
str6:
    .string "ERROR: true branch is bad (2)\n"
str7:
    .string "ERROR: else branch is bad (2)\n"
str8:
    .string "control flow OK if no previous output\n"
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $176, %rsp
main0:
    movl $0, -16(%rbp)
    movl -16(%rbp), %eax
    movl %eax, -8(%rbp)
    movl $0, -20(%rbp)
    movl -20(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main1
main1:
    movl $10, -24(%rbp)
    movl -20(%rbp), %eax
    cmpl -24(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -28(%rbp)
    movl -28(%rbp), %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    leaq str0(%rip), %rax
    movq %rax, -36(%rbp)
    movq -36(%rbp), %rdi
    movl -20(%rbp), %esi
    movq $0, %rax
    call printf
    movl -16(%rbp), %eax
    addl -20(%rbp), %eax
    movl %eax, -8(%rbp)
    leaq str1(%rip), %rax
    movq %rax, -44(%rbp)
    movq -44(%rbp), %rdi
    movl -16(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main3
main3:
    movl $1, -48(%rbp)
    movl -20(%rbp), %eax
    addl -48(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main1
main4:
    movl $45, -52(%rbp)
    movl -16(%rbp), %eax
    cmpl -52(%rbp), %eax
    setne %al
    movzbq %al, %rax
    movl %eax, -56(%rbp)
    movl -56(%rbp), %eax
    cmpl $0, %eax
    jne main5

    jmp main6
main5:
    leaq str2(%rip), %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main6
main6:
    movl $11, -68(%rbp)
    movl -68(%rbp), %eax
    movl %eax, -4(%rbp)
    movl $10, -72(%rbp)
    movl -72(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main7
main7:
    movl $0, -76(%rbp)
    movl -72(%rbp), %eax
    cmpl -76(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -80(%rbp)
    movl -80(%rbp), %eax
    cmpl $0, %eax
    jne main8

    jmp main10
main8:
    leaq str3(%rip), %rax
    movq %rax, -88(%rbp)
    movq -88(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $1, -92(%rbp)
    movl -68(%rbp), %eax
    addl -92(%rbp), %eax
    movl %eax, -4(%rbp)
    jmp main9
main9:
    movl $1, -96(%rbp)
    movl -72(%rbp), %eax
    addl -96(%rbp), %eax
    movl %eax, -12(%rbp)
    jmp main7
main10:
    movl $11, -100(%rbp)
    movl -68(%rbp), %eax
    cmpl -100(%rbp), %eax
    setne %al
    movzbq %al, %rax
    movl %eax, -104(%rbp)
    movl -104(%rbp), %eax
    cmpl $0, %eax
    jne main11

    jmp main12
main11:
    leaq str4(%rip), %rax
    movq %rax, -112(%rbp)
    movq -112(%rbp), %rdi
    movq $0, %rax
    call printf
    leaq str5(%rip), %rax
    movq %rax, -120(%rbp)
    movq -120(%rbp), %rdi
    movl -68(%rbp), %esi
    movq $0, %rax
    call printf
    jmp main12
main12:
    movl $1, -124(%rbp)
    movl $2, -128(%rbp)
    movl -124(%rbp), %eax
    cmpl -128(%rbp), %eax
    setg %al
    movzbq %al, %rax
    movl %eax, -132(%rbp)
    movl -132(%rbp), %eax
    cmpl $0, %eax
    jne main13

    jmp main14
main13:
    leaq str6(%rip), %rax
    movq %rax, -140(%rbp)
    movq -140(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main15
main14:
    jmp main15
main15:
    movl $1, -144(%rbp)
    movl $2, -148(%rbp)
    movl -144(%rbp), %eax
    cmpl -148(%rbp), %eax
    setl %al
    movzbq %al, %rax
    movl %eax, -152(%rbp)
    movl -152(%rbp), %eax
    cmpl $0, %eax
    jne main16

    jmp main17
main16:
    jmp main18
main17:
    leaq str7(%rip), %rax
    movq %rax, -160(%rbp)
    movq -160(%rbp), %rdi
    movq $0, %rax
    call printf
    jmp main18
main18:
    leaq str8(%rip), %rax
    movq %rax, -168(%rbp)
    movq -168(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -172(%rbp)
    movl -172(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

||||||| 3014bf8
=======


str0:
    .string "%ld\n"
str1:
    .string "%d\n"
get_int:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
get_int0:
    movq %rdi, %rax
    movq %rax, -8(%rbp)
    leaq str1(%rip), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rdi
    movq -8(%rbp), %rsi
    movq $0, %rax
    call printf
    movq -8(%rbp), %rax
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
    leaq str0(%rip), %rax
    movq %rax, -16(%rbp)
    movq -16(%rbp), %rdi
    movq -8(%rbp), %rsi
    movq $0, %rax
    call printf
    movq -8(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $688, %rsp
main0:
    movq $10, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -32(%rbp)
    movq -32(%rbp), %rax
    movq %rax, -8(%rbp)
    movq $4284967296, %rbx
    movq $4294967295, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -40(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -16(%rbp)
    movq $4294967292, %rbx
    movq $4294967295, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -48(%rbp)
    movq -48(%rbp), %rax
    movq %rax, -24(%rbp)
    jmp main1
main1:
    movq $6, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -56(%rbp)
    movq -24(%rbp), %rax
    cmpq -56(%rbp), %rax
    setl %al
    movzbq %al, %rax
    movq %rax, -64(%rbp)
    movq -64(%rbp), %rax
    cmpq $0, %rax
    jne main2

    jmp main4
main2:
    movq -24(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -72(%rbp)
    movq -8(%rbp), %rax
    addq -72(%rbp), %rax
    movq %rax, -8(%rbp)
    movq -24(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -80(%rbp)
    movq $3, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -88(%rbp)
    movq -8(%rbp), %rax
    subq -88(%rbp), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -104(%rbp)
    movq -80(%rbp), %rax
    subq -104(%rbp), %rax
    movq %rax, -112(%rbp)
    movq -8(%rbp), %rax
    subq -112(%rbp), %rax
    movq %rax, -8(%rbp)
    movq -24(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -120(%rbp)
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -128(%rbp)
    movq -8(%rbp), %rax
    imul -128(%rbp), %rax
    movq %rax, -136(%rbp)
    movq -136(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -144(%rbp)
    movq -120(%rbp), %rax
    imul -144(%rbp), %rax
    movq %rax, -152(%rbp)
    movq -8(%rbp), %rax
    imul -152(%rbp), %rax
    movq %rax, -8(%rbp)
    movq $21, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -160(%rbp)
    movq -24(%rbp), %rax
    imul -160(%rbp), %rax
    movq %rax, -168(%rbp)
    movq -168(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -176(%rbp)
    movq $7, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -184(%rbp)
    movq -184(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -192(%rbp)
    movq -176(%rbp), %rax
    cqto
    movq -192(%rbp), %rcx
    idiv %rcx
    movq %rax, -200(%rbp)
    movq $15, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -208(%rbp)
    movq -208(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -216(%rbp)
    movq -200(%rbp), %rax
    addq -216(%rbp), %rax
    movq %rax, -224(%rbp)
    movq -8(%rbp), %rax
    cqto
    movq -224(%rbp), %rcx
    idiv %rcx
    movq %rax, -8(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -232(%rbp)
    movq -8(%rbp), %rax
    addq -232(%rbp), %rax
    movq %rax, -240(%rbp)
    movq $100, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -248(%rbp)
    movq -240(%rbp), %rax
    imul -248(%rbp), %rax
    movq %rax, -256(%rbp)
    movq -256(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -264(%rbp)
    movq -8(%rbp), %rax
    addq -264(%rbp), %rax
    movq %rax, -8(%rbp)
    movq $41, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -272(%rbp)
    movq -24(%rbp), %rax
    imul -272(%rbp), %rax
    movq %rax, -280(%rbp)
    movq -280(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -288(%rbp)
    movq $18, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -296(%rbp)
    movq -296(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -304(%rbp)
    movq -288(%rbp), %rax
    cqto
    movq -304(%rbp), %rcx
    idiv %rcx
    movq %rdx, -312(%rbp)
    movq $15, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -320(%rbp)
    movq -320(%rbp), %rdi
    movq $0, %rax
    call get_int
    movq %rax, -328(%rbp)
    movq -312(%rbp), %rax
    addq -328(%rbp), %rax
    movq %rax, -336(%rbp)
    movq -8(%rbp), %rax
    cqto
    movq -336(%rbp), %rcx
    idiv %rcx
    movq %rdx, -8(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -344(%rbp)
    movq -344(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -352(%rbp)
    movq -16(%rbp), %rax
    addq -352(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -360(%rbp)
    movq -360(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -368(%rbp)
    movq -8(%rbp), %rax
    movq %rax, -376(%rbp)
    movq $3, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -384(%rbp)
    movq -376(%rbp), %rax
    subq -384(%rbp), %rax
    movq %rax, -392(%rbp)
    movq -392(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -400(%rbp)
    movq -368(%rbp), %rax
    subq -400(%rbp), %rax
    movq %rax, -408(%rbp)
    movq -16(%rbp), %rax
    subq -408(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -416(%rbp)
    movq -416(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -424(%rbp)
    movq -8(%rbp), %rax
    movq %rax, -432(%rbp)
    movq $5, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -440(%rbp)
    movq -432(%rbp), %rax
    imul -440(%rbp), %rax
    movq %rax, -448(%rbp)
    movq -448(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -456(%rbp)
    movq -424(%rbp), %rax
    imul -456(%rbp), %rax
    movq %rax, -464(%rbp)
    movq -16(%rbp), %rax
    imul -464(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -472(%rbp)
    movq $21, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -480(%rbp)
    movq -472(%rbp), %rax
    imul -480(%rbp), %rax
    movq %rax, -488(%rbp)
    movq -488(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -496(%rbp)
    movq $7, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -504(%rbp)
    movq -504(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -512(%rbp)
    movq -496(%rbp), %rax
    cqto
    movq -512(%rbp), %rcx
    idiv %rcx
    movq %rax, -520(%rbp)
    movq $15, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -528(%rbp)
    movq -528(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -536(%rbp)
    movq -520(%rbp), %rax
    addq -536(%rbp), %rax
    movq %rax, -544(%rbp)
    movq -16(%rbp), %rax
    cqto
    movq -544(%rbp), %rcx
    idiv %rcx
    movq %rax, -16(%rbp)
    movq -8(%rbp), %rax
    movq %rax, -552(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -560(%rbp)
    movq -552(%rbp), %rax
    addq -560(%rbp), %rax
    movq %rax, -568(%rbp)
    movq $100, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -576(%rbp)
    movq -568(%rbp), %rax
    imul -576(%rbp), %rax
    movq %rax, -584(%rbp)
    movq -584(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -592(%rbp)
    movq -16(%rbp), %rax
    addq -592(%rbp), %rax
    movq %rax, -16(%rbp)
    movq -24(%rbp), %rax
    movq %rax, -600(%rbp)
    movq $41, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -608(%rbp)
    movq -600(%rbp), %rax
    imul -608(%rbp), %rax
    movq %rax, -616(%rbp)
    movq -616(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -624(%rbp)
    movq $18, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -632(%rbp)
    movq -632(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -640(%rbp)
    movq -624(%rbp), %rax
    cqto
    movq -640(%rbp), %rcx
    idiv %rcx
    movq %rdx, -648(%rbp)
    movq $15, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -656(%rbp)
    movq -656(%rbp), %rdi
    movq $0, %rax
    call get_long
    movq %rax, -664(%rbp)
    movq -648(%rbp), %rax
    addq -664(%rbp), %rax
    movq %rax, -672(%rbp)
    movq -16(%rbp), %rax
    cqto
    movq -672(%rbp), %rcx
    idiv %rcx
    movq %rdx, -16(%rbp)
    jmp main3
main3:
    movq $3, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -680(%rbp)
    movq -24(%rbp), %rax
    addq -680(%rbp), %rax
    movq %rax, -24(%rbp)
    jmp main1
main4:
    movq $0, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -688(%rbp)
    movq -688(%rbp), %rax
    movq %rbp, %rsp
    popq %rbp
    ret

>>>>>>> dce
