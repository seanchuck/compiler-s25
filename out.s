✅ Generated reg_alloc.html with inline register allocation diagrams.

Assigning LocalVar { name: "i", typ: Int, reg: Some(Reg(R13)) } to Reg(R13d)
Assigning GlobalArrElement { name: "x", index: LocalVar { name: "i", typ: Int, reg: Some(Reg(R13)) }, typ: Int, reg: None } to Address(Some("x"), None, R10, 4, Int)
Assigning GlobalArrElement { name: "y", index: LocalVar { name: "i", typ: Int, reg: Some(Reg(R13)) }, typ: Int, reg: None } to Address(Some("y"), None, R10, 4, Int)
Assigning LocalVar { name: "i", typ: Int, reg: Some(Reg(R12)) } to Reg(R12d)
Assigning LocalVar { name: "_t10", typ: Int, reg: Some(Reg(R13)) } to Reg(R13d)
Assigning GlobalArrElement { name: "y", index: LocalVar { name: "i", typ: Int, reg: Some(Reg(R12)) }, typ: Int, reg: None } to Address(Some("y"), None, R10, 4, Int)
Assigning LocalVar { name: "i", typ: Int, reg: Some(Reg(R14)) } to Reg(R14d)
Assigning LocalVar { name: "_t18", typ: Int, reg: Some(Reg(R12)) } to Reg(R12d)
.comm y, 24, 4
.comm x, 24, 4
str0:
    .string "%d "
str1:
    .string "\n"
.globl main
main:
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rsp, %rbp
    subq $112, %rsp
    movq $5, %rax
    movq %rax, y
    movq $5, %rax
    movq %rax, x
main0:
    movl $0, -8(%rbp)
    movl -8(%rbp), %eax
    movl %eax, %r13d
    jmp main1
main1:
    movl $5, %r12d
    cmpl %r12d, %r13d
    setl %al
    movzbq %al, %rax
    movl %eax, %r12d
    movl %r12d, %eax
    cmpl $0, %eax
    jne main2

    jmp main4
main2:
    movl $1, %r12d
    movl %r13d, %eax
    addl %r12d, %eax
    movl %eax, %r12d
    movl %r13d, %r10d
    addl $1, %r10d
    movl %r12d, %eax
    movl %eax, x(, %r10, 4)
    movl $0, %r12d
    movl %r13d, %r10d
    addl $1, %r10d
    movl %r12d, %eax
    movl %eax, y(, %r10, 4)
    jmp main3
main3:
    movl $1, -32(%rbp)
    movl %r13d, %eax
    addl -32(%rbp), %eax
    movl %eax, %r13d
    jmp main1
main4:
    movl $0, -36(%rbp)
    movl -36(%rbp), %eax
    movl %eax, %r12d
    jmp main5
main5:
    movl $5, %r13d
    cmpl %r13d, %r12d
    setl %al
    movzbq %al, %rax
    movl %eax, %r13d
    movl %r13d, %eax
    cmpl $0, %eax
    jne main6

    jmp main8
main6:
    movl $4, %r13d
    movl %r13d, %eax
    subl %r12d, %eax
    movl %eax, %r13d
    movl %r13d, %r10d
    addl $1, %r10d
    movl x(, %r10, 4), %eax
    movl %eax, %r13d
    movl %r12d, %r10d
    addl $1, %r10d
    movl %r13d, %eax
    movl %eax, y(, %r10, 4)
    jmp main7
main7:
    movl $1, -60(%rbp)
    movl %r12d, %eax
    addl -60(%rbp), %eax
    movl %eax, %r12d
    jmp main5
main8:
    movl $0, -64(%rbp)
    movl -64(%rbp), %eax
    movl %eax, %r14d
    jmp main9
main9:
    movl $5, %r12d
    cmpl %r12d, %r14d
    setl %al
    movzbq %al, %rax
    movl %eax, %r12d
    movl %r12d, %eax
    cmpl $0, %eax
    jne main10

    jmp main12
main10:
    leaq str0(%rip), %rax
    movq %rax, %r13
    movl %r14d, %r10d
    addl $1, %r10d
    movl y(, %r10, 4), %eax
    movl %eax, %r12d
    movq %r13, %rdi
    movl %r12d, %esi
    movq $0, %rax
    call printf
    jmp main11
main11:
    movl $1, -88(%rbp)
    movl %r14d, %eax
    addl -88(%rbp), %eax
    movl %eax, %r14d
    jmp main9
main12:
    leaq str1(%rip), %rax
    movq %rax, -96(%rbp)
    movq -96(%rbp), %rdi
    movq $0, %rax
    call printf
    movl $0, -100(%rbp)
    movl -100(%rbp), %eax
    movq %rbp, %rsp
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    ret

