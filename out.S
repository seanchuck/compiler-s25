looking at temp _t0
coming in as type Long
looking at temp _t1
coming in as type Long
C
looking at temp _t2
coming in as type Int
A
looking at temp _t0
coming in as type Int
A

bouttta assign between Const(
    1,
) and LocalVar(
    "_t0",
)
boutta map this thing
boutta map this thing
LONG
boutta map this thing
INT
boutta map this thing
bouttta assign between LocalVar(
    "_t1",
) and LocalArrElement(
    "_t0",
    LocalVar(
        "_t2",
    ),
)
boutta map this thing
boutta map this thing
boutta map this thing
instruction is: MethodCall {
    name: "do_print",
    args: [],
    return_type: Void,
    dest: None,
}
instruction is: LoadConst {
    src: 0,
    dest: LocalVar(
        "_t0",
    ),
    typ: Int,
}
INT
boutta map this thing
instruction is: Ret {
    typ: Int,
    value: Some(
        LocalVar(
            "_t0",
        ),
    ),
}
boutta map this thing
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
main0:
    movq $0, %rax
    call do_print
    movl $0, -4(%rbp)
    movl -4(%rbp), %eax
    movq %rbp, %rsp
    popq %rbp
    ret

do_print:
    pushq %rbp
    movq %rsp, %rbp
    subq $32, %rsp
do_print0:
    movq $1, %rax
    movq %rax, -16(%rbp)
    movq $1, %rbx
    movq $0, %rax
    shlq $32, %rax
    orq %rax, %rbx
    movq %rbx, -24(%rbp)
    movl $0, -28(%rbp)
    leaq -16(%rbp), %r11
    movq -28(%rbp), %r10
    addq $1, %r10
    movq -24(%rbp), %rax
    movq %rax, (%r11, %r10, 8)
    movq %rbp, %rsp
    popq %rbp
    ret

