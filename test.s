str0:
    .string "%d\n"  # string constant

.globl main
main:
    # pre-call ritual
    pushq %rbp         # save base pointer
    movq  %rsp, %rbp   # save stack pointer

    # call function `printf`
    leaq str0(%rip), %rdi  # 1st arg; load the address of str constant into %rdi
    movq $-32, %rsi                 # 2nd arg; load int constant into %rsi
    movq $0, %rax                  # zero rax before printf
    call printf                    # call with the above args

    # analogous to 'return 0;' in C's main
    mov $0, %rax

    # post-call ritual
    movq %rbp, %rsp
    popq %rbp
    ret  # return to where the function was called