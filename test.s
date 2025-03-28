# int a;
#
# void main() {
#     a = 2;
#     printf("Hello Decaf%d\n", a);
# }

.comm a, 4, 4

str0:
    .string "Hello Decaf%d\n"

.globl _main
_main:
    # pre-call ritual
    pushq %rbp         # save base pointer
    movq  %rsp, %rbp   # save stack pointer

    # call function `printf`
    leaq format_str_0(%rip), %rdi  # 1st arg; load the address of str constant into %rdi
    movq a, %rsi
    movq $0, %rax                  # zero rax before printf
    call _printf                   # call with the above args

    # analogous to 'return 0;' in C's main
    mov $0, %rax

    # post-call ritual
    movq %rbp, %rsp
    popq %rbp
    ret  # return to where the function was called