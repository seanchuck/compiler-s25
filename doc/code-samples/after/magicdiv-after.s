str0:
   .string "b: %d, c: %d\n"
.globl main
main:
   pushq %rbx
   pushq %rbp
   movq %rsp, %rbp
   subq $104, %rsp
main0:
   pushq %rdx
   movl $2600, %r11d
   movabs $368934881474191033, %rax
   mul %r11
   movl %edx, %esi
   neg %esi
   popq %rdx
   pushq %rdx
   movl $2600, %r11d
   movabs $970881267037344822, %rax
   mul %r11
   movl %edx, %ecx
   popq %rdx
   leaq str0(%rip), %rax
   movq %rax, %rbx
   movq %rsi, 8(%rsp)
   movq %rcx, 24(%rsp)
   movq %rbx, %rdi
   movl 8(%rsp), %esi
   movl 24(%rsp), %edx
   xorq %rax, %rax
   call printf
   movq 8(%rsp), %rsi
   movq 24(%rsp), %rcx
   movl $0, -44(%rbp)
   movl -44(%rbp), %eax
   movq %rbp, %rsp
   popq %rbp
   popq %rbx
   ret