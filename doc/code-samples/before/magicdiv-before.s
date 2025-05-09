str0:
   .string "b: %d, c: %d\n"
.globl main
main:
   pushq %rbp
   movq %rsp, %rbp
   subq $96, %rsp
main0:
   movl $2600, -16(%rbp)
   movl -16(%rbp), %eax
   movl %eax, -4(%rbp)
   movl $-50, -20(%rbp)
   pushq %rdx
   movl -4(%rbp), %eax
   cdq
   idivl -20(%rbp)
   movl %eax, -24(%rbp)
   popq %rdx
   movl -24(%rbp), %eax
   movl %eax, -8(%rbp)
   movl $19, -28(%rbp)
   pushq %rdx
   movl -4(%rbp), %eax
   cdq
   idivl -28(%rbp)
   movl %eax, -32(%rbp)
   popq %rdx
   movl -32(%rbp), %eax
   movl %eax, -12(%rbp)
   leaq str0(%rip), %rax
   movq %rax, -40(%rbp)
   movq -40(%rbp), %rdi
   movl -8(%rbp), %esi
   movl -12(%rbp), %edx
   xorq %rax, %rax
   call printf
   movl $0, -44(%rbp)
   movl -44(%rbp), %eax
   movq %rbp, %rsp
   popq %rbp
   ret