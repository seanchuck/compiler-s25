str0:
   .string "%d %d\n"
str1:
   .string "%d %d\n"
.globl main
main:
   pushq %rbp
   movq %rsp, %rbp
   subq $160, %rsp
main0:
   movl $10, -32(%rbp)
   movl -32(%rbp), %eax
   movl %eax, -4(%rbp)
   movl $20, -36(%rbp)
   movl -36(%rbp), %eax
   movl %eax, -8(%rbp)
   movl $30, -40(%rbp)
   movl -40(%rbp), %eax
   movl %eax, -12(%rbp)
   movl -4(%rbp), %eax
   addl -8(%rbp), %eax
   movl %eax, -44(%rbp)
   movl -44(%rbp), %eax
   movl %eax, -16(%rbp)
   movl $3, -48(%rbp)
   movl -12(%rbp), %eax
   imul -48(%rbp), %eax
   movl %eax, -52(%rbp)
   movl -52(%rbp), %eax
   movl %eax, -20(%rbp)
   movl -16(%rbp), %eax
   imul -20(%rbp), %eax
   movl %eax, -56(%rbp)
   movl $100, -60(%rbp)
   movl -56(%rbp), %eax
   subl -60(%rbp), %eax
   movl %eax, -64(%rbp)
   movl -64(%rbp), %eax
   movl %eax, -20(%rbp)
   leaq str0(%rip), %rax
   movq %rax, -72(%rbp)
   movq -72(%rbp), %rdi
   movl -16(%rbp), %esi
   movl -20(%rbp), %edx
   xorq %rax, %rax
   call printf
   movl $16, -76(%rbp)
   movl -16(%rbp), %eax
   cdq
   idivl -76(%rbp)
   movl %edx, -80(%rbp)
   movl -80(%rbp), %eax
   movl %eax, -24(%rbp)
   movl $100, -84(%rbp)
   pushq %rdx
   movl -20(%rbp), %eax
   cdq
   idivl -84(%rbp)
   movl %eax, -88(%rbp)
   popq %rdx
   movl -88(%rbp), %eax
   movl %eax, -28(%rbp)
   leaq str1(%rip), %rax
   movq %rax, -96(%rbp)
   movq -96(%rbp), %rdi
   movl -24(%rbp), %esi
   movl -28(%rbp), %edx
   xorq %rax, %rax
   call printf
   movl $0, -100(%rbp)
   movl -100(%rbp), %eax
   movq %rbp, %rsp
   popq %rbp
   ret


