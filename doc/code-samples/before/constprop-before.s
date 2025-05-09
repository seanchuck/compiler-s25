.comm x, 4, 4
.comm y, 4, 4
.comm z, 4, 4
.globl main
main:
   pushq %rbp
   movq %rsp, %rbp
   subq $112, %rsp
main0:
   movl $10, -16(%rbp)
   movl -16(%rbp), %eax
   movl %eax, -4(%rbp)
   movl $20, -20(%rbp)
   movl -20(%rbp), %eax
   movl %eax, -8(%rbp)
   movl -4(%rbp), %eax
   addl -8(%rbp), %eax
   movl %eax, -24(%rbp)
   movl -24(%rbp), %eax
   movl %eax, -12(%rbp)
   movl $2, -28(%rbp)
   movl -12(%rbp), %eax
   imul -28(%rbp), %eax
   movl %eax, -32(%rbp)
   movl -32(%rbp), %eax
   movl %eax, x
   movl $10, -36(%rbp)
   movl x, %eax
   subl -36(%rbp), %eax
   movl %eax, -40(%rbp)
   movl -40(%rbp), %eax
   movl %eax, y
   movl $2, -44(%rbp)
   pushq %rdx
   movl y, %eax
   cdq
   idivl -44(%rbp)
   movl %eax, -48(%rbp)
   popq %rdx
   movl -48(%rbp), %eax
   movl %eax, z
   movl $0, -52(%rbp)
   movl -52(%rbp), %eax
   movq %rbp, %rsp
   popq %rbp
   ret
