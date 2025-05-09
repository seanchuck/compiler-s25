str0:
   .string "%d %d\n"
str1:
   .string "%d %d\n"
.globl main
main:
   pushq %rbx
   pushq %rbp
   movq %rsp, %rbp
   subq $152, %rsp
main0:
   movl $10, -32(%rbp)
   movl -32(%rbp), %edi
   movl $20, %ebx
   movl %ebx, %esi
   movl $30, %ebx
   movl %ebx, %ecx
   movl %edi, %ebx
   addl %esi, %ebx
   movl %ebx, %esi
   movl $3, %ebx
   movl %ebx, %ebx
   imul %ecx, %ebx
   movl %ebx, %ebx
   movl %esi, %ecx
   imul %ebx, %ecx
   movl $100, %ebx
   movl %ecx, %eax
   subl %ebx, %eax
   movl %eax, %ebx
   movl %ebx, %ecx
   leaq str0(%rip), %rax
   movq %rax, %rbx
   movq %rdi, 0(%rsp)
   movq %rsi, 8(%rsp)
   movq %rcx, 24(%rsp)
   movq %rbx, %rdi
   movl 8(%rsp), %esi
   movl 24(%rsp), %edx
   xorq %rax, %rax
   call printf
   movq 0(%rsp), %rdi
   movq 8(%rsp), %rsi
   movq 24(%rsp), %rcx
   movl $16, %ebx
   movl %esi, %eax
   cdq
   idivl %ebx
   movl %edx, %ebx
   movl %ebx, %esi
   movl $100, %ebx
   pushq %rdx
   movl %ecx, %eax
   cdq
   idivl %ebx
   movl %eax, %ebx
   popq %rdx
   movl %ebx, %ecx
   leaq str1(%rip), %rax
   movq %rax, %rbx
   movq %rdi, 0(%rsp)
   movq %rsi, 8(%rsp)
   movq %rcx, 24(%rsp)
   movq %rbx, %rdi
   movl 8(%rsp), %esi
   movl 24(%rsp), %edx
   xorq %rax, %rax
   call printf
   movq 0(%rsp), %rdi
   movq 8(%rsp), %rsi
   movq 24(%rsp), %rcx
   movl $0, -100(%rbp)
   movl -100(%rbp), %eax
   movq %rbp, %rsp
   popq %rbp
   popq %rbx
   ret