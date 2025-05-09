str0:
   .string "%d\n"
.globl main
main:
   pushq %rbp
   movq %rsp, %rbp
   subq $192, %rsp
main0:
   movq $5, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -96(%rbp)
   movq -96(%rbp), %rax
   movq %rax, -8(%rbp)
   movq $999, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -104(%rbp)
   movq -104(%rbp), %rax
   movq %rax, -64(%rbp)
   movq -8(%rbp), %rax
   movq %rax, -16(%rbp)
   movq $2, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -112(%rbp)
   movq -16(%rbp), %rax
   addq -112(%rbp), %rax
   movq %rax, -120(%rbp)
   movq -120(%rbp), %rax
   movq %rax, -24(%rbp)
   movq $3, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -128(%rbp)
   movq -24(%rbp), %rax
   cqto
   movq -128(%rbp), %rcx
   idiv %rcx
   movq %rdx, -136(%rbp)
   movq -136(%rbp), %rax
   movq %rax, -32(%rbp)
   movq $1, %rax
   movq %rax, -72(%rbp)
   movq -72(%rbp), %rax
   movq %rax, -80(%rbp)
   movq -72(%rbp), %rax
   cmpq $0, %rax
   jne main1


   jmp main2
main1:
   movq -32(%rbp), %rax
   movq %rax, -40(%rbp)
   movq -64(%rbp), %rax
   movq %rax, -56(%rbp)
   jmp main3
main2:
   movq $0, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -144(%rbp)
   movq -144(%rbp), %rax
   movq %rax, -40(%rbp)
   movq -64(%rbp), %rax
   movq %rax, -56(%rbp)
   jmp main3
main3:
   movq $3, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -152(%rbp)
   movq -56(%rbp), %rax
   addq -152(%rbp), %rax
   movq %rax, -160(%rbp)
   movq -160(%rbp), %rax
   movq %rax, -8(%rbp)
   movq -80(%rbp), %rax
   xor $1, %rax
   movq %rax, -168(%rbp)
   movq -168(%rbp), %rax
   movq %rax, -88(%rbp)
   leaq str0(%rip), %rax
   movq %rax, -176(%rbp)
   movq -176(%rbp), %rdi
   movq -40(%rbp), %rsi
   movq $0, %rax
   call printf
   movq $0, %rbx
   movq $0, %rax
   shlq $32, %rax
   orq %rax, %rbx
   movq %rbx, -184(%rbp)
   movq -184(%rbp), %rax
   movq %rbp, %rsp
   popq %rbp
   ret
