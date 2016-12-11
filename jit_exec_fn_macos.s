.globl ___exec_fn
___exec_fn:
	push %rbp
	mov %rsp, %rbp

	// arg 0 is in rdi
	// arg 1 is in rsi

	mov %rdi,%r8
	// move top of stack into register
	mov (%rsi),%eax
	// move rest of stack to proper address
	mov %rsi,%rdi
	add $4,%rdi

	// the jmp following this lea is 3 bytes long when encoded. we
	// want the address of the (pop) instruction immediately after that
	lea 3(%rip),%rdx

	jmp *%r8

	pop %rbp
	ret
