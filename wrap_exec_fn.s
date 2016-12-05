.section .text
.globl __wrap_exec_fn
.type __wrap_exec_fn,@function
__wrap_exec_fn:
	push %rbp
	mov %rsp, %rbp
	push %r8

	// arg0: rdi
	// arg1: rsi

	mov %rdi,%r8
	// move top of stack into register
	mov (%esi),%eax
	// move rest of stack to proper address
	mov %rsi,%rdi
	add $4,%rdi

	// fudgy
	mov 10(%rip),%rdx

	jmp *%r8
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

.globl __wrap_exec_fn_ret
.type __wrap_exec_fn_ret,@function
__wrap_exec_fn_ret:
	pop %r8
	ret
