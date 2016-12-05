#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

// implemented in assembly
int __exec_fn(void *code, int *stack);

int
jit_exec(uint8_t *code, int code_len, int *stack)
{
	uint8_t *c = mmap(NULL, code_len, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (c == NULL)
		return -999;

	for (size_t i = 0; i < code_len; i++)
		c[i] = code[i];

	int err = mprotect(c, code_len, PROT_READ|PROT_EXEC);
	if (err)
		return -998;

	int result = __exec_fn(c, stack);

	(void)munmap(c, code_len);

	return result;
}
