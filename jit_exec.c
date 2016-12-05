#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

int __wrap_exec_fn(void *code, int *stack);

int
jit_exec(uint8_t *code, int code_len, int *stack)
{
	uint8_t *c = mmap(NULL, code_len, PROT_READ|PROT_WRITE|PROT_EXEC, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (c == NULL)
		return -999;

	for (size_t i = 0; i < code_len; i++)
		c[i] = code[i];

	printf("jit_exec: %p (len: %d) stack: %p\n", c, code_len, stack);

	int result = __wrap_exec_fn(c, stack);

	printf("(RESULT: %d)\n", result);

	return result;
}
