#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

#define ERR_NO_CODE         (-999)
#define ERR_BAD_CODE_LEN    (-998)
#define ERR_NO_STACK        (-997)
#define ERR_MMAP_FAILED     (-996)
#define ERR_MPROTECT_FAILED (-995)

// implemented in assembly
int __exec_fn(const void *code, int *stack);

int
jit_exec(const uint8_t *code, int code_len, int *stack)
{
	if (!code)
		return ERR_NO_CODE;
	if (code_len <= 0)
		return ERR_BAD_CODE_LEN;
	if (!stack)
		return ERR_NO_STACK;

	uint8_t *segment = mmap(NULL, code_len, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
	if (!segment)
		return ERR_MMAP_FAILED;

	for (size_t i = 0; i < code_len; i++)
		segment[i] = code[i];

	int err = mprotect(segment, code_len, PROT_READ|PROT_EXEC);
	if (err)
		return ERR_MPROTECT_FAILED;

	int result = __exec_fn(segment, stack);

	// don't care if munmap fails, we already got our answer.
	(void)munmap(segment, code_len);

	return result;
}
