#include <stdio.h>

// External declarations
extern void dump_init(void);
extern void dump(char* loop_name, char* invocations, int arg_count, ...);
extern void after_dump(char* loop_name);

int
__cere__loop_region(int* numbers, int size)
{
  printf("[VERBOSE] __cere__loop_region: ENTRY\n");
  fflush(stdout);

  // Manually call dump to see what happens
  printf("[VERBOSE] __cere__loop_region: Before dump() call\n");
  fflush(stdout);

  // Call dump with the same parameters the pass would use
  dump("__cere__loop_region", "1", 2, numbers, &size);

  printf("[VERBOSE] __cere__loop_region: After dump() call\n");
  fflush(stdout);

  int work = 0;
  for (int i = 0; i < size; ++i) {
    work += numbers[i] * (i + 1);
  }

  printf("[VERBOSE] __cere__loop_region: Before after_dump() call\n");
  fflush(stdout);

  after_dump("__cere__loop_region");

  printf("[VERBOSE] __cere__loop_region: After after_dump() call\n");
  fflush(stdout);

  return work;
}

int
main()
{
  printf("[VERBOSE] main: Before dump_init()\n");
  fflush(stdout);

  dump_init();

  printf("[VERBOSE] main: After dump_init()\n");
  fflush(stdout);

  int numbers[] = {1, 2, 3, 4, 5};

  printf("[VERBOSE] main: Before __cere__loop_region() call\n");
  fflush(stdout);

  int result = __cere__loop_region(numbers, 5);

  printf("[VERBOSE] main: After __cere__loop_region() call, result=%d\n", result);
  printf("Result: %d\n", result);
  fflush(stdout);

  return 0;
}
