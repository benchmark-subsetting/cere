#include <stdio.h>

extern void dump_init(void);
extern void dump(char* loop_name, char* invocations, int arg_count, ...);
extern void after_dump(char* loop_name);

int
__cere__loop_region(int* numbers, int size)
{
  printf("[DEBUG] Entering __cere__loop_region\n");
  fflush(stdout);

  int work = 0;
  for (int i = 0; i < size; ++i) {
    work += numbers[i] * (i + 1);
  }

  printf("[DEBUG] Exiting __cere__loop_region, work=%d\n", work);
  fflush(stdout);

  return work;
}

int
main()
{
  printf("[DEBUG] Before dump_init\n");
  fflush(stdout);

  dump_init();

  printf("[DEBUG] After dump_init\n");
  fflush(stdout);

  int numbers[] = {1, 2, 3, 4, 5};

  printf("[DEBUG] Before __cere__loop_region\n");
  fflush(stdout);

  int result = __cere__loop_region(numbers, 5);

  printf("[DEBUG] After __cere__loop_region, result=%d\n", result);
  fflush(stdout);

  printf("Result: %d\n", result);
  return 0;
}
