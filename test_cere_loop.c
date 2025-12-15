#include <stdio.h>

int
__cere__loop_region(int* numbers, int size)
{
  int work = 0;
  for (int i = 0; i < size; ++i) {
    work += numbers[i] * (i + 1);
  }
  return work;
}

int
main()
{
  int numbers[] = {1, 2, 3, 4, 5};
  int result = __cere__loop_region(numbers, 5);
  printf("Result: %d\n", result);
  return 0;
}
