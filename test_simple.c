#include <stdio.h>

int
__cere__compute(int a, int b)
{
  return a + b;
}

int
main()
{
  int result = __cere__compute(5, 3);
  printf("Result: %d\n", result);
  return 0;
}
