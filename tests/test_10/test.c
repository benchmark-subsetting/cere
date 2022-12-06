#include <stdio.h>
#include <stdlib.h>

#define stderr_print(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)

static int value = 10;

void check(void)
{
  int i;
  for (i = 0; i < 10; i++) {
      stderr_print("value = %d\n", value);
  }
}

void init(void)
{
    value = 42;
}

int main(int argc, char **argv)
{
    init();
    check();
    return 0;
}
