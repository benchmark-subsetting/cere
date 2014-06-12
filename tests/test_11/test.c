#include <stdio.h>
#include <stdlib.h>
  

static int value = 10;

void check(void)
{
  int i;
  for (i = 0; i < 10; i++) {
      printf("value = %d\n", value); 
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
