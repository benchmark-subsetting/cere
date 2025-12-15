#include <stdio.h>

extern void dump_init(void);

int main() {
  printf("Before dump_init\n");
  fflush(stdout);

  dump_init();

  printf("After dump_init\n");
  fflush(stdout);

  return 0;
}
