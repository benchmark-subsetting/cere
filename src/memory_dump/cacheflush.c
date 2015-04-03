#include <stdlib.h>
#include <stdio.h>

char *cacheflush();

char *cacheflush() {
  const size_t size = 16 * 1024 * 1024;
  int i, j;
  char *c = malloc(size);
  for (i = 0; i < 5; i++)
    for (j = 0; j < size; j++)
      c[j] += i * j;

  return c;
}
