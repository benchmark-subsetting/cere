#include <stdio.h>
#include <stdlib.h>
#include <mm_malloc.h>
#define PAGESIZE 4096
#define round_to_page(addr) ((char *)(((long unsigned)(addr)) & ~(PAGESIZE-1)))
#define WIDTH 4096*32
#define CACHE_SIZE_MB 20

void cacheflush(void) {
    const size_t size = CACHE_SIZE_MB*1024*1024;
    int i, j;
    char *c = malloc(size);
    for (i = 0; i < 5; i++)
      for (j = 0; j < size; j++)
        c[j] = i*j;
}


int loop(int *a, int size)
{
  int i;
  int n = a[0];
  for (i = 1; i < 1000; i++) {
      //fprintf(stderr, "touched %p\n", round_to_page(&a[n]));
      n=a[n];
  }
  return n;
}

void init(int *a, int size)
{
  int i;
  srand(1);
  for (i= 0; i < size; i++) {
      a[i] = rand()%(size);
  }
}



int main(int argc, char **argv)
{
    int c = 0;
    int k;

    int * a = malloc(sizeof(int)*WIDTH);
    int * d = malloc(sizeof(int)*4096*10);
    int * b = malloc(sizeof(int)*WIDTH);

    init(a, WIDTH);
    init(b, WIDTH);

    cacheflush();

    printf("a: %p -> %p\n", &a[0], &a[WIDTH]);
    printf("b: %p -> %p\n", &b[0], &b[WIDTH]);

    fprintf(stderr, "ITERATION = 1\n");
    c += loop(a, WIDTH);

    fprintf(stderr, "ITERATION = 2\n");
    c += loop(a, WIDTH);

    fprintf(stderr, "ITERATION = 3\n");
    c += loop(a, WIDTH);

    fprintf(stderr, "ITERATION = 4\n");
    c += loop(b, WIDTH);
    printf("%d\n", c);

    return 0;
}
