#include <stdlib.h>
int main() {
    const size_t size = 30*1024*1024;
    int i, j;
    char *c = malloc(size);
    for (i = 0; i < 10; i++)
      for (j = 0; j < size; j++)
        c[j] = i*j;
}
