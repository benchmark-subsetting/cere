#include "pages.h"
int warm(char * addr)
{
  int a = 0;
  for (int i = 0; i < PAGESIZE; i++) {
      a += addr[i];
  }
  return a;
}

void antideadcode(void * n)
{
}
