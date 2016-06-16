#include <omp.h>
#include <stdio.h>


void fct1(int k,int l[])
{

  int i;
  int p;   printf("Start\n");

#pragma omp parallel for
  for(i=0;i<4;i++)
  {  
    printf("LOOPA%d\n",i);
    p+=i;
  }


  for(i=0;i<4;i++)
  {  
    printf("LOOPB%d\n",i);
    p+=i;
  }

#pragma omp parallel for 
  for(i=0;i<4;i++)
  {  
    p+=i;printf("LOOPC%d thread__%d\n",i,omp_get_thread_num());
  }
  printf("End\n");
}


int main() 
{
  int k;
  int l[100];
  printf("Only in vivo\n");
  int i;
  for(i=0;i<3;i++)
    fct1(k,l); 
}
