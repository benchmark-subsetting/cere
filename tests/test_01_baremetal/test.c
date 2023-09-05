#include <stdio.h>
#include <stdlib.h>

int checksum(int *a, int size)
{
    int check=0, i;
    for (i = 0; i < size; i++) {
        check += a[i];
    }
    return check;
}

void init(int *a, int size)
{
    int i;
    for (i = 0; i < size; i++)
        a[i]=i+1;
}

int main(int argc, char **argv)
{
    int size = 4096;
    int  a[4096];
    init(a, size);
    int check = checksum(&(a[0]), size);
    return 0;
}
