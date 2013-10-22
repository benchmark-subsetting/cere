#include <stdio.h>

int checksum(int *a, int size)
{
    int check=0, i;
    for (i = 0; i < size; i++)
        check += a[i];
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
    int size=512;
    int a[size];
    init(a, size);
    int check = checksum(a, size);
    fprintf(stderr, "Checksum : %d\n", check);
    return 0;
}
