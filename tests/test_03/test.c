#include <stdio.h>
#include <stdlib.h>

int a[512];

int checksum(int size)
{
    int check=0, i;
    for (i = 0; i < size; i++) {
        check += a[i];
        if (i == size-1)
            fprintf(stderr, "Checksum invitro: %d\n", check);
    }
    return check;
}

void init(int size)
{
    int i;
    for (i = 0; i < size; i++)
        a[i]=i+1;
}

int main(int argc, char **argv)
{
    int size=512;
    init(size);
    int check = checksum(size);
    fprintf(stderr, "Checksum : %d\n", check);
    return 0;
}
