#include <stdlib.h>
#include <stdio.h>

void dump();
void* load();

double b[5000];
char c[11] = "hello world";

void initialize(int ** a, int n) {
    int i;
    for (i=0; i<n; i++) {
        a[0][i] = -i*i;
    }
}

int codelet(int **a, int n) {
    int sum = 0;
    int i;
    for (i=0; i<n; i++) {
        sum += a[0][i];
    }

    return sum;
}

int main(int argc, char* argv[]) {

    int mode = 0;
    int n = 1024;
    int** a = (int**) malloc(1*sizeof(int*));
    a[0] = (int*) malloc(n*sizeof(int));
    dump_init();

    if (argc != 2) {
        printf("usage: %s 0/1/2 # 0 (normal) 1 (dump) 2 (replay)\n", argv[0]); 
        exit(1);
    } else {
        mode = atoi(argv[1]);
    }

    
    initialize(a, n);

    if (mode == 1) {
        // Dump mode
        dump("mainloop", 1, 2, a, &n);
    } 

    if (mode == 2) {
        // Replay mode
        void * args[2];
        load("mainloop", 1, 2, args);
        a = args[0];
        n = *((int*)args[1]);
    }

    int sum = codelet(a, n);
    printf("Checksum = %d\n", sum);
    return 0;
}
