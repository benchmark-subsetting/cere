#include <stdlib.h>
#include <stdio.h>

void dump();
void* load();

void initialize(int * a, int n) {
    int i;
    for (i=0; i<n; i++) {
        a[i] = i;
    }
}

int codelet(int *a, int n) {
    int sum = 0;
    int i;
    for (i=0; i<n; i++) {
        sum += a[i];
    }

    return sum;
}

int main(int argc, char* argv[]) {

    int mode = 0;
    int n = 1024;
    int* a = (int*) malloc(n*sizeof(int));

    if (argc != 2) {
        printf("usage: %s 0/1/2 # 0 (normal) 1 (dump) 2 (replay)\n", argv[0]); 
        exit(1);
    } else {
        mode = atoi(argv[1]);
    }

    
    initialize(a, n);

    if (mode == 1) {
        // Dump mode
        void * addresses[] = {(void*) a, (void*) &n};
        dump(2, addresses);
    } 

    if (mode == 2) {
        // Replay mode
        void * addresses[2];
        load(2, addresses);
        a = addresses[0];
        n = *((int*)addresses[1]);
    }

    int sum = codelet(a, n);
    printf("Checksum = %d\n", sum);
    return 0;
}
