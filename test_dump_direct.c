#include <stdio.h>

// Déclarations des fonctions de libcere_dump
extern void dump_init(const char* func_name);
extern void dump(char* loop_name, char* invocations_str, int arg_count, ...);
extern void after_dump(char* loop_name);

int main() {
    fprintf(stderr, "[TEST] Starting test\n");
    fflush(stderr);

    fprintf(stderr, "[TEST] Calling dump_init\n");
    fflush(stderr);
    dump_init("main");

    fprintf(stderr, "[TEST] After dump_init, calling dump\n");
    fflush(stderr);

    int data[] = {1, 2, 3, 4, 5};
    dump("__cere__test_region", "1", 1, data);

    fprintf(stderr, "[TEST] After dump, calling after_dump\n");
    fflush(stderr);
    after_dump("__cere__test_region");

    fprintf(stderr, "[TEST] Test completed\n");
    fflush(stderr);
    return 0;
}
