#include <stdio.h>

void my_start(char* loopName)
{
    fprintf(stderr, "Start Instrumentation >%s<\n", loopName);
}


void my_stop(char* loopName)
{
    fprintf(stderr, "Stop Instrumentation >%s<\n", loopName);
}
