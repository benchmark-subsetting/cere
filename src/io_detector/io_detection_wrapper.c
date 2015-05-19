#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void cere_markerInit(char* filename) {
    init_io_detection(filename);
}

void cere_markerClose() {
    close_io_detection();
}

void cere_markerStartRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
    start_io_detection(regName);
}

void cere_markerStopRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
    stop_io_detection(regName);
}
