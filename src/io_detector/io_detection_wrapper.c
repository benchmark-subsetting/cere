#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void cere_markerInit() {
    init_io_detection();
}

void cere_markerClose() {
    close_io_detection();
}

void cere_markerStartRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
    if ( requested_invoc == curr_invoc ) {
        start_io_detection();
    }
}

void cere_markerStopRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
    if ( requested_invoc == curr_invoc ) {
        stop_io_detection();
    }
}
