#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void cere_markerInit() {
    rdtsc_markerInit();
}

void cere_markerClose() {
    rdtsc_markerClose();
}

void cere_markerStartRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
    if ( requested_invoc == curr_invoc ) {
        rdtsc_markerStartRegion(regName, vivo);
    }
}

void cere_markerStopRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
    if ( requested_invoc == curr_invoc ) {
        rdtsc_markerStopRegion(regName, vivo);
    }
}
