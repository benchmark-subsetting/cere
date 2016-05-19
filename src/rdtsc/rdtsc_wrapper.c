/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2015, Universite de Versailles St-Quentin-en-Yvelines  *
 *                                                                           *
 * CERE is free software: you can redistribute it and/or modify it under     *
 * the terms of the GNU Lesser General Public License as published by        *
 * the Free Software Foundation, either version 3 of the License,            *
 * or (at your option) any later version.                                    *
 *                                                                           *
 * CERE is distributed in the hope that it will be useful,                   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 * GNU General Public License for more details.                              *
 *                                                                           *
 * You should have received a copy of the GNU General Public License         *
 * along with CERE.  If not, see <http://www.gnu.org/licenses/>.             *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

extern void rdtsc_markerInit();
extern void rdtsc_markerClose();
extern void rdtsc_markerStartRegion(char*, bool);
extern void rdtsc_markerStopRegion(char*, bool);

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
