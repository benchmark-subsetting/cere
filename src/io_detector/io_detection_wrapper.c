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
