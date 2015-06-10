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
//#include "rdtsc.h"

int fake_call2(){
	return rand();
}

void fake_call()
{
	int j, k=0;
	rdtsc_markerStartRegion("fake_call", 1, 0);
	for(j=0; j<100; j++)
	{
		k += fake_call2();
	}
	rdtsc_markerStopRegion("fake_call", 1, 0);
}

int main()
{
	int i, j;
	int TAB[100];
	rdtsc_markerInit();

	rdtsc_markerStartRegion("test_1", 0, 1);
	for(i=0; i<10; i++)
	{
		fake_call();
	}
	rdtsc_markerStopRegion("test_1", 0, 1);

	//~ for(i=0; i<10; i++)
	//~ {
		//~ rdtsc_markerStartRegion("test_2", 1);
		//~ for(j=0; j<10; j++)
		//~ {
			//~ TAB[i] = i*j;
			//~ fake_call();
		//~ }
		//~ rdtsc_markerStopRegion("test_2", 1);
	//~ }

	return 0;
}
