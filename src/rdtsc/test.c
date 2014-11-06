#include <stdio.h>
#include <stdlib.h>
#include "rdtsc.h"

int fake_call2(){
	return rand();
}

void fake_call()
{
	int j, k=0;
	rdtsc_markerStartRegion("fake_call", 1);
	for(j=0; j<100; j++)
	{
		k += fake_call2();
	}
	rdtsc_markerStopRegion("fake_call", 1);
}

int main()
{
	int i, j;
	int TAB[100];
	rdtsc_markerInit();

	rdtsc_markerStartRegion("test_1", 1);
	for(i=0; i<10; i++)
	{
		fake_call();
	}
	rdtsc_markerStopRegion("test_1", 1);

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
