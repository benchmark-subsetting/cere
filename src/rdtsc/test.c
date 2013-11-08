#include <stdio.h>
#include <stdlib.h>


int main()
{
	int i, j;
	int TAB[100];
	likwid_markerInit();

	for(i=0; i<10; i++)
	{
		rdtsc_markerStartRegion("test_1");
		fprintf(stderr, "coucou\n");
		rdtsc_markerStopRegion("test_1");
	}

	for(i=0; i<100; i++)
	{
		rdtsc_markerStartRegion("test_2");
		for(j=0; j<1000; j++)
		{
			TAB[i] = i*j;
		}
		rdtsc_markerStopRegion("test_2");
	}
	likwid_markerClose();
}
