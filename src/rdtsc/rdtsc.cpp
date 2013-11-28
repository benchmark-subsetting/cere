#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include "rdtsc.h"

void likwid_markerInit()
{}

void likwid_markerClose()
{
	std::map<std::string, region*>::iterator it;
	std::ofstream result("rdtsc_result.csv", std::ios::out);
	if(result) {
		for (it = htable.begin(); it != htable.end(); ++it)
		{
			result << *(it->second->name) << std::endl;
			result << "call count," << it->second->call_count << std::endl;
			result << "CPU_CLK_UNHALTED_CORE," << it->second->counter << std::endl;
		}
		result.close();
	}
	else
		std::cerr << "Unable to open result file !" << std::endl;
}

/*find the region name in the hash table
 * if does not exists, create it
 * else record start counter
*/
void rdtsc_markerStartRegion(char *reg) {
	std::string regionName(reg);
	region *r=NULL;
	if ((htable.find(regionName)) == htable.end())
	{
		if ((r = new region) == NULL)
		{
			std::cerr << "Unable to allocate new region >" << regionName << "<" << std::endl;
			exit(EXIT_FAILURE);
		}
		if ((r->name = new std::string(regionName)) == NULL)
		{
			std::cerr << "Unable to allocate new region >" << regionName << "<" << std::endl;
			exit(EXIT_FAILURE);
		}
		r->counter = 0;
		r->call_count = 0;
		htable[regionName] = r;
	}
	if(loopsName.empty()) loopsName.push(regionName); //stack empty, we are not in nested loops
	else {
		loopsName.push(loopsName.top()+"#"+regionName);
	}
	htable[regionName]->call_count += 1;
	htable[regionName]->start = rdtsc();
}

/*find the region name in the hash table
 * if does not exists, we have a problem
 * else increment counter by t1-t0
*/
void rdtsc_markerStopRegion(char *reg) {
	unsigned long long int stop = rdtsc();
	std::string regionName(reg);
	if ((htable.find(regionName)) == htable.end())
		std::cerr << "Unable to find the markerStart for region >" << regionName << "<" << std::endl;
	else {
		if(loopsName.top() != *(htable[regionName]->name)) {
			delete htable[regionName]->name;
			htable[regionName]->name = new std::string(loopsName.top());
		}
		loopsName.pop();
		htable[regionName]->counter += stop - htable[regionName]->start;
	}
}

//For fortran code
void rdtsc_markerstartregion_(char *regionName, int len)
{
	rdtsc_markerStartRegion( regionName );
}

void rdtsc_markerstopregion_(char *regionName, int len)
{
	rdtsc_markerStopRegion( regionName );
}

void likwid_markerinit_()
{
	likwid_markerInit();
}

void likwid_markerclose_()
{
	likwid_markerClose();
}


