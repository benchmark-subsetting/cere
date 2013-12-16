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
		result << "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE" << std::endl;
		//We substract one to call count as we removed the first execution measure
		for (it = htable.begin(); it != htable.end(); ++it)
		{
			if(it->second->call_count > 1 && (it->second->name->find("extracted") != std::string::npos))
				it->second->call_count -= 1;
			result << *(it->second->name) << "," << it->second->call_count << "," << it->second->counter << std::endl;
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
	std::string baseName(reg);
	std::string regionName;
	region *r=NULL;
	if(loopsName.empty()) regionName = baseName;
	else regionName = loopsName.top()+"#"+baseName;
	loopsName.push(regionName);

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
	//In in-vitro mode, remove the first measure of a region
	if(htable[regionName]->call_count == 1 && (htable[regionName]->name->find("extracted") != std::string::npos))
		htable[regionName]->counter = 0;
	htable[regionName]->call_count += 1;
	htable[regionName]->start = rdtsc();
}

/*find the region name in the hash table
 * if does not exists, we have a problem
 * else increment counter by t1-t0
*/
void rdtsc_markerStopRegion(char *reg) {
	unsigned long long int stop = rdtsc();
	std::string baseName(reg);
	std::string regionName = loopsName.top();
	/* XXX we should check that baseName is a suffix of regionName */

	if ((htable.find(regionName)) == htable.end())
		std::cerr << "Unable to find the markerStart for region >" << regionName << "<" << std::endl;
	else {
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


