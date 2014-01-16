#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include "rdtsc.h"

void likwid_markerInit()
{
	atexit(likwid_markerClose);
}

void likwid_markerClose()
{
	//Ensure there is not regions still open
	while(!loopsName.empty())
		rdtsc_markerStopRegion(loopsName.top().c_str());
	//Dump to file
	std::map<std::string, region*>::iterator it;
	std::ofstream result("rdtsc_result.csv", std::ios::out);
	if(result) {
		result << "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE" << std::endl;
		//We substract one to call count as we removed the first execution measure
		for (it = htable.begin(); it != htable.end(); ++it)
		{
			if(it->second->call_count > 1 && (it->second->name->find("extracted") != std::string::npos))
				it->second->call_count -= 1;
			result << *(it->second->name) << "," << it->second->call_count << "," << it->second->counter;
			for(std::vector<unsigned long long int>::iterator j=it->second->cycles.begin(); j!=it->second->cycles.end(); j++) {
				result << "," << *j;
			}
			result << std::endl;
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
void rdtsc_markerStartRegion(const char *reg, bool trace) {
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
		r->traced = trace;
		htable[regionName] = r;
	}
	//In in-vitro mode, remove the first measure of a region
	if(htable[regionName]->call_count == 1 && (htable[regionName]->name->find("extracted") != std::string::npos)) {
		if (htable[regionName]->traced) htable[regionName]->cycles.pop_back();
		htable[regionName]->counter = 0;
	}
	htable[regionName]->call_count += 1;
	htable[regionName]->start = rdtsc();
}

/*find the region name in the hash table
 * if does not exists, we have a problem
 * else increment counter by t1-t0
*/
void rdtsc_markerStopRegion(const char *reg, bool trace) {
	unsigned long long int stop = rdtsc();
	std::string baseName(reg);
	std::string regionName = loopsName.top();
	/* XXX we should check that baseName is a suffix of regionName */

	if ((htable.find(regionName)) == htable.end())
		std::cerr << "Unable to find the markerStart for region >" << regionName << "<" << std::endl;
	else {
		loopsName.pop();
		if (htable[regionName]->traced) htable[regionName]->cycles.push_back(stop - htable[regionName]->start);
		htable[regionName]->counter += stop - htable[regionName]->start;
	}
}

//For fortran code
void rdtsc_markerstartregion_(const char *regionName, int len, bool trace)
{
	rdtsc_markerStartRegion( regionName, trace );
}

void rdtsc_markerstopregion_(const char *regionName, int len, bool trace)
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


