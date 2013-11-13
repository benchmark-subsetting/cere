#include <stdio.h>
#include <string.h>
#include "rdtsc.h"

void print_hash_table()
{
	struct htable_iter iter;
	region *p;
	for (p = htable_first(&regionHtab,&iter); p; p = htable_next(&regionHtab, &iter))
	{
		fprintf(stderr, "%s\n", p->name);
	}
}

// Comparison function.
static bool streq(const void *e, void *string)
{
	return strcmp(((region *)e)->name, string) == 0;
}

static uint32_t hash_string(const char *string)
{
	uint32_t ret;
	for (ret = 0; *string; string++)
		ret = (ret << 5) - ret + *string;
	return ret;
}

static size_t rehash(const void *e, void *unused)
{
	return hash_string(((region *)e)->name);
}

void likwid_markerInit()
{
	htable_init(&regionHtab, rehash, NULL);
}

void likwid_markerClose()
{
	struct htable_iter iter;
	region *p=NULL;
	FILE *result=NULL;
	result = fopen("rdtsc_result.csv", "w");
	for (p = htable_first(&regionHtab,&iter); p; p = htable_next(&regionHtab, &iter))
	{
		fprintf(result, "%s\n", p->name);
		fprintf(result, "call count,%u\n", p->call_count);
		fprintf(result, "CPU_CLK_UNHALTED_CORE,%llu\n", p->counter);
	}
	fclose(result);
	htable_clear(&regionHtab);
}

/*find the region name in the hash table
 * if does not exists, create it
 * else record start counter
*/
void rdtsc_markerStartRegion(char *regionName) {
	region *r=NULL;
	if ((r = htable_get(&regionHtab, hash_string(regionName), streq, regionName)) == NULL)
	{
		if ((r = malloc(sizeof(region))) == NULL)
		{
			fprintf(stderr, "Unable to allocate new region %s\n", regionName);
			exit(EXIT_FAILURE);
		}
		if ((r->name = malloc((strlen(regionName)+1)*sizeof(char))) == NULL)
		{
			fprintf(stderr, "Unable to allocate new region %s\n", regionName);
			exit(EXIT_FAILURE);
		}
		strcpy(r->name, regionName);
		r->counter = 0;
		r->call_count = 0;
		htable_add(&regionHtab, hash_string(r->name), r);
	}
	r->call_count += 1;
	r->start = rdtsc();
}

/*find the region name in the hash table
 * if does not exists, we have a problem
 * else increment counter by t1-t0
*/
void rdtsc_markerStopRegion(char *regionName) {
	unsigned long long int stop = rdtsc();
	region *r=NULL;
	if ((r = htable_get(&regionHtab, hash_string(regionName), streq, regionName)) == NULL)
		fprintf(stderr, "Unable to find the markerStart for region >%s<\n", regionName);
	else r->counter += stop - r->start;
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


