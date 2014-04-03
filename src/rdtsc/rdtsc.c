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

// Comparison function.
static bool streq2(const void *e, void *string)
{
	return strcmp(((global_region_call_count *)e)->name, string) == 0;
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

static size_t rehash2(const void *e, void *unused)
{
	return hash_string(((global_region_call_count *)e)->name);
}

void push(char* new_region, char* call_stack)
{
	unsigned size = strlen(call_stack);

	if (size == 0) {
		strcpy(call_stack, new_region);
	}
	else if (size+strlen(new_region)+2 < CALL_STACK_SIZE) {
		call_stack[size]='#';
		call_stack[size+1]='\0';
		strcat(call_stack, new_region);
	}
	else {
		fprintf(stderr, "RDTSC: Call stack overflow, sorry...\n");
		exit(EXIT_FAILURE);
	}
}

void pop(char* call_stack)
{
	int last_index = 0;
	char *pch = strchr(call_stack, '#');

	while(pch != NULL) {
		last_index = pch-call_stack;
		pch = strchr(pch+1, '#');
	}
	call_stack[last_index]='\0';
}

void dump_trace(region *r, int nbEltToDump)
{
	char *fileName = malloc((strlen(r->name)+5)*sizeof(char));
	strcpy(fileName, r->name);
	strcat(fileName, ".bin");
	FILE *binRes = fopen(fileName, "ab");
	if(binRes == NULL) {
		fprintf(stderr, "Cannot open Binary File!\n");
	}
	else {
		int i,j;
		if(r->invivo) j=0;
		else j=1;
		for (i = j; i < nbEltToDump; i++)
		{
			fwrite((const void*)(&r->trace_counter[i]), sizeof(unsigned long long int), 1, binRes);
			fwrite((const void*)(&r->global_call_count[i]), sizeof(unsigned int), 1, binRes);
		}
	}
}

void likwid_markerInit()
{
	htable_init(&regionHtab, rehash, NULL);
	htable_init(&call_count_reminder, rehash2, NULL);
	atexit(likwid_markerClose);
}

void likwid_markerClose()
{
	while(strlen(call_stack) > 0) {
		rdtsc_markerStopRegion(call_stack, 0);
	}
	struct htable_iter iter;
	if (htable_first(&regionHtab,&iter) == NULL) return;
	region *p=NULL;
	FILE *result=NULL;
	result = fopen("rdtsc_result.csv", "w");
	if(result == NULL) {
		fprintf(stderr, "RDTSC: Cannot open result file.\n");
		exit(EXIT_FAILURE);
	}
	else {
		fprintf(result, "Codelet Name,call count,CPU_CLK_UNHALTED_CORE\n");
		for (p = htable_first(&regionHtab,&iter); p; p = htable_next(&regionHtab, &iter))
		{
			//Remove 1 to call count in invitro mode
			if(!p->invivo) p->call_count -= 1;
			fprintf(result, "%s,%u,%llu\n", p->name, p->call_count, p->counter);
			if(p->traced) {
				dump_trace(p, p->call_count%TRACE_SIZE);
			}
		}
		fclose(result);
	}
	htable_clear(&regionHtab);
}

/*find the region name in the hash table
 * if does not exists, create it
 * else record start counter
*/
void rdtsc_markerStartRegion(char *reg, int trace) {
	push(reg, call_stack);
	char* regionName=call_stack;
	region *r=NULL;
	if ((r = htable_get(&regionHtab, hash_string(regionName), streq, regionName)) == NULL)
	{
		if ((r = malloc(sizeof(region))) == NULL)
		{
			fprintf(stderr, "RDTSC: Unable to allocate new region %s\n", regionName);
			exit(EXIT_FAILURE);
		}
		if ((r->name = malloc((strlen(regionName)+1)*sizeof(char))) == NULL)
		{
			fprintf(stderr, "RDTSC: Unable to allocate new region name %s\n", regionName);
			exit(EXIT_FAILURE);
		}
		strcpy(r->name, regionName);
		if(strstr(regionName, "__extracted__") != NULL) r->invivo = 0;
		r->traced = trace;
		r->counter = 0;
		r->call_count = 0;
		htable_add(&regionHtab, hash_string(r->name), r);
	}
	r->call_count += 1;
	if(r->traced) {
		global_region_call_count *t=NULL;
		if ((t = htable_get(&call_count_reminder, hash_string(reg), streq2, reg)) == NULL ) {
			if ((t = malloc(sizeof(global_region_call_count))) == NULL)
			{
				fprintf(stderr, "RDTSC: Unable to allocate new region %s\n", reg);
				exit(EXIT_FAILURE);
			}
			if ((t->name = malloc((strlen(reg)+1)*sizeof(char))) == NULL)
			{
				fprintf(stderr, "RDTSC: Unable to allocate new region name %s\n", reg);
				exit(EXIT_FAILURE);
			}
			strcpy(t->name, reg);
			t->val = 0;
			htable_add(&call_count_reminder, hash_string(reg), t);
		}
		t->val += 1;
		r->global_call_count[(r->call_count-1)%TRACE_SIZE] = t->val;
	}
	r->start = rdtsc();
}

void rdtsc_markerStopRegion(char *reg, int trace) {
	unsigned long long int stop = rdtsc();
	char* regionName = call_stack;
	/* We must check that reg is base name of regionName */
	region *r=NULL;
	if ((r = htable_get(&regionHtab, hash_string(regionName), streq, regionName)) == NULL)
		fprintf(stderr, "RDTSC: Unable to find the markerStart for region >%s<\n", regionName);
	else {
		pop(call_stack);
		//If invitro, remove first measure
		if(!r->invivo && r->call_count==1)
			r->counter += stop - r->start;
		if(r->traced) {
			r->trace_counter[(r->call_count-1)%TRACE_SIZE] = stop - r->start;
			if(r->call_count%TRACE_SIZE == 0) {
				dump_trace(r, TRACE_SIZE);
			}
		}
	}
}

//For fortran code
void rdtsc_markerstartregion_(char *regionName, int len, int trace)
{
	rdtsc_markerStartRegion( regionName, trace );
}

void rdtsc_markerstopregion_(char *regionName, int len, int trace)
{
	rdtsc_markerStopRegion( regionName, trace );
}

void likwid_markerinit_()
{
	likwid_markerInit();
}

void likwid_markerclose_()
{
	likwid_markerClose();
}


