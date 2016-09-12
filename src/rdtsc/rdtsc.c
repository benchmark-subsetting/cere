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
#include <string.h>
#include <ccan/hash/hash.h>
#include "rdtsc.h"


void calibrate_serialize_overhead()
{
    int count = 10000;
    int i;
    serialize();
    unsigned long long start = rdtsc();
    for (i = 0; i < count; i++)
      serialize();
    unsigned long long stop = rdtsc();
    serialize_overhead = (stop - start) / count;
}

void print_hash_table()
{
    struct htable_iter iter;
    region *p;
    for (p = htable_first(&regionHtab,&iter); p; p = htable_next(&regionHtab, &iter))
    {
        fprintf(stderr, "RDTSC: print_hash_table() %s\n", p->name);
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
    if(r->trace_results != NULL)
    {
        int i,j;
        if(r->invivo) j=0;
        else j=1;
        for (i = j; i < nbEltToDump; i++) {
            double counter = (double) r->trace_counter[i];
            double count = (double) r->global_call_count[i];
            fwrite((const void*)(&counter), sizeof(double), 1, r->trace_results);
            fwrite((const void*)(&count), sizeof(double), 1, r->trace_results);
        }
    }
}

void rdtsc_markerInit()
{
    calibrate_serialize_overhead();
    htable_init(&regionHtab, rehash, NULL);
    htable_init(&call_count_reminder, rehash2, NULL);
    INITIALIZED=true;
}

void rdtsc_markerClose()
{
    while(strlen(call_stack) > 0) {
        rdtsc_markerStopRegion(call_stack, 1);
    }
    struct htable_iter iter;
    if (htable_first(&regionHtab,&iter) == NULL) return;
    region *p=NULL;
    for (p = htable_first(&regionHtab,&iter); p; p = htable_next(&regionHtab, &iter))
    {
        char *fileName = malloc((strlen(p->name)+5)*sizeof(char));
        strcpy(fileName, p->name);
        strcat(fileName, ".csv");
        FILE *result = fopen(fileName, "w");
        free(fileName);
        if(result == NULL) {
            fprintf(stderr, "RDTSC: Cannot open result file for region %s.\n",p->name);
            exit(EXIT_FAILURE);
        }
        fprintf(result, "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE\n");
        //Remove 1 to call count in invitro mode
        if(!p->invivo && p->call_count > 1) p->call_count -= 1;
        fprintf(result, "%s,%u,%llu\n", p->name, p->call_count, p->counter);
        if(p->traced) {
            dump_trace(p, p->call_count%TRACE_SIZE);
            fclose(p->trace_results);
        }
        fclose(result);
    }
    htable_clear(&regionHtab);
}

/*find the region name in the hash table
 * if does not exists, create it
 * else record start counter
*/
void rdtsc_markerStartRegion(char *reg, bool vivo) {
    if(!INITIALIZED) return;

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
        r->invivo = vivo;
        char * ge = getenv("CERE_TRACE");
        if(ge) r->traced = 1;
        else r->traced = 0;
        r->counter = 0;
        r->call_count = 0;
        r->trace_results=NULL;
        htable_add(&regionHtab, hash_string(r->name), r);
    }

    r->call_count += 1;
    //If invitro, remove first measure
    if(!r->invivo && r->call_count==2)
        r->counter = 0;
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
        //Open binary trace file
        if(r->call_count==1) {
            char *fileName = malloc((strlen(r->name)+5)*sizeof(char));
            strcpy(fileName, r->name);
            strcat(fileName, ".bin");
            r->trace_results = fopen(fileName, "ab");
            free(fileName);
            if(r->trace_results == NULL) {
                perror("RDTSC: Cannot open Binary File!");
                exit(EXIT_FAILURE);
            }
        }
        t->val += 1;
        r->global_call_count[(r->call_count-1)%TRACE_SIZE] = t->val;
    }
    serialize();
    r->start = rdtsc();
}

void rdtsc_markerStopRegion(char *reg, bool vivo) {
    //serialize();
    unsigned long long int stop = rdtsc();
    unsigned long long int loop_cycles;

    if(!INITIALIZED) return;

    char* regionName = call_stack;
    /* We must check that reg is base name of regionName */
    region *r=NULL;
    if ((r = htable_get(&regionHtab, hash_string(regionName), streq, regionName)) == NULL)
        fprintf(stderr, "RDTSC: Unable to find the markerStart for region >%s<\n", regionName);
    else {
        loop_cycles = stop - r->start;

        /*Uncomment to remove the cost of a serialize()*/
        //if(loop_cycles > serialize_overhead) {
            //loop_cycles -= serialize_overhead;
        //} else {
            //loop_cycles = 0;
            //r->nb_invocation_skiped++;
        //}
        pop(call_stack);
        r->counter += loop_cycles;
        if(r->traced) {
            r->trace_counter[(r->call_count-1)%TRACE_SIZE] = loop_cycles;
            if(r->call_count%TRACE_SIZE == 0) {
                dump_trace(r, TRACE_SIZE);
            }
        }
    }
}

//For fortran code
void rdtsc_markerstartregion_(char *regionName, int len, bool vivo)
{
    rdtsc_markerStartRegion( regionName, vivo );
}

void rdtsc_markerstopregion_(char *regionName, int len, bool vivo)
{
    rdtsc_markerStopRegion( regionName, vivo );
}

void rdtsc_markerinit_()
{
    rdtsc_markerInit();
}

void rdtsc_markerclose_()
{
    rdtsc_markerClose();
}
