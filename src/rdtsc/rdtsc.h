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
#include <ccan/htable/htable.h>
#define CALL_STACK_SIZE 8192
#define TRACE_SIZE 100000

static char call_stack[CALL_STACK_SIZE];
unsigned long long int serialize_overhead;

static struct htable regionHtab;
static struct htable call_count_reminder;

bool INITIALIZED=false;

typedef struct
{
	char *name;
	unsigned int val;
}global_region_call_count;

typedef struct
{
	char* name;
	int traced;
	int invivo;
	unsigned long long int serialize_overhead;
	unsigned long long int start;
	unsigned long long int counter;
	unsigned int call_count;
	unsigned long long int trace_counter[TRACE_SIZE];
	unsigned int global_call_count[TRACE_SIZE];
	FILE *trace_results;
} region;

__inline__ void serialize(void) {
#if defined (__amd64__)
    __asm__ volatile("cpuid"
            :::"%eax","%ebx","%ecx","%edx");  // clobbered registers
#elif defined (__aarch64__)
    __asm__ volatile("isb" : : : "memory");
#endif
}

__inline__ unsigned long long int rdtsc() {
    unsigned long long int a, d;

#if defined(__amd64__)
    __asm__ volatile ("rdtsc" : "=a" (a), "=d" (d));
    return (d<<32) | a;
#elif defined(__aarch64__)
    __asm__ volatile("mrs %0, cntvct_el0" : "=r" (a));
    return a;
#endif
}

static bool streq(const void*, void*);
static size_t rehash(const void*, void*);
void rdtsc_markerInit();
void rdtsc_markerClose();
void rdtsc_markerStartRegion(char*, bool);
void rdtsc_markerStopRegion(char*, bool);

void rdtsc_markerinit_();
void rdtsc_markerclose_();
void rdtsc_markerstartregion_(char*, int, bool);
void rdtsc_markerstopregion_(char*, int, bool);
