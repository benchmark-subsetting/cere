#include "htable/htable/htable.h"

unsigned long long int t0;
unsigned long long int t1;
static struct htable regionHtab;

typedef struct
{
	char* name;
	unsigned long long int start;
	unsigned long long int counter;
	unsigned int call_count;
} region;

__inline__ unsigned long long int rdtsc() {
	unsigned long long int a, d;
	__asm__ volatile ("rdtsc" : "=a" (a), "=d" (d));
	return (d<<32) | a;
}

static uint32_t hash_string(const char*);

static bool streq(const void*, void*);
static size_t rehash(const void*, void*);
void likwid_markerInit();
void likwid_markerClose();
void rdtsc_markerStartRegion(char*);
void rdtsc_markerStopRegion(char*);

void likwid_markerinit_();
void likwid_markerclose_();
void rdtsc_markerstartregion_(char*, int);
void rdtsc_markerstopregion_(char*, int);
