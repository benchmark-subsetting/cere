#include "../ccan/ccan/htable/htable.h"
#define CALL_STACK_SIZE 4096
#define TRACE_SIZE 100000

static char call_stack[CALL_STACK_SIZE];
unsigned long long int serialize_overhead;

static struct htable regionHtab;
static struct htable call_count_reminder;

int LEVEL;
bool GLOBAL;
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
    __asm__("cpuid"
            :::"%eax","%ebx","%ecx","%edx");  // clobbered registers
}

__inline__ unsigned long long int rdtsc() {
	unsigned long long int a, d;
	__asm__ volatile ("rdtsc" : "=a" (a), "=d" (d));
	return (d<<32) | a;
}

static uint32_t hash_string(const char*);

static bool streq(const void*, void*);
static size_t rehash(const void*, void*);
void likwid_markerInit(bool);
void likwid_markerClose();
void rdtsc_markerStartRegion(char*, int);
void rdtsc_markerStopRegion(char*, int);

void likwid_markerinit_(bool);
void likwid_markerclose_();
void rdtsc_markerstartregion_(char*, int, int);
void rdtsc_markerstopregion_(char*, int, int);
