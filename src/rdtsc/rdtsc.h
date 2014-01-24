#include <stack>
#include <map>
#include <string>

typedef struct
{
	//~ std::string *name;
	unsigned long long int start;
	unsigned long long int counter;
	unsigned int call_count;
	bool traced;
	std::vector<unsigned long long int> cycles;
} region;

__inline__ unsigned long long int rdtsc() {
	unsigned long long int a, d;
	__asm__ volatile ("rdtsc" : "=a" (a), "=d" (d));
	return (d<<32) | a;
}

static std::map<std::string, region*> htable;
static std::stack<std::string> loopsName;

#ifdef __cplusplus
extern "C" {
#endif
void likwid_markerInit();
void likwid_markerClose();
void rdtsc_markerStartRegion(const char *, bool);
void rdtsc_markerStopRegion(const char *, bool trace=false);

void likwid_markerinit_();
void likwid_markerclose_();
void rdtsc_markerstartregion_(const char *, int, bool);
void rdtsc_markerstopregion_(const char *, int, bool trace=false);
#ifdef __cplusplus
} //extern "C"
#endif
