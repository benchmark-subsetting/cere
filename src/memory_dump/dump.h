#include "../htable/htable/htable.h"

static struct htable regionHtab;

typedef struct
{
	char* name;
	unsigned int call_count;
} region;

static uint32_t hash_string(const char*); 

static bool streq(const void*, void*);
static size_t rehash(const void*, void*);

void dump_init();
void dump_close();
void dump_region(int, off64_t, off64_t);
void dump_mem(pid_t, int, void* []);
//~ void write_bin_file(char*, char*, char*, void*);
void dump(char*, int, int, ...);
void load(char*, int, int, void* []);
