#include <string.h>
#include <err.h>
#include "dump.h"
#include "counters.h"

// Comparison function.
static bool 
streq(const void *e, void *string)
{
    return strcmp(((struct region_counter *)e)->name, string) == 0;
}

static uint32_t 
hash_string(const char *string)
{
    uint32_t ret;
    for (ret = 0; *string; string++)
        ret = (ret << 5) - ret + *string;
    return ret;
}

static size_t 
rehash(const void *e, void *unused)
{
    return hash_string(((struct region_counter *)e)->name);
}

void 
init_counters(void)
{
  htable_init(&state.counters, rehash, NULL);
}

struct region_counter * 
get_region(char * loop_name)
{
  struct region_counter *r=NULL;
  
  /* Try to get region */
  r = htable_get(&state.counters, hash_string(loop_name), streq, loop_name);

  /* If it exists return it */
  if (r) 
    return r;

  /* If it does not, we must create one */

  r = malloc(sizeof(*r));
  if (!r)
    errx(EXIT_FAILURE, "dump: Unable to allocate new region %s\n", loop_name);

  r->name = strdup(loop_name);
  if (!r)
    errx(EXIT_FAILURE, "dump: Unable to allocate new region %s\n", loop_name);
  
  /* Initialize call count to zero and add entry to hash table */
  r->call_count = 0;
  htable_add(&state.counters, hash_string(r->name), r);
  return r;
}

