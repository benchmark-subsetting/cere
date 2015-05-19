#include <linux/limits.h>
#include "../ccan/ccan/htable/htable.h"

#define BUF_SIZE 64

static struct htable regionHtab;
pid_t tracing = 0;
unsigned max_str_size = 0;

void init_regions(char*);
void init_io_detection(char*);
void start_io_detection(char*);
void stop_io_detection(char*);
void close_io_detection();

static uint32_t hash_string(const char*);

static bool streq(const void*, void*);
static size_t rehash(const void*, void*);

typedef struct
{
  char *name;
  int nb_invoc;
  int current_invocation;
  int *requested_invocations;
  pid_t pid;
  bool strace;
} region;
