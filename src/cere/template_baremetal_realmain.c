#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>

static volatile int want_exit = 0;

void run{loop}(void);
void anti_dead_code_elimination(int n, ...) {{}}

static void sigsegv_handler(int sig, siginfo_t *si, void *unused)
{{
  // Sometimes atexit handlers which may have non captured elements
  // try to access non capture data. If we have already reached the
  // end of the replay, we can exit safely.
  if (want_exit) _exit(0);
  raise(SIGKILL);
}}
void real_main() {{
  int i;
  //kill program after 10 minutes
  int call_count = {in_vitro_call_count};
  for(i=0; i<call_count+1; i++) {{
    run{loop}();
  }}

  want_exit = 1;
  exit(0);
}}
