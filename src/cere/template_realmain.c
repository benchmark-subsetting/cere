#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>

static volatile int want_exit = 0;

void run{loop}(void);
void anti_dead_code_elimination(int n, ...) {{}}
void sigcatch(int signal) {{
  fprintf(stderr, "Timeout...\n");
  exit(EXIT_FAILURE);
}}

static void sigsegv_handler(int sig, siginfo_t *si, void *unused)
{{
  // Sometimes atexit handlers which may have non captured elements
  // try to access non capture data. If we have already reached the
  // end of the replay, we can exit safely.
  if (want_exit) _exit(0);

  char * touched_addr = si->si_addr;
  fprintf(stderr, "Detected segfault at: %p\n", touched_addr);
  fprintf(stderr, "Dumping process memory map\n");
  fprintf(stderr, "==========================\n");
  char cmd[255];
  pid_t pid = getpid();
  snprintf(cmd, sizeof(cmd), "cat /proc/%d/maps >&2", pid);
  system(cmd);
  fprintf(stderr, "==========================\n");
  raise(SIGKILL);
}}
void real_main() {{
  int i;
  //kill program after 10 minutes
  long long int max_seconds=10*{time_out};
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = sigsegv_handler;
  int res = sigaction(SIGSEGV, &sa, NULL);
  assert(res != -1);
  signal(SIGALRM, sigcatch);
  int call_count = {in_vitro_call_count};
  char * call_count_var = getenv("CERE_REPLAY_REPETITIONS");
  if (call_count_var != NULL) {{
    char * endptr;
    call_count = strtol(call_count_var, &endptr, 10);
    if (errno != 0 || call_count <= 0) {{
      fprintf(stderr, "Invalid value for CERE_REPLAY_REPETITIONS");
      exit(1);
    }}
  }}
  for(i=0; i<call_count+1; i++) {{
    alarm(max_seconds);
    run{loop}();
  }}

  want_exit = 1;
  exit(0);
}}
