#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <assert.h>
void anti_dead_code_elimination(int n, ...) {{}}
void sigcatch(int signal) {{
  fprintf(stderr, "Timeout...\n");
  exit(EXIT_FAILURE);
}}
static void sigsegv_handler(int sig, siginfo_t *si, void *unused)
{{
  char * touched_addr = si->si_addr;
  printf("Detected segfault at: %p\n", touched_addr);
  printf("Dumping process memory map\n");
  printf("==========================\n");
  char cmd[255];
  pid_t pid = getpid();
  snprintf(cmd, sizeof(cmd), "cat /proc/%d/maps", pid);
  system(cmd);
  printf("==========================\n");
  raise(SIGKILL);
}}
void real_main(int empty) {{
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
  {likwid}
  for(i=0; i<{in_vitro_call_count}; i++) {{
    alarm(max_seconds);
    run{loop}();
  }}
  exit(0);
}}
