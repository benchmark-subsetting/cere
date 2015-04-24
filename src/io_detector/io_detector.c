//******************************io-detector.c*********************************//
// This library generates for each region an IO trace using strace. These     //
// traces are then parsed to detect IOs. Regions doing IOs are not replayable //
//****************************************************************************//

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include "io_detector.h"


struct detector_state state;

void init_io_detection() {
  state.trace_file = strdup("io_detection_trace");
}

void start_io_detection() {
  char command[BUF_SIZE];

  // Remove previous IOs traces.
  if (unlink(state.trace_file) < 0) {
    if (errno != 2) perror("Iocheck: Cannot delete previous trace\n");
  }

  pid_t pid = fork();
  if(pid < 0) perror("Iocheck: fork failed\n");
  if (pid != 0) {
    snprintf(command, BUF_SIZE, "%d", pid);

    char *args[] = {
      "/usr/bin/strace",
      "-p",
      command,
      "-e",
      "trace=read,write",
      "-o",
      state.trace_file,
      NULL
    };
    // Run strace
    execvp(args[0], args);
    errx(EXIT_FAILURE, "Iocheck: could not execute strace\n");
  }

  // we are in the child process, let's wait for strace to start tracing before
  // pursuing the application execution
  state.pid = getppid();
  sleep(0.1);
  int f;
  while((f = open(state.trace_file, O_RDONLY )) == -1) sleep(0.1);
  close(f);
}

void stop_io_detection() {
  //Kill strace
  if (kill(state.pid, SIGTERM) < 0) perror("Iocheck: kill strace failed\n");
  exit(EXIT_SUCCESS);
}

// We should never get to this point when tracing a single region as we exit
// after the stop probe. But with multiple trace, the application will be
// executed untill the end.
void close_io_detection() {
  exit(EXIT_SUCCESS);
}
