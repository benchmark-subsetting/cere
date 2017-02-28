/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016-2017, Universite de Versailles St-Quentin-en-Yvelines  *
 *                                                                           *
 * CERE is free software: you can redistribute it and/or modify it under     *
 * the terms of the GNU Lesser General Public License as published by        *
 * the Free Software Foundation, either version 3 of the License,            *
 * or (at your option) any later version.                                    *
 *                                                                           *
 * CERE is distributed in the hope that it will be useful,                   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 * GNU General Public License for more details.                              *
 *                                                                           *
 * You should have received a copy of the GNU General Public License         *
 * along with CERE.  If not, see <http://www.gnu.org/licenses/>.             *
 *****************************************************************************/

#include <../ccan/ccan/htable/htable.h>
#include <assert.h>
#include <bits/wordsize.h>
#include <dirent.h>
#include <err.h>
#include <errno.h>
#include <unistd.h>
#include <sched.h>
#include <signal.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ptrace.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>

#include "ptrace.h"
#include "types.h"

#define WORDSIZE (sizeof(register_t))

#define _DEBUG 1
#undef _DEBUG

#include "debug.h"

size_t ntids = 0;
pid_t tids[MAX_TIDS] = {0};
int npending = 0;
bool pending[MAX_TIDS] = {false};
int cleared[MAX_TIDS] = {0};
event_t queued[MAX_TIDS];

void ptrace_cont(pid_t pid) {
  if (ptrace(PTRACE_CONT, pid, NULL, NULL) == -1) {
    errx(EXIT_FAILURE, "ptrace_cont : %s\n", strerror(errno));
  }
  debug_print("ptrace_cont %d ...\n\n", pid);
}

void ptrace_syscall(pid_t pid) {
  if (ptrace(PTRACE_SYSCALL, pid, NULL, NULL) != 0) {
    errx(EXIT_FAILURE, "Failed ptrace_syscall %d: %s\n", pid, strerror(errno));
  }
  debug_print("ptrace_syscall %d ...\n\n", pid);
}


#if defined(__amd64__)
void ptrace_setregs(pid_t pid, struct user_regs_struct *regs) {
  int r = 0;
  do {
    r = ptrace(PTRACE_SETREGS, pid, NULL, regs);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO ||
                        errno == ESRCH));
}

void ptrace_getregs(pid_t pid, struct user_regs_struct *regs) {
  int r = 0;
  do {
    r = ptrace(PTRACE_GETREGS, pid, NULL, regs);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO ||
                        errno == ESRCH));
}
#elif defined(__aarch64__)
#define NT_PRSTATUS 1

void ptrace_getregs(pid_t pid, struct user_pt_regs *regs) {
  int r = 0;
  *regs = (struct user_pt_regs) {};
  struct iovec iovec;
  iovec.iov_base = regs;
  iovec.iov_len = sizeof *regs;
  do {
    r = ptrace(PTRACE_GETREGSET, pid, NT_PRSTATUS, &iovec);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO ||
                        errno == ESRCH));
}

void ptrace_setregs(pid_t pid, struct user_pt_regs *regs) {
  int r = 0;
  struct iovec iovec;
	iovec.iov_base = regs;
	iovec.iov_len = sizeof *regs;
  do {
    r = ptrace(PTRACE_SETREGSET, pid, NT_PRSTATUS, &iovec);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO ||
                        errno == ESRCH));
}
#endif

void ptrace_getdata(pid_t child, long addr, char *str, int len) {
  union u {
    long val;
    char chars[WORDSIZE];
  } data;

  char *laddr;
  int i, j;
  i = 0;
  j = len / WORDSIZE;
  laddr = str;

  while (i < j) {
    int save_errno = errno;
    data.val = ptrace(PTRACE_PEEKDATA, child, addr + i * WORDSIZE, NULL);
    if (data.val == -1 && save_errno != errno)
      errx(EXIT_FAILURE, "Error get data : %s\n", strerror(errno));
    memcpy(laddr, data.chars, WORDSIZE);
    ++i;
    laddr += WORDSIZE;
  }
  j = len % WORDSIZE;
  if (j != 0) {
    data.val = ptrace(PTRACE_PEEKDATA, child, addr + i * WORDSIZE, NULL);
    memcpy(laddr, data.chars, j);
  }
  str[len] = '\0';
}


void ptrace_putdata(pid_t child, long addr, char *str, int len) {
  union u {
    long val;
    char chars[WORDSIZE];
  } data;

  char *laddr;
  int i, j;
  i = 0;
  j = len / WORDSIZE;
  laddr = str;
  while (i < j) {
    memcpy(data.chars, laddr, WORDSIZE);
    ptrace(PTRACE_POKEDATA, child, addr + i * WORDSIZE, data.val);
    ++i;
    laddr += WORDSIZE;
  }
  j = len % WORDSIZE;
  if (j != 0) {
    memcpy(data.chars, laddr, j);
    if (ptrace(PTRACE_POKEDATA, child, addr + i * WORDSIZE, data.val) == -1)
      errx(EXIT_FAILURE, "Error put data : %s\n", strerror(errno));
  }
}

static void register_thread(pid_t new_thread) {
  debug_print("Following new thread %d.\n", new_thread);
  if (ntids >= MAX_TIDS) {
    errx(EXIT_FAILURE, "Too many threads MAX_TIDS=%d\n", MAX_TIDS);
  }
  tids[ntids++] = new_thread;
}

static int find_thread_pos(pid_t tid) {
  int i;
  for (i=0; i < ntids; i ++) {
    if (tids[i] == tid) return i;
  }
  return -1;
}

void continue_all(void) {
  for (int i=0; i< ntids; i++) {
    if (!pending[i])
      ptrace_syscall(tids[i]);
  }
}

void stop_all_except(pid_t pid) {
  for (int i=0; i<ntids; i++) {
    if (tids[i] == pid) continue;
    debug_print("SEND SIGSTOP to %d\n", tids[i]);
    int r = syscall(SYS_tkill, tids[i], SIGSTOP);
    if (r != 0) {
      errx(EXIT_FAILURE, "tkill failed : %s\n", strerror(errno));
    }
  }
  for (int i=0; i<ntids; i++) {
    if (tids[i] == pid) continue;
    debug_print("WAITING for SIGSTOP from %d\n", tids[i]);
    event_t e = wait_event(tids[i]);
    if (e.signo != SIGSTOP) {
      npending += 1;
      pending[i] = true;
      cleared[i] += 1;
      queued[i] = e;
    }
  }
}

event_t wait_event(pid_t wait_for) {
  int r, status = 0;
  event_t event;

  debug_print("wait_event: %d -> ", wait_for);

  /* First check if there are any pending events */
  for (int i=0; npending > 0 && i < ntids ; i++) {
    if (wait_for != -1 && tids[i] != wait_for) continue;
    if (pending[i]) {
      pending[i] = false;
      npending -= 1;
      debug_print("return queued signal : %d for %d  -> ", queued[i].signo, queued[i].tid);
      return queued[i];
    }
  }

  do {
    event.tid = waitpid(wait_for, &status, __WALL);
    sched_yield();
  } while (event.tid == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH ||
                          errno == EIO));

  if (WIFEXITED(status)) {
    debug_print("tid %d exited\n", event.tid);
    for (int i=0; i < ntids; i++) {
      if (tids[i] == event.tid) {
        ntids--;
        if (ntids == 0) {
          debug_print("%s\n", "Finished capture\n");
          exit(1);
        }
        else {
          tids[i] = tids[ntids];
          queued[i] = queued[ntids];
          if (pending[i]) npending -=1;
          pending[i] = pending[ntids];
          cleared[i] = cleared[ntids];
          if (wait_for == event.tid) {
            errx(EXIT_FAILURE,
                 "wait_event waiting on an exited process %d\n", event.tid);
          }
          else {
            return wait_event(wait_for);
          }
        }
      }
    }
    errx(EXIT_FAILURE, "Non tracked tid %d exited", event.tid);
  }

  if (WIFSIGNALED(status))
    raise(WTERMSIG(status));

  /* If the process is not stopped, we cannot do much... */
  if (!WIFSTOPPED(status))
    errx(EXIT_FAILURE, "%d is not stopped\n", event.tid);

  event.signo = WSTOPSIG(status) & ~0x80;

  debug_print("%d received signal : %d %d\n", event.tid, event.signo);
  switch(event.signo) {
  case SIGSEGV:
    /* In case of SEGV we want to retrieve the si_addr */
    { siginfo_t sig;
      if (ptrace(PTRACE_GETSIGINFO, event.tid, (void *)0, &sig) == -1) {
        errx(EXIT_FAILURE, "ptrace(PTRACE_GETSIGINFO) : %s\n", strerror(errno));
      }
      event.sigaddr = sig.si_addr;
    }
    return event;
  case SIGTRAP:
    if (status>>8 == (SIGTRAP | (PTRACE_EVENT_CLONE<<8))) {
      debug_print("%s\n", "PTRACE_EVENT_CLONE received");

      pid_t new_thread;
      if (ptrace(PTRACE_GETEVENTMSG, event.tid, NULL, &new_thread) == -1) {
        errx(EXIT_FAILURE, "Failed to get get tid of "
             "newly cloned thread: %s\n", strerror(errno));
      }

      /* Check the thread is already registered (happens if the new child
         SIGSTOP is received before the parent SIGTRAP) */
      if (find_thread_pos(new_thread) == -1) {
        register_thread(new_thread);
	/* New thread starts with a SIGSTOP, either it was already caught before
           parent SIGTRAP in that case thread is already registered, either we
           should catch it now.  */
	event_t e = wait_event(new_thread);
	assert(e.signo == SIGSTOP);
      }

      /* Let both parent and child thread continue */
      ptrace_syscall(new_thread);
      ptrace_syscall(event.tid);

      /* Continue waiting */
      return wait_event(wait_for);
    }
    return event;
  case SIGSTOP:
    {

    /* A SIGSTOP here is either the SIGSTOP after stopping a thread,
       the first thread starting
       or a spawned thread starting before receiving its
       parent SIGTRAP */
    int i = find_thread_pos(event.tid);

    /* If it is a new unregistered thread, register it ! */
    if (i == -1) {
      register_thread(event.tid);
      return event;
    }
    else if (cleared[i] > 0) {
      /* This can happen if a tid is already stopped when we
         call stop_all_except. The SIGSTOP is queued and must be
         cleared */
      cleared[i] -= 1;
      debug_print("%s\n", "Cleared queued SIGSTOP from tracee");
      ptrace_syscall(event.tid);
      return wait_event(wait_for);
    } else {
      return event;
    }
    }
  case SIGABRT:
    debug_print("%s\n", "Finished capture\n");
    exit(0);
  default:
    /* Return all other events */
    return event;
  }
}

static void parse_proc_task(pid_t pid) {
  DIR *proc_dir;
  char dirname[BUFSIZ];
  struct dirent *entry;

  snprintf(dirname, sizeof dirname, "/proc/%d/task", pid);
  if (!(proc_dir = opendir(dirname))) {
    errx(EXIT_FAILURE, "Error opening "
         "/proc/%d/task : %s\n", pid, strerror(errno));
  }

  while ((entry = readdir(proc_dir)) != NULL) {
    // ignore . and ..
    if(entry->d_name[0] == '.')
      continue;

    int tid = atoi(entry->d_name);
    if (find_thread_pos(tid) == -1) {
      register_thread(tid);
    }
    debug_print("read tid = %d\n", tid);
  }
  closedir(proc_dir);
}

void follow_threads(pid_t pid) {

  parse_proc_task(pid);

  /* Attach all tids except main one which requested TRACEME in dump.c */
  for (int t = 0; t < ntids; t++) {
    int r;
    if (tids[t] != pid) {
      do {
        r = ptrace(PTRACE_ATTACH, tids[t], (void *)0, 0);
      } while (r == -1L && (errno == EBUSY || errno == EIO || errno == EFAULT ||
                            errno == ESRCH));
      if (r == -1L) {
        errx(EXIT_FAILURE, "Cannot attach thread %d: %s\n",
             tids[t], strerror(errno));
      }

      /* Wait for SIGSTOP */
      event_t e = wait_event(tids[t]);
      assert(e.signo == SIGSTOP);
    }

    ptrace(PTRACE_SETOPTIONS, tids[t], NULL,
           PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE);
    debug_print("Attached to %d\n", tids[t]);
  }
}
