/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016, Universite de Versailles St-Quentin-en-Yvelines       *
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

#define MAX_TIDS 2048
size_t ntids = 0;
pid_t tids[MAX_TIDS];
bool pending[MAX_TIDS] = {false};
siginfo_t sigs[MAX_TIDS] = {0};


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


void continue_all(void) {
  for (int i=0; i<ntids; i++) {
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
    siginfo_t sig;
    wait_process(tids[i], &sig);
    if (sig.si_signo != SIGSTOP) {
      pending[i] = true;
      sigs[i] = sig;
    }
  }
}

pid_t wait_process(pid_t wait_for, siginfo_t * sig) {
  int r, status = 0;
  pid_t pid;

  debug_print("wait_process : %d -> ", wait_for);

  for (int i=0; i < ntids; i++) {
    if (wait_for != -1 && tids[i] != wait_for) continue;
    if (pending[i]) {
      pending[i] = false;
      *sig = sigs[i];
      debug_print("tid %d kept signal : %d -> ", tids[i], sig->si_signo);
      return tids[i];
    }
  }

  int count = 0;
  do {
    pid = waitpid(wait_for, &status, __WALL);
    sched_yield();
  } while (pid == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH ||
                        errno == EIO));

  if (WIFEXITED(status)) {
    debug_print("tid %d exited\n", pid);
    for (int i=0; i < ntids; i++) {
      if (tids[i] == pid) {
        ntids--;
        if (ntids == 0) {
          debug_print("%s\n", "Finished capture\n");
          exit(1);
        }
        else {
          tids[i] = tids[ntids];
          sigs[i] = sigs[ntids];
          pending[i] = pending[ntids];
          if (wait_for == pid) {
            return pid;
          }
          else {
            return wait_process(wait_for, sig);
          }
        }
      }
    }
    errx(EXIT_FAILURE, "Non tracked tid %d exited", pid);
  }

  if (WIFSIGNALED(status))
    raise(WTERMSIG(status));

  if (!WIFSTOPPED(status)) {
    debug_print("%s\n", "bad place to be!");
  }

  /* If the process has continue instead of be stopped */
  /* the process is beyond the control of ptrace  */
  /* So we fail*/
  if (WIFCONTINUED(status))
    errx(EXIT_FAILURE, "%d continue\n", pid);

  if (ptrace(PTRACE_GETSIGINFO, pid, (void *)0, sig) == -1) {
    errx(EXIT_FAILURE, "ptrace(PTRACE_GETSIGINFO) : %s\n", strerror(errno));
  }

  int sicode = sig->si_code;

  if (WIFSTOPPED(status)) {
    int num = WSTOPSIG(status);
    debug_print("%d received signal : %d -> ", pid, num);

    if (num == SIGSEGV) {
      if (sicode == SEGV_MAPERR)
        debug_print("SEGV_MAPERR address not mapped to object : %p\n",
                    sig->si_addr);
      else if (sicode == SEGV_ACCERR)
        debug_print("SEGV_ACCERR invalid permissions for mapped object : %p\n",
                    sig->si_addr);
      else
        errx(EXIT_FAILURE, "SEGV bad si_code : %d\n", sicode);
    } else if (num == SIGTRAP || num == (SIGTRAP | 0x80)) {
      if (sicode <= 0)
        debug_print(
            "%s\n",
            "SIGTRAP  was  delivered  as  a result of a user-space action");
      else if (sicode == 0x80)
        debug_print("%s\n", "SIGTRAP was sent by the kernel");
      else if (sicode == SIGTRAP || sicode == (SIGTRAP | 0x80))
        debug_print("%s\n", "This is a syscall-stop");
      else if (status>>8 == (SIGTRAP | (PTRACE_EVENT_CLONE<<8))) {
        debug_print("%s\n", "PTRACE_EVENT_CLONE received");
        pid_t new_thread;
        int r = ptrace(PTRACE_GETEVENTMSG, pid, NULL, &new_thread);
        if (r == -1) {
          errx(EXIT_FAILURE, "Failed to get get tid of newly cloned thread: %s\n", strerror(errno));
        }

        debug_print("Following new thread %d.\n", new_thread);
        tids[ntids++] = new_thread;

        siginfo_t sig2;
        wait_process(new_thread, &sig2);

        ptrace_syscall(new_thread);
        ptrace_syscall(pid);
        return wait_process(wait_for, sig);
      }
      else
        errx(EXIT_FAILURE, "TRAP bad si_code : %d\n", sicode);
    } else if (num == SIGSTOP) {
      debug_print("%s\n", "SIGSTOP received from tracee");
    } else if (num == SIGABRT) {
      debug_print("%s\n", "Finished capture\n");
      exit(0);
    }
    else
      errx(EXIT_FAILURE, "??? signo : %d\n", num);
  }
  return pid;
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
    tids[ntids++] = tid;
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
      siginfo_t sig;
      wait_process(tids[t], &sig);
    }

    ptrace(PTRACE_SETOPTIONS, tids[t], NULL,
           PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE);
    debug_print("Attached to %d\n", tids[t]);
  }
}
