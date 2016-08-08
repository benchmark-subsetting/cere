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
//#undef _DEBUG

#include "debug.h"

#define MAX_TIDS 2048
size_t ntids = 0;
pid_t tids[MAX_TIDS];
bool pending[MAX_TIDS] = {false};
siginfo_t sigs[MAX_TIDS] = {0};

void ptrace_getsiginfo(pid_t pid, siginfo_t *sig) {
  if (ptrace(PTRACE_GETSIGINFO, pid, (void *)0, sig) == -1)
    errx(EXIT_FAILURE, "ptrace(PTRACE_GETSIGINFO) : %s\n", strerror(errno));
}

void ptrace_cont(pid_t pid) {
  int r = 0;
  do {
    r = ptrace(PTRACE_CONT, pid, NULL, NULL);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH));
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

void ptrace_singlestep(pid_t pid) {
  int r = 0;
  do {
    r = ptrace(PTRACE_SINGLESTEP, pid, NULL, NULL);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO ||
                        errno == ESRCH));
}

void ptrace_interrupt(pid_t pid) {
  if (ptrace(PTRACE_INTERRUPT, pid, 0, 0) == -1) {
    errx(EXIT_FAILURE, "ptrace(PTRACE_INTERRUPT) : %s\n", strerror(errno));
  }
}

void attach_all_threads(int nbthread, pid_t tid[nbthread]) {
  long t, r;
  unsigned long c = 0L;

  /* Attach all threads  */
  for (t = 0; t < nbthread; t++) {
    do {
      r = ptrace(PTRACE_SEIZE, tid[t], (void *)0, 0);

#ifdef _DEBUG
      if (c++ % 10000000 == 0)
        debug_print("Try attach %d\n", tid[t]);
#endif

    } while (r == -1L && (errno == EBUSY || errno == EIO || errno == EFAULT ||
                          errno == ESRCH));

    ptrace(PTRACE_SETOPTIONS, tid[t], NULL, PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE);



    // XXX: Why do we do this. Should not we just fail  with
    //  errx(EXIT_FAILURE, "Cannot attach thread %s\n", strerror(errno));
    if (r == -1L) {
      const int saved_errno = errno;
      while (t-- > 0)
        do {
          r = ptrace(PTRACE_DETACH, tid[t], (void *)0, (void *)0);
          debug_print("Detach of %d\n", tid[t]);
        } while (r == -1L &&
                 (errno == EBUSY || errno == EFAULT || errno == ESRCH));
      errno = saved_errno;
      break;
    }
  }
  debug_print("Attach to %d ... %d\n", tid[0], tid[nbthread - 1]);
}

void detach_all_threads(int nbthread, pid_t tid[nbthread]) {

  long t, r;
  unsigned long c = 0L;

  /* Detach from all tasks. */
  for (t = 0; t < nbthread; t++) {
    do {
      r = ptrace(PTRACE_DETACH, tid[t], (void *)0, (void *)0);

#ifdef _DEBUG
      if (c++ % 10000000 == 0)
        debug_print("Try detach %d\n", tid[t]);
#endif

    } while (r == -1 && (errno == EBUSY || errno == EFAULT || errno != ESRCH));
  }
  debug_print("Detach of %d ... %d\n", tid[0], tid[nbthread - 1]);
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

size_t get_tids(pid_t **const listptr, size_t *const sizeptr, const pid_t pid)
{
    char     dirname[64];
    DIR     *dir;
    pid_t   *list;
    size_t   size, used = 0;

    if (!listptr || !sizeptr || pid < (pid_t)1) {
        errno = EINVAL;
        return (size_t)0;
    }

    if (*sizeptr > 0) {
        list = *listptr;
        size = *sizeptr;
    } else {
        list = *listptr = NULL;
        size = *sizeptr = 0;
    }

    if (snprintf(dirname, sizeof dirname, "/proc/%d/task/", (int)pid) >= (int)sizeof dirname) {
        errno = ENOTSUP;
        return (size_t)0;
    }

    dir = opendir(dirname);
    if (!dir) {
        errno = ESRCH;
        return (size_t)0;
    }

    while (1) {
        struct dirent *ent;
        int            value;
        char           dummy;

        errno = 0;
        ent = readdir(dir);
        if (!ent)
            break;

        /* Parse TIDs. Ignore non-numeric entries. */
        if (sscanf(ent->d_name, "%d%c", &value, &dummy) != 1)
            continue;

        /* Ignore obviously invalid entries. */
        if (value < 1)
            continue;

        /* Make sure there is room for another TID. */
        if (used >= size) {
            size = (used | 127) + 128;
            list = realloc(list, size * sizeof list[0]);
            if (!list) {
                closedir(dir);
                errno = ENOMEM;
                return (size_t)0;
            }
            *listptr = list;
            *sizeptr = size;
        }

        /* Add to list. */
        list[used++] = (pid_t)value;
    }
    if (errno) {
        const int saved_errno = errno;
        closedir(dir);
        errno = saved_errno;
        return (size_t)0;
    }
    if (closedir(dir)) {
        errno = EIO;
        return (size_t)0;
    }

    /* None? */
    if (used < 1) {
        errno = ESRCH;
        return (size_t)0;
    }

    /* Make sure there is room for a terminating (pid_t)0. */
    if (used >= size) {
        size = used + 1;
        list = realloc(list, size * sizeof list[0]);
        if (!list) {
            errno = ENOMEM;
            return (size_t)0;
        }
        *listptr = list;
        *sizeptr = size;
    }

    /* Terminate list; done. */
    list[used] = (pid_t)0;
    errno = 0;
    return used;
}


void ptrace_attach(pid_t pid) {
  tids[ntids++] = pid;
  // XXX: must do with get_tids ? in case of helper thread
  attach_all_threads(ntids, tids);
}

void ptrace_detach(pid_t pid) {
  pid_t tids[] = {pid};
  detach_all_threads(1, tids);
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
    ptrace_syscall(tids[i]);
  }
  debug_print("%s\n", "--continue_all--");
}

void stop_all_except(pid_t pid) {
  for (int i=0; i<ntids; i++) {
    if (tids[i] == pid) continue;
    debug_print("SEND SIGSTOP to %d\n", tids[i]);
    int r = syscall(__NR_tkill, tids[i], SIGSTOP);
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

  do {
    pid = waitpid(wait_for, &status, __WALL);
  } while (pid == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH ||
                        errno == EIO));

  if (WIFEXITED(status)) {
    debug_print("tid %d exited\n", pid);
    for (int i=0; i < ntids; i++) {
      if (tids[i] == pid) {
        ntids--;
        if (ntids == 0) {
          return;
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

  ptrace_getsiginfo(pid, sig);
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

void ptrace_listen(pid_t pid) { ptrace(PTRACE_LISTEN, pid, 0, 0); }

void ptrace_syscall(pid_t pid) {
  assert(pid != -1);
  int r;
  int count = 0;
  do {
    r = ptrace(PTRACE_SYSCALL, pid, NULL, NULL);
    count ++;
    if (count % 100000 == 0) {
      debug_print("stuck in trace syscall with %d\n",pid);
    }
    } while(r != 1 && errno == ESRCH);
  if (r == -1) {
    errx(EXIT_FAILURE, "Failed PTRACE_SYSCALL %d: %s\n", pid, strerror(errno));
  }
  debug_print("ptrace_syscall %d ...\n\n", pid);
}

void *ptrace_ripat(pid_t pid, void *addr) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  void *ret = (void *)regs.rip;
  regs.rip = (register_t)addr;
  ptrace_setregs(pid, &regs);
  return ret;
}

void show_registers(FILE *const out, pid_t tid, const char *const note) {
  struct user_regs_struct regs;
  ptrace_getregs(tid, &regs);
#if (defined(__x86_64__) || defined(__i386__)) && __WORDSIZE == 64
  fprintf(out, "Task %d: RIP=%p, RSP=%p, RAX=%p, RDI=%p, RSI=%p, RDX=%p, "
               "ORIG_RAX=%p . %s\n",
          (int)tid, (void *)regs.rip, (void *)regs.rsp, (void *)regs.rax,
          (void *)regs.rdi, (void *)regs.rsi, (void *)regs.rdx,
          (void *)regs.orig_rax, note);

#elif(defined(__arm__))
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp,
          note);
#elif(defined(__aarch64__))
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp,
          note);
#endif
}

void print_registers(FILE *const out, struct user_regs_struct *regs,
                     const char *const note) {
#if (defined(__x86_64__) || defined(__i386__)) && __WORDSIZE == 64
  fprintf(out,
          "RIP=%p, RSP=%p, RAX=%p, RDI=%p, RSI=%p, RDX=%p, ORIG_RAX=%p . %s\n",
          (void *)regs->rip, (void *)regs->rsp, (void *)regs->rax,
          (void *)regs->rdi, (void *)regs->rsi, (void *)regs->rdx,
          (void *)regs->orig_rax, note);

#elif(defined(__arm__))
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp,
          note);
#elif(defined(__aarch64__))
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp,
          note);
#endif
}

void put_string(pid_t pid, char *src, void *dst, size_t nbyte) {
  debug_print("PUT STRING %s at %p\n", src, dst);
  ptrace_putdata(pid, (long long unsigned)dst, src, nbyte);
}
