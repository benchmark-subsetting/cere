#ifndef __TYPES__H
#define __TYPES__H

#include <stdint.h>

#define PAST_INV 30
#define MAX_PATH 256
#define MAX_IGNORE 32
#define TRACE_SIZE (16384 * 100)
#define LOG_SIZE 64
#define SIZE_LOOP 256

#define SYS_send (-1)
#define SYS_hook (-2)

enum tracer_state_t {
  TRACER_UNLOCKED = 1,
  TRACER_LOCKED = 2,
  TRACER_DUMPING = 3
};

enum tracer_state_t tracer_state = 0;

#endif /* __TYPES__H */
