/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2017, Universite de Versailles St-Quentin-en-Yvelines  *
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

// This C++ helper handles C++ stream issues with fork
// by calling dump_init BEFORE std::cout initialization

#include <cstdlib>
#include <cstring>

extern "C" {

// Forward declarations from tracee.c
void _dump_init(bool new_kad);
extern bool dump_initialized;

// Flush all C++ streams before fork
void
cpp_flush_streams()
{
  // This function is kept for compatibility but is less critical
  // now that we initialize early
}

// Early initialization function called before C++ global constructors
// This runs with priority 101, BEFORE std::cout (priority 100)
//
// IMPORTANT: This is linked into libcere_dump.so, so if a C++ program links
// with libcere_dump, this will run. We need to initialize early to avoid
// deadlocks with std::cout mutexes.
static void early_dump_init() __attribute__((constructor(101)));

static void
early_dump_init()
{
  // If already initialized, skip (can happen if multi_dump_init also called)
  if (dump_initialized) {
    return;
  }

  // We're linked with libcere_dump.so, which means we're likely instrumented
  // Initialize early with multi-codelet mode (kill_after_dump=false)
  // This is safe - if single codelet is needed, multi_dump_init() will be
  // called from main() but will be no-op since we already initialized

  // Check if CERE_MODE is set to determine mode
  const char* cere_mode = std::getenv("CERE_MODE");
  bool is_multi = true;  // Default to multi mode (safer)

  if (cere_mode) {
    // If CERE_MODE contains "dump" and has multiple regions/invocations, use
    // multi
    if (std::strstr(cere_mode, ",") != nullptr ||
        std::strstr(cere_mode, ";") != nullptr) {
      is_multi = true;
    } else {
      is_multi = false;
    }
  }

  // Call dump init with appropriate mode BEFORE std::cout initialization
  // For multi-codelet: kill_after_dump = false
  // For single codelet: kill_after_dump = true
  _dump_init(!is_multi);
}

}  // extern "C"
