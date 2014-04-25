#define _LARGEFILE64_SOURCE
#include <err.h>
#include <stdlib.h>
#include <stdio.h>

/**********************************************************************
 * REPLAY MODE                                                        *
 **********************************************************************/

/* load: restores memory before replay
 * loop_name: name of dumped loop
 * invocation: invocation number to load
 * count: number of args
 * addresses: an array of <count> addresses
 */
void 
load(char* loop_name, int invocation, int count, void* addresses[count]) {
    char path[BUFSIZ];

    snprintf(path, sizeof(path), "dump/%s/%d/core.map", loop_name, invocation);
    FILE* core_map = fopen(path, "r");
    if (!core_map) 
        errx(EXIT_FAILURE, "Could not open %s", path);

    char buf[BUFSIZ + 1];
    while(fgets(buf, BUFSIZ, core_map)) {
        int pos;
        off64_t address;
        sscanf(buf, "%d %lx", &pos, &address);
        addresses[pos] = (char*)(address);
    }

    fclose(core_map);
}
