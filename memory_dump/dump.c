#define _LARGEFILE64_SOURCE
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h> 
#include <unistd.h>
#include <sys/mman.h>

 
/* dump_region: dumps to a file "memdump.<start>"
 * the memory segment starting at address <start>
 * and ending at address <end>
 */
void dump_region(int mem, 
                 off64_t start, 
                 off64_t end)
{
    char buf[4096];
    char path[256];

    snprintf(path, sizeof(path), "memdump.%lu", start);
    int out = open(path, O_WRONLY|O_CREAT,S_IRWXU);

    if (out == -1) {
        fprintf(stderr, "Could not open dump file: %s", path); 
        return;
    }
 
    lseek64(mem, start, SEEK_SET);
    while(start < end) {
        int rd;
        rd = read(mem, buf, 4096);
        write(out, buf, rd);
        start += 4096;
    }

    close(out);
}

/* dump_mem: dumps the memory of a PTRACED process
 * child: the pid of the process to dump
 * size: the number of addresses
 * adresses: the addresses that we want to preserve
 */
void dump_mem(pid_t child, int size, void* addresses[]) {
    /* Wait for the child to stop */
    wait(NULL);

    FILE *maps;
    int mem;
    char path[256];

    snprintf(path, sizeof(path), "/proc/%d/maps", child);
    maps = fopen(path, "r");

    snprintf(path, sizeof(path), "/proc/%d/mem", child);
    mem = open(path, O_RDONLY);

    if(!maps || mem == -1) {
        fprintf(stderr, "Error reading the memory using /proc/ interface");
        exit(-1);
    }

    FILE * core_info = fopen("core.info", "w"); 
    FILE * core_map = fopen("core.map", "w"); 
    if (!core_info || !core_map) {
        fprintf(stderr, "Could not create core.info or core.map file");
        exit(-1);
    }

    char buf[BUFSIZ + 1];
    while(fgets(buf, BUFSIZ, maps)) {
        off64_t start, end;
        sscanf(buf, "%lx-%lx", &start, &end);

        int i;
        int dump = 0;

        /* Check if region contains any needed address */
        for (i=0;i<size;i++) {
            off64_t aoff = ((off64_t) addresses[i]);
            if  (aoff >= start && aoff <= end) {
                dump = 1;
                fprintf(core_map, "%d %lu %lu\n", i, aoff-start, start);
            } 
        }

        /* in that case dump the region */
        if (dump) {
            fprintf(core_info, "%lu %lu\n", start, end-start);
            dump_region(mem, start, end);
        }
    }

    close(mem);
    fclose(maps);
    fclose(core_info);
    fclose(core_map);
}
 
/* dump: dumps memory
 * size: number of args
 * addresses: an array of <size> addresses to save
 */
void dump(int size, void* addresses[]) {
    pid_t child;
    child = fork();
    if(child == 0) {
        /* Trace and freeze the child process */
        ptrace(PTRACE_TRACEME, 0, NULL, NULL);
        raise(SIGTRAP);
    }
    else {
        dump_mem(child, size, addresses);
        ptrace(PTRACE_CONT, child, NULL, NULL);
        exit(0);
    }
}

/**********************************************************************
 * REPLAY MODE                                                        *
 **********************************************************************/ 

/* map_segment: loads a memory dump file to memory and returns
 * a pointer to where it was mapped
 */
char* map_segment(off64_t start, off64_t size) {
    char path[256];
    snprintf(path, sizeof(path), "memdump.%lu", start);
    int in = open(path, O_RDONLY);

    if (in == -1) {
        fprintf(stderr, "Could not open dump file");
        exit(-1);
    }
    char *src = mmap (NULL, 139264, PROT_READ|PROT_WRITE, MAP_PRIVATE, in, 0);
    if (src == MAP_FAILED) {
        fprintf(stderr, "Could not map dump to memory");
        exit(-1);
    }
    return src;
}

/* load: restores memory before replay
 * size: number of args
 * addresses: an array of <size> addresses
 */
void load(int size, void* addresses[]) {
    FILE* core_info = fopen("core.info", "r");
    FILE* core_map = fopen("core.map", "r");
    if (!core_info || !core_map) {
        fprintf(stderr, "Could not open core.info or core.map");
        exit(-1);
    }
    
    char buf[BUFSIZ + 1];
    while(fgets(buf, BUFSIZ, core_info)) { 
        off64_t start, size;
        sscanf(buf, "%lu %lu", &start, &size);
        char *src = map_segment(start, size);

        rewind(core_map);
        while(fgets(buf, BUFSIZ, core_map)) {
            int pos;
            off64_t offset, segment;
            sscanf(buf, "%d %lu %lu", &pos, &offset, &segment);
            if (segment == start) {
                addresses[pos] = src + offset; 
            }
        }
    }

    fclose(core_info);
    fclose(core_map);
}
