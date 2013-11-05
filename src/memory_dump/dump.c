#define _LARGEFILE64_SOURCE
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

/* dump_region: dumps to a file "<start>.memdump"
 * the memory segment starting at address <start>
 * and ending at address <end>
 */
void dump_region(int mem, 
                 off64_t start, 
                 off64_t end)
{
    char buf[4096];
    char path[256];

    snprintf(path, sizeof(path), "%lx.memdump", start);
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
 * count: the number of addresses
 * addresses: the addresses that we want to preserve
 */
void dump_mem(pid_t child, int count, void* addresses[]) {
    char path[256];
    FILE *maps;
    int mem;
    int i;

    FILE * core_map = fopen("core.map", "w"); 
        if (!core_map) {
        fprintf(stderr, "Could not create core.map file");
        exit(-1);
    }

    for (i=0; i<count; i++) {
        fprintf(core_map, "%d %lx\n", i, (off64_t) addresses[i]);
    }

    fclose(core_map);

    snprintf(path, sizeof(path), "/proc/%d/maps", child);
    maps = fopen(path, "r");

    snprintf(path, sizeof(path), "/proc/%d/mem", child);
    mem = open(path, O_RDONLY);

    //snprintf(path, sizeof(path), "cat /proc/%d/maps", child);
    //system(path);

    if(!maps || mem == -1) {
        fprintf(stderr, "Error reading the memory using /proc/ interface");
        exit(-1);
    }

    char buf[BUFSIZ + 1];
    while(fgets(buf, BUFSIZ, maps)) {
        off64_t start, end;
        sscanf(buf, "%lx-%lx", &start, &end);

        int i;
        int dump = 0;

        /* Ignore prog or syscall segments */
        if (start < 0x500000
            || start > 0xffffffffff000000) continue;
        /* dump the region */
        dump_region(mem, start, end);
    }

    close(mem);
    fclose(maps);
}
 
/* dump: dumps memory
 * loop_name: name of dumped loop
 * count: number of arguments
 * args: the arguments to dump 
 */
void dump(char* loop_name, int count, ...) {

    struct stat sb;
    char path[256];
    char cwd[256];
    char dump_path[] = "dump";

    /* Keep the current working directory */
    if (getcwd(cwd, sizeof(cwd)) == NULL)
    {
        fprintf(stderr, "Could not get current working direcory");
        exit(-1);
    }

    /* Check that dump exists or try to create it, then enter it */
    if(stat(dump_path, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
        if(mkdir(dump_path, 0777) != 0) {
            fprintf(stderr, "Could not create dump directory");
            exit(-1);
        } 
    }

    snprintf(path, sizeof(path), "%s/%s", dump_path, loop_name);
    /* If dump already exists for this loop_name, skip dump */ 
    if(mkdir(path, 0777) != 0) {
        fprintf(stderr, "Skip dump\n");
        return;
    }
    
    void * addresses[count];
    va_list ap;
    int j;
    va_start(ap, count); 
    for(j=0; j<count; j++)
        addresses[j] = va_arg(ap, void*); 
    va_end(ap);

    pid_t child;
    child = fork();
    /*Execution should continue through the son */
    if(child == 0) {
        /* Trace and freeze the child process */
        ptrace(PTRACE_TRACEME, 0, NULL, NULL);
        raise(SIGTRAP);
        exit(0);
    }
    else {
        /* Wait for the child to stop */
        wait(NULL);
        if(chdir(path) != 0) {
            fprintf(stderr, "cannot enter loop dump directory");
            exit(-1);
        }
        dump_mem(child, count, addresses);
        ptrace(PTRACE_CONT, child, NULL, NULL);
        /* Come back to original working directory */
        if(chdir(cwd) != 0) {
            fprintf(stderr, "cannot come back to original working directory");
            exit(-1);
        }
    }
}

/**********************************************************************
 * REPLAY MODE                                                        *
 **********************************************************************/ 

/* load: restores memory before replay
 * loop_name: name of dumped loop
 * count: number of args
 * addresses: an array of <count> addresses
 */
void load(char* loop_name, int count, void* addresses[count]) {
    char path[256];
    snprintf(path, sizeof(path), "dump/%s/core.map", loop_name);
    FILE* core_map = fopen(path, "r");
    if (!core_map) {
        fprintf(stderr, "Could not open %s", path);
        exit(-1);
    }
    
    char buf[BUFSIZ + 1];
    while(fgets(buf, BUFSIZ, core_map)) {
        int pos;
        off64_t address;
        sscanf(buf, "%d %lx", &pos, &address);
        addresses[pos] = (char*)(address); 
    }

    fclose(core_map);
}
