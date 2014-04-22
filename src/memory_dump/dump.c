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
#include "dump.h"
#include "snoop.h"

#define LOG_SIZE 8

// Comparison function.
static bool streq(const void *e, void *string)
{
    return strcmp(((region *)e)->name, string) == 0;
}

static uint32_t hash_string(const char *string)
{
    uint32_t ret;
    for (ret = 0; *string; string++)
        ret = (ret << 5) - ret + *string;
    return ret;
}

static size_t rehash(const void *e, void *unused)
{
    return hash_string(((region *)e)->name);
}

void dump_init()
{
    htable_init(&regionHtab, rehash, NULL);
    page_log_on(LOG_SIZE);
    atexit(dump_close);
}

void dump_close()
{
  page_log_off();
  htable_clear(&regionHtab);
}

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

//~ /*Only dumping int and double scalar*/
//~ void write_bin_file(char *path, char* name, char *type, void *adrr) {
    //~ char scalar_path[256];
    //~ char scalar_file[256];
    //~ if(!strcmp(type, "i32*")) {
        //~ snprintf(scalar_path, sizeof(scalar_path), "%s/%s", path, "integers");
        //~ snprintf(scalar_file, sizeof(scalar_file), "%s/%s", scalar_path, name);
        //~ mkdir(scalar_path, 0777);
//~ 
        //~ FILE *scalar = fopen(strcat(scalar_file, ".bin"), "a");
        //~ if (!scalar) {
            //~ fprintf(stderr, "Could not create scalar file");
            //~ exit(-1);
        //~ }
        //~ fwrite((const void*)(adrr), sizeof(int), 1, scalar);
        //~ fclose(scalar);
    //~ }
    //~ else if(!strcmp(type, "double*")) {
        //~ snprintf(scalar_path, sizeof(scalar_path), "%s/%s", path, "doubles");
        //~ snprintf(scalar_file, sizeof(scalar_file), "%s/%s", scalar_path, name);
        //~ mkdir(scalar_path, 0777);
//~ 
        //~ FILE *scalar = fopen(strcat(scalar_file, ".bin"), "a");
        //~ if (!scalar) {
            //~ fprintf(stderr, "Could not create scalar file");
            //~ exit(-1);
        //~ }
        //~ fwrite((const void*)(adrr), sizeof(double), 1, scalar);
        //~ fclose(scalar);
    //~ }
//~ }

/* dump: dumps memory
 * loop_name: name of dumped loop
 * to_dump; the invocation to dump
 * count: number of arguments
 * args: the arguments to dump 
 */
static void _dump(char* loop_name, int to_dump, int count, void * addresses[]);
void dump(char* loop_name, int to_dump, int count, ...)
{
  void * addresses[count];
  char *type, *name;
  va_list ap;
  int j;
  va_start(ap, count); 
  for(j=0; j<count; j++) {
      addresses[j] = va_arg(ap, void*);
      /*Only to dump scalars. Will be removed*/
      //~ type = va_arg(ap, char*); //retrieve the type
      //~ name = va_arg(ap, char*); //retrieve the name
      //~ write_bin_file(path, name, type, addresses[j]);
  }
  va_end(ap);

  _dump(loop_name, to_dump, count, addresses);
}

static void _dump(char* loop_name, int to_dump, int count, void* addresses[]) {
    struct stat sb;
    char path[256];
    char cwd[256];
    char invocation[256];
    char dump_path[] = "dump";
    region *r=NULL;

    if ((r = htable_get(&regionHtab, hash_string(loop_name), streq, loop_name)) == NULL)
    {
        if ((r = malloc(sizeof(region))) == NULL)
        {
            fprintf(stderr, "DUMP: Unable to allocate new region %s\n", loop_name);
            exit(EXIT_FAILURE);
        }
        if ((r->name = malloc((strlen(loop_name)+1)*sizeof(char))) == NULL)
        {
            fprintf(stderr, "DUMP: Unable to allocate new region name %s\n", loop_name);
            exit(EXIT_FAILURE);
        }
        strcpy(r->name, loop_name);
        r->call_count = 0;
        htable_add(&regionHtab, hash_string(r->name), r);
    }
    //Test if it's the good invocation to dump
    if(++r->call_count != to_dump) return; 

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
    snprintf(path, sizeof(path), "%s/%s/", dump_path, r->name);

    /* If dump already exists for this loop_name, skip dump */ 
    if(mkdir(path, 0777) == 0) {
        sprintf(invocation, "%d", to_dump);
        if(mkdir(strcat(path, invocation), 0777) != 0) {
            fprintf(stderr, "Skip dump\n");
            return;
        }
    }
    else {
        fprintf(stderr, "Skip dump\n");
        return;
    }

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
        page_log_off();
        /* Wait for the child to stop */
        wait(NULL);
        if(chdir(path) != 0) {
            fprintf(stderr, "cannot enter loop dump directory");
            exit(-1);
        }
        page_log_dump("hotpages.map");
        dump_mem(child, count, addresses);
        ptrace(PTRACE_CONT, child, NULL, NULL);
        /* Come back to original working directory */
        if(chdir(cwd) != 0) {
            fprintf(stderr, "cannot come back to original working directory");
            exit(-1);
        }
        page_log_on(LOG_SIZE);
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
void load(char* loop_name, int to_load, int count, void* addresses[count]) {
    char path[256];
    snprintf(path, sizeof(path), "dump/%s/%d/core.map", loop_name, to_load);
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
