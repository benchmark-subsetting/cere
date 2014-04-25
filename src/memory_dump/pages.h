#define PAGESIZE 4096
#define round_to_page(addr) ((char *)(((long unsigned)(addr)) & ~(PAGESIZE-1))) 
