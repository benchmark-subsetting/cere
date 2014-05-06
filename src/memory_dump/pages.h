#define PAGESIZE 4096
#define round_to_page(addr) ((char *)(((long unsigned)(addr)) & ~(PAGESIZE-1))) 
#define round_up_page(size) ((((size)-1)/PAGESIZE+1)*PAGESIZE)
