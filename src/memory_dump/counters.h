struct region_counter
{
  char* name;
  unsigned int call_count;
}; 

void init_counters(void);
struct region_counter * get_region(char * loop_name);
