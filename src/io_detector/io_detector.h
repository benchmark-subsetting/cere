#define BUF_SIZE 64

void init_io_detection();
void start_io_detection();
void stop_io_detection();
void close_io_detection();

struct detector_state {
  pid_t pid;
  char *trace_file;
};

extern struct detector_state state;
