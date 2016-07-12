#ifdef _DEBUG
#define debug_print(fmt, ...) \
            fprintf(stderr, fmt, __VA_ARGS__)
#else
#define debug_print(fmt, ...)
#endif
