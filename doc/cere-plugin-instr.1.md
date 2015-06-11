plugin-instr(1) -- Instrumentation plugin
==================================================================

## DESCRIPTION

**CERE** provides a mechanism of instrumentation by inserting probes around regions
inside the application with cere-instrument(1) for instance or at replay with
cere-replay(1). One can then redefines these probes inside its own library to
use them. **CERE** passes that accept users libraries have the **--plugin-instr**
flag. This flag expects a static or dynamic library.

## PROBES

There are 4 functions the user must redefine:

 * void cere_markerInit(char*):
    This function is inserted by **CERE** at the beginning of the main function.
    The argument is the name of the file passed through **--regions-file** for
    cere-instrument(1).

 * void cere_markerClose():
    This function is called through atexit(3), when the application exits.

 * cere_markerStartRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc):
    Start probe, inserted just before the region of insterest.
    regName: region name.
    vivo: false if we are in replay mode, otherwise true.
    requested_invoc: **--invocation** flag value from cere-instrument(1)
    curr_invoc: The region current invocation. This value is given by **CERE**

 * cere_markerStopRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc)
    Stop probe, inserted just after the region.

## REDEFINING CERE PROBES

Let's say your favorite instrumentation library has already 4 functions:

```C
    void my_init_function(char*);
    void my_close_function();
    void my_start_function(char*);
    void my_stop_function(char*);
```

Redefine **CERE** functions with yours:

```
    void cere_markerInit(char* filename) {
      my_init_function(filename);
    }
    void cere_markerClose() {
      my_close_function();
    }
    void cere_markerStartRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
      my_start_function(regName);
    }
    void cere_markerStopRegion(char* regName, bool vivo, int requested_invoc, int curr_invoc) {
      my_stop_function(regName);
    }
```

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-instrument(1) cere-replay(1)
