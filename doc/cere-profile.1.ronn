cere profile(1) -- Profiles an application using CERE
=====================================================

## SYNOPSIS

```
cere profile [-h] [--app] [--regions] [--force]
```

## DESCRIPTION

**cere profile** instruments and profiles the application.
It offers two profiling modes:

  * `--app`, application profiling

  * `--regions`, regions profiling

By default if no profiling mode is chosen, both modes are enabled.

Before running **cere profile**, one must run **cere configure**.

The profiling step is required by other **CERE** commands such as **cere test** and
**cere report**.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--app`:
    Enables application profiling. Application profiling inserts a probe at the
    start of the *main* function and another probe at the end of the program using
    atexit(3). The probes measure the application whole execution time using the
    Time Stamp Counter.

  * `--regions`:
    Enables region profiling. In *loop mode* regions are outer loops, in *OpenMP
    mode* regions are parallel regions. All the regions of the program are
    identified and outlined as separate functions. The program is run using google
    perftool google-pprof(1) and a call graph of the regions recorded.

  * `--force`:
    By default *cere profile* does nothing if a previous profile exists. This flag
    deletes the old measures and launches a new profile.

## OUTPUT FILES

### --app output

  * `.cere/profile/app_cycles.csv`: the application runtime in CPU cycles.

### --regions output

  * `.cere/profile/app.prof`: the google perf tool output.
  * `.cere/profile/graph_.dot`: dot representation of the region call graph.
  * `.cere/profile/graph_.pdf`: image of the call graph.
  * `.cere/profile/graph_.pkl`: the python graph object.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-configure(1) cere-trace(1) cere-test(1) cere-report(1)
