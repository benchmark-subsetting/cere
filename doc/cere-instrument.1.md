cere instrument(1) -- Instrument a region inside the application
==================================================================

## SYNOPSIS

```
cere instrument [-h] --region REGION | --regions-file REGIONS_FILE
          [--plugin-instr PLUGIN_INSTR] [--invocation INVOCATION]
          [--omp-num-threads OMP_ NUM_THREAD] [--norun] [--force]
```

## DESCRIPTION

**cere instrument** allows the user to instrument a region inside the original
application with probes around the region. These probes are defined via the
**--plugin-instr** flag to perform measures on the region. For instance,
**cere instrument** is used by cere-trace(1) and cere-check-io(1).

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to instrument. The list of valid regions can be displayed
    with the cere-regions(1) command.

  * `--regions-file REGIONS_FILE`:
    Input file where you can list regions you want to trace.

  * `--plugin-instr PLUGIN_INSTR`:
    Library to instrument the region. For more information on how to use another
    library please refer to cere-plugin-instr(1).

  * `--invocation INVOCATION`:
    Selects the invocation to instrument. By default, all invocations are
    instrumented.

  * `--omp-num-threads OMP_ NUM_THREAD`:
    If PCERE is enabled, selects the number of OpenMP threads during instrumentation
    execution. If this argument is not specified, PCERE runs the application with
    the system default parameters.

  * `--norun`:
    Builds the instrumented binary but does not run it automatically.

  * `--force`:
    If the region is marked as invalid, the instrumentation stops. The **--force**
    flag forces the instrumentation.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-regions(1) cere-trace(1) cere-configure(1) cere-plugin-instr(1) cere-check-io(1)
