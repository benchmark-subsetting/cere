cere trace(1) -- Replay invocation(s) and predict region time
==================================================================

## SYNOPSIS

```
cere trace [-h] --region REGION | --regions-file REGIONS_FILE [--norun]
          [--read READ] [--force]
```

## DESCRIPTION

**cere trace** record the execution time of every invocation for the selected
region. To achieve this **cere trace** instrument the binary with probes around
the region and runs it. The trace is recorded into a binary file and a csv file.
**cere trace** also allow you to read the execution time of a particular
invocation from the trace. If **--multiple-trace** was enabled with
cere-configure(1), other regions can be traced with the selected one to decrease
the instrumentation time.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to trace. The list of valid regions can be displayed with
    the cere-regions(1) command.

  * `--regions-file REGIONS_FILE`:
    Input file where you can list regions you want to trace.

  * `--read INVOCATION`:
    Read the desired invocation from the trace and print it in the terminal.

  * `--norun`:
    Builds the replay binary but does not run it automatically.

  * `--force`:
    By default, **cere trace** does not trace a region if a previous measure
    already exists. The **--force** flag forces the trace.

## OUTPUT FILES

  * `.cere/traces/REGION.csv`:
    Cumulative invocations execution time and call count for the region.

  * `.cere/traces/REGION.bin`:
    Trace of invocations.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-regions(1) cere-configure(1) cere-selectinv(1)
