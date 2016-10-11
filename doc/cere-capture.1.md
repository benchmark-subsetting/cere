cere capture(1) -- Captures the memory and cache state of a region
==================================================================

## SYNOPSIS

```
cere capture [-h] --region REGION [--invocation INVOCATION] [--norun]
          [--force]
```

## DESCRIPTION

**cere capture** captures the memory and cache state just before entering the region
of interest. To achieve this, **cere capture** builds an instrumented binary of the
original application with capture probes and runs this instrumented binary.
If no particular invocation is selected, **cere capture** records all the
representative invocations selected by cere-selectinv(1).

When capturing parallel programs, one can capture first touch NUMA effects by
setting CERE_FIRSTTOUCH="TRUE".

Attention, when capturing OpenMP parallel programs one should always export
KMP_BLOCKTIME=0. This avoids an active loop between the tracer and sched_yield
calls from the OpenMP runtime that *significantly* slows the trace. In practice
cere capture exports KMP_BLOCKTIME=0 for you.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to capture. The list of valid regions can be displayed with
    the cere-regions(1) command.

  * `--invocation INVOCATION`:
    Selects the invocation to capture. By default, all the representative
    invocations chosen by cere-selectinv(1) are captured.

  * `--norun`:
    Builds the capture-instrumented binary but does not run it automatically.

  * `--force`:
    By default, **cere capture** does not captures a region+invocation pair if a
    previous dump exists. The **--force** flag forces a recapture.

## OUTPUT FILES

  * `.cere/dumps/REGION/INVOCATION/`:
    Files needed to restore the memory and the cache state in order to replay
    the region.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-regions(1) cere-replay(1) cere-configure(1) cere-selectinv(1)
