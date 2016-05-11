cere replay(1) -- Replay invocation(s) and predict region time
==================================================================

## SYNOPSIS

```
cere replay [-h] --region REGION [--invocation INVOCATION]
          [--invitro-callcount CERE_REPLAY_REPETITIONS] [--plugin-instr PLUGIN_INSTR]
          [--norun] [--noinstrumentation] [--force]
```

## DESCRIPTION

**cere replay** loads the memory and cache state just before directly jumping to
the region of interest. To achieve this, **cere replay** shunts the main function
and jump to a wrapper function that load the memory and call the outlined region.
If no particular invocation is selected, **cere replay** replays all the
representative invocations selected by cere-selectinv(1), and predicts the
runtime of the region according to the model computed by cere-selectinv(1).

The warmup type can be selected by setting the environnement variable
`CERE_WARMUP` to one of the tree values below.  Three warmup modes are
available,

  * `COLD`, no warmup at all.

  * `WORKLOAD` (the default mode), optimistic warmup the whole workload is
     touched before replaying.

  * `PAGETRACE`, the trace of most recently touched pages is replayed before
    entering the region of interest.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to replay. The list of valid regions can be displayed with
    the cere-regions(1) command.

  * `--invocation INVOCATION`:
    Selects the invocation to replay. By default, all the representative
    invocations chosen by cere-selectinv(1) are replayed.

  * `--invitro-callcount CERE_REPLAY_REPETITIONS`:
    Select the meta-repetition of the replay. By default **cere replay** runs
    the region 10 times.

  * `--plugin-instr PLUGIN_INSTR`:
    Library to instrument the replay. For more information on how to use another
    library please refer to cere-plugin-instr(1).

  * `--norun`:
    Builds the replay binary but does not run it automatically.

  * `--noinstrumentation`:
    Disable the instrumentation of the replay. Override **--plugin-instr** flag.

  * `--force`:
    By default, **cere replay** does not replay a region+invocation pair if a
    previous replay result exists. The **--force** flag forces the replay.

## OUTPUT FILE

  * `.cere/replays/REGION_INVOCATION`:
    Replay execution time of the region multiplied by CERE_REPLAY_REPETITIONS

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-regions(1) cere-capture(1) cere-configure(1) cere-selectinv(1) cere-plugin-instr(1)
