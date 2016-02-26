cere flag(1) -- Replay region with specified compilation flags
==================================================================

## SYNOPSIS

```
cere flag [-h] --region REGION --flags FLAGS1, FLAGS2, ...
          [--invocation INVOCATION] [--invitro-callcount INVITRO_CALLCOUNT]
          [--plugin-instr PLUGIN_INSTR] [--norun] [--noinstrumentation]
          [--force]
```

## DESCRIPTION

**cere flag** calls cere-replay(1) on the selected region to predict the
region performance when compiled with specified compilation flags. The user can
then choose the best set of flags for each region before calling cere-hybrid(1).

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to replay. The list of valid regions can be displayed with
    the cere-regions(1) command.

  * `--flags FLAGS1, FLAGS2, ...`:
    The region is replayed for each set of flags (comma separated).

  * `--invocation INVOCATION`:
    Selects the invocation to replay. By default, all the representative
    invocations chosen by cere-selectinv(1) are replayed.

  * `--invitro-callcount INVITRO_CALLCOUNT`:
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
    Replay execution time of the region * INVITRO_CALLCOUNT

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-replay(1) cere-hybrid(1)
