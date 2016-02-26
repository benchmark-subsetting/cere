cere check-matching(1) -- Checks if a region matches between replay and original versions
=========================================================================================

## SYNOPSIS

```
cere check-matching [-h] --region REGION | --regions-file REGIONS_FILE
           [--max_error MAX_ERROR] [--force]
```

## DESCRIPTION

**cere check-matching** checks if a region matches in CPU cycles between replay
and original version of the region. To do so, **cere check-matching** trace the
region with cere-trace(1), then selects representatives invocation by calling
cere-selectinv(1). **cere check-matching** then captures and replay selected
invocations to predict the replay time of the region thanks to cere-capture(1)
and cere-replay(1). Finally **cere check-matching** compare replay time with
the original execution time of the region and output the matching results.
**cere check-matching** also update the call graph. cere-report(1) can be the
used to visualize the matching result for tested region(s).

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to check. The list of valid regions can be displayed
    with the cere-regions(1) command.

  * `--regions-file REGIONS_FILE`:
    Input file where you can list regions you want to check.

  * `--max_error MAX_ERROR`:
    If the error between original and replay execution time is lower or equal than
    MAX_ERROR, then the region is matching. The default MAX_ERROR value is 15%

  * `--force`:
    Force all passes to ignore already done jobs and redo everything.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-configure(1) cere-regions(1) cere-trace(1) cere-selectinv(1) cere-capture(1)
cere-replay(1) cere-report(1)
