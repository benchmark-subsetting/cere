cere flag(1) -- Find best compilation flags for regions
==================================================================

## SYNOPSIS

```
cere flag [-h] [--region REGION] [--invitro-callcount CERE_REPLAY_REPETITIONS] 
               [--force] FLAGS_FILE
```

## DESCRIPTION

**cere flag** finds for each region the best compilation flags and predict the
full application speed-up when compiling each region with its best flags.
**cere flag** calls cere-replay(1) on either the requested region with --region
or by default on regions selected with cere-select-ilp(1). Each region is compiled
with compilation flags specified in FLAGS_FILE and then executed to predict the
performance of the region. Flags that gave the best performance compared to the
default compilation flags are kept. Finally the full application speed-up is
predicted based on each region best performance.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--region REGION`:
    Selects the region to replay. The list of valid regions can be displayed with
    the cere-regions(1) command.

  * `--invitro-callcount CERE_REPLAY_REPETITIONS`:
    Select the meta-repetition of the replay. By default **cere replay** runs
    the region 10 times.

  * `--force`:
    **cere flag** keeps any previous results for regions. Use --force if you want
    to measure again or if you changed the input file.

  * `FLAGS_FILE`:
    Csv file where each row is a sequence of compilation flags that the user
    wants to test. An example can be find at CERE_PATH/examples/flags.csv.

## OUTPUT FILE

  * `.cere/replays/regions_flags.csv`:
    This file contains for each region the best sequence id, the original and
    the best runtime. This file can be used as an input of cere-hybrid(1)

## COPYRIGHT

cere is Copyright (C) 2014-2016 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-replay(1) cere-hybrid(1) cere-select-ilp(1)
