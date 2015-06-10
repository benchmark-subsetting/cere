cere check-select-max-cov(1) -- Automatic regions selector
==========================================================

## SYNOPSIS

```
cere select-max-cov [-h] [--max-error MAX_ERROR] [--min-coverage MIN_COVERAGE]
           [--force]
```

## DESCRIPTION

**cere select-max-cov** automatically selects the best set of matching regions to
maximize the application coverage regardless of the replay cost. cere-profile(1)
must be executed before running **cere select-max-cov**. cere-report(1) can be
then used to visualize the selection summary.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--max-error MAX_ERROR`:
    If the error between original and replay execution time is lower or equal than
    MAX_ERROR, then the region is matching. The default MAX_ERROR value is 15%

  * `--min-coverage MIN_COVERAGE`:
    The region minimum coverage value for being selected. By default, this value
    is 1%.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-configure(1) cere-profile(1) cere-report(1) cere-select-ilp(1)
