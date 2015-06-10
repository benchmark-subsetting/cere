cere check-select-ilp(1) -- Automatic regions selector
========================================================

## SYNOPSIS

```
cere select-ilp [-h] [--max-error MAX_ERROR] [--min-coverage MIN_COVERAGE]
           [--force]
```

## DESCRIPTION

**cere select-ilp** automatically selects the best set of matching regions to
maximize the application coverage and minimize the replay cost. **cere select-ilp**
calls cere-check-matching(1) on all regions, and selects among the matching ones
the best set of codelets. The optimization problem is formulated as an Integer
Linear Programming problem. cere-profile(1) must be executed before running **cere
select-ilp**. cere-report(1) can be then used to visualize the selection summary.

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

cere-configure(1) cere-profile(1) cere-report(1) cere-select-max-cov(1)
