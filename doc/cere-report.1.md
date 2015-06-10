cere report(1) -- Generates an html summary of regions matching error
=====================================================================

## SYNOPSIS

```
cere report [-h] [--path PATH]
```

## DESCRIPTION

**cere report** creates an html summary that shows regions call tree, regions
matching, regions source code, invocations clustering for each region, the
percentage of execution time covered by matching codelets and a graph to show
the coverage you could reach versus the maximum tolerated error. **cere report**
can be used after calling cere-select-ilp(1) or cere-select-max-cov(1).

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--path PATH`:
    The path of the application to report. Default is the current folder.

## OUTPUT FILES

  * `PATH.html`:
    The html report.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-select-ilp(1) cere-select-max-cov(1)
