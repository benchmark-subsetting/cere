cere hybrid(1) -- Create an hybrid binary
==================================================================

## SYNOPSIS

```
cere hybrid [-h] --regions-file REGIONS_FILE
            [--extraction-lvl {loop,function}] [--instrumentation]
```

## DESCRIPTION

**cere hybrid** allows the user to create an hybrid executable of the
application, where each region can be compile with a specific set of flags read
from **--regions-file** option. You can create this file either manually or
automatically with cere-flag(1). Each region is extracted into a new file,
which is then compiled with the selected flags. These files are then linked to
generate the hybrid binary.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--regions-file REGIONS_FILE`:
    Input file that list for each region the choosen compilation flags. This
    file is created with cere-flag(1).
    You can also create it manually, format and example can be found here:
    `CERE_PATH/tests/test_UI_Hybrid/.cere/flags/regions_flags.csv`

  * `[--extraction-lvl {loop,function}]`
    Sometimes extracting the loop alone into a new file does not trigger desired
    optimizations since we loose the compilation context and therefore the hybrid
    performance will not match what you expected with replays. You can then
    choose to extract the entire function that contains the region of interest to
    preserve the context. Default is loop level extraction.
    Limitation for function level: Only one region to extract per function.

  * `[--instrumentation]`
    Measure the hybrid execution cycles. Cycles are dumped into `main.csv` file.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-flag(1) cere-configure(1)
