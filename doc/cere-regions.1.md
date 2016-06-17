cere regions(1) -- List extractible regions
==================================================================

## SYNOPSIS

```
cere regions [-h] [--static]
```

## DESCRIPTION

**cere regions** list extractible regions. To achieve this all regions are
outlined at compilation time and listed in the file **regions.csv**. this file
contains for each region, informations such as the file and the function from
where the region is outlined, the start line and coverage measures. To get regions
coverage, you must first run **cere profile**. If you don't need coverage
informations use **cere regions --static**

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--static`:
    With the **static** flag, all regions are outlined and listed but the binary
    is not executed. So keep in mind that a region may not be extractible as it
    is not necessarily executed. Also coverage informations are not available.

## OUTPUT FILES

  * `regions.csv`:
    File containing for each region, the region name, region location, and
    coverage informations.

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-profile(1)
