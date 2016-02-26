cere hybrid(1) -- Create an hybrid binary
==================================================================

## SYNOPSIS

```
cere hybrid [-h] --regions-file REGIONS_FILE
```

## DESCRIPTION

**cere hybrid** allows the user to create an hybrid executable of the
application, where each region can be compile with a specific set of flags read
from **--regions-file** option. Each region is extracted into a new file, which
is then compiled with the selected flags. These files are then linked to
generate the hybrid binary.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--regions-file REGIONS_FILE`:
    Input file where you can list for each region the choosen compilation flags.  
    Format must be a comma seperated csv file:  
      region,flag  
      \_\_cere\_\_sundials_dense_denseGETRF_82,-msse4.2  
      \_\_cere\_\_sundials_dense_denseGETRS_154,-fno-vectorize  
    

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-flag(1) cere-configure(1)
