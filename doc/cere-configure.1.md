cere configure(1) -- Configures CERE to build and run an application
====================================================================

## SYNOPSIS

```
cere configure [-h] --run-cmd=RUN_CMD --build-cmd=BUILD_CMD --clean-cmd=CLEAN_CMD
               [--multiple-trace]
```

## DESCRIPTION

The first step before using **CERE** on an application is running **cere
configure**. This command tells **CERE** which commands must be used to build and
run the application.

## OPTIONS

  * `-h`:
    Prints a synopsis and a list of the most commonly used options.

  * `--run-cmd=RUN_CMD`:
    Sets the command used to run the application.

  * `--build-cmd=BUILD_CMD`:
    Sets the command used to build the application.

  * `--clean-cmd=CLEAN_CMD`:
    Sets the command used to clean the application.

  * `--multiple-trace`:
    Enables tracing multiple regions in a single run.  By default, multiple
    tracing is disabled and cere-trace(1) will only trace the requested region.
    If **--multiple-trace** is activated, cere-trace(1) traces all possible regions
    in a single run (regions must be non-nested). This can considerably decrease
    the tracing cost.

## EXAMPLES

Given a simple application `app` that is built using the following `Makefile`:

```make
all: app
app: app.o
     $(LD) -o $@ $<
app.o: app.c
     $(CC) -c $<
clean:
     rm app *.o *.ll
```

The user should call **cere configure** with the following arguments:

```
cere configure --build-cmd="make CC=ccc LD=ccc"
               --clean-cmd="make clean"
               --run-cmd="./app"
```

## OUTPUT FILES

  * `cere.json`:
    the configuration file. This file is read by most of **CERE**
    passes. You can manually edit this file.

## CONFIGURING ENVIRONMENT VARIABLES

First of all, CERE uses the `CERE_MODE` variable to determine the compliation mode (default, capture, replay, ...). The build command will be run in an environment where this variable is defined. In case of a more "complex" build sytem (such as autotools or CMake), the user can simply refer to this variable with `$CERE_MODE` to ensure that the variable is passed correctly along the whole compilation process.

Moreover, CERE will look for the `.cere` directory, and run all the commands in the working directory by default. In order to change this beheviour, you can define the `CERE_WORKING_PATH` environment variable. If not defined, this variale will default to the current directory.

In order to further refine this behaviour : 

- build and clean commands are launched from the path pointed by the `CERE_BUILD_PATH` variable. If not defined, this variable defaults to `CERE_WORKING_PATH`

- run commands are launched from the path pointed by the `CERE_RUN_PATH` variable. If not defined, this variable defaults to `CERE_WORKING_PATH`


## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-trace(1) cere-capture(1) cere-replay(1)
