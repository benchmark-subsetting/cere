cere tutorial(1) -- Example on how to use CERE
====================================================================

## SYNOPSIS

CERE allows to capture a region of code and to replay it whithout running the
entire program. Costly optimization and profiling process, such as iterative
compilation or architecture selection, can be accelerated through CERE fast
replay.

CERE automatically profiles, captures and replays every extractible region of your
application and produces an html report that summarizes the results.

To orchestrate CERE
operations one uses the `cere` command-line program which is located at the root
of CERE distribution. In this tutorial we will assume that cere directory is
located at `~/cere/`. The first step is including cere root directory in the
`PATH` environment variable:

```bash
$ export PATH=~/cere/:$PATH
```

Now try executing the `cere --help` command. As the output shows, `cere` includes
a number of sub commands that will be used during this tutorial:

```bash
user@ix:~/cere/$ ../cere --help
INFO 06/10/2015 10:31:59 CERE : Start
usage: cere [-h]
            {configure,profile,capture,replay,test,select-max-cov,select-ilp,instrument,trace,check,regions,report,io-checker,selectinv}
            ...

CERE command line

positional arguments:
  {configure,profile,capture,replay,test,select-max-cov,select-ilp,instrument,trace,check,regions,report,io-checker,selectinv}
                        Call CERE modules
    configure           Configure CERE to build and run an application
    profile             Profiles an application
    capture             captures a region
    replay              replay a region
    test                Test the matching for a list of region
    select-max-cov      Select regions to maximise the matching coverage
    select-ilp          Select matching regions
    instrument          Instrument a region in the application
    trace               produce or read a region trace
    check               Compare for a given region, the assembly between
                        original region and replay region
    regions             List extractible regions
    report              Generates the html report for an application
    io-checker          Check if a region does IOs
    selectinv           select representatives invocations from region trace

optional arguments:
  -h, --help            show this help message and exit
```

In this tutorial we will use CERE to replay the most important regions of the
Block Tri-diagonal (BT) solver from the
[NAS-SER 3.0 benchmarks](http://http://www.nas.nasa.gov/).

## CONFIGURATION OF BT

First enter the BT benchmark directory:

```bash
$ cd ~/cere/examples/NPB3.0-SER/BT/
```

CERE capture and replay require specific LLVM compiler passes. To easily compile
an application, CERE includes a compiler wrapper, `ccc`, which is located at
`~/cere/src/ccc/ccc`. You can either use `ccc` directly to compile and link a
program, or modify the `Makefile` so it uses `ccc`.

For BT the `Makefile` has already been configured to use `ccc`. The file
`~cere/example/NPB3.0-SER/config/make.def` contains the following definitions:

```m4
     F77 	= ../../../src/ccc/ccc ${MODE} ${INSTRU} ${INSTRU_OPTS}
     FLINK	= ../../../src/ccc/ccc ${MODE} ${INSTRU} ${INSTRU_OPTS}
     CC 	= ../../../src/ccc/ccc ${MODE} ${INSTRU} ${INSTRU_OPTS}
     CLINK 	= ../../../src/ccc/ccc ${MODE} ${INSTRU} ${INSTRU_OPTS}
```

## CONFIGURATION OF CERE

Now we have to tell CERE which commands must be used to build and run the
application. For this we use cere-configure(1) with the following arguments

```bash
cere configure --build-cmd="make CLASS=A" --run-cmd="../bin/bt.A"
```

cere-configure(1) saves the project configuration in the file `cere.json`.
This file is read by most of CERE passes. You can manually edit this file if you
wish to change the initial values.

## PROFILE THE APPLICATION

To determine which are the regions of interest, CERE must profile the application
and the contribution of each region.  cere-profile(1) is used to determine the
application runtime and the percentage of execution time for each extractible
region using [Google gperftools](https://code.google.com/p/gperftools/). This
command also captures the region call graph.

### Measuring application runtime

To measure the application runtime, probes are inserted at the very beginning of
the main function and at the application exit. RDTSC is used to count CPU cycles
between these two probes. To only run the application runtime measure, type:

```bash
cere profile --app
```

The application runtime is saved in the file `.cere/profile/app_cycles.csv`.

### Region instrumentation

Before doing any profiling or optimization work with CERE, one must select
a representative subset of regions. Usually one focus on the hot spot regions that
contribute the most to the total execution time.

cere-profile(1) can measure the contribution of each region and capture the region
call graph. It outlines every extractible region as independent functions which it
profiles using
[Google gperftools CPU profiler](https://code.google.com/p/gperftools/).  The
output is then parsed with *pprof* and converted to CERE internal callgraph
representation.

`
cere profile --regions
`

This command generates the following output files:

    * `.cere/profile/graph_.dot`: dot representation of the region call graph.

    * `.cere/profile/graph_.pdf`: pdf representation of the region call graph.

    * `.cere/profile/app.prof`: the Google raw perftool output.

    * `.cere/profile/graph_.pkl`: cere internal representation of the region call graph.

### Full profiling

To generate the full profiling type:

```
cere profile
```

For more option see also cere-profile(1)

## LIST EXTRACTIBLE REGIONS

With **cere profile --regions** you can already have a visual idea of what are
the important regions in your application. But you can also list them in a file.
With the command:

**cere regions**

**Output files:**
    * regions.csv: File containing for each region, the region name, region
    location, and coverage informations.

For more option see also cere-regions(1)

## AUTOMATIC SELECTORS

One of main CERE goal is to provide a set of codelets that represent the best
the application.
CERE can automatically select the best regions depending on what you want.
CERE provide two selectors:

1. select-max-cov: this selector choose the best regions to maximise the application
coverage with codelets. This method maximise the coverage regardless replay cost.
2. select-ilp: this selector also choose regions to maximise the coverage but with a constraint:
we can't select nested regions. This method gives the best coverage while minimizing
the replay cost.

For both selectors, the process is the same:

1. CERE choose in the graph generated by the profiling pass a set of
interesting regions. Interesting regions are by default regions with coverage
greater than 1%. This threshold is configurable.
2. CERE extract and replay the choosen regions. For each region, the error
between the replay and the original execution is calculated. By default if this
error is lower than 15% we consider this region as a valid one. This threshold is
also configurable.
3. The graph is updated, and the process restart at step 1.

### Use select-max-cov

```
cere select-max-cov
```

### Use select-ilp

```
cere select-ilp
```

You can find more information in cere-select-max-cov(1) and cere-select-ilp(1)

## CERE REPORT

CERE provides a report tool to visualize in html format several informations
about the extraction process for your application. You can find an example of
report for BT in CERE_PATH/doc/images/BT_report.html.
To generate the report:

**cere report**

**Output files:**
    * BT.html: The report of the selection process.

## GO FURTHER

Coming soon

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-trace(1) cere-capture(1) cere-replay(1)
