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

    $ export PATH=~/cere/:$PATH

Now try executing the `cere --help` command. As the output shows, `cere` includes
a number of sub commands that will be used during this tutorial:


    user@ix:~/cere/$ ./cere --help
    INFO 06/10/2015 10:31:59 CERE : Start
    usage: cere [-h]
                {configure,profile,capture,replay,check-matching,select-max-cov,select-ilp,
                 instrument,trace,check,regions,report,io-checker,selectinv}
                ...

    CERE command line

    positional arguments:
        {configure,profile,capture,replay,check-matching,select-max-cov,select-ilp,
         instrument,trace,check,regions,report,io-checker,selectinv}
                            Call CERE modules
        configure           Configure CERE to build and run an application
        profile             Profiles an application
        capture             captures a region
        replay              replay a region
        check-matching      Test the matching for a list of region
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


In this tutorial we will use CERE to replay the most important regions of the
discrete 3D fast Fourier Transform (FT) solver from the
[NAS-SER 3.0 benchmarks](http://www.nas.nasa.gov/).

## CONFIGURATION OF FT

First enter the FT benchmark directory:

    $ cd ~/cere/examples/NPB3.0-SER/FT/

CERE capture and replay require specific LLVM compiler passes. To easily compile
an application, CERE includes a compiler wrapper, `ccc`, which is located at
`~/cere/src/ccc/ccc`. You can either use `ccc` directly to compile and link a
program, or modify the `Makefile` so it uses `ccc`.

For FT the `Makefile` has already been configured to use `ccc`. The file
`~cere/example/NPB3.0-SER/config/make.def` contains the following definitions:


    F77 = ../../../src/ccc/ccc ${MODE}
    FLINK = ../../../src/ccc/ccc ${MODE}
    CC = ../../../src/ccc/ccc ${MODE}
    CLINK = ../../../src/ccc/ccc ${MODE}


## CONFIGURATION OF CERE

Now we have to tell CERE which commands must be used to build and run the
application. For this we use cere-configure(1) with the following arguments

    $ cere configure --build-cmd="make CLASS=A" --run-cmd="../bin/ft.A"

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

    $ cere profile --app

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

    $ cere profile --regions

This command generates the following output files:

* `.cere/profile/graph_.dot`: dot representation of the region call graph.

* `.cere/profile/graph_.pdf`: pdf representation of the region call graph.

* `.cere/profile/app.prof`: the Google raw perftool output.

* `.cere/profile/graph_.pkl`: cere internal representation of the region call graph.

### Full profiling

The previous two profiling steps (application and regions) can be combined in a
single cere command:

    $ cere profile

## LISTING EXTRACTIBLE REGIONS

At any moment one can list the extractible regions, their source code location,
and their contribution to the total running time with the following command:

    $ cere regions

This command outputs the file `regions.csv` containing for each region, the region
name, region location, and coverage informations. If no profile information is
available cere-regions(1) will still output the region information but it will
lack per-region execution time.

## REPLAYING A REGION

As you can see in `regions.csv`, the region `__cere__fft3d_swarztrauber__27` covers
around 66% of FT runtime. It means that if we can successfully replay this region,
we could predict 66% of FT execution time with only few executions of the region.

### Find representative invocations

Swarztauber region is called 32768 times in FT. It is too costly to capture the
memory and call state for each and every one of the invocations. CERE includes an
invocation clustering step that can reduce the 32768 invocations to a small set of
representative invocations that are enough to capture the behavior of the region.

To find representative invocations, cere-selectinv(1) reads the trace generated by
cere-trace(1) and clusterizes the invocations. One invocation per cluster is
selected to represent its cluster.

To trace invocations execution time run:

    $ cere trace --region=__cere__fft3d_swarztrauber__27

This command generates the following output files:

* `.cere/traces/__cere__fft3d_swarztrauber__27.bin`: Trace of invocations.

* `.cere/traces/__cere__fft3d_swarztrauber__27.csv`: Cumulative invocations
    execution time and call count.

To select representative invocations run:

    $ cere selectinv --region=__cere__fft3d_swarztrauber__27

This command generates the following output file:

* `.cere/traces/__cere__fft3d_swarztrauber__27.invocations`:
    Each row correspond to a cluster with row N stands for the cluster N. Each
    row contains the cluster representative invocation, the invocation execution
    time in cycle and the part of the cluster in the total execution time of the
    region.

* `.cere/plots/__cere__fft3d_swarztrauber__27_byPhase.png`:
    Image of the trace clustering.

As you can see in these 2 files, the region runtime can be simulated by only
replaying 2 invocations instead of the 32768 original invocations.  The number and
value of representative invocations can vary from a machine to another so you may
have more or less representative invocations.

### Capturing representative invocations

Last step before replaying `__cere__fft3d_swarztrauber__27` region, is to capture
using cere-capture(1), the memory and cache state for the 2 invocations we need
to replay. This is done with the following command:

    $ cere capture --region=__cere__fft3d_swarztrauber__27

This command generates a set of files in
`.cere/dumps/__cere__fft3d_swarztrauber__27/INVOCATIONS/` which is needed to
restore the memory and the cache state in order to replay the region.

### Replaying the region

Finally we can replay with cere-replay(1) the region of interest. Only the 2
selected invocations are replayed, and are used to simulate the region total runtime.

    $ cere replay --region=__cere__fft3d_swarztrauber__27

This command generates the file
`.cere/replays/__cere__fft3d_swarztrauber__27_INVOCATION` which contains the
replay execution time of the region multiplied by the INVITRO_CALLCOUNT.

### Replay output

Replay command outputs in the terminal, the runtime of each invocvation replayed
in cycles, and the simulated runtime of the region based on representative values.

    INFO 06/11/2015 15:36:59 Replay : Predicted cycles for region: __cere__fft3d_swarztrauber__27
    INFO 06/11/2015 15:36:59 Replay :  Invocation 17632: In vitro cycles = 279057.6 (part = 15955.7846108)
    INFO 06/11/2015 15:36:59 Replay :  Invocation 2438: In vitro cycles = 114033.6 (part = 17341.9549768)
    INFO 06/11/2015 15:36:59 Replay :  Overall predicted cycles = 6430148516.65

The real runtime of the region can be found in `.cere/traces/__cere__fft3d_swarztrauber__27.csv`
In our example the value is:

    $ cat .cere/traces/__cere__fft3d_swarztrauber__27.csv
    Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE
    __cere__fft3d_swarztrauber__27,32768,6158949523

In our example the measured value is 6158949523 cycles.

### Prediction speedup and accuracy

By default, CERE replays each invocation 10 times. CERE includes different warmup
modes: some of them are inaccurate but very fast, while others are more costly but
better capture the cache state. Depending on the replay mode and the architecture,
using CERE to measure this region achieves a 100 to 1000 speedup.

The prediction accuracy is also high: **CERE** predicts a runtime of 6430148516
while it is really 6158949523. The prediction error is therefore 4%.

## AUTOMATIC REGION VALIDATOR

It is important before using a region to guarantee that the replay actually
matches the original behavior. Otherwise what you observe in replay may not be
what is trully happening in the real execution of the
application. cere-check-matching(1) automatically executes the steps described in
the previous section and tells you if the region is matching or not. The command
to run is:

    cere check-matching --region=__cere__fft3d_swarztrauber__27

The tail of the command output should looks like this:

    ...
    INFO 06/11/2015 16:24:31 Check-matching : Results for region: __cere__fft3d_swarztrauber__27
    INFO 06/11/2015 16:24:31 Check-matching :   MATCHING: In vitro = 6430148516.65 & invivo = 6158949523.0 (error = 4.21761632639%, coverage = 66.1%)
    INFO 06/11/2015 16:24:31 Check-matching :     Invocation 17632: In vitro cycles = 279057.6 & in vivo cycles = 269038.0 (error = 3.59051321304%, part = 15955.7846108)
    INFO 06/11/2015 16:24:31 Check-matching :     Invocation 2438: In vitro cycles = 114033.6 & in vivo cycles = 107614.0 (error = 5.62956882884%, part = 17341.9549768)

**CERE** tells us that `__cere__fft3d_swarztrauber__27` is matching and can then
be used to predict its original runtime.

## AUTOMATIC REGION SELECTORS

One of CERE's tasks is to extract the set of codelets that best represents a given
application. What constitutes the "best set" of representatives really depends on
your objective. For some applications one wants a full set of codelets that best
captures the execution time of the application; in other cases one prefers a
smaller set of codelets that is much faster to replay. CERE provide two selectors:

1. select-max-cov: chooses the set of regions that maximise the application
coverage with codelets. This method maximises the coverage regardless of the replay cost.

2. select-ilp: chooses a set of regions that provide a good tradeoff between
coverage and replay-cost. The optimization problem is formulated as an Integer
Linear Programming problem.

In both cases, the overall process is similar.  CERE chooses in the graph
generated by the profiling pass a set of interesting regions. Interesting regions
are by default regions with coverage greater than 1%. This threshold can be
configured by the user.

CERE extracts and replay the choosen regions. For each region, the error between
the replay and the original execution is calculated. By default if this error is
lower than 15% we consider this region as a valid one. This threshold is also
configurable.

You can try both selectors by running one (or all) of the following commands:

    $ cere select-max-cov
    $ cere select-ilp

You can find more information in the manual pages for cere-select-max-cov(1) and
cere-select-ilp(1).

## CERE REPORT

CERE provides a report tool to visualize in html format several informations
about the extraction process of your application. You can call cere-report(1)
after any cere command.
To generate the report you can use the following command:

    $ cere report

An example of report generated for the FT class A serial benchmark can be found
[here](https://benchmark-subsetting.github.io/cere/reports/NAS3.0-SER/FT.html).

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-configure(1) cere-trace(1) cere-profile(1) cere-capture(1) cere-regions(1)
cere-replay(1) cere-check-matching(1) cere-report(1) cere-select-max-cov(1)
