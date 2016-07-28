cere OMP tutorial(1) -- Example on how to use CERE for OpenMP application
==========================================================================

## SYNOPSIS

CERE allows to capture a region of code and to replay it whithout running the
entire program. Costly optimization and profiling process, such as iterative
compilation or architecture selection, can be accelerated through CERE fast
replay.

CERE automatically profiles, captures and replays every extractible region of your
application and produces an html report that summarizes the results.

To orchestrate CERE operations one uses the `cere` command-line program.
Try executing the `cere --help` command. As the output shows, `cere` includes
a number of sub commands that will be used during this tutorial:


    user@ix:~/cere/$ cere --help
    INFO 06/10/2015 10:31:59 CERE : Start
    usage: cere [-h] [--version]
                {configure,profile,capture,replay,check-matching,select-max-cov,select-ilp,
                 instrument,trace,check,regions,report,selectinv,flag,hybrid}
                ...

    CERE command line

    positional arguments:
        {configure,profile,capture,replay,check-matching,select-max-cov,select-ilp,
         instrument,trace,check,regions,report,selectinv,flag,hybrid}
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
        selectinv           select representatives invocations from region trace
        flag                test flags performance
        hybrid              test optimal optimization flags

    optional arguments:
      -h, --help            show this help message and exit


In this tutorial we will use CERE to replay the most important regions of the
Block Tri-diagonal (BT) solver from the
[NAS-C-OpenMP3.0 benchmarks](http://benchmark-subsetting.github.io/cNPB/).

## CONFIGURATION OF BT

First enter the BT benchmark directory:

    $ cd ~/cere/examples/NPB3.0-omp-C-master/BT/

CERE capture and replay require specific LLVM compiler passes. To easily
compile an application, CERE includes a compiler wrapper, `cerec`.  You can
either use `cerec` directly to compile and link a program, or modify the
`Makefile` so it uses `cerec`.

For IS the `Makefile` has already been configured to use `cerec`. The file
`~cere/example/NPB3.0-omp-C-master/config/make.def` contains the following definitions:

    CC = cerec
    CLINK = cerec

## CONFIGURATION OF CERE

Now we have to tell CERE which commands must be used to build and run the
application. For this we use cere-configure(1) with the following arguments

    $ cere configure --build-cmd="make CLASS=A" --clean-cmd="make clean" --run-cmd="../bin/bt.A" --omp

cere-configure(1) saves the project configuration in the file `cere.json`.
This file is read by most of CERE passes. You can manually edit this file if you
wish to change the initial values. With the --omp flag we tell CERE that we are
working with an OpenMP application.

## PROFILE THE APPLICATION

To determine which are the regions of interest, CERE must profile the application
and the contribution of each region. cere-profile(1) is used to determine the
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
[Google gperftools CPU profiler](https://code.google.com/p/gperftools/). The
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

As you can see in `regions.csv`, the region `__cere__bt_adi_211` covers
around 30% of BT runtime. It means that if we can successfully replay this region,
we could predict 30% of BT execution time with only few executions of the region.

### Find representative invocations

adi_211 region is called 201 times in BT. It is too costly to capture the
memory and call state for each and every one of the invocations. CERE includes an
invocation clustering step that can reduce the 201 invocations to a small set of
representative invocations that are enough to capture the behavior of the region.

To find representative invocations, cere-selectinv(1) reads the trace generated by
cere-trace(1) and clusterizes the invocations. One invocation per cluster is
selected to represent its cluster.

To trace invocations execution time run:

    $ cere trace --region=__cere__bt_adi_211

This command generates the following output files:

* `.cere/traces/__cere__bt_adi_211.bin`: Trace of invocations.

* `.cere/traces/__cere__bt_adi_211.csv`: Cumulative invocations
    execution time and call count.

To select representative invocations run:

    $ cere selectinv --region=__cere__bt_adi_211

This command generates the following output file:

* `.cere/traces/__cere__bt_adi_211.invocations`:
    Each row correspond to a cluster with row N stands for the cluster N. Each
    row contains the cluster representative invocation, the invocation execution
    time in cycle and the part of the cluster in the total execution time of the
    region.

* `.cere/plots/__cere__bt_adi_211_byPhase.png`:
    Image of the trace clustering.

As you can see in these 2 files, the region runtime can be simulated by only
replaying 2 invocations instead of the 201 original invocations. The number and
value of representative invocations can vary from a machine to another so you may
have more or less representative invocations.

### Capturing representative invocations

Last step before replaying `__cere__bt_adi_211` region, is to capture
using cere-capture(1), the memory and cache state for the 2 invocations we need
to replay. It is important that you carefully set the number of threads when
capturing invocations with `export OMP_NUM_THREADS=N`.
This is done with the following command:

    $ export OMP_NUM_THREADS=1
    $ cere capture --region=__cere__bt_adi_211

This step might take a long time depending on your machine.
This command generates a set of files in
`.cere/dumps/__cere__bt_adi_211/INVOCATIONS/` which is needed to
restore the memory and the cache state in order to replay the region.

### Replaying the region

Finally we can replay with cere-replay(1) the region of interest. Only the 2
selected invocations are replayed, and are used to simulate the region total runtime.
You can change the number of threads at replay (i.e. capture with one thread and replay
with 4). But the replay faithfullness to the original performance is only guaranteed
if you replay with a lower number of thread than at capture. For this tutorial,
we replay with 2 threads to estimate the impact of multi-threading.

    $ export OMP_NUM_THREADS=2
    $ cere replay --region=__cere__bt_adi_211

This command generates the file
`.cere/replays/__cere__bt_adi_211_INVOCATION` which contains the
replay execution time of the region multiplied by the CERE_REPLAY_REPETITIONS.

### Replay output

Replay command outputs in the terminal, the runtime of each invocvation replayed
in cycles, and the simulated runtime of the region based on representative values.

    INFO 07/28/2016 16:20:01 Replay : Replaying with 2 threads
    INFO 07/28/2016 16:20:17 Replay : Predicted cycles for region: __cere__bt_adi_211
    INFO 07/28/2016 16:20:17 Replay :  Invocation 68: In vitro cycles = 319963382.2 (part = 200.017373494)
    INFO 07/28/2016 16:20:17 Replay :  Invocation 13: In vitro cycles = 316362337.2 (part = 1.0)
    INFO 07/28/2016 16:20:17 Replay :  Overall predicted cycles = 64314597659.1

The real runtime of the region can be found in `.cere/traces/__cere__bt_adi_211.csv`
In our example the value is:

    $ cat .cere/traces/__cere__bt_adi_211.csv
    Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE
    __cere__bt_adi_211,201,109836492303

In our example the measured value is 109836492303 cycles.

### Prediction speedup

By default, CERE replays each invocation 10 times. CERE includes different warmup
modes: some of them are inaccurate but very fast, while others are more costly but
better capture the cache state. Depending on the replay mode and the architecture,
using CERE to measure this region achieves a 100 to 1000 speedup.

The original performance of this region with one thread is 109836492303 cycles
while replaying it with two threads gave 64314597659 cycles. CERE then predicts
a speedup of 1.7 when running this region from 1 to 2 threads.

## AUTOMATIC REGION VALIDATOR

It is important before using a region to guarantee that the replay actually
matches the original behavior. Otherwise what you observe in replay may not be
what is trully happening in the real execution of the
application. cere-check-matching(1) automatically executes the steps described in
the previous section and tells you if the region is matching or not. The command
to run is:

    cere check-matching --region=___cere__bt_adi_211

The tail of the command output should looks like this:

    ...
    INFO 07/28/2016 16:50:17 Check-matching : Results for region: __cere__bt_adi_211
    INFO 07/28/2016 16:50:17 Check-matching :   MATCHING: In vitro = 1.00675017365e+11 & invivo = 1.09836492303e+11 (error = 8.34101194038%, coverage = 28.43%)
    INFO 07/28/2016 16:50:17 Check-matching :       Invocation 68: In vitro cycles = 500835238.3 & in vivo cycles = 546118362.0 (error = 8.29181489781%, part = 200.017373494)
    INFO 07/28/2016 16:50:17 Check-matching :       Invocation 13: In vitro cycles = 499268447.1 & in vivo cycles = 603331919.0 (error = 17.2481296982%, part = 1.0)
    INFO 07/28/2016 16:50:17 CERE : Stop

**CERE** tells us that `___cere__bt_adi_211` is matching and can then
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

An example of report generated for the BT class A serial benchmark can be found
[here](https://benchmark-subsetting.github.io/cere/reports/NAS3.0-SER/BT.html).

## COPYRIGHT

cere is Copyright (C) 2014-2015 Université de Versailles St-Quentin-en-Yvelines

## SEE ALSO

cere-configure(1) cere-trace(1) cere-profile(1) cere-capture(1) cere-regions(1)
cere-replay(1) cere-check-matching(1) cere-report(1) cere-select-max-cov(1)
cere-tutorial(1)
