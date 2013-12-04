/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "SimParameters.h"
#include "LJTable.h"
#include "PatchList.h"
#include "Molecule.h"
#include "ComputeList.h"
#include "ComputeNonbondedUtil.h"
#include "ResultSet.h"

void exit_usage(const char *argv0) {
  printf("Usage: %s --input <file> [--output <file>] "
          "[--standard <file>] [--iterations <int>]\n",argv0);
  exit(-1);
}

void NAMD_die(const char *msg) {
  printf("error: %s\n",msg);
  exit(-1);
}

int main(int argc, char **argv) {

  char *input = 0;
  char *output = 0;
  char *standard = 0;
  int iterations = -1;

  int i;

  if ( (argc-1)%2 ) exit_usage(argv[0]);
  
  for( i=1; i<argc; i+=2 ) {
    if ( strcmp(argv[i],"--input") == 0 ) input = argv[i+1];
    else if ( strcmp(argv[i],"--output") == 0 ) output = argv[i+1];
    else if ( strcmp(argv[i],"--standard") == 0 ) standard = argv[i+1];
    else if ( strcmp(argv[i],"--iterations") == 0 ) {
      if ( sscanf(argv[i+1],"%d",&iterations) != 1 || iterations < 0 ) {
        printf("argument of --iterations must be an integer >= 0\n");
        exit_usage(argv[0]);
      }
    }
    else {
      printf("unknown argument: %s\n",argv[i]);
      exit_usage(argv[0]);
    }
  }

  if ( !input ) {
    printf("no input file specified\n");
    exit_usage(argv[0]);
  }

  FILE *input_file = fopen(input,"r");
  if ( ! input_file ) {
    printf("unable to open input file\n");
    exit(-2);
  }

  // create objects, then pass file to read

  SimParameters simParams;
  if ( simParams.readfile(input_file) ) {
    printf("error reading SimParameters from input file\n");
    exit(-3);
  }

  LJTable ljTable;
  if ( ljTable.readfile(input_file) ) {
    printf("error reading LJTable from input file\n");
    exit(-4);
  }

  Molecule molecule;
  if ( molecule.readfile(input_file) ) {
    printf("error reading Molecule from input file\n");
    exit(-5);
  }

  ComputeNonbondedUtil::select(&simParams,&molecule,&ljTable);

  PatchList patchList(simParams.lattice);
  if ( patchList.readfile(input_file) ) {
    printf("error reading patchList from input file\n");
    exit(-6);
  }

  ComputeList computeList;
  if ( computeList.readfile(input_file,&patchList) ) {
    printf("error reading computeList from input file\n");
    exit(-7);
  }

  fclose(input_file);

  // calculate iterations for benchmark
  int benchmarkrun = 0;
  if ( iterations < 0 ) {
    double complexity = (double) molecule.numAtoms;
    complexity *= simParams.cutoff;
    complexity *= simParams.cutoff;
    complexity *= simParams.cutoff;
    iterations = (int) ceil(3.0e9 / complexity);
    benchmarkrun = 1;
  }

  if ( benchmarkrun ) {
    printf("*** OFFICIAL BENCHMARK RUN - %d ITERATIONS ***\n",iterations);
  } else {
    printf("*** TEST RUN - %d ITERATIONS ***\n",iterations);
  }

#define RUN_AND_CHECKSUM \
    patchList.zeroresults(); \
    computeList.runComputes(&patchList); \
    if ( patchList.reductionData[exclChecksumIndex] != \
				molecule.numCalcExclusions ) { \
      printf("exclusion checksum failure!\n"); \
      exit(-8); \
    } \

#define SET_MODE(E,F,M) \
    patchList.doEnergy = E; \
    patchList.doFull = F; \
    patchList.doMerge = M; \
    printf("iteration %d: %d %d %d\n",i,E,F,M);

#define WRITE(R) \
    patchList.setresults(&R); \
    if ( output ) R.writefile(output_file); \
    if ( standard ) { \
      comp.readfile(standard_file); \
      R.samemode(comp); \
      R.compare(comp); \
    }

  ResultSet r100(molecule.numAtoms);
  ResultSet r110(molecule.numAtoms);
  ResultSet r111(molecule.numAtoms);
  ResultSet r000(molecule.numAtoms);
  ResultSet r010(molecule.numAtoms);
  ResultSet r011(molecule.numAtoms);
  ResultSet comp(molecule.numAtoms);

  FILE *standard_file = 0;
  if ( standard ) {
    standard_file = fopen(standard,"r");
    if ( ! standard_file ) {
      printf("unable to open standard file\n");
      exit(-2);
    }
    printf("comparing to standard file %s\n",standard);
  }

  FILE *output_file = 0;
  if ( output ) {
    if ( output[0] == '-' ) {
      printf("sending output to stdout\n");
      output_file = stdout;
    } else {
      output_file = fopen(output,"w");
      if ( ! output_file ) {
        printf("unable to open output file\n");
        exit(-2);
      }
      printf("writing to output file %s\n",output);
    }
  }

  for ( i = 0; i < iterations; ++i ) {
    SET_MODE(1,0,0)
    RUN_AND_CHECKSUM
    WRITE(r100)
    SET_MODE(1,1,0)
    RUN_AND_CHECKSUM
    WRITE(r110)
    SET_MODE(1,1,1)
    RUN_AND_CHECKSUM
    WRITE(r111)
    SET_MODE(0,0,0)
    RUN_AND_CHECKSUM
    WRITE(r000)
    SET_MODE(0,1,0)
    RUN_AND_CHECKSUM
    WRITE(r010)
    SET_MODE(0,1,1)
    RUN_AND_CHECKSUM
    WRITE(r011)

    r100.compare(r110);
    r100.compare(r111);
    r100.compare(r000);
    r100.compare(r010);
    r100.compare(r011);

    r110.compare(r111);
    r110.compare(r000);
    r110.compare(r010);
    r110.compare(r011);

    r111.compare(r000);
    r111.compare(r010);
    r111.compare(r011);

    r000.compare(r010);
    r000.compare(r011);

    r010.compare(r011);

    patchList.moveatoms();

  }

  if ( benchmarkrun ) {
    printf("*** OFFICIAL BENCHMARK RUN - %d ITERATIONS ***\n",iterations);
  } else {
    printf("*** TEST RUN - %d ITERATIONS ***\n",iterations);
  }

  printf("SUCCESSFUL COMPLETION\n");
  return 0;
}

