/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include "PatchList.h"
#include "Patch.h"
#include "ReductionMgr.h"
#include "ResultSet.h"
#include <math.h>

PatchList::PatchList(Lattice &l) : lattice(l) {
  numPatches = -1;
  patches = 0;
  reductionData = new double[reductionDataSize];
  doEnergy = doFull = doMerge = 0;
}

PatchList::~PatchList() {
  delete [] patches;
  delete [] reductionData;
}

void PatchList::moveatoms() {
  int i;
  for ( i=0; i<numPatches; ++i ) {
    patches[i].moveatoms();
  }
}

void PatchList::zeroresults() {
  int i;
  for ( i=0; i<numPatches; ++i ) {
    patches[i].zeroforces();
  }
  for ( i = 0; i < reductionDataSize; ++i ) {
    reductionData[i] = 0;
  }
}

void PatchList::setresults(ResultSet *r) {
  r->doEnergy = doEnergy;
  r->doFull = doFull;
  r->doMerge = doMerge;
  int i;
  for ( i=0; i<numPatches; ++i ) {
    patches[i].setforces(r->f_nbond,r->f_slow);
  }
  for ( i = 0; i < outputDataSize; ++i ) {
    r->reductions[i] = reductionData[i];
  }
  r->netf_nbond = 0.;
  r->netf_slow = 0.;
  r->netf_both = 0.;
  r->rmsf_nbond = 0.;
  r->rmsf_slow = 0.;
  r->rmsf_both = 0.;
  for ( i=0; i<r->numAtoms; ++i ) {
    Vector both = r->f_nbond[i] + r->f_slow[i];
    r->netf_nbond += r->f_nbond[i];
    r->netf_slow += r->f_slow[i];
    r->netf_both += both;
    r->rmsf_nbond += r->f_nbond[i].length2();
    r->rmsf_slow += r->f_slow[i].length2();
    r->rmsf_both += both.length2();
  }
  r->rmsf_nbond = sqrt( r->rmsf_nbond / r->numAtoms );
  r->rmsf_slow = sqrt( r->rmsf_slow / r->numAtoms );
  r->rmsf_both = sqrt( r->rmsf_both / r->numAtoms );
}

int PatchList::readfile(FILE *file) {
  
  char buf[1024];

  if ( ! fgets(buf,1024,file) || strcmp(buf,"PATCHLIST_BEGIN\n") ) {
    printf("Missing PATCHLIST_BEGIN\n");
    return -1;
  }

  if ( fscanf(file,"%d\n",&numPatches) != 1 ) {
    printf("numPatches read error\n");
    return -2;
  }

  patches = new Patch[numPatches];

  int i;
  for ( i=0; i<numPatches; ++i ) {
    if ( patches[i].readfile(file) ) {
      printf("patch read error\n");
      return -3;
    }
  }

  if ( ! fgets(buf,1024,file) || strcmp(buf,"PATCHLIST_END\n") ) {
    printf("Missing PATCHLIST_END\n");
    return -4;
  }

  return 0;
}

