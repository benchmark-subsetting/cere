/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include "ComputeList.h"
#include "Compute.h"
#include "PatchList.h"

ComputeList::ComputeList() {
  numSelfComputes = 0;
  selfComputes = 0;
  numPairComputes = 0;
  pairComputes = 0;
}

ComputeList::~ComputeList() {
  delete [] selfComputes;
  delete [] pairComputes;
}

int ComputeList::readfile(FILE *file, PatchList *patchList) {

  char buf[1024];

  if ( ! fgets(buf,1024,file) || strcmp(buf,"COMPUTEPAIR_BEGIN\n") ) {
    printf("Missing COMPUTEPAIR_BEGIN\n");
    return -1;
  }

  if ( fscanf(file,"%d\n",&numPairComputes) != 1 ) {
    printf("numPairComputes read error\n");
    return -2;
  }

  pairComputes = new PairCompute[numPairComputes];

  int i;
  for ( i=0; i<numPairComputes; ++i ) {
    int p1,i1,p2,i2;
    p1=i1=p2=i2=0;
    if ( fscanf(file,"%d %d %d %d\n",&p1,&i1,&p2,&i2) != 4 ) {
      printf("paircompute read error\n");
      return -3;
    }
    pairComputes[i].patchId1 = p1;
    pairComputes[i].image1 = i1;
    pairComputes[i].patchId2 = p2;
    pairComputes[i].image2 = i2;
  }

  if ( ! fgets(buf,1024,file) || strcmp(buf,"COMPUTEPAIR_END\n") ) {
    printf("Missing COMPUTEPAIR_END\n");
    return -4;
  }

  numSelfComputes = patchList->numPatches;
  selfComputes = new SelfCompute[numSelfComputes];
  for ( i=0; i<numSelfComputes; ++i ) {
    selfComputes[i].patchId = i;
  }

  return 0;
}

void ComputeList::runComputes(PatchList *patchList) {

  int i;

  for ( i=0; i<numSelfComputes; ++i ) {
    selfComputes[i].doWork(patchList);
  }

  for ( i=0; i<numPairComputes; ++i ) {
    pairComputes[i].doWork(patchList);
  }

}

