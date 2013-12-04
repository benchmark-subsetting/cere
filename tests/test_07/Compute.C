/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include "Compute.h"
#include "PatchList.h"
#include "Patch.h"

void SelfCompute::doWork(PatchList *patchList) {

    Patch *p1 = &(patchList->patches[patchId]);
    int doEnergy = patchList->doEnergy;
    nonbonded params;
    params.p[0] = p1->atoms;
    params.p[1] = p1->atoms;
    params.ff[0] = p1->f_nbond;
    params.ff[1] = p1->f_nbond;
    params.numAtoms[0] = p1->numAtoms;
    params.numAtoms[1] = p1->numAtoms;
    params.reduction = patchList->reductionData;
    params.pressureProfileReduction = 0;

    params.minPart = 0; // minPart;
    params.maxPart = 1; // maxPart;
    params.numParts = 1; // numParts;

    if ( patchList->doFull ) {
      params.fullf[0] = p1->f_slow;
      params.fullf[1] = p1->f_slow;
      if ( patchList->doMerge ) {
        if ( doEnergy ) calcMergeSelfEnergy(&params);
        else calcMergeSelf(&params);
      } else {
        if ( doEnergy ) calcFullSelfEnergy(&params);
        else calcFullSelf(&params);
      }
    }
    else
      if ( doEnergy ) calcSelfEnergy(&params);
      else calcSelf(&params);

}

void PairCompute::doWork(PatchList *patchList) {

    Patch *p1 = &(patchList->patches[patchId1]);
    Patch *p2 = &(patchList->patches[patchId2]);
    int doEnergy = patchList->doEnergy;
    nonbonded params;
    params.p[0] = p1->image(image1,patchList->lattice);
    params.p[1] = p2->image(image2,patchList->lattice);
    params.ff[0] = p1->f_nbond;
    params.ff[1] = p2->f_nbond;
    params.numAtoms[0] = p1->numAtoms;
    params.numAtoms[1] = p2->numAtoms;
    params.reduction = patchList->reductionData;
    params.pressureProfileReduction = 0;

    params.minPart = 0; // minPart;
    params.maxPart = 1; // maxPart;
    params.numParts = 1; // numParts;

    if ( patchList->doFull ) {
      params.fullf[0] = p1->f_slow;
      params.fullf[1] = p2->f_slow;
      if ( patchList->doMerge ) {
        if ( doEnergy ) calcMergePairEnergy(&params);
        else calcMergePair(&params);
      } else {
        if ( doEnergy ) calcFullPairEnergy(&params);
        else calcFullPair(&params);
      }
    }
    else
      if ( doEnergy ) calcPairEnergy(&params);
      else calcPair(&params);

}

