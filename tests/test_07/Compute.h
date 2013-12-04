/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef COMPUTE_H
#define COMPUTE_H

#include <stdio.h>
#include "ComputeNonbondedUtil.h"
class PatchList;

class SelfCompute : private ComputeNonbondedUtil {
public:

  SelfCompute() {};
  ~SelfCompute() {};

  int patchId;

  void doWork(PatchList *);

};

class PairCompute : private ComputeNonbondedUtil {
public:

  PairCompute() {};
  ~PairCompute() {};

  int patchId1;
  int image1;
  int patchId2;
  int image2;

  void doWork(PatchList *);

};

#endif // COMPUTE_H

