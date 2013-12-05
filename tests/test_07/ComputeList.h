/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef COMPUTELIST_H
#define COMPUTELIST_H

#include <stdio.h>

class SelfCompute;
class PairCompute;
class PatchList;

class ComputeList {
public:

  int numSelfComputes;
  int numPairComputes;

  SelfCompute *selfComputes;
  PairCompute *pairComputes;

  ComputeList();
  ~ComputeList();

  int readfile(FILE *, PatchList *);

  void runComputes(PatchList *);

};

#endif // COMPUTELIST_H

