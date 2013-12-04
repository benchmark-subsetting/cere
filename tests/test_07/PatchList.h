/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef PATCHLIST_H
#define PATCHLIST_H

#include <stdio.h>
#include "Patch.h"
class Lattice;
class ResultSet;

class PatchList {
public:

  int doEnergy;
  int doFull;
  int doMerge;

  int numPatches;

  Patch *patches;

  PatchList(Lattice &);
  ~PatchList();

  int readfile(FILE *);

  Lattice &lattice;

  void zeroresults();

  void moveatoms();

  double *reductionData;

  void setresults(ResultSet *);

};

#endif // PATCHLIST_H

