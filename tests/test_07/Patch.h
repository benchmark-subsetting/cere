/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef PATCH_H
#define PATCH_H

#include "NamdTypes.h"
#include "Vector.h"
#include <stdio.h>
class Lattice;

class Patch {
public:

  int numAtoms;

  CompAtom *atoms;

  CompAtom* images[27];

  Vector *f_nbond;
  Vector *f_slow;
  Vector *p_orig;

  void zeroforces();
  void setforces(Vector *, Vector *);

  int i_move;
  void moveatoms();

  CompAtom *image(int trans, Lattice &lattice);
  void clearimages();

  Patch();
  ~Patch();

  int readfile(FILE *);

};

#endif // PATCH_H

