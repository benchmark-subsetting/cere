/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

/*
   The class Molecule is used to hold all of the structural information
   for a simulation.  This information is read in from a .psf file and
   cross checked with the Parameters object passed in.  All of the structural
   information is then stored in arrays for use.
*/

#include "Molecule.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "ObjectArena.h"


Molecule::Molecule()
{
  exclArena = new ObjectArena<char>;
  all_exclusions = 0;
  numAtoms=0;
  numCalcExclusions=0;
  atomVdwType = 0;
}

Molecule::~Molecule()
{
  delete exclArena;
  delete [] all_exclusions;
  delete [] atomVdwType;
}


int Molecule::readfile(FILE *file) {

  char buf[1024];

  if ( ! fgets(buf,1024,file) || strcmp(buf,"MOLECULE_BEGIN\n") ) {
    printf("Missing MOLECULE_BEGIN\n");
    return -1;
  }

  if ( fscanf(file,"%d %d\n",&numAtoms,&numCalcExclusions) != 2 ) {
    printf("numAtoms read error\n");
    return -2;
  }

  atomVdwType = new int[numAtoms];
  all_exclusions = new ExclusionCheck[numAtoms];

  int i;
  for ( i=0; i<numAtoms; ++i ) {
    int vdw,min,max;
    vdw=min=max=0;
    if ( fscanf(file,"%d %d %d",&vdw,&min,&max) != 3 ) {
      printf("vdw/min/max read error\n");
      return -3;
    }
    atomVdwType[i] = vdw;
    all_exclusions[i].min = min;
    all_exclusions[i].max = max;
    all_exclusions[i].flags = 0;
    if ( min <= max ) {
      int s = max - min + 1;
      char *f = all_exclusions[i].flags = exclArena->getNewArray(s);
      for ( int k=0; k<s; ++k ) {
        int fk=0;
        if ( fscanf(file," %d",&fk) != 1 ) {
          printf("exclcheck read error\n");
          return -4;
        }
        f[k] = fk;
      }
    }
    if ( ! fgets(buf,1024,file) || strcmp(buf,"\n") ) {
      printf("newline read error\n");
      return -5;
    }
  }

  if ( ! fgets(buf,1024,file) || strcmp(buf,"MOLECULE_END\n") ) {
    printf("Missing MOLECULE_END\n");
    return -6;
  }

  return 0;
}

