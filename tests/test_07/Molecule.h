/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

/*
   This class is used to store all of the structural       
   information for a simulation.  It reas in this information 
   from a .psf file, cross checks and obtains some information
   from the Parameters object that is passed in, and then     
   stores all this information for later use.	
*/


#ifndef MOLECULE_H

#define MOLECULE_H

#include "common.h"
#include "NamdTypes.h"
#include "Vector.h"
#include <stdio.h>

template<class Type> class ObjectArena;

class ExclusionCheck {
public:
  int32 min,max;
  char *flags;
};
#define EXCHCK_FULL 1
#define EXCHCK_MOD 2

// List maintaining the global atom indicies sorted by helix groups.
class Molecule
{

private:

	ObjectArena<char> *exclArena;
	ExclusionCheck *all_exclusions;
				//  List of all exclusions, including
				//  explicit exclusions and those calculated
				//  from the bonded structure based on the
				//  exclusion policy.  Also includes 1-4's.

public:
	int numAtoms;		//  Number of atoms 
	int numCalcExclusions;	//  Number of exclusions requiring calculation

	Molecule();
	~Molecule();		//  Destructor

	int readfile(FILE*);

        int *atomVdwType;

	//  Get the vdw type of an atom
	Index atomvdwtype(int anum) const
	{
	   	return atomVdwType[anum];
	}

	//  Check for exclusions, either explicit or bonded.
        //  Returns 1 for full, 2 for 1-4 exclusions.
	int checkexcl(int atom1, int atom2) const;

	ExclusionCheck *get_excl_check_for_atom(int anum) const
			 { return &all_exclusions[anum]; }

};

#endif

