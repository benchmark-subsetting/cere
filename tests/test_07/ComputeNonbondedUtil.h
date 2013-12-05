/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

/*
   Common operations for ComputeNonbonded classes
*/

#ifndef COMPUTENONBONDEDUTIL_H
#define COMPUTENONBONDEDUTIL_H

#include "NamdTypes.h"
#include "ReductionMgr.h"
class LJTable;
class SimParameters;
class Molecule;

// function arguments
struct nonbonded {
  CompAtom* p[2];
  Force* ff[2];
  // for full electrostatics
  Force* fullf [2];

  // used by pair and self
  int numAtoms[2];
  BigReal *reduction;
  BigReal *pressureProfileReduction;

  // used by excl
  Position p_ij;
  int m14;

  // used by self
  int minPart;
  int maxPart;
  int numParts;

};

class ComputeNonbondedUtil {

public:

  ComputeNonbondedUtil() {}
  virtual ~ComputeNonbondedUtil() {}
  static void select(SimParameters *, Molecule *, LJTable *);

  static void (*calcPair)(nonbonded *);
  static void (*calcPairEnergy)(nonbonded *);
  static void (*calcSelf)(nonbonded *);
  static void (*calcSelfEnergy)(nonbonded *);

  static void (*calcFullPair)(nonbonded *);
  static void (*calcFullPairEnergy)(nonbonded *);
  static void (*calcFullSelf)(nonbonded *);
  static void (*calcFullSelfEnergy)(nonbonded *);

  static void (*calcMergePair)(nonbonded *);
  static void (*calcMergePairEnergy)(nonbonded *);
  static void (*calcMergeSelf)(nonbonded *);
  static void (*calcMergeSelfEnergy)(nonbonded *);

  static void (*calcSlowPair)(nonbonded *);
  static void (*calcSlowPairEnergy)(nonbonded *);
  static void (*calcSlowSelf)(nonbonded *);
  static void (*calcSlowSelfEnergy)(nonbonded *);

#if 0
  enum { exclChecksumIndex,
	 electEnergyIndex, fullElectEnergyIndex, vdwEnergyIndex,
//sd-db
	 electEnergyIndex_s, fullElectEnergyIndex_s, vdwEnergyIndex_s,
//sd-de
	 TENSOR(virialIndex), TENSOR(fullElectVirialIndex),
         VECTOR(pairVDWForceIndex), VECTOR(pairElectForceIndex),
	 reductionDataSize };
#endif

  static Bool commOnly;
  static Bool fixedAtomsOn;
  static Real cutoff;
  static BigReal cutoff2;
  static BigReal groupcutoff2;
  static BigReal dielectric_1;
  static const LJTable* ljTable;
  static const Molecule* mol;
  static BigReal r2_delta, r2_delta_1;
  static int r2_delta_exp;
  static BigReal *table_alloc;
  static BigReal *table_short;
  static BigReal *table_noshort;
  static BigReal *fast_table;
  static BigReal *scor_table;
  static BigReal *slow_table;
  static BigReal *corr_table;
  static BigReal *full_table;
  static BigReal *vdwa_table;
  static BigReal *vdwb_table;
  static BigReal scaling;
  static BigReal scale14;
  static Real switchOn;
  static BigReal switchOn_1;
  static BigReal switchOn2;
  static BigReal c0;
  static BigReal c1;
  static BigReal c3;
  static BigReal c5;
  static BigReal c6;
  static BigReal c7;
  static BigReal c8;
  // static BigReal d0;
//sd-db
  static Bool fepOn;
  static BigReal lambda;
  static BigReal lambda2;
//sd-de
  static Bool lesOn;
  static int lesFactor;
  static BigReal lesScaling;

  static BigReal *lambda_table;

  static Bool pairInteractionOn;
  static Bool pairInteractionSelf;

  static Bool pressureProfileNonbonded;
  static int pressureProfileSlabs;
  static BigReal pressureProfileThickness;
  static BigReal pressureProfileMin;

  // for particle mesh Ewald
  static BigReal ewaldcof;
  static BigReal pi_ewaldcof;

  // for simplifying some common functions
  inline static BigReal square(	const BigReal &x,
 				const BigReal &y,
				const BigReal &z)
	{
	return(x*x+y*y+z*z);
	}

  static void calc_pair(nonbonded *);
  static void calc_pair_energy(nonbonded *);
  static void calc_pair_fullelect(nonbonded *);
  static void calc_pair_energy_fullelect(nonbonded *);
  static void calc_pair_merge_fullelect(nonbonded *);
  static void calc_pair_energy_merge_fullelect(nonbonded *);
  static void calc_pair_slow_fullelect(nonbonded *);
  static void calc_pair_energy_slow_fullelect(nonbonded *);

  static void calc_self(nonbonded *);
  static void calc_self_energy(nonbonded *);
  static void calc_self_fullelect(nonbonded *);
  static void calc_self_energy_fullelect(nonbonded *);
  static void calc_self_merge_fullelect(nonbonded *);
  static void calc_self_energy_merge_fullelect(nonbonded *);
  static void calc_self_slow_fullelect(nonbonded *);
  static void calc_self_energy_slow_fullelect(nonbonded *);

//alchemical fep calcualtion
  static void calc_pair_energy_fep(nonbonded *);
  static void calc_pair_energy_fullelect_fep (nonbonded *);
  static void calc_pair_energy_merge_fullelect_fep (nonbonded *);
  static void calc_pair_energy_slow_fullelect_fep (nonbonded *);
  static void calc_self_energy_fep (nonbonded *);
  static void calc_self_energy_fullelect_fep (nonbonded *);
  static void calc_self_energy_merge_fullelect_fep (nonbonded *);
  static void calc_self_energy_slow_fullelect_fep (nonbonded *);

//locally enhanced sampling calcualtion
  static void calc_pair_les(nonbonded *);
  static void calc_pair_energy_les(nonbonded *);
  static void calc_pair_fullelect_les (nonbonded *);
  static void calc_pair_energy_fullelect_les (nonbonded *);
  static void calc_pair_merge_fullelect_les (nonbonded *);
  static void calc_pair_energy_merge_fullelect_les (nonbonded *);
  static void calc_pair_slow_fullelect_les (nonbonded *);
  static void calc_pair_energy_slow_fullelect_les (nonbonded *);
  static void calc_self_les (nonbonded *);
  static void calc_self_energy_les (nonbonded *);
  static void calc_self_fullelect_les (nonbonded *);
  static void calc_self_energy_fullelect_les (nonbonded *);
  static void calc_self_merge_fullelect_les (nonbonded *);
  static void calc_self_energy_merge_fullelect_les (nonbonded *);
  static void calc_self_slow_fullelect_les (nonbonded *);
  static void calc_self_energy_slow_fullelect_les (nonbonded *);

//pair interaction calcualtion
  static void calc_pair_energy_int(nonbonded *);
  static void calc_self_energy_int (nonbonded *);

};

#endif

