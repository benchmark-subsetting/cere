/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef REDUCTIONMGR_H
#define REDUCTIONMGR_H

#include "NamdTypes.h"

#define VECTOR(A) A ## _X, A ## _Y, A ## _Z
#define TENSOR(A) A ## _XX, A ## _XY, A ## _XZ, \
                  A ## _YX, A ## _YY, A ## _YZ, \
                  A ## _ZX, A ## _ZY, A ## _ZZ

#define ADD_VECTOR(R,RL,D,DL) \
  R->item( RL ## _X ) += D[ DL ## _X ]; \
  R->item( RL ## _Y ) += D[ DL ## _Y ]; \
  R->item( RL ## _Z ) += D[ DL ## _Z ]

#define ADD_VECTOR_OBJECT(R,RL,D) \
  R->item( RL ## _X ) += D.x; \
  R->item( RL ## _Y ) += D.y; \
  R->item( RL ## _Z ) += D.z

#define ADD_TENSOR(R,RL,D,DL) \
  R->item( RL ## _XX) += D[ DL ## _XX ]; \
  R->item( RL ## _XY) += D[ DL ## _XY ]; \
  R->item( RL ## _XZ) += D[ DL ## _XZ ]; \
  R->item( RL ## _YX) += D[ DL ## _YX ]; \
  R->item( RL ## _YY) += D[ DL ## _YY ]; \
  R->item( RL ## _YZ) += D[ DL ## _YZ ]; \
  R->item( RL ## _ZX) += D[ DL ## _ZX ]; \
  R->item( RL ## _ZY) += D[ DL ## _ZY ]; \
  R->item( RL ## _ZZ) += D[ DL ## _ZZ ]

#define ADD_TENSOR_OBJECT(R,RL,D) \
  R->item( RL ## _XX) += D.xx; \
  R->item( RL ## _XY) += D.xy; \
  R->item( RL ## _XZ) += D.xz; \
  R->item( RL ## _YX) += D.yx; \
  R->item( RL ## _YY) += D.yy; \
  R->item( RL ## _YZ) += D.yz; \
  R->item( RL ## _ZX) += D.zx; \
  R->item( RL ## _ZY) += D.zy; \
  R->item( RL ## _ZZ) += D.zz

#define GET_VECTOR(O,R,A) \
  O.x = R->item( A ## _X ); \
  O.y = R->item( A ## _Y ); \
  O.z = R->item( A ## _Z )

#define GET_TENSOR(O,R,A) \
  O.xx = R->item( A ## _XX); \
  O.xy = R->item( A ## _XY); \
  O.xz = R->item( A ## _XZ); \
  O.yx = R->item( A ## _YX); \
  O.yy = R->item( A ## _YY); \
  O.yz = R->item( A ## _YZ); \
  O.zx = R->item( A ## _ZX); \
  O.zy = R->item( A ## _ZY); \
  O.zz = R->item( A ## _ZZ)

  enum { electEnergyIndex, fullElectEnergyIndex, vdwEnergyIndex,
         TENSOR(virialIndex), TENSOR(fullElectVirialIndex),
         outputDataSize, exclChecksumIndex,
         electEnergyIndex_s, fullElectEnergyIndex_s, vdwEnergyIndex_s,
         VECTOR(pairVDWForceIndex), VECTOR(pairElectForceIndex),
         reductionDataSize };

typedef enum
{
 // energy
  REDUCTION_ANGLE_ENERGY,
  REDUCTION_BOND_ENERGY,
  REDUCTION_DIHEDRAL_ENERGY,
  REDUCTION_ELECT_ENERGY,
  REDUCTION_ELECT_ENERGY_F,
  REDUCTION_ELECT_ENERGY_SLOW,
  REDUCTION_ELECT_ENERGY_SLOW_F,
  REDUCTION_IMPROPER_ENERGY,
  REDUCTION_KINETIC_ENERGY,
  REDUCTION_CENTERED_KINETIC_ENERGY,
  REDUCTION_LJ_ENERGY,
  REDUCTION_LJ_ENERGY_F,
  REDUCTION_BC_ENERGY,
  REDUCTION_SMD_ENERGY,
  REDUCTION_MISC_ENERGY,
 // pressure
  TENSOR(REDUCTION_VIRIAL_NORMAL),
  TENSOR(REDUCTION_VIRIAL_NBOND),
  TENSOR(REDUCTION_VIRIAL_SLOW),
#ifdef ALTVIRIAL
  TENSOR(REDUCTION_ALT_VIRIAL_NORMAL),
  TENSOR(REDUCTION_ALT_VIRIAL_NBOND),
  TENSOR(REDUCTION_ALT_VIRIAL_SLOW),
#endif
  TENSOR(REDUCTION_INT_VIRIAL_NORMAL),
  TENSOR(REDUCTION_INT_VIRIAL_NBOND),
  TENSOR(REDUCTION_INT_VIRIAL_SLOW),
  VECTOR(REDUCTION_EXT_FORCE_NORMAL),
  VECTOR(REDUCTION_EXT_FORCE_NBOND),
  VECTOR(REDUCTION_EXT_FORCE_SLOW),
 // momentum
  VECTOR(REDUCTION_MOMENTUM),
  VECTOR(REDUCTION_ANGULAR_MOMENTUM),
 // used for minimization
  REDUCTION_MIN_F_DOT_F,
  REDUCTION_MIN_F_DOT_V,
  REDUCTION_MIN_V_DOT_V,
  REDUCTION_MIN_HUGE_COUNT,
 // used for pair interaction calculations
  VECTOR(REDUCTION_PAIR_VDW_FORCE),
  VECTOR(REDUCTION_PAIR_ELECT_FORCE),
 // checksum
  REDUCTION_ATOM_CHECKSUM,
  REDUCTION_COMPUTE_CHECKSUM,
  REDUCTION_BOND_CHECKSUM,
  REDUCTION_ANGLE_CHECKSUM,
  REDUCTION_DIHEDRAL_CHECKSUM,
  REDUCTION_IMPROPER_CHECKSUM,
  REDUCTION_EXCLUSION_CHECKSUM,
  REDUCTION_MARGIN_VIOLATIONS,
 // semaphore (must be last)
  REDUCTION_MAX_RESERVED
} ReductionTag;

// Later this can be dynamic
enum {
  REDUCTIONS_BASIC,
  REDUCTIONS_USER1,
  REDUCTIONS_USER2,
 // semaphore (must be last)
  REDUCTION_MAX_SET_ID
};


#endif

