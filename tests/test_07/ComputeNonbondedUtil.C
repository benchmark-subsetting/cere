/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

/*
   Common operations for ComputeNonbonded classes
*/

#ifndef NAMD_RESTRICT
#define restrict
#endif

#ifdef WIN32ERFC
#if !defined(SPEC_CPU_WINDOWS_ICL)
extern "C" {
  double erfc(double);
}
#else
#include <mathimf.h>
#endif 
#endif

#include "ComputeNonbondedUtil.h"
#include "SimParameters.h"
#include "Molecule.h"
#include "LJTable.h"
#include <stdio.h>

Bool		ComputeNonbondedUtil::commOnly;
Bool		ComputeNonbondedUtil::fixedAtomsOn;
Real            ComputeNonbondedUtil::cutoff;
BigReal         ComputeNonbondedUtil::cutoff2;
BigReal         ComputeNonbondedUtil::groupcutoff2;
BigReal         ComputeNonbondedUtil::dielectric_1;
const LJTable*  ComputeNonbondedUtil::ljTable = 0;
const Molecule* ComputeNonbondedUtil::mol;
BigReal		ComputeNonbondedUtil::r2_delta;
BigReal		ComputeNonbondedUtil::r2_delta_1;
int		ComputeNonbondedUtil::r2_delta_exp;
BigReal*	ComputeNonbondedUtil::table_alloc = 0;
BigReal*	ComputeNonbondedUtil::table_short;
BigReal*	ComputeNonbondedUtil::table_noshort;
BigReal*	ComputeNonbondedUtil::fast_table;
BigReal*	ComputeNonbondedUtil::scor_table;
BigReal*	ComputeNonbondedUtil::slow_table;
BigReal*	ComputeNonbondedUtil::corr_table;
BigReal*	ComputeNonbondedUtil::full_table;
BigReal*	ComputeNonbondedUtil::vdwa_table;
BigReal*	ComputeNonbondedUtil::vdwb_table;
BigReal         ComputeNonbondedUtil::scaling;
BigReal         ComputeNonbondedUtil::scale14;
Real            ComputeNonbondedUtil::switchOn;
BigReal         ComputeNonbondedUtil::switchOn_1;
BigReal         ComputeNonbondedUtil::switchOn2;
BigReal         ComputeNonbondedUtil::c0;
BigReal         ComputeNonbondedUtil::c1;
BigReal         ComputeNonbondedUtil::c3;
BigReal         ComputeNonbondedUtil::c5;
BigReal         ComputeNonbondedUtil::c6;
BigReal         ComputeNonbondedUtil::c7;
BigReal         ComputeNonbondedUtil::c8;
// BigReal         ComputeNonbondedUtil::d0;
// fepb
Bool            ComputeNonbondedUtil::fepOn;
BigReal         ComputeNonbondedUtil::lambda;
BigReal         ComputeNonbondedUtil::lambda2;
//fepe
Bool            ComputeNonbondedUtil::lesOn;
int             ComputeNonbondedUtil::lesFactor;
BigReal         ComputeNonbondedUtil::lesScaling;

BigReal*	ComputeNonbondedUtil::lambda_table = 0;

Bool            ComputeNonbondedUtil::pairInteractionOn;
Bool            ComputeNonbondedUtil::pairInteractionSelf;

Bool            ComputeNonbondedUtil::pressureProfileNonbonded;
int             ComputeNonbondedUtil::pressureProfileSlabs;
BigReal         ComputeNonbondedUtil::pressureProfileThickness;
BigReal         ComputeNonbondedUtil::pressureProfileMin;

BigReal		ComputeNonbondedUtil::ewaldcof;
BigReal		ComputeNonbondedUtil::pi_ewaldcof;

void (*ComputeNonbondedUtil::calcPair)(nonbonded *);
void (*ComputeNonbondedUtil::calcPairEnergy)(nonbonded *);
void (*ComputeNonbondedUtil::calcSelf)(nonbonded *);
void (*ComputeNonbondedUtil::calcSelfEnergy)(nonbonded *);

void (*ComputeNonbondedUtil::calcFullPair)(nonbonded *);
void (*ComputeNonbondedUtil::calcFullPairEnergy)(nonbonded *);
void (*ComputeNonbondedUtil::calcFullSelf)(nonbonded *);
void (*ComputeNonbondedUtil::calcFullSelfEnergy)(nonbonded *);

void (*ComputeNonbondedUtil::calcMergePair)(nonbonded *);
void (*ComputeNonbondedUtil::calcMergePairEnergy)(nonbonded *);
void (*ComputeNonbondedUtil::calcMergeSelf)(nonbonded *);
void (*ComputeNonbondedUtil::calcMergeSelfEnergy)(nonbonded *);

void (*ComputeNonbondedUtil::calcSlowPair)(nonbonded *);
void (*ComputeNonbondedUtil::calcSlowPairEnergy)(nonbonded *);
void (*ComputeNonbondedUtil::calcSlowSelf)(nonbonded *);
void (*ComputeNonbondedUtil::calcSlowSelfEnergy)(nonbonded *);

// define splitting function
#define SPLIT_NONE	1
#define SPLIT_SHIFT	2
#define SPLIT_C1	3
#define SPLIT_XPLOR	4

  
void ComputeNonbondedUtil::select(SimParameters *simParams, Molecule *molecule, LJTable *ljTable_in)
{
  ljTable = ljTable_in;

  commOnly = simParams->commOnly;
  fixedAtomsOn = ( simParams->fixedAtomsOn && ! simParams->fixedAtomsForces );

  cutoff = simParams->cutoff;
  cutoff2 = cutoff*cutoff;

//fepb
  fepOn = simParams->fepOn;
  lambda = lambda2 = 0;
  lesOn = simParams->lesOn;
  lesScaling = lesFactor = 0;

  delete [] lambda_table;
  lambda_table = 0;

  pairInteractionOn = simParams->pairInteractionOn;
  pairInteractionSelf = simParams->pairInteractionSelf;

  pressureProfileNonbonded = simParams->pressureProfileNonbonded;
  if (pressureProfileNonbonded) {
    pressureProfileSlabs = simParams->pressureProfileSlabs;
    pressureProfileThickness = simParams->pressureProfileThickness;
    pressureProfileMin = simParams->pressureProfileMin;
  }

  if ( fepOn ) {
    lambda = simParams->lambda;
    lambda2 = simParams->lambda2;
    lambda_table = new BigReal[2*3*3];
    for ( int ip=0; ip<3; ++ip ) {
      for ( int jp=0; jp<3; ++jp ) {
        BigReal lambda_pair = 1.0;
        BigReal d_lambda_pair = 1.0;
        if (ip || jp) {
          if (ip && jp && ip != jp) {
            lambda_pair = 0.0;
            d_lambda_pair = 0.0;
          } else {
            if (ip == 1 || jp == 1) {
              lambda_pair = lambda;
              d_lambda_pair = lambda2;
            } else if ( ip == 2 || jp == 2) {
              lambda_pair = 1.0 - lambda;
              d_lambda_pair = 1.0 - lambda2;
            }
          }
        }
        lambda_table[2*(3*ip+jp)] = lambda_pair;
        lambda_table[2*(3*ip+jp)+1] = d_lambda_pair;
      }
    }
    ComputeNonbondedUtil::calcPairEnergy = calc_pair_energy_fep;
    ComputeNonbondedUtil::calcSelfEnergy = calc_self_energy_fep;
    ComputeNonbondedUtil::calcFullPairEnergy = calc_pair_energy_fullelect_fep;
    ComputeNonbondedUtil::calcFullSelfEnergy = calc_self_energy_fullelect_fep;
    ComputeNonbondedUtil::calcMergePairEnergy = calc_pair_energy_merge_fullelect_fep;
    ComputeNonbondedUtil::calcMergeSelfEnergy = calc_self_energy_merge_fullelect_fep;
    ComputeNonbondedUtil::calcSlowPairEnergy = calc_pair_energy_slow_fullelect_fep;
    ComputeNonbondedUtil::calcSlowSelfEnergy = calc_self_energy_slow_fullelect_fep;
  } else if ( lesOn ) {
    lesFactor = simParams->lesFactor;
    lesScaling = 1.0 / (double)lesFactor;
    lambda_table = new BigReal[(lesFactor+1)*(lesFactor+1)];
    for ( int ip=0; ip<=lesFactor; ++ip ) {
      for ( int jp=0; jp<=lesFactor; ++jp ) {
        BigReal lambda_pair = 1.0;
        if (ip || jp ) {
          if (ip && jp && ip != jp) {
            lambda_pair = 0.0;
          } else {
            lambda_pair = lesScaling;
          }
        }
        lambda_table[(lesFactor+1)*ip+jp] = lambda_pair;
      }
    }
    ComputeNonbondedUtil::calcPair = calc_pair_les;
    ComputeNonbondedUtil::calcPairEnergy = calc_pair_energy_les;
    ComputeNonbondedUtil::calcSelf = calc_self_les;
    ComputeNonbondedUtil::calcSelfEnergy = calc_self_energy_les;
    ComputeNonbondedUtil::calcFullPair = calc_pair_fullelect_les;
    ComputeNonbondedUtil::calcFullPairEnergy = calc_pair_energy_fullelect_les;
    ComputeNonbondedUtil::calcFullSelf = calc_self_fullelect_les;
    ComputeNonbondedUtil::calcFullSelfEnergy = calc_self_energy_fullelect_les;
    ComputeNonbondedUtil::calcMergePair = calc_pair_merge_fullelect_les;
    ComputeNonbondedUtil::calcMergePairEnergy = calc_pair_energy_merge_fullelect_les;
    ComputeNonbondedUtil::calcMergeSelf = calc_self_merge_fullelect_les;
    ComputeNonbondedUtil::calcMergeSelfEnergy = calc_self_energy_merge_fullelect_les;
    ComputeNonbondedUtil::calcSlowPair = calc_pair_slow_fullelect_les;
    ComputeNonbondedUtil::calcSlowPairEnergy = calc_pair_energy_slow_fullelect_les;
    ComputeNonbondedUtil::calcSlowSelf = calc_self_slow_fullelect_les;
    ComputeNonbondedUtil::calcSlowSelfEnergy = calc_self_energy_slow_fullelect_les;
  } else if ( pairInteractionOn || pressureProfileNonbonded ) {
    ComputeNonbondedUtil::calcPairEnergy = 0; //calc_pair_energy_int;
    ComputeNonbondedUtil::calcSelfEnergy = 0; //calc_self_energy_int;
  } else {
    ComputeNonbondedUtil::calcPair = calc_pair;
    ComputeNonbondedUtil::calcPairEnergy = calc_pair_energy;
    ComputeNonbondedUtil::calcSelf = calc_self;
    ComputeNonbondedUtil::calcSelfEnergy = calc_self_energy;
    ComputeNonbondedUtil::calcFullPair = calc_pair_fullelect;
    ComputeNonbondedUtil::calcFullPairEnergy = calc_pair_energy_fullelect;
    ComputeNonbondedUtil::calcFullSelf = calc_self_fullelect;
    ComputeNonbondedUtil::calcFullSelfEnergy = calc_self_energy_fullelect;
    ComputeNonbondedUtil::calcMergePair = calc_pair_merge_fullelect;
    ComputeNonbondedUtil::calcMergePairEnergy = calc_pair_energy_merge_fullelect;
    ComputeNonbondedUtil::calcMergeSelf = calc_self_merge_fullelect;
    ComputeNonbondedUtil::calcMergeSelfEnergy = calc_self_energy_merge_fullelect;
    ComputeNonbondedUtil::calcSlowPair = calc_pair_slow_fullelect;
    ComputeNonbondedUtil::calcSlowPairEnergy = calc_pair_energy_slow_fullelect;
    ComputeNonbondedUtil::calcSlowSelf = calc_self_slow_fullelect;
    ComputeNonbondedUtil::calcSlowSelfEnergy = calc_self_energy_slow_fullelect;
  }

//fepe

  // we add slightly more than 2 angstroms to get the same numbers.
  // don't know why...ask jim... :-)
  const BigReal &hcutoff = simParams->hgroupCutoff;
  groupcutoff2 = (cutoff+hcutoff)*(cutoff+hcutoff);

  dielectric_1 = 1.0/simParams->dielectric;
  mol = molecule;
  scaling = simParams->nonbondedScaling;
  if ( simParams->exclude == SCALED14 )
  {
    scale14 = simParams->scale14;
  }
  else
  {
    scale14 = 1.;
  }
  if ( simParams->switchingActive )
  {
    switchOn = simParams->switchingDist;
    switchOn_1 = 1.0/switchOn;
    // d0 = 1.0/(cutoff-switchOn);
    switchOn2 = switchOn*switchOn;
    c0 = 1.0/(cutoff2-switchOn2);
  }
  else
  {
    switchOn = cutoff;
    switchOn_1 = 1.0/switchOn;
    // d0 = 0.;  // avoid division by zero
    switchOn2 = switchOn*switchOn;
    c0 = 0.;  // avoid division by zero
  }
  c1 = c0*c0*c0;
  c3 = 3.0 * (cutoff2 - switchOn2);
  c5 = 0;
  c6 = 0;
  c7 = 0;
  c8 = 0;

  const int PMEOn = simParams->PMEOn;

  if ( PMEOn ) {
    ewaldcof = simParams->PMEEwaldCoefficient;
    BigReal TwoBySqrtPi = 1.12837916709551;
    pi_ewaldcof = TwoBySqrtPi * ewaldcof;
  }

  int splitType = SPLIT_NONE;
  if ( simParams->switchingActive ) splitType = SPLIT_SHIFT;
  if ( simParams->fullDirectOn || simParams->FMAOn || PMEOn ) {
    switch ( simParams->longSplitting ) {
      case C1:
      splitType = SPLIT_C1;
      break;

      case XPLOR:
      NAMD_die("Sorry, XPLOR splitting not supported.");
      break;

      case SHARP:
      NAMD_die("Sorry, SHARP splitting not supported.");
      break;

      default:
      NAMD_die("Unknown splitting type found!");

    }
  }

  BigReal r2_tol = 0.1;
  
  r2_delta = 1.0;
  r2_delta_exp = 0;
  while ( r2_delta > r2_tol ) { r2_delta /= 2.0; r2_delta_exp += 1; }
  r2_delta_1 = 1.0 / r2_delta;

  BigReal r2_tmp = 1.0;
  int cutoff2_exp = 0;
  while ( cutoff2 > r2_tmp ) { r2_tmp *= 2.0; cutoff2_exp += 1; }

  int i;
  int n = (r2_delta_exp + cutoff2_exp) * 64 + 1;

  if ( table_alloc ) delete [] table_alloc;
  table_alloc = new BigReal[60*n+16];
  BigReal *table_align = table_alloc;
  while ( ((long)table_align) % 128 ) ++table_align;
  table_noshort = table_align;
  table_short = table_align + 16*n;
  slow_table = table_align + 32*n;
  fast_table = table_align + 36*n;
  scor_table = table_align + 40*n;
  corr_table = table_align + 44*n;
  full_table = table_align + 48*n;
  vdwa_table = table_align + 52*n;
  vdwb_table = table_align + 56*n;
  BigReal *fast_i = fast_table;
  BigReal *scor_i = scor_table;
  BigReal *slow_i = slow_table;
  BigReal *vdwa_i = vdwa_table;
  BigReal *vdwb_i = vdwb_table;

  // fill in the rest of the table
  for ( i=0; i<n; ++i ) {

    const BigReal r2_base = r2_delta * ( 1 << (i/64) );
    const BigReal r2_del = r2_base / 64.0;
    const BigReal r2 = r2_base + r2_del * (i%64);

    const BigReal r = sqrt(r2);
    const BigReal r_1 = 1.0/r;
    const BigReal r_2 = 1.0/r2;

    // fast_ is defined as (full_ - slow_)
    // corr_ and fast_ are both zero at the cutoff, full_ is not
    // all three are approx 1/r at short distances

    // for actual interpolation, we use fast_ for fast forces and
    // scor_ = slow_ + corr_ - full_ and slow_ for slow forces
    // since these last two are of small magnitude

    BigReal fast_energy, fast_gradient;
    BigReal scor_energy, scor_gradient;
    BigReal slow_energy, slow_gradient;

    // corr_ is PME direct sum, or similar correction term
    // corr_energy is multiplied by r until later
    // corr_gradient is multiplied by -r^2 until later
    BigReal corr_energy, corr_gradient;

    if ( PMEOn ) {
      BigReal tmp_a = r * ewaldcof;
      BigReal tmp_b = erfc(tmp_a);
      corr_energy = tmp_b;
      corr_gradient = pi_ewaldcof*exp(-(tmp_a*tmp_a))*r + tmp_b;
    } else {
      corr_energy = corr_gradient = 0;
    }

    switch(splitType) {
      case SPLIT_NONE:
        fast_energy = 1.0/r;
        fast_gradient = -1.0/r2;
        scor_energy = scor_gradient = 0;
        slow_energy = slow_gradient = 0;
	break;
      case SPLIT_SHIFT: {
	BigReal shiftVal = r2/cutoff2 - 1.0;
	shiftVal *= shiftVal;
	BigReal dShiftVal = 2.0 * (r2/cutoff2 - 1.0) * 2.0*r/cutoff2;
        fast_energy = shiftVal/r;
        fast_gradient = dShiftVal/r - shiftVal/r2;
        scor_energy = scor_gradient = 0;
        slow_energy = slow_gradient = 0;
        } 
	break;
      case SPLIT_C1:
	// calculate actual energy and gradient
	slow_energy = 0.5/cutoff * (3.0 - (r2/cutoff2));
	slow_gradient = -1.0/cutoff2 * (r/cutoff);
	// calculate scor from slow and corr
	scor_energy = slow_energy + (corr_energy - 1.0)/r;
	scor_gradient = slow_gradient - (corr_gradient - 1.0)/r2;
	// calculate fast from slow
	fast_energy = 1.0/r - slow_energy;
	fast_gradient = -1.0/r2 - slow_gradient;
	break;
    }

    // foo_gradient is calculated as ( d foo_energy / d r )
    // and now divided by 2r to get ( d foo_energy / d r2 )

    fast_gradient *= 0.5 * r_1;
    scor_gradient *= 0.5 * r_1;
    slow_gradient *= 0.5 * r_1;

    // let modf be 1 if excluded, 1-scale14 if modified, 0 otherwise,
    // add scor_ - modf * slow_ to slow terms and
    // add fast_ - modf * fast_ to fast terms.

    BigReal vdwa_energy, vdwa_gradient;
    BigReal vdwb_energy, vdwb_gradient;

    const BigReal r_6 = r_2*r_2*r_2;
    const BigReal r_12 = r_6*r_6;

    // Lennard-Jones switching function
    const BigReal c2 = cutoff2-r2;
    const BigReal c4 = c2*(c3-2.0*c2);
    const BigReal switchVal =         // used for Lennard-Jones
                        ( r2 > switchOn2 ? c2*c4*c1 : 1.0 );
    const BigReal dSwitchVal =        // d switchVal / d r2
                        ( r2 > switchOn2 ? 2*c1*(c2*c2-c4) : 0.0 );

    vdwa_energy = switchVal * r_12;
    vdwb_energy = switchVal * r_6;

    vdwa_gradient = switchVal * ( dSwitchVal - 6.0 * switchVal * r_2 ) * r_12;
    vdwb_gradient = switchVal * ( dSwitchVal - 3.0 * switchVal * r_2 ) * r_6;


    *(fast_i++) = fast_energy;
    *(fast_i++) = fast_gradient;
    *(fast_i++) = 0;
    *(fast_i++) = 0;
    *(scor_i++) = scor_energy;
    *(scor_i++) = scor_gradient;
    *(scor_i++) = 0;
    *(scor_i++) = 0;
    *(slow_i++) = slow_energy;
    *(slow_i++) = slow_gradient;
    *(slow_i++) = 0;
    *(slow_i++) = 0;
    *(vdwa_i++) = vdwa_energy;
    *(vdwa_i++) = vdwa_gradient;
    *(vdwa_i++) = 0;
    *(vdwa_i++) = 0;
    *(vdwb_i++) = vdwb_energy;
    *(vdwb_i++) = vdwb_gradient;
    *(vdwb_i++) = 0;
    *(vdwb_i++) = 0;

  }

/*
  // patch up data for i=0, in particular slow_
  fast_table[0] = fast_table[4] - fast_table[5] * r2_delta;
  fast_table[1] = fast_table[5];  // fast_gradient
  fast_table[2] = 0;
  fast_table[3] = 0;
  scor_table[0] = scor_table[4] - scor_table[5] * r2_delta;
  scor_table[1] = scor_table[5];  // scor_gradient
  scor_table[2] = 0;
  scor_table[3] = 0;
  slow_table[0] = slow_table[4] - slow_table[5] * r2_delta;
  slow_table[1] = slow_table[5];  // slow_gradient
  slow_table[2] = 0;
  slow_table[3] = 0;
*/

  int j;
  for ( j=0; j<5; ++j ) {
    BigReal *t0 = 0;
    switch (j) {
      case 0: 
        t0 = fast_table;
      break;
      case 1: 
        t0 = scor_table;
      break;
      case 2: 
        t0 = slow_table;
      break;
      case 3: 
        t0 = vdwa_table;
      break;
      case 4: 
        t0 = vdwb_table;
      break;
    }
    BigReal *t;
    for ( i=0,t=t0; i<(n-1); ++i,t+=4 ) {
      BigReal x = ( r2_delta * ( 1 << (i/64) ) ) / 64.0;
      BigReal v1 = t[0];
      BigReal g1 = t[1];
      BigReal v2 = t[4];
      BigReal g2 = t[5];
      // explicit formulas for v1 + g1 x + c x^2 + d x^3
      BigReal c = ( 3.0 * (v2 - v1) - x * (2.0 * g1 + g2) ) / ( x * x );
      BigReal d = ( -2.0 * (v2 - v1) + x * (g1 + g2) ) / ( x * x * x );
      // since v2 - v1 is imprecise, we refine c and d numerically
      // important because we need accurate forces (more than energies!)
      for ( int k=0; k < 2; ++k ) {
        BigReal dv = (v1 - v2) + ( ( d * x + c ) * x + g1 ) * x;
        BigReal dg = (g1 - g2) + ( 3.0 * d * x + 2.0 * c ) * x;
        c -= ( 3.0 * dv - x * dg ) / ( x * x );
        d -= ( -2.0 * dv + x * dg ) / ( x * x * x );
      }
      // store in the array;
      t[2] = c;  t[3] = d;
    }
    BigReal dvmax = 0;
    BigReal dgmax = 0;
    BigReal dvmax_i = 0;
    BigReal dgmax_i = 0;
    for ( i=0,t=t0; i<(n-1); ++i,t+=4 ) {
      BigReal x = ( r2_delta * ( 1 << (i/64) ) ) / 64.0;
      BigReal dv = ( ( t[3] * x + t[2] ) * x + t[1] ) * x + t[0] - t[4];
      BigReal dg = ( 3.0 * t[3] * x + 2.0 * t[2] ) * x + t[1] - t[5];
      if ( fabs(dv) > dvmax ) { dvmax = fabs(dv); dvmax_i = i; }
      if ( fabs(dg) > dgmax ) { dgmax = fabs(dg); dgmax_i = i; }
      // if ( dv != 0.0 ) CkPrintf("TABLE %d ENERGY ERROR %g AT %g\n",j,dv,x*i);
      // if ( dg != 0.0 ) CkPrintf("TABLE %d FORCE ERROR %g AT %g\n",j,dg,x*i);
    }
/*
    if ( ( ( dvmax != 0.0 ) || ( dgmax != 0.0 ) ) && ! CkMyPe() ) {
      iout << iINFO << "NONZERO IMPRECISION IN COULOMB TABLE: " <<
	dvmax << " (" << dvmax_i << ") " << dgmax << " (" << dgmax_i << ")\n"
								<< endi;
    }
*/
  }

  for ( i=0; i<4*n; ++i ) {
    corr_table[i] = fast_table[i] + scor_table[i];
    full_table[i] = fast_table[i] + slow_table[i];
  }

  for ( i=0; i<n; ++i ) {
   for ( int j=0; j<4; ++j ) {
    table_short[16*i+j] = table_noshort[16*i+j] = vdwa_table[4*i+j];
    table_short[16*i+4+j] = table_noshort[16*i+4+j] = vdwb_table[4*i+j];
    table_short[16*i+8+j] = fast_table[4*i+j];
    table_short[16*i+12+j] = scor_table[4*i+j];
    table_noshort[16*i+8+j] = corr_table[4*i+j];
    table_noshort[16*i+12+j] = full_table[4*i+j];
   }
  }

#if 0
  char fname[100];
  sprintf(fname,"/tmp/namd.table.pe%d.dat",CkMyPe());
  FILE *f = fopen(fname,"w");
  for ( i=0; i<(n-1); ++i ) {
    const BigReal r2_base = r2_delta * ( 1 << (i/64) );
    const BigReal r2_del = r2_base / 64.0;
    const BigReal r2 = r2_base + r2_del * (i%64);
    BigReal *t;
    fprintf(f,"%g",r2);
    t = fast_table + 4*i;
    fprintf(f," %g %g %g %g", t[0], t[1], t[2], t[3]);
    t = scor_table + 4*i;
    fprintf(f," %g %g %g %g", t[0], t[1], t[2], t[3]);
    t = slow_table + 4*i;
    fprintf(f," %g %g %g %g", t[0], t[1], t[2], t[3]);
    t = corr_table + 4*i;
    fprintf(f," %g %g %g %g", t[0], t[1], t[2], t[3]);
    t = full_table + 4*i;
    fprintf(f," %g %g %g %g", t[0], t[1], t[2], t[3]);
    fprintf(f,"\n");
  }
  fclose(f);
#endif

}

// #include "PressureProfile.h"

// clear all
// define interaction type (pair or self)
#define NBPAIR	1
#define NBSELF	2

#define NBTYPE NBPAIR
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define FULLELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define MERGEELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef MERGEELECT
#define SLOWONLY
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef SLOWONLY
#undef FULLELECT
#undef  NBTYPE

#define NBTYPE NBSELF
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define FULLELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define MERGEELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef MERGEELECT
#define SLOWONLY
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef SLOWONLY
#undef FULLELECT
#undef  NBTYPE

#define FEPFLAG
#define CALCENERGY

#define NBTYPE NBPAIR
#include "ComputeNonbondedBase.h"
#define FULLELECT
#include "ComputeNonbondedBase.h"
#define MERGEELECT
#include "ComputeNonbondedBase.h"
#undef MERGEELECT
#define SLOWONLY
#include "ComputeNonbondedBase.h"
#undef SLOWONLY
#undef FULLELECT
#undef  NBTYPE

#define NBTYPE NBSELF
#include "ComputeNonbondedBase.h"
#define FULLELECT
#include "ComputeNonbondedBase.h"
#define MERGEELECT
#include "ComputeNonbondedBase.h"
#undef MERGEELECT
#define SLOWONLY
#include "ComputeNonbondedBase.h"
#undef SLOWONLY
#undef FULLELECT
#undef  NBTYPE

#undef CALCENERGY
#undef FEPFLAG

#define LESFLAG

#define NBTYPE NBPAIR
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define FULLELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define MERGEELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef MERGEELECT
#define SLOWONLY
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef SLOWONLY
#undef FULLELECT
#undef  NBTYPE

#define NBTYPE NBSELF
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define FULLELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#define MERGEELECT
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef MERGEELECT
#define SLOWONLY
#include "ComputeNonbondedBase.h"
#define CALCENERGY
#include "ComputeNonbondedBase.h"
#undef CALCENERGY
#undef SLOWONLY
#undef FULLELECT
#undef  NBTYPE

#undef LESFLAG

#if 0
#define INTFLAG
#define CALCENERGY

#define NBTYPE NBPAIR
#include "ComputeNonbondedBase.h"
#undef  NBTYPE

#define NBTYPE NBSELF
#include "ComputeNonbondedBase.h"
#undef  NBTYPE

#undef CALCENERGY
#undef INTFLAG
#endif

