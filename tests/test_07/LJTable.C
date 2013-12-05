/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include "LJTable.h"
#include <string.h>

//----------------------------------------------------------------------  
LJTable::LJTable()
{
  table_dim = 0;
  table_alloc = 0;
}

int LJTable::readfile(FILE *file) {

  char buf[1024];

  if ( ! fgets(buf,1024,file) || strcmp(buf,"LJTABLE_BEGIN\n") ) {
    printf("Missing LJTABLE_BEGIN\n");
    return -1;
  }

  if ( fscanf(file,"%d\n",&table_dim) != 1 ) {
    printf("table_dim read error\n");
    return -2;
  }

  table_alloc = new char[2*table_dim*table_dim*sizeof(TableEntry) + 31];
  char *table_align = table_alloc;
  while ( (long)table_align % 32 ) table_align++;
  table = (TableEntry *) table_align;

  for (register int i=0; i < table_dim; i++)
    for (register int j=i; j < table_dim; j++)
    {
      TableEntry *curij = &(table[2*(i*table_dim+j)]);
      TableEntry *curji = &(table[2*(j*table_dim+i)]);

      double a,b,c,d;
      if ( fscanf(file,"%lf %lf %lf %lf\n",&a,&b,&c,&d) != 4 ) {
        printf("value read error\n");
        return -3;
      }
      curij->A = a;
      curij->B = b;
      (curij+1)->A = c;
      (curij+1)->B = d;

      // Copy to transpose entry
      *curji = *curij;
      *(curji + 1) = *(curij + 1);
    }

  if ( ! fgets(buf,1024,file) || strcmp(buf,"LJTABLE_END\n") ) {
    printf("Missing LJTABLE_END\n");
    return -4;
  }

  return 0;
}

//----------------------------------------------------------------------  
LJTable::~LJTable()
{
  delete [] table_alloc;
}

//----------------------------------------------------------------------
void LJTable::compute_vdw_params(int i, int j,
				 LJTable::TableEntry *cur, 
				 LJTable::TableEntry *cur_scaled)
{
#if 0
  Parameters *params = Node::Object()->parameters;

  Real A, B, A14, B14;
  // BigReal sigma_max;
  //  We need the A and B parameters for the Van der Waals.  These can
  //  be explicitly be specified for this pair or calculated from the
  //  sigma and epsilon values for the two atom types
  if (params->get_vdw_pair_params(i,j, &A, &B, &A14, &B14))
  {
    cur->A = A;
    cur->B = B;
    cur_scaled->A = A14;
    cur_scaled->B = B14;

    BigReal sigma_ij, sigma_ij14;

    if ((B == 0) || (A/B < 0)) sigma_ij = 0;
    else sigma_ij = pow((BigReal)(A/B),(BigReal)(1./6.));

    if ((B14 == 0) || (A14/B14 < 0)) sigma_ij14 = 0;
    else sigma_ij14 = pow((BigReal)(A14/B14),(BigReal)(1./6.));

    // sigma_max = ( sigma_ij > sigma_ij14 ? sigma_ij : sigma_ij14 );
  }
  else
  {
    //  We didn't find explicit parameters for this pair. So instead,
    //  get the parameters for each atom type separately and use them
    //  to calculate the values we need
    Real sigma_i, sigma_i14, epsilon_i, epsilon_i14;
    Real sigma_j, sigma_j14, epsilon_j, epsilon_j14;

    params->get_vdw_params(&sigma_i, &epsilon_i, &sigma_i14,
				       &epsilon_i14,i);
    params->get_vdw_params(&sigma_j, &epsilon_j, &sigma_j14, 
				       &epsilon_j14,j);
  	
    BigReal sigma_ij = 0.5 * (sigma_i+sigma_j);
    BigReal epsilon_ij = sqrt(epsilon_i*epsilon_j);
    BigReal sigma_ij14 = 0.5 * (sigma_i14+sigma_j14);
    BigReal epsilon_ij14 = sqrt(epsilon_i14*epsilon_j14);

    // sigma_max = ( sigma_ij > sigma_ij14 ? sigma_ij : sigma_ij14 );

    //  Calculate sigma^6
    sigma_ij *= sigma_ij*sigma_ij;
    sigma_ij *= sigma_ij;
    sigma_ij14 *= sigma_ij14*sigma_ij14;
    sigma_ij14 *= sigma_ij14;
    
    //  Calculate LJ constants A & B
    cur->B = 4.0 * sigma_ij * epsilon_ij;
    cur->A = cur->B * sigma_ij;
    cur_scaled->B = 4.0 * sigma_ij14 * epsilon_ij14;
    cur_scaled->A = cur_scaled->B * sigma_ij14;
  }
  //  Calculate exclcut2
  // cur_scaled->exclcut2 = cur->exclcut2 = 0.64 * sigma_max * sigma_max;
#endif

}

