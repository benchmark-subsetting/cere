/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include "Patch.h"
#include "Lattice.h"

Patch::Patch() {
  numAtoms = -1;
  atoms = 0;
  f_nbond = 0;
  f_slow = 0;
  p_orig = 0;
  i_move = 0;
  int i;
  for ( i=0; i<27; ++i ) images[i] = 0;
}

Patch::~Patch() {
  clearimages();
  delete [] atoms;
  delete [] f_nbond;
  delete [] f_slow;
  delete [] p_orig;
  int i;
}

void Patch::moveatoms() {
  const BigReal mag = 0.3;
  int im = ++i_move;
  for ( int i=0; i<numAtoms; ++i ) {
    CompAtom &a = atoms[i];
    const Vector &p = p_orig[i];
    BigReal imid = (im * (a.id + 1)) | 0xffff;
    a.position.x = p.x + mag * cos(1.0 * imid);
    a.position.y = p.y + mag * cos(2.0 * imid);
    a.position.z = p.z + mag * cos(3.0 * imid);
  }
}


void Patch::zeroforces() {
  int i;
  for ( i=0; i<numAtoms; ++i ) {
    f_nbond[i] = 0.;
    f_slow[i] = 0.;
  }
}

void Patch::setforces(Vector *fn, Vector *fs) {
  int i;
  for ( i=0; i<numAtoms; ++i ) {
    fn[atoms[i].id] = f_nbond[i];
    fs[atoms[i].id] = f_slow[i];
  }
}

CompAtom* Patch::image(int trans, Lattice &lattice) {
  if ( ! images[trans] ) {
    images[trans] = lattice.create(atoms,numAtoms,trans);
  }
  return images[trans];
}

void Patch::clearimages() {
  Lattice lattice;
  int i;
  for ( i=0; i<27; ++i ) {
    lattice.destroy(&images[i],i);
  }
}

int Patch::readfile(FILE *file) {
  
  char buf[1024];

  if ( ! fgets(buf,1024,file) || strcmp(buf,"PATCH_BEGIN\n") ) {
    printf("Missing PATCH_BEGIN\n");
    return -1;
  }

  if ( fscanf(file,"%d\n",&numAtoms) != 1 ) {
    printf("numAtoms read error\n");
    return -2;
  }

  atoms = new CompAtom[numAtoms];
  f_nbond = new Vector[numAtoms];
  f_slow = new Vector[numAtoms];
  p_orig = new Vector[numAtoms];

  int i;
  for ( i=0; i<numAtoms; ++i ) {
    double x,y,z,q;
    int id,hgs,ngia,af,gf,part;
    x=y=z=q=id=hgs=ngia=af=gf=part=0;
    if ( fscanf(file,"%lf %lf %lf %lf %d %d %d %d %d %d\n",
	&x,&y,&z,&q,&id,&hgs,&ngia,&af,&gf,&part) != 10 ) {
      printf("atom read error\n");
      return -3;
    }
    CompAtom &a = atoms[i];
    Vector &p = p_orig[i];
    a.position.x = p.x = x;
    a.position.y = p.y = y;
    a.position.z = p.z = z;
    a.charge = q;
    a.id = id;
    a.hydrogenGroupSize = hgs;
    a.nonbondedGroupIsAtom = ngia;
    a.atomFixed = af;
    a.groupFixed = gf;
    a.partition = part;
  }

  if ( ! fgets(buf,1024,file) || strcmp(buf,"PATCH_END\n") ) {
    printf("Missing PATCH_END\n");
    return -4;
  }

  return 0;
}

