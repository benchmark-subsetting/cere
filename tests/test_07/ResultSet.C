
#include "ResultSet.h"
#include <stdlib.h>

void ResultSet::writefile(FILE *file) {
  int i;
  fprintf(file,"RESULTSET_BEGIN\n");
  fprintf(file,"%d %d %d %d\n",numAtoms,doEnergy,doFull,doMerge);
  for ( i=0; i<outputDataSize; i+=3 ) {
    fprintf(file,"%f %f %f\n",reductions[i],reductions[i+1],reductions[i+2]);
  }
#define XYZ(A) A.x,A.y,A.z
  fprintf(file,"%f %f %f %f\n",XYZ(netf_nbond),rmsf_nbond);
  fprintf(file,"%f %f %f %f\n",XYZ(netf_slow),rmsf_slow);
  fprintf(file,"%f %f %f %f\n",XYZ(netf_both),rmsf_both);
#undef XYZ
  fprintf(file,"RESULTSET_END\n");
}

int ResultSet::readfile(FILE *file) {
  int i;
  int numAtoms_in = 0;
  fscanf(file,"RESULTSET_BEGIN\n");
  fscanf(file,"%d %d %d %d\n",&numAtoms_in,&doEnergy,&doFull,&doMerge);
  if ( numAtoms_in != numAtoms ) {
    printf("atom number mismatch in standard file\n");
    exit(-9);
  }
  for ( i=0; i<outputDataSize; i+=3 ) {
    fscanf(file,"%lf %lf %lf\n",reductions+i,reductions+i+1,reductions+i+2);
  }
#define XYZ(A) &(A.x),&(A.y),&(A.z)
  fscanf(file,"%lf %lf %lf %lf\n",XYZ(netf_nbond),&rmsf_nbond);
  fscanf(file,"%lf %lf %lf %lf\n",XYZ(netf_slow),&rmsf_slow);
  fscanf(file,"%lf %lf %lf %lf\n",XYZ(netf_both),&rmsf_both);
#undef XYZ
  fscanf(file,"RESULTSET_END\n");

  return 0;
}

void equal(double x, double y) {
  double d = x - y;
  if ( d > 0.00001 || d < -0.00001 ) {
    printf("error: numeric test failed! (error = %g)\n",d);
    exit(-10);
  }
}

void equal(double *x, double *y) {
  int i;
  for ( i=0; i<9; ++i ) equal(x[i],y[i]);
}

void equal(double *w, double *x, double *y, double *z) {
  int i;
  for ( i=0; i<9; ++i ) equal(w[i]+x[i],y[i]+z[i]);
}

void ResultSet::check() {
  equal(netf_nbond.length(),0.);
  equal(netf_slow.length(),0.);
  equal(netf_both.length(),0.);
}

void ResultSet::samemode(ResultSet &r) {
  if ( doEnergy != r.doEnergy ||
       doFull != r.doFull ||
       doMerge != r.doMerge ) {
    printf("mode mismatch in standard file\n");
    exit(-9);
  }
}

void ResultSet::compare(ResultSet &r) {

  check();
  r.check();
  int e = ( doEnergy == r.doEnergy );
  if(e)equal(reductions[2],r.reductions[2]);
  if ( doFull == r.doFull ) {
    if(e)equal(reductions[0]+reductions[1],r.reductions[0]+r.reductions[1]);
    equal(rmsf_both,r.rmsf_both);
    equal(reductions+3,reductions+12,r.reductions+3,r.reductions+12);
    if ( doMerge == r.doMerge ) {
      if(e)equal(reductions[0],r.reductions[0]);
      equal(rmsf_nbond, r.rmsf_nbond);
      equal(reductions+3,r.reductions+3);
      if(e)equal(reductions[1],r.reductions[1]);
      equal(rmsf_slow, r.rmsf_slow);
      equal(reductions+12,r.reductions+12);
    }
  }
  if ( !doMerge && !r.doMerge ) {
    if(e)equal(reductions[0],r.reductions[0]);
    equal(rmsf_nbond, r.rmsf_nbond);
    equal(reductions+3,r.reductions+3);
  }

}

