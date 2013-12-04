
#ifndef RESULTSET_H
#define RESULTSET_H
#include "Vector.h"
#include "ReductionMgr.h"
#include <stdio.h>

class ResultSet {
public:
  int numAtoms, doEnergy, doFull, doMerge;
  Vector * f_nbond;
  Vector * f_slow;
  double reductions[outputDataSize];
  Vector netf_nbond;
  Vector netf_slow;
  Vector netf_both;
  double rmsf_nbond;
  double rmsf_slow;
  double rmsf_both;

  ResultSet(int n) {
    numAtoms = n;
    doEnergy = doFull = doMerge = -1;
    f_nbond = new Vector[numAtoms];
    f_slow = new Vector[numAtoms];
  }
  ~ResultSet() {
    delete [] f_nbond;
    delete [] f_slow;
  }

  void writefile(FILE *);
  int readfile(FILE *);

  void check();
  void samemode(ResultSet &);
  void compare(ResultSet &);

};

#endif // RESULTSET_H

