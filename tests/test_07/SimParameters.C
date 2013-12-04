/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "SimParameters.h"

SimParameters::SimParameters() {

#define SIMPARAM(T,N,V) N = V
#include "SimParamList.h"
#undef SIMPARAM

}

inline int parse_param(const char *param, const char *name,
			const char *value, int &var) {
  if ( ! strcmp(param,name) ) { var = atoi(value); return 1; }
  return 0;
}

inline int parse_param(const char *param, const char *name,
			const char *value, BigReal &var) {
  if ( ! strcmp(param,name) ) { var = atof(value); return 1; }
  return 0;
}

inline int parse_param(const char *param, const char *name,
			const char *value, Vector &var) {
  if ( ! strcmp(param,name) ) { var.set(value); return 1; }
  return 0;
}


int SimParameters::setparam(const char *param, const char *value) {

#define SIMPARAM(T,N,V) if ( parse_param(param,#N,value,N) ) return 0;
#include "SimParamList.h"
#undef SIMPARAM

  return -1;
}

int SimParameters::readfile(FILE *file) {

  char buf[1024];

  if ( ! fgets(buf,1024,file) || strcmp(buf,"SIMPARAMETERS_BEGIN\n") ) {
    printf("Missing SIMPARAMETERS_BEGIN\n");
    return -1;
  }

  while ( 1 ) {
    if ( ! fgets(buf,1024,file) ) {
      printf("Missing SIMPARAMETERS_END\n");
      return -3;
    }
    if ( ! strcmp(buf,"SIMPARAMETERS_END\n") ) {
      break;
    }
    char *param = strtok(buf," ");
    if ( ! param ) continue;
    char *value = strtok(0,"#");
    if ( ! value ) {
      printf("Missing value for %s\n",param);
      return -4;
    }
    if ( setparam(param,value) ) {
      printf("Bad parameter %s %s\n",param,value);
      return -5;
    }
  }

  lattice.set(cellBasisVector1,cellBasisVector2,cellBasisVector3,cellOrigin);

  return 0;
}

