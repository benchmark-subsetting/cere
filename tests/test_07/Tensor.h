/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef TENSOR_H
#define TENSOR_H

#include <math.h>
#include <stdio.h>
#include "common.h"
#include "Vector.h"

class Tensor {
   public:
     BigReal xx, xy, xz;
     BigReal yx, yy, yz;
     BigReal zx, zy, zz;
     
     inline Tensor(void) {
       xx=xy=xz=yx=yy=yz=zx=zy=zz=0.0;
     }

     inline Tensor(const Tensor &t2) {
       xx = t2.xx; xy = t2.xy; xz = t2.xz;
       yx = t2.yx; yy = t2.yy; yz = t2.yz;
       zx = t2.zx; zy = t2.zy; zz = t2.zz;
     }

     static inline Tensor identity(BigReal v1 = 1.0) {
       Tensor tmp;
       tmp.xx = tmp.yy = tmp.zz = v1;
       return tmp;
     }

     static inline Tensor diagonal(const Vector &v1) {
       Tensor tmp;
       tmp.xx = v1.x; tmp.xy = 0; tmp.xz = 0;
       tmp.yx = 0; tmp.yy = v1.y; tmp.yz = 0;
       tmp.zx = 0; tmp.zy = 0; tmp.zz = v1.z;
       return tmp;
     }

     static inline Tensor symmetric(const Vector &v1, const Vector &v2) {
       Tensor tmp;
       tmp.xx = v1.x; tmp.xy = v2.x; tmp.xz = v2.y;
       tmp.yx = v2.x; tmp.yy = v1.y; tmp.yz = v2.z;
       tmp.zx = v2.y; tmp.zy = v2.z; tmp.zz = v1.z;
       return tmp;
     }

     static inline Tensor triangular(const Vector &v1, const Vector &v2) {
       Tensor tmp;
       tmp.xx = v1.x; tmp.xy = v2.x; tmp.xz = v2.y;
       tmp.yx = 0; tmp.yy = v1.y; tmp.yz = v2.z;
       tmp.zx = 0; tmp.zy = 0; tmp.zz = v1.z;
       return tmp;
     }

     ~Tensor(void) { }

     inline Tensor& operator=(const Tensor &t2) {
       xx = t2.xx; xy = t2.xy; xz = t2.xz;
       yx = t2.yx; yy = t2.yy; yz = t2.yz;
       zx = t2.zx; zy = t2.zy; zz = t2.zz;
       return *this;
     }

     inline Tensor& operator=(const BigReal &r2) {
       xx=xy=xz=yx=yy=yz=zx=zy=zz=r2;
       return *this;
     }

     inline Tensor& operator+=(const Tensor &t2) {
       xx += t2.xx; xy += t2.xy; xz += t2.xz;
       yx += t2.yx; yy += t2.yy; yz += t2.yz;
       zx += t2.zx; zy += t2.zy; zz += t2.zz;
       return *this;
     }

     inline Tensor& operator-=(const Tensor &t2) {
       xx -= t2.xx; xy -= t2.xy; xz -= t2.xz;
       yx -= t2.yx; yy -= t2.yy; yz -= t2.yz;
       zx -= t2.zx; zy -= t2.zy; zz -= t2.zz;
       return *this;
     }

     inline Tensor& operator*=(const BigReal &r2) {
       xx *= r2; xy *= r2; xz *= r2;
       yx *= r2; yy *= r2; yz *= r2;
       zx *= r2; zy *= r2; zz *= r2;
       return *this;
     }

     inline Tensor& operator/=(const BigReal &r2) {
       xx /= r2; xy /= r2; xz /= r2;
       yx /= r2; yy /= r2; yz /= r2;
       zx /= r2; zy /= r2; zz /= r2;
       return *this;
     }

     inline friend int operator==(const Tensor &t1, const Tensor &t2) {
       return (
       t1.xx == t2.xx && t1.xy == t2.xy && t1.xz == t2.xz &&
       t1.yx == t2.yx && t1.yy == t2.yy && t1.yz == t2.yz &&
       t1.zx == t2.zx && t1.zy == t2.zy && t1.zz == t2.zz );
     }

     inline friend int operator!=(const Tensor &t1, const Tensor &t2) {
       return ( ! ( t1 == t2 ) );
     }

     inline friend Tensor operator+(const Tensor& t1, const Tensor& t2) {
       Tensor tmp(t1);
       tmp += t2;
       return tmp;
     }

     inline friend Tensor operator-(const Tensor& t1, const Tensor& t2) {
       Tensor tmp(t1);
       tmp -= t2;
       return tmp;
     }

     inline friend Tensor operator-(const Tensor &t1) {
       Tensor tmp(t1);
       tmp *= -1.0;
       return tmp;
     }

     inline friend Tensor operator*(const BigReal &r1, const Tensor &t2) {
       Tensor tmp(t2);
       tmp *= r1;
       return tmp;
     }

     inline friend Tensor operator*(const Tensor &t1, const BigReal &r2) {
       Tensor tmp(t1);
       tmp *= r2;
       return tmp;
     }

     inline friend Tensor operator/(const Tensor &t1, const BigReal &r2) {
       Tensor tmp(t1);
       tmp /= r2;
       return tmp;
     }

     inline friend Vector operator*(const Tensor &t1, const Vector &v2) {
       Vector tmp;
       tmp.x = t1.xx * v2.x + t1.xy * v2.y + t1.xz * v2.z;
       tmp.y = t1.yx * v2.x + t1.yy * v2.y + t1.yz * v2.z;
       tmp.z = t1.zx * v2.x + t1.zy * v2.y + t1.zz * v2.z;
       return tmp;
     }

     inline friend Vector operator*(const Vector &v1, const Tensor &t2) {
       Vector tmp;
       tmp.x = t2.xx * v1.x + t2.yx * v1.y + t2.zx * v1.z;
       tmp.y = t2.xy * v1.x + t2.yy * v1.y + t2.zy * v1.z;
       tmp.z = t2.xz * v1.x + t2.yz * v1.y + t2.zz * v1.z;
       return tmp;
     }

     inline friend Tensor outer(const Vector &v1, const Vector &v2);

     inline friend Tensor transpose(const Tensor &t1) {
       Tensor tmp;
       tmp.xx = t1.xx; tmp.yx = t1.xy; tmp.zx = t1.xz;
       tmp.xy = t1.yx; tmp.yy = t1.yy; tmp.zy = t1.yz;
       tmp.xz = t1.zx; tmp.yz = t1.zy; tmp.zz = t1.zz;
       return tmp;
     }

     inline friend Tensor symmetric(const Tensor &t1) {
       Tensor tmp;
       tmp.xx = t1.xx; tmp.xy = 0.5*(t1.xy+t1.yx); tmp.xz = 0.5*(t1.xz+t1.zx);
       tmp.yx = tmp.xy; tmp.yy = t1.yy; tmp.yz = 0.5*(t1.yz+t1.zy);
       tmp.zx = tmp.xz; tmp.zy = tmp.yz; tmp.zz = t1.zz;
       return tmp;
     }

     inline friend Tensor triangular(const Tensor &t1) {
       Tensor tmp;
       tmp.xx = t1.xx; tmp.xy = 0.5*(t1.xy+t1.yx); tmp.xz = 0.5*(t1.xz+t1.zx);
       tmp.yx = 0; tmp.yy = t1.yy; tmp.yz = 0.5*(t1.yz+t1.zy);
       tmp.zx = 0; tmp.zy = 0; tmp.zz = t1.zz;
       return tmp;
     }

     inline friend Vector diagonal(const Tensor &t1) {
       return Vector(t1.xx,t1.yy,t1.zz);
     }

     inline friend Vector off_diagonal(const Tensor &t1) {
       return Vector(t1.xy,t1.xz,t1.yz);
     }

     inline friend BigReal trace(const Tensor &t1) {
       return (t1.xx + t1.yy + t1.zz);
     }

/*
     // set the vector based on a string.  If bad, return FALSE
     // the string can be in the form "x y z" or "x, y, z"
     Bool set(const char *s) {
	double a[3];    // read into doubles, since I don't know what
	char tmp[100];  // a "BigReal" is in real life
	// cheap way to get commas, etc.  a poor regex
       int i=sscanf(s, "%lf%99[ \t,]%lf%99[ \t,]%lf%99s",
                    a, tmp, a+1, tmp, a+2, tmp);
       if (i != 5) return FALSE;
       const char *t = s;       // now count commas (for "1,,,,2,  , 3")
       int flg = 0;                 // and check for "1 2,,3"
       i = 0;
       for (;*t;t++) {
          if (*t == ',') { 
             if (flg == 0) {   // expecting non-whitespace
                return FALSE;  //    so error
             }
             flg = 0;          // now expect non-whitespace
             i++;              // and increment comma counter
          }
          else if (*t != ' ' && *t != '\t') {  // got non-whitespace
             flg = 1;          // so the next can be whitespace or commas
          }
       }
       if (i == 0 || i == 2) {  // allow "1 2 3" or "1, 2,3" forms
          x = a[0]; y = a[1]; z = a[2];
          return TRUE;
       }
       return FALSE;
     }
*/

};

     inline Tensor outer(const Vector &v1, const Vector &v2) {
       Tensor tmp;
       tmp.xx = v1.x * v2.x;
       tmp.xy = v1.x * v2.y;
       tmp.xz = v1.x * v2.z;
       tmp.yx = v1.y * v2.x;
       tmp.yy = v1.y * v2.y;
       tmp.yz = v1.y * v2.z;
       tmp.zx = v1.z * v2.x;
       tmp.zy = v1.z * v2.y;
       tmp.zz = v1.z * v2.z;
       return tmp;
     }

#endif

