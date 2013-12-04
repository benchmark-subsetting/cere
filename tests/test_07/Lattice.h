/**
***  Copyright (c) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 by
***  The Board of Trustees of the University of Illinois.
***  All rights reserved.
**/

#ifndef LATTICE_H
#define LATTICE_H

#include "NamdTypes.h"
#include <math.h>
#include "Tensor.h"

// need macro for RINT() on Windows (but it is slower)
#define RINT(X) floor((X)+0.5)

typedef Vector ScaledPosition;

class Lattice
{
public:
  Lattice(void) : a1(0,0,0), a2(0,0,0), a3(0,0,0),
                  b1(0,0,0), b2(0,0,0), b3(0,0,0),
                  o(0,0,0), p1(0), p2(0), p3(0) {};

  // maps a transformation triplet onto a single integer
  static int index(int i=0, int j=0, int k=0)
  {
    return 9 * (k+1) + 3 * (j+1) + (i+1);
  }

  // sets lattice basis vectors and origin (fixed center)
  void set(Vector A, Vector B, Vector C, Position Origin)
  {
    a1 = A; a2 = B; a3 = C; o = Origin;
    p1 = ( a1.length2() ? 1 : 0 );
    p2 = ( a2.length2() ? 1 : 0 );
    p3 = ( a3.length2() ? 1 : 0 );
    if ( ! p1 ) a1 = Vector(1.0,0.0,0.0);
    if ( ! p2 ) {
      Vector u1 = a1 / a1.length();
      Vector e_z(0.0,0.0,1.0);
      if ( fabs(e_z * u1) < 0.9 ) { a2 = cross(e_z,a1); }
      else { a2 = cross(Vector(1.0,0.0,0.0),a1); }
      a2 /= a2.length();
    }
    if ( ! p3 ) {
      a3 = cross(a1,a2);
      a3 /= a3.length();
    }
    if ( volume() < 0.0 ) a3 *= -1.0;
    recalculate();
  }

  // rescale lattice dimensions by factor, origin doesn't move
  void rescale(Tensor factor)
  {
    a1 = factor * a1;
    a2 = factor * a2;
    a3 = factor * a3;
    recalculate();
  }

  // rescale a position, keeping origin constant, assume 3D
  void rescale(Position &p, Tensor factor) const
  {
    p -= o;
    p = factor * p;
    p += o;
  }

  // transform scaled position to unscaled position
  Position unscale(ScaledPosition s) const
  {
    return (o + a1*s.x + a2*s.y + a3*s.z);
  }

  // transform unscaled position to scaled position
  ScaledPosition scale(Position p) const
  {
    p -= o;
    return Vector(b1*p,b2*p,b3*p);
  }

  // transforms a position nearest to a SCALED reference position
  Position nearest(Position data, ScaledPosition ref) const
  {
    ScaledPosition sn = scale(data);
    if ( p1 ) {
      BigReal tmp = sn.x - ref.x;
      sn.x = ref.x + tmp - RINT(tmp);
    }
    if ( p2 ) {
      BigReal tmp = sn.y - ref.y;
      sn.y = ref.y + tmp - RINT(tmp);
    }
    if ( p3 ) {
      BigReal tmp = sn.z - ref.z;
      sn.z = ref.z + tmp - RINT(tmp);
    }
    return unscale(sn);
  }

  // transforms a position nearest to a SCALED reference position
  // adds transform for later reversal
  Position nearest(Position data, ScaledPosition ref, Transform *t) const
  {
    ScaledPosition sn = scale(data);
    if ( p1 ) {
      BigReal tmp = sn.x - ref.x;
      BigReal rit = RINT(tmp);
      sn.x = ref.x + tmp - rit;
      t->i -= (int) rit;
    }
    if ( p2 ) {
      BigReal tmp = sn.y - ref.y;
      BigReal rit = RINT(tmp);
      sn.y = ref.y + tmp - rit;
      t->j -= (int) rit;
    }
    if ( p3 ) {
      BigReal tmp = sn.z - ref.z;
      BigReal rit = RINT(tmp);
      sn.z = ref.z + tmp - rit;
      t->k -= (int) rit;
    }
    return unscale(sn);
  }

  // applies stored transform to original coordinates
  Position apply_transform(Position data, const Transform &t) const
  {
    return ( data + t.i*a1 + t.j*a2 + t.k*a3 );
  }

  // reverses cumulative transformations for output
  Position reverse_transform(Position data, const Transform &t) const
  {
    return ( data - t.i*a1 - t.j*a2 - t.k*a3 );
  }

  // calculates shortest vector from p2 to p1 (equivalent to p1 - p2)
  Vector delta(Position pos1, Position pos2) const
  {
    Vector diff = pos1 - pos2;
    Vector result = diff;
    if ( p1 ) result -= a1*RINT(b1*diff);
    if ( p2 ) result -= a2*RINT(b2*diff);
    if ( p3 ) result -= a3*RINT(b3*diff);
    return result;
  }

  // calculates shortest vector from origin to p1 (equivalent to p1 - o)
  Vector delta(Position pos1) const
  {
    Vector diff = pos1 - o;
    Vector result = diff;
    if ( p1 ) result -= a1*RINT(b1*diff);
    if ( p2 ) result -= a2*RINT(b2*diff);
    if ( p3 ) result -= a3*RINT(b3*diff);
    return result;
  }

  // calculates vector to bring p1 closest to origin
  Vector wrap_delta(Position pos1) const
  {
    Vector diff = pos1 - o;
    Vector result(0.,0.,0.);
    if ( p1 ) result -= a1*RINT(b1*diff);
    if ( p2 ) result -= a2*RINT(b2*diff);
    if ( p3 ) result -= a3*RINT(b3*diff);
    return result;
  }

  // calculates vector to bring p1 closest to origin in unscaled coordinates
  Vector wrap_nearest_delta(Position pos1) const
  {
    Vector diff = pos1 - o;
    Vector result0(0.,0.,0.);
    if ( p1 ) result0 -= a1*RINT(b1*diff);
    if ( p2 ) result0 -= a2*RINT(b2*diff);
    if ( p3 ) result0 -= a3*RINT(b3*diff);
    diff += result0;
    BigReal dist = diff.length2();
    Vector result(0.,0.,0.);
    for ( int i1=-p1; i1<=p1; ++i1 ) {
      for ( int i2 =-p2; i2<=p2; ++i2 ) {
        for ( int i3 =-p3; i3<=p3; ++i3 ) {
          Vector newresult = i1*a1+i2*a2+i3*a3;
          BigReal newdist = (diff+newresult).length2();
          if ( newdist < dist ) {
            dist = newdist;
            result = newresult;
          }
        }
      }
    }
    return result0 + result;
  }

  CompAtom* create(CompAtom *d, int n, int i) const
  {
    CompAtom *dt;
    if ( i != 13 )
    {
      dt = new CompAtom[n];
      Vector shift = (i%3-1) * a1 + ((i/3)%3-1) * a2 + (i/9-1) * a3;
      for( int j = 0; j < n; ++j ) {
        dt[j] = d[j];
        dt[j].position += shift;
      }
    }
    else
    {
      dt = d;
    }
    return dt;
  }

  void destroy(CompAtom **d, int i) const
  {
    if ( i != 13 ) delete [] *d;
    *d = NULL;
  }

  // lattice vectors
  Vector a() const { return a1; }
  Vector b() const { return a2; }
  Vector c() const { return a3; }

  // only if along x y z axes
  int orthogonal() const {
    return ( ! ( a1.y || a1.z || a2.x || a2.z || a3.x || a3.y ) );
  }

  // origin (fixed center of cell)
  Vector origin() const
  {
    return o;
  }

  // reciprocal lattice vectors
  Vector a_r() const { return b1; }
  Vector b_r() const { return b2; }
  Vector c_r() const { return b3; }

  // periodic along this direction
  int a_p() const { return p1; }
  int b_p() const { return p2; }
  int c_p() const { return p3; }

  BigReal volume(void) const
  {
    return ( p1 && p2 && p3 ? cross(a1,a2) * a3 : 0.0 );
  }

private:
  Vector a1,a2,a3; // real lattice vectors
  Vector b1,b2,b3; // reciprocal lattice vectors (more or less)
  Vector o; // origin (fixed center of cell)
  int p1, p2, p3; // periodic along this lattice vector?

  // calculate reciprocal lattice vectors
  void recalculate(void) {
    {
      Vector c = cross(a2,a3);
      b1 = c / ( a1 * c );
    }
    {
      Vector c = cross(a3,a1);
      b2 = c / ( a2 * c );
    }
    {
      Vector c = cross(a1,a2);
      b3 = c / ( a3 * c );
    }
  }

};

#endif

