












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine maxmin(qin,q1,q2,ii,io,js,je,qmin,qmax)
c
c  PURPOSE: This subroutine returns the minimum and maximum of the
c  2-D zone centered array qin over all active grid zones.  Uneven
c  boundaries, eg steps and jumps, can be accounted for.
c
c  INPUT ARGUMENTS:
c    qin   = the 2-D array to be searched
c    q1,q2 = 1-D worker arrays with dimensions of the trailin index of q
c    ii,io = vectors of starting and ending indices in the 1-direction
c            (permits using uneven boundaries, eg steps and jumps)
c    js,je = scalars of smallest starting index and largest ending index
c            in 2-direction
c
c  OUTPUT ARGUMENTS:
c    qmin = minimum value
c    qmax = maximum value
c
c  LOCALS:
c----------------------------------------------------------------------
      implicit NONE




      integer in, jn, kn, ijkn, neqm
      parameter(in =           128+5
     &        , jn =           128+5
     &        , kn =           128+5)
      parameter(ijkn =   128+5)
      parameter(neqm = 1)
c
      integer nbvar
      parameter(nbvar = 14)

c
      real*8    pi, tiny, huge
      parameter(pi   = 3.14159265358979324)
      parameter(tiny =          1.000d-99 )
      parameter(huge =          1.000d+99 )
c
      real*8    zro, one, two, haf
      parameter(zro  = 0.0 )
      parameter(one  = 1.0 )
      parameter(two  = 2.0 )
      parameter(haf  = 0.5 )
c
      integer nbuff,mreq
      parameter(nbuff = 40, mreq=300)
      real*8     qin(in,jn),q1(jn),q2(jn),qmin,qmax
      integer  ii(jn),io(jn),js,je
c
      integer  i,j
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////////
c=======================================================================
c
      do 20 j=js,je
        q1(j)= qin(ii(j),j)
        q2(j)= qin(ii(j),j)
        do 10 i=ii(j),io(j)
          q1(j) = max(q1(j), qin(i,j))
          q2(j) = min(q2(j), qin(i,j))
10      continue
20    continue
c
      qmax = q1(js)
      qmin = q2(js)
      do 30 j=js,je
	qmax = max(qmax, q1(j))
	qmin = min(qmin, q2(j))
30    continue
c
      return
      end
