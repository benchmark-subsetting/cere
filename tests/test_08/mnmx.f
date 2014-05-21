












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                  M N M X                  \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine mnmx ( qty, i1, j1, k1, i2, j2, k2
     1                 , qmin, imin, jmin, kmin
     2                 , qmax, imax, jmax, kmax )
c
c    dac:zeus3d.mnmx <------------- finds extrema of a regular 3-d array
c    from mln:zeus04.minmax; jms:zeus2d.minmax            february, 1990
c
c    written by: David Clarke
c    modified 1: converted to ZEUS-MP by Mordecai-Mark Mac Low M
c
c  PURPOSE: This subroutine returns the maximum and minimum value of a
c  rectangular 3-D array, along with the coordinates of the extrema.
c
c  INPUT VARIABLES:
c    qty             the 3-D array to be searched for extrema.
c    i1              inner i index.
c    j1              inner j index.
c    k1              inner k index.
c    i2              outer i index.
c    j2              outer j index.
c    k2              outer k index.
c
c  OUTPUT VARIABLES:
c    qmin            minimum value
c    imin,jmin,kmin  coordinates of minimum value
c    qmax            maximum value
c    imax,jmax,kmax  coordinates of maximum value
c
c  LOCAL VARIABLES:
c
c  j-sweep
c    qmaxj (qminj)   vector of maximum (minimum) values of "qty" from
c                    each i-sweep.  This vector is filled during a
c                    j-sweep.
c    imaxj           i-index of each "qmaxj"
c    iminj           i-index of each "qminj"
c
c  k-sweep
c    qmaxk (qmink)   vector of maximum (minimum) values of "qmaxj"
c                    ("qminj") from each j-sweep.
c    imaxk           i-index of each "qmaxk"
c    jmaxk           j-index of each "qmaxk"
c    imink           i-index of each "qmink"
c    jmink           j-index of each "qmink"
c
c  grand maximum
c    qmax (qmin)     maximum (minimum) value of "qmaxk" ("qmink").
c    imax            i-index of "qmax"
c    jmax            j-index of "qmax"
c    kmax            k-index of "qmax"
c    imin            i-index of "qmin"
c    jmin            j-index of "qmin"
c    kmin            k-index of "qmin"
c
c----------------------------------------------------------------------
c




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

      real*8  w1da(ijkn    ) , w1db(ijkn    ) , w1dc(ijkn    )
     &,     w1dd(ijkn    ) , w1de(ijkn    ) , w1df(ijkn    )
     &,     w1dg(ijkn    ) , w1dh(ijkn    ) , w1di(ijkn    )
     &,     w1dj(ijkn    ) , w1dk(ijkn    ) , w1dl(ijkn    )
     &,     w1dm(ijkn    ) , w1dn(ijkn    ) , w1do(ijkn    )
     &,     w1dp(ijkn    ) , w1dq(ijkn    ) , w1dr(ijkn    )
     &,     w1ds(ijkn    ) , w1dt(ijkn    ) , w1du(ijkn    )

c added 1D arrays w1dk through w1du for   M-MML 4 Mar 98

      real*8  w3da(in,jn,kn) , w3db(in,jn,kn) , w3dc(in,jn,kn)
     &,     w3dd(in,jn,kn) , w3de(in,jn,kn) , w3df(in,jn,kn)
     &,     w3dg(in,jn,kn)
     &,     w3di(in,jn,kn) , w3dj(in,jn,kn)


      common /scratch/  w1da,w1db,w1dc,w1dd,w1de,w1df
     &,                 w1dg,w1dh,w1di,w1dj,w1dk,w1dl,w1dm
     &,                 w1dn,w1do,w1dp,w1dq,w1dr,w1ds,w1dt
     &,                 w1du
      common /scratch/  w3da,w3db,w3dc,w3dd,w3de,w3df,w3dg
     &,                 w3di,w3dj


       integer is, ie, js, je, ks, ke
     &       , ia, ja, ka, igcon
       integer nx1z, nx2z, nx3z
c
       common /gridcomi/
     &   is, ie, js, je, ks, ke
     & , ia, ja, ka, igcon
     & , nx1z, nx2z, nx3z
c
       real*8  x1a   (in),  x2a   (jn),  x3a   (kn)
     &     , x1ai  (in),  x2ai  (jn),  x3ai  (kn)
     &     ,dx1a   (in), dx2a   (jn), dx3a   (kn)
     &     ,dx1ai  (in), dx2ai  (jn), dx3ai  (kn)
     &     ,vol1a  (in), vol2a  (jn), vol3a  (kn)
     &     ,dvl1a  (in), dvl2a  (jn), dvl3a  (kn)
     &     ,dvl1ai (in), dvl2ai (jn), dvl3ai (kn)
       real*8  g2a   (in), g31a   (in), dg2ad1 (in)
     &     , g2ai  (in), g31ai  (in), dg31ad1(in)
       real*8  g32a  (jn), g32ai  (jn), dg32ad2(jn)
     &     , g4 a  (jn)
c
       real*8  x1b   (in),  x2b   (jn),  x3b   (kn)
     &     , x1bi  (in),  x2bi  (jn),  x3bi  (kn)
     &     ,dx1b   (in), dx2b   (jn), dx3b   (kn)
     &     ,dx1bi  (in), dx2bi  (jn), dx3bi  (kn)
     &     ,vol1b  (in), vol2b  (jn), vol3b  (kn)
     &     ,dvl1b  (in), dvl2b  (jn), dvl3b  (kn)
     &     ,dvl1bi (in), dvl2bi (jn), dvl3bi (kn)
       real*8  g2b   (in), g31b   (in), dg2bd1 (in) 
     &     , g2bi  (in), g31bi  (in), dg31bd1(in)
       real*8  g32b  (jn), g32bi  (jn), dg32bd2(jn)
     &     , g4 b  (jn)
c
       real*8   vg1  (in),   vg2  (jn),   vg3  (kn)
       real*8 x1fac, x2fac, x3fac
c
       common /gridcomr/
     &       x1a   ,  x2a   ,  x3a   
     &     , x1ai  ,  x2ai  ,  x3ai  
     &     ,dx1a   , dx2a   , dx3a   
     &     ,dx1ai  , dx2ai  , dx3ai  
     &     ,vol1a  , vol2a  , vol3a  
     &     ,dvl1a  , dvl2a  , dvl3a  
     &     ,dvl1ai , dvl2ai , dvl3ai 
     &     , g2a   , g31a   , dg2ad1 
     &     , g2ai  , g31ai  , dg31ad1
     &     , g32a  , g32ai  , dg32ad2
     &     , g4 a
c
       common /gridcomr/
     &       x1b   ,  x2b   ,  x3b   
     &     , x1bi  ,  x2bi  ,  x3bi  
     &     ,dx1b   , dx2b   , dx3b   
     &     ,dx1bi  , dx2bi  , dx3bi  
     &     ,vol1b  , vol2b  , vol3b  
     &     ,dvl1b  , dvl2b  , dvl3b  
     &     ,dvl1bi , dvl2bi , dvl3bi 
     &     , g2b   , g31b   , dg2bd1  
     &     , g2bi  , g31bi  , dg31bd1
     &     , g32b  , g32bi  , dg32bd2
     &     , g4 b
c
       common /gridcomr/
     &        vg1  ,   vg2  ,   vg3  
     &     , x1fac , x2fac  , x3fac
c

C*ca imp
C*ca par
C*ca scratch
c
       integer       j       , k       , i1      , j1      , k1
     2             , i2      , j2      , k2      , imin    , jmin
     3             , kmin    , imax    , jmax    , kmax
       real*8        qmin    , qmax
c
       integer       imaxj   (ijkn), iminj   (ijkn)
     1             , imaxk   (ijkn), imink   (ijkn)
     2             , jmaxk   (ijkn), jmink   (ijkn)
       real*8        qmaxj   (ijkn), qminj   (ijkn)
     1             , qmaxk   (ijkn), qmink   (ijkn)
c
       real*8        qty     (  in,  jn,  kn)
c
       equivalence   ( imaxj   , wq1d     ), ( iminj   , wr1d     )
     1             , ( imaxk   , ws1d     ), ( imink   , wt1d     )
     2             , ( jmaxk   , wu1d     ), ( jmink   , wv1d     )
     3             , ( qmaxj   , ww1d     ), ( qminj   , wx1d     )
     4             , ( qmaxk   , wy1d     ), ( qmink   , wz1d     )
       integer       ijkl    , ijkx    , ijks    , i
       real*8        q       (  in * jn * kn)
       equivalence   ( q       , w3dg     )
c
c WARNING: Scratch array wg3d is used by this routine (mnmx)!
c
c
c      External statements
c
       integer       ISMIN   , ISMAX
       external      ISMIN   , ISMAX
c
c-----------------------------------------------------------------------
c
       k = k1
       j = j1
       ijkl = 0

       do 20 k=k1,k2


         do 10 j=j1,j2


           do 5 i=i1,i2

             ijkl = ijkl + 1
             q(ijkl) = qty(i,j,k)
5          continue
10       continue
20     continue

       ijkl = (i2 - i1 + 1) * (j2 - j1 + 1) * (k2 - k1 + 1)
       ijkx = ISMAX(ijkl,q,1)
       qmax = q(ijkx)
       kmax = ijkx/((i2-i1+1)*(j2-j1+1)) + k1
       jmax = (ijkx-(kmax-k1)*(i2-i1+1)*(j2-j1+1))/(i2-i1+1) + j1
       imax = ijkx-(kmax-k1)*(i2-i1+1)*(j2-j1+1)
     1      - (jmax-j1)*(i2-i1+1) + i1
       ijks = ISMIN(ijkl,q,1)
       qmin = q(ijks)
       kmin = ijks/((i2-i1+1)*(j2-j1+1)) + k1
       jmin = (ijks-(kmin-k1)*(i2-i1+1)*(j2-j1+1))/(i2-i1+1) + j1
       imin = ijks-(kmin-k1)*(i2-i1+1)*(j2-j1+1)
     1      - (jmin-j1)*(i2-i1+1) + i1
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                  M N M X                  \\\\\\\\\\
c
c=======================================================================
