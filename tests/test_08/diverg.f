












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                D I V E R G                \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine diverg ( c1, c2, c3, inorm, isum, div, sumd )
c
c    dac:zeus3d.diverg <------------ computes divergence of vector field
c    from jms:zeus2d.divb                                september, 1990
c
c    written by: David Clarke
c    modified 1: 11-17-99 by PSLi, for ZeusMP.
c
c  PURPOSE:  Computes divergence of vector field (c1, c2, c3), where
c  each of c1, c2, and c3 are face-centred quantities.  Therefore, the
c  divergence will be a zone-centred quantity, and is computed in the
c  range: ismn to iemx, jsmn to jemx, and ksmn to kemx.
c
c  INPUT VARIABLES:
c    c1       1-component of vector field
c    c2       2-component of vector field
c    c3       3-component of vector field
c    inorm    =1 => normalise divergence
c             =0 => do not normalise divergence
c    isum     =1 => perform reduction sum on "div"
c             =0 => no reduction
c
c  OUTPUT VARIABLES:
c    div      divergence of vector field
c    sumd     sum of "div"
c
c  LOCAL VARIABLES:
c
c  EXTERNALS: [NONE]
c
c-----------------------------------------------------------------------
c
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

       integer       i       , j       , k       , l       , ip1
     1             , jp1     , kp1     , inorm   , isum
       real*8        sumd    , sumn    , nmax    , nmaxi
c
       real*8        norm    (ijkn), sumdk   (ijkn), sumnk   (ijkn)
     1           , nmaxk   (ijkn)
c
       real*8        c1      (  in,  jn,  kn), c2      (  in,  jn,  kn)
     1           , c3      (  in,  jn,  kn), div     (  in,  jn,  kn)
c
c      Careful!  "wa1d" - "wi1d" are used by BININT.
c
C       equivalence   ( norm    , ww1d     ), ( sumdk   , wx1d     )
C     1             , ( sumnk   , wy1d     ), ( nmaxk   , wz1d     )
c
c-----------------------------------------------------------------------
c
c      Compute divergence of vector field ("div").
c

       do 30 k=ks-2,ke+2

         kp1 = k + 1

         do 20 j=js-2,je+2

           jp1 = j + 1

           do 10 i=is-2,ie+2

             ip1        = i + 1
             div(i,j,k) = ( g2a(ip1) * g31a(ip1) * c1(ip1,j,k)
     1                    - g2a(i  ) * g31a(i  ) * c1(i  ,j,k) )
     2                  *                         dvl1ai(i)
     3                  + ( g32a(jp1) * c2(i,jp1,k)
     4                    - g32a(j  ) * c2(i,j  ,k) )
     5                  *   g2bi(i)             * dvl2ai(j)
     6                  + ( c3(i,j,kp1) - c3(i,j,k) )
     7                  *   g31bi(i) * g32bi(j) * dvl3ai(k)
10         continue
20       continue
30     continue
c
c      Perform reduction on "div" if desired.  The reduction is done
c  first over each k-sweep, then for all k.  This is done to aid the
c  EDITOR autotasking process.
c
       sumd = 0.0
       if (isum .eq. 1) then

         do 60 k=ks,ke


           do 50 j=js,je


             do 40 i=is,ie

               sumd = sumd + div(i,j,k)
40           continue
50         continue
60       continue
       endif
c
c      Normalise divergence field if desired.
c
       if (inorm .eq. 1) then
c
c      Evaluate two normalising constants:
c
c  1.  sumn = sum over all grid zones the ratio: (average absolute
c      magnetic field) / (sum of grid zone dimensions), and
c
c  2.  nmax = maximum over the grid of the above ratios.
c
         sumn = 0.0
         nmax = 0.0

         do 90 k=ks,ke

           kp1      = k + 1

           do 80 j=js,je

             jp1 = j + 1

             do 70 i=is,ie

               ip1      = i + 1
               norm (1) = 0.5 * ( abs ( c1(ip1,j  ,k  ) + c1(i,j,k) )
     1                          + abs ( c2(i  ,jp1,k  ) + c2(i,j,k) )
     2                          + abs ( c3(i  ,j  ,kp1) + c3(i,j,k) ) )
     3                  / (dx1a(i) + g2b(i) * dx2a(j)   +
     4                     g31b(i) * g32b(j) * dx3a(k) )
               sumn     = sumn     + norm(1)
               nmax     =   max ( nmax    , norm(1) )
70           continue
80         continue
90       continue
c
c      Apply normalising constant "sumn" to the scalar "sumd" and
c  "nmax" to the array "div".
c
         if (sumn .eq. 0.0) sumd  = 0.0
         if (sumn .ne. 0.0) sumd  = sumd / sumn
         if (nmax .eq. 0.0) nmaxi = 0.0
         if (nmax .ne. 0.0) nmaxi = 1.0 / nmax

         do 120 k=ks,ke


           do 110 j=js,je


             do 100 i=is,ie

               div(i,j,k) = div(i,j,k) * nmaxi
100          continue
110        continue
120      continue
c
       endif
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                D I V E R G                \\\\\\\\\\
c
c=======================================================================
c
c
