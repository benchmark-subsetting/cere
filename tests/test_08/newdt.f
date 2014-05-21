












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 N E W D T                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine newdt (ibeg,iend,jbeg,jend,kbeg,kend
     &                ,  imin,jmin,kmin
     &                ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)
c
c    mln:zeus3d.nudt <------------------------ mhd time step calculation
c                                                              may, 1986
c
c    written by: Mike Norman
c    modified 1: May, 1986 by David Clarke; adapted for mhd.
c    modified 2: October, 1987 by Mike Norman; reworked for covariant
c                formulation.
c    modified 3: June, 1988 by Jim Stone; incorporated into ZEUS2D.
c    modified 4: February, 1990 by David Clarke; incorporated into
c                ZEUS3D.
c    modified 5: September, 1990 by David Clarke; moved magnetic fields
c                to face-centres.
c    modified 6: Feb. 15, 1996 by Robert Fiedler; completely rewritten
c                for ZEUS-MP.
c    modified 7: Dec. 23, 1996 by Robert Fiedler; added radiation.
c
c  PURPOSE:  Computes the new timestep for explicit calculations from
c  the values of the field variables updated by the source and transport
c  steps.
c
c  In explicit calculations, the timestep is given by:
c
c     dt = courno * sqrt [ min ( dtcs**2 + dtv1**2 + dtv2**2 + dtv3**2
c                              + dtal**2 + dtqq**2 ) ]
c
c  where the variable names are described below.  The timestep can be
c  reduced in size by any amount, but can be larger than the old timstep
c  by no more than a factor of 1.26.
c
c BOUNDARY VALUES USED:
c
c    Macro defined  var   ii    oi    ij    oj    ik    ok
c    -------------  ---  ----  ----  ----  ----  ----  ----
c                    d   is-1        js-1        ks-1
c    TOTAL_ENERGY    d   is-1  ie+1  js-1  je+1  ks-1  ke+1
c    TOTAL_ENERGY    s1        ie+1
c    TOTAL_ENERGY    s2                    je+1
c    TOTAL_ENERGY    s3                                ke+1
c
c    Note that s1,s2,s3 are stored in w3da,w3db,w3dc.
c
c  LOCAL VARIABLES:
c
c  i-sweep
c  dr*i      inverse of distance across zone in 1-, 2-, or 3-direction
c  drimax    maximum of dr1i, dr2i, and dr3i
c  dt**i2i   square of the inverse time step of the ** physical process
c            gathered during the i-sweep.  Possible values of ** are:
c                cs = sound speed
c                v1 = fluid motion in x1 direction
c                v2 = fluid motion in x2 direction
c                v3 = fluid motion in x2 direction
c                al = Alfven speed
c                qq = artificial viscosity
c            The first five are vectors, while the last is a scalar
c            which has been computed in ARTIFICIALVISC (passed in root).
c  dttoi2i   sum of dt**i2i (without artificial viscosity contribution)
c
c  j-sweep
c  dttoi2j   vector of maximum values of dttoi2i from each i-sweep.
c            This vector is filled during a j-sweep.
c  imaxj     i-index of each dttoi2j
c  dt**i2j   values of above at zone i=imaxj
c
c  k-sweep
c  dttoi2k   vector of maximum values of dttoi2j from each j-sweep.
c  imaxk     i-index of each dttoi2k.
c  jmaxk     j-index of each dttoi2k.
c  dt**i2j   values of above at zone i=imaxk, j=jmaxk
c
c  grand maximum inverse time
c  imax      i-index where dttoi2k is a maximum
c  jmax      j-index where dttoi2k is a maximum
c  kmax      k-index where dttoi2k is a maximum
c  dt**      time step of the ** physical process at i=imax, j=jmax,
c            and k=kmax
c  dtnew     new timestep, which is limited to be no greater than 1.26
c            times the previous timestep.
c
c  EXTERNALS:
c
c-----------------------------------------------------------------------
c
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

      real*8   d (in,jn,kn), e (in,jn,kn),
     1       v1(in,jn,kn), v2(in,jn,kn), v3(in,jn,kn)

      real*8   b1(in,jn,kn), b2(in,jn,kn), b3(in,jn,kn)




      common /fieldr/  d, e, v1, v2, v3

      common /fieldr/  b1, b2, b3




      real*8
     . b1floor   ,b2floor   ,b3floor   ,ciso      
     .,courno    ,dfloor
     .,dtal    ,dtcs    ,dtv1    ,dtv2    ,dtv3
     .,dtqq    ,dtnew

     .,dtrd
     .,dt        ,dtdump 
     .,dthdf     ,dthist    ,dtmin     ,dttsl
CJH  .,dtqqi2 
     .,dtqqi2    ,dtnri2    ,dtrdi2    ,dtimrdi2
     .,dtusr
     .,efloor    ,erfloor   ,gamma     ,gamm1
     .,qcon      ,qlin 
     .,tdump
     .,thdf      ,thist     ,time      ,tlim      ,cpulim
     .,trem      ,tsave     ,ttsl
     .,tused     ,tusr

     .,v1floor   ,v2floor   ,v3floor 
     .,emf1floor ,emf2floor ,emf3floor 
     .,gpfloor

      integer

     . ifsen(6)

     .,idebug
     .,iordb1    ,iordb2    ,iordb3    ,iordd
     .,iorde     ,iorder    ,iords1    ,iords2
     .,iords3
     .,istpb1    ,istpb2    ,istpb3    ,istpd     ,istpe     ,istper
     .,istps1    ,istps2    ,istps3
C     .,isymm
     .,ix1x2x3   ,jx1x2x3
     .,nhy       ,nlim      ,nred      ,mbatch
     .,nwarn     ,nseq      ,flstat
c output file handles (efh 04/15/99)
     .,ioinp     ,iotsl     ,iolog     ,iohst     ,iomov     ,iores
     .,ioshl

      common /rootr/ 
     . b1floor ,b2floor ,b3floor ,ciso    ,courno
     .,dfloor
     .,dtal    ,dtcs    ,dtv1    ,dtv2    ,dtv3
     .,dtqq    ,dtnew

     .,dtrd
     .,dt      ,dtdump  ,dthdf
     .,dthist  ,dtmin   ,dttsl
CJH  .,dtqqi2  ,dtusr
     .,dtqqi2  ,dtusr   ,dtnri2  ,dtrdi2  ,dtimrdi2
     .,efloor  ,erfloor ,gamma   ,gamm1
     .,qcon    ,qlin
     .,tdump   ,thdf    ,thist
     .,time    ,tlim    ,cpulim  ,trem    ,tsave
     .,tused   ,tusr    ,ttsl    

     .,v1floor ,v2floor ,v3floor
     .,emf1floor ,emf2floor ,emf3floor
     .,gpfloor

      common /rooti/ 
     . ifsen   ,idebug
     .,iordb1  ,iordb2
     .,iordb3  ,iordd   ,iorde   ,iorder  ,iords1
     .,iords2  ,iords3 
     .,istpb1  ,istpb2  ,istpb3  ,istpd   ,istpe   ,istper
     .,istps1  ,istps2  ,istps3
C     .,isymm   
     .,ix1x2x3 ,jx1x2x3
     .,nhy     ,nlim    ,nred    ,mbatch
     .,nwarn   ,nseq    ,flstat
     .,ioinp   ,iotsl   ,iolog   ,iohst   ,iomov   ,iores
     .,ioshl

      character*2  id
      character*15 hdffile, hstfile, resfile, usrfile

      character*8  tslfile

      common /chroot2/  id
      common /chroot1/  hdffile, hstfile, resfile, usrfile

     .,tslfile

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

c
       integer     ibeg,iend,jbeg,jend,kbeg,kend
       integer     i       , j       , k
     1           , imin    ,  jmin   , kmin

       real*8        se

c
       real*8        dr1is, dr2is, dr3is, drimaxs
       real*8        dtcsm, dtv1m, dtv2m, dtv3m, dtalm
     &           , dttoi2, dttoi2m

c
c      External statements
c
c-----------------------------------------------------------------------
c
c Find the minimum time step required by the Courant condition for
c this tile.  We will first compute 1/dt**2 required by each of the
c various physical processes in the calculation, and save the maximum
c value of their sum at each zone.  In the process, we will compute
c the updated velocity.
c
       do 30 k=kbeg,kend
         do 20 j=jbeg,jend
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
           do 10 i=ibeg,iend
             dr1is      = dx1ai(i)
             dr2is      = g2bi (i) * dx2ai(j)
             dr3is      = g31bi(i) * g32bi(j) * dx3ai(k)
             drimaxs    =   max ( dr1is  , dr2is  , dr3is   )
             v1(i,j,k)  = w3da(i,j,k) * 2.0 /(d(i-1,j  ,k  ) + d(i,j,k))
             v2(i,j,k)  = w3db(i,j,k) * 2.0 /(d(i  ,j-1,k  ) + d(i,j,k))
     1                  * g2bi(i)
             v3(i,j,k)  = w3dc(i,j,k) * 2.0 /(d(i  ,j  ,k-1) + d(i,j,k))
     1                  * g31bi(i) * g32bi(j)


             se         = e(i,j,k) / d(i,j,k)

             dtcs       = gamma * gamm1 * se        * drimaxs**2

             dtv1       = ( (v1(i,j,k) - vg1(i)) * dr1is   )**2
             dtv2       = ( (v2(i,j,k) - vg2(j)) * dr2is   )**2
             dtv3       = ( (v3(i,j,k) - vg3(k)) * dr3is   )**2


             dtal       = ( ( b1(i,j,k) + b1(i+1,j  ,k  ) )**2
     1                    + ( b2(i,j,k) + b2(i  ,j+1,k  ) )**2
     2                    + ( b3(i,j,k) + b3(i  ,j  ,k+1) )**2 )
     3                  * 0.25 * drimaxs**2 / d(i,j,k)

             dttoi2     = dtcs          + dtv1
     1                  + dtv2          + dtv3
     2                  + dtal
             if (dttoi2 .gt. dttoi2m) then
               dttoi2m = dttoi2
               dtcsm   = dtcs
               dtv1m   = dtv1
               dtv2m   = dtv2
               dtv3m   = dtv3
               dtalm   = dtal
               imin    = i
               jmin    = j
               kmin    = k
             endif
10         continue
20       continue
30     continue
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 N E W D T                 \\\\\\\\\\
c
c=======================================================================
c
c
