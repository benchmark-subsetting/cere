












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                  N U D T                  \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine nudt
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
c    modified 6: March 5, 1996 by Robert Fiedler; completely rewritten
c                for ZEUS-MP.
c    modified 7: Aug. 23, 1996 by Robert Fiedler; minor change to 
c                substep counter.
c    modified 8: Dec. 19, 1996 by Robert Fiedler; added radiation 
c                diffusion time step.
c    modified 9: Feb. 14, 1997 by RAF; corrected for non-hydro and
c                lower dimensionality options.
c    modified 10: kluge for Garching T3E (search on M-MML) 26 Feb 98
c    modified 11: December, 1999 by PSLi; add in dt calculation in case
c                 of subcycle of artificial viscosity.
c
c  PURPOSE:  Computes the new timestep for explicit calculations from
c  the values of the field variables updated by the source and transport
c  steps.
c
c  In explicit calculations, the timestep is given by:
c
c     dt = courno * sqrt [ min ( dtcs**2 + dtv1**2 + dtv2**2 + dtv3**2
c                              + dtal**2 + dtqq**2 + dtrd**2 ) ]
c
c  where the variable names are described below.  The timestep can be
c  reduced in size by any amount, but can be larger than the old timstep
c  by no more than a factor of 1.26.
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
c                rd = radiation diffusion
c            The first five are vectors; the next to last is a scalar
c            which has been computed in ARTIFICIALVISC (passed in root).
c            Last is a scalar computed in rad_pfl (passed in radexp).
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
c  imin      i-index where dttoi2k is a maximum
c  jmin      j-index where dttoi2k is a maximum
c  kmin      k-index where dttoi2k is a maximum
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




      real*8     fiis(nbvar),  fois(nbvar)
     &      ,  fijs(nbvar),  fojs(nbvar)
     &      ,  fiks(nbvar),  foks(nbvar)
      integer  niis( 3),  nois( 3)
     &      ,  nijs( 3),  nojs( 3)
     &      ,  niks( 3),  noks( 3)
      integer  bvstat(6,nbvar)

      integer  niib(jn,kn), niib2(jn,kn), niib3(jn,kn), niib23(jn,kn)
     &      ,  noib(jn,kn), noib2(jn,kn), noib3(jn,kn), noib23(jn,kn)
     &      ,  nijb(in,kn), nijb3(in,kn), nijb1(in,kn), nijb31(in,kn)
     &      ,  nojb(in,kn), nojb3(in,kn), nojb1(in,kn), nojb31(in,kn)
     &      ,  nikb(in,jn), nikb1(in,jn), nikb2(in,jn), nikb12(in,jn)
     &      ,  nokb(in,jn), nokb1(in,jn), nokb2(in,jn), nokb12(in,jn)


      real*8    diib(jn,kn,3),  doib(jn,kn,3)
     &     ,  dijb(in,kn,3),  dojb(in,kn,3)
     &     ,  dikb(in,jn,3),  dokb(in,jn,3)
      real*8    eiib(jn,kn,2),  eoib(jn,kn,2)
     &     ,  eijb(in,kn,2),  eojb(in,kn,2)
     &     ,  eikb(in,jn,2),  eokb(in,jn,2)
      real*8   v1iib(jn,kn,2), v1oib(jn,kn,2)
     &     , v1ijb(in,kn,2), v1ojb(in,kn,2)
     &     , v1ikb(in,jn,2), v1okb(in,jn,2)
      real*8   v2iib(jn,kn,2), v2oib(jn,kn,2)
     &     , v2ijb(in,kn,2), v2ojb(in,kn,2)
     &     , v2ikb(in,jn,2), v2okb(in,jn,2)
      real*8   v3iib(jn,kn,2), v3oib(jn,kn,2)
     &     , v3ijb(in,kn,2), v3ojb(in,kn,2)
     &     , v3ikb(in,jn,2), v3okb(in,jn,2)

      real*8   b1iib(jn,kn,2), b1oib(jn,kn,2)
     &     , b1ijb(in,kn,2), b1ojb(in,kn,2)
     &     , b1ikb(in,jn,2), b1okb(in,jn,2)
      real*8   b2iib(jn,kn,2), b2oib(jn,kn,2)
     &     , b2ijb(in,kn,2), b2ojb(in,kn,2)
     &     , b2ikb(in,jn,2), b2okb(in,jn,2)
      real*8   b3iib(jn,kn,2), b3oib(jn,kn,2)
     &     , b3ijb(in,kn,2), b3ojb(in,kn,2)
     &     , b3ikb(in,jn,2), b3okb(in,jn,2)
      real*8   emf1iib(jn,kn,3), emf1oib(jn,kn,3)
     &     , emf1ijb(in,kn,3), emf1ojb(in,kn,3)
     &     , emf1ikb(in,jn,3), emf1okb(in,jn,3)
      real*8   emf2iib(jn,kn,3), emf2oib(jn,kn,3)
     &     , emf2ijb(in,kn,3), emf2ojb(in,kn,3)
     &     , emf2ikb(in,jn,3), emf2okb(in,jn,3)
      real*8   emf3iib(jn,kn,3), emf3oib(jn,kn,3)
     &     , emf3ijb(in,kn,3), emf3ojb(in,kn,3)
     &     , emf3ikb(in,jn,3), emf3okb(in,jn,3)




      common /bndryr/
     &   fiis,  fois,  fijs,  fojs,  fiks,  foks
     & , diib,  doib,  dijb,  dojb,  dikb,  dokb
     & , eiib,  eoib,  eijb,  eojb,  eikb,  eokb
     & ,v1iib, v1oib, v1ijb, v1ojb, v1ikb, v1okb
     & ,v2iib, v2oib, v2ijb, v2ojb, v2ikb, v2okb
     & ,v3iib, v3oib, v3ijb, v3ojb, v3ikb, v3okb

     & ,b1iib, b1oib, b1ijb, b1ojb, b1ikb, b1okb
     & ,b2iib, b2oib, b2ijb, b2ojb, b2ikb, b2okb
     & ,b3iib, b3oib, b3ijb, b3ojb, b3ikb, b3okb
     & ,emf1iib, emf1oib, emf1ijb, emf1ojb, emf1ikb, emf1okb
     & ,emf2iib, emf2oib, emf2ijb, emf2ojb, emf2ikb, emf2okb
     & ,emf3iib, emf3oib, emf3ijb, emf3ojb, emf3ikb, emf3okb




      common /bndryi/   niis,   nois,   nijs,   nojs,   niks,   noks
     &      ,  bvstat
     &      ,  niib, niib2, niib3, niib23
     &      ,  noib, noib2, noib3, noib23
     &      ,  nijb, nijb3, nijb1, nijb31
     &      ,  nojb, nojb3, nojb1, nojb31
     &      ,  nikb, nikb1, nikb2, nikb12
     &      ,  nokb, nokb1, nokb2, nokb12


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



      integer stat, req


      logical periodic(3)
      logical reorder
      integer myid, myid_w, nprocs, nprocs_w, coords(3)
      integer ierr, nreq, nsub
      integer comm3d
      integer ntiles(3)
      integer n1m, n1p, n2m, n2p, n3m, n3p
      integer i_slice,j_slice,k_slice
      integer ils_slice,jls_slice,kls_slice
      integer ilsm_slice,jlsm_slice,klsm_slice
      integer ibuf_in(nbuff), ibuf_out(nbuff)
      real*8    buf_in(nbuff), buf_out(nbuff)
      common /mpicomi/ myid, myid_w, nprocs, nprocs_w, coords
     &               , comm3d, ntiles
     &               , n1m, n1p, n2m, n2p, n3m, n3p
     &               , i_slice, j_slice, k_slice
     &               , ils_slice, jls_slice, kls_slice
     &               , ilsm_slice, jlsm_slice, klsm_slice
     &               , ibuf_in, ibuf_out
     &               , stat, req, ierr, nreq, nsub
      common /mpicoml/ periodic, reorder
      common /mpicomr/ buf_in, buf_out





c
       integer       i       ,  j      , k
     1             , imax    ,  jmax   , kmax
     &             , imin    ,  jmin   , kmin
     &             , k0      ,  k1     , k2
     &             , j0

c
       real*8        dtcsm, dtv1m, dtv2m, dtv3m, dtalm, dttoi2m

c
c      External statements
c
       external newdt
c
c-----------------------------------------------------------------------
c
c Find the minimum time step required by the Courant condition for
c this tile.  We will first compute 1/dt**2 required by each of the
c various physical processes in the calculation, and save the maximum
c value of their sum at each zone.  In the process, we will compute
c the updated velocity.
c

       dttoi2m = 0.0
       dtcsm   = 0.0
       dtv1m   = 0.0
       dtv2m   = 0.0
       dtv3m   = 0.0
       dtalm   = 0.0
       imin    = is
       jmin    = js
       kmin    = ks
       if (nx2z .eq. 1) then
c
c For one-dimensional problems only
c
         j0 = js
       else
         j0 = js + 1
       endif
       if (nx3z .eq. 1) then
c
c For one-dimensional or two-dimensional problems only
c
         k0 = ks
       else
         k0 = ks + 1
       endif
c
c Divide the computational volume into three equal pieces.  We must
c have at least 3 active zones in the 3-direction.
c
       k1 = int( real( ke - ks + 1 ) / 3.0 ) + ks
       k2 = int( real( ke - ks + 1 ) / 3.0 ) + k1



c......................................................................
c
c i boundaries
c
c    1) Post sends and receives. 
c
       nreq = 0
       nsub = nsub + 1

       imax = ie
       jmax = je
       kmax = ke
       call bvald  (1,0,0,0,0,0,d   )




c
c    2) Do first 1/3 of the interior points.
c
       call newdt (is+1,imax,j0  ,jmax,k0  ,k1
     &                ,  imin,jmin,kmin
     &                ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)
c
c       subroutine newdt (ibeg,iend,jbeg,jend,kbeg,kend
c     &                ,  imin,jmin,kmin
c     &                ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)
c




c......................................................................
c
c j boundaries
c
c    1) Post sends and receives.
c
       nreq = 0
       nsub = nsub + 1

       call bvald  (0,0,1,0,0,0,d   )




c
c    2) Do middle 1/3 of the interior points, plus some on borders.
c
       call newdt (is  ,is  ,j0  ,jmax,k0  ,k1
     &                ,  imin,jmin,kmin
     &                ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)

       if (nx3z .gt. 1) then
         call newdt (is  ,ie  ,j0  ,jmax,k1+1,k2
     &                  ,  imin,jmin,kmin
     &                  ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)
       endif




c......................................................................
c
c k boundaries
c
c    1) Post sends and receives.
c
       nreq = 0
       nsub = nsub + 1

       call bvald  (0,0,0,0,1,0,d   )




c
c    2) Do last 1/3 of the interior points, plus some on borders.
c
       if (nx2z .gt. 1) then
         call newdt (is  ,ie  ,js  ,js  ,k0  ,k2
     &                  ,  imin,jmin,kmin
     &                  ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)

       endif
       if (nx3z .gt. 1) then
         call newdt (is  ,ie  ,js  ,je  ,k2+1,kmax
     &                  ,  imin,jmin,kmin
     &                  ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)
       endif
c
c      Mark the velocity boundary values out of date.
c
       do 10 i = 1,6
c v1, v2, v3
         bvstat(i,3) = 0      
         bvstat(i,4) = 0      
         bvstat(i,5) = 0      
10     continue




c......................................................................
c
c Finally, do the remaining border zones.
c
       if (ke .gt. ks) then
         call newdt (is  ,ie  ,js  ,je  ,ks  ,ks  
     &                  ,  imin,jmin,kmin
     &                  ,  dtcsm,dtv1m,dtv2m,dtv3m,dtalm,dttoi2m)

       endif



c
c-----------------------------------------------------------------------
c
c Compute preliminary new time step.
c




CPS

       dtnew  = courno / ( sqrt ( dttoi2m + dtqqi2 ) + tiny )

C





       dt     =   min ( dtnew, 1.26*dt )
c      dt     =   min ( dt   , tlim/3.0D2)

c
c      if (dt .le. 5.0*dtmin) then
       if (dt .le. dtmin) then
c
c Determine which tile requires this short time step and report its
c coordinates and other info.  Convert 1/dt**2 to dt for each 
c physical process.
c


c
           dtcs = 1.0 / ( sqrt ( dtcsm ) + tiny )
           dtv1 = 1.0 / ( sqrt ( dtv1m ) + tiny )
           dtv2 = 1.0 / ( sqrt ( dtv2m ) + tiny )
           dtv3 = 1.0 / ( sqrt ( dtv3m ) + tiny )
           dtal = 1.0 / ( sqrt ( dtalm ) + tiny )
           dtqq = 1.0 / ( sqrt ( dtqqi2) + tiny )

cpu2006           write (2, 2010) coords(1), coords(2), coords(3)
cpu2006     &                   , imin, jmin, kmin, nhy, dt, dtmin, dtcs
cpu2006     &                   , dtv1, dtv2, dtv3, dtqq, dtal


         nwarn = nwarn + 1
       endif
       return
c
c-----------------------------------------------------------------------
c----------------------- Write format statements -----------------------
c-----------------------------------------------------------------------
c
2010   format('NUDT    : **** WARNING **** Hot zone on tile (',i4,','
     &       ,i4,',',i4,')',/
     &       ,'NUDT    : at i=',i4,' j=',i4,' k=',i4,' (dt < 5*dtmin)'
     &       ,', nhy  = ',i6,',',/
     2       ,'NUDT    : dt   = ',1pe12.5,', dtmin= ',1e12.5,', dtcs = '
     3       ,1e12.5,',',/
     4       ,'NUDT    : dtv1 = ',1e12.5,', dtv2 = ',1e12.5,', dtv3 = '
     5       ,1e12.5,',',/

     6       ,'NUDT    : dtqq = ',1e12.5,', dtal = '

     7       ,1e12.5,'.')
c
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                  N U D T                  \\\\\\\\\\
c
c=======================================================================
c
c
