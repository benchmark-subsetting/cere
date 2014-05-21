












































































c
c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 H S M O C                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine hsmoc ( emf1, emf2, emf3 )

c
c    dac:zeus3d.mocemfs <-------------------------- MoC estimate of emfs
c                                                          october, 1992
c
c    written by: David Clarke
c    modified 1: Byung-Il Jun, July 1994
c                implemented John Hawley and Jim Stone's scheme to
c                fix pt. explosion of magnetic field in passive field.
c                Basically, this scheme mixes emfs computed with simple
c                upwinding(Evans and Hawley) and MoC.
c                The upwinded values are used to compute the wave
c                speeds for the characteristic cones for the MoC part.
c    modified 2: Robert Fiedler, February 1995
c                upgraded to ZEUS-3D version 3.4 -- improved cache
c                utilization and added parallelization directives for 
c                 multiprocessors.
c    modified 3: Mordecai-Mark Mac Low, December 1997 - March 1998
c                rewritten for ZEUS-MP without overlapping.  Calls to 
c                interpolation routines have been inlined.
c    modified 4: PSLi, December 1999
c                minor modications to prevent scratch arrays overwritten.
c
c  PURPOSE:  Uses the Method of Characteristics (MoC, invented by Jim
c  Stone, John Hawley, Chuck Evans, and Michael Norman; see Stone and
c  Norman, ApJS, v80, p791) to evaluate the velocity and magnetic field
c  needed to estimate emfs that are properly upwinded in the character-
c  istic velocities for the set of equations describing transverse
c  Alfven waves.  This is *not* the full  characteristic problem, but
c  a subset which has been found (reference above) to yield good results
c  for the propagation of Alfven waves.  This routine differs from the
c  previous routines MOC1, MOC2, and MOC3 in version 3.1 in that the
c  Lorentz forces are computed *before* the emfs are estimated.  Thus,
c  the emfs now use the velocities which have been updated with all the
c  source terms, including the transverse Lorenz forces.
c
c  The characteristic equations governing the propagation of Alfven
c  waves in the 1-direction are (see ZEUS3D notes "Method of Character-
c  istics"):
c
c  "plus" characteristic (C+):
c
c    ( db1/dt + (v2 - a2) * db1/dx2 + (v3 - a3) * db1/dx3 ) / sqrt(d)
c  + ( dv1/dt + (v2 - a2) * dv1/dx2 + (v3 - a3) * dv1/dx3 )  =  S    (1)
c
c  "minus" characteristic (C-):
c
c    ( db1/dt + (v2 + a2) * db1/dx2 + (v3 + a3) * db1/dx3 ) / sqrt(d)
c  - ( dv1/dt + (v2 + a2) * dv1/dx2 + (v3 + a3) * dv1/dx3 )  = -S    (2)
c
c  where   a2 = b2/sqrt(d) is the Alfven velocity in the 2-direction
c          a3 = b3/sqrt(d) is the Alfven velocity in the 3-direction
c          g1, g2, g3 are the metric factors
c          S = b1 * ( b2/g2 * dg1/dx2 + b3/g3 * dg1/dx3 )
c
c  Equations (1) and (2) can be written in Lagrangian form:
c
c      1    D+/-           D+/-
c   ------- ----(b1)  +/-  ----(v1)  =  +/- S                        (3)
c   sqrt(d)  Dt             Dt
c
c  where the Lagrangian derivatives are given by
c
c  D+/-     d                   d                   d
c  ----  =  --  +  (v2 -/+ a2) ---  +  (v3 -/+ a3) ---               (4)
c   Dt      dt                 dx2                 dx3
c
c  Differencing equations (3) [e.g. D+(b1) = b* - b+; D-(b1) = b* - b-],
c  and then solving for the advanced time values of b* and v*, one gets:
c                             _                                _
c           sqrt (d+ * d-)   |     b+          b-               |
c  b* =  ------------------- |  -------- + --------- + v+ - v-  |    (5)
c        sqrt(d+) + sqrt(d-) |_ sqrt(d+)    sqrt(d-)           _|
c
c                 1
c  v* =  ------------------- [ v+*sqrt(d+) + v-*sqrt(d-) + b+ - b- ]
c        sqrt(d+) + sqrt(d-)                                         (6)
c
c     + S Dt
c
c  where b+(-), and v+(-) are the upwinded values of the magnetic field
c  and velocity interpolated to the time-averaged bases of C+ (C-), and
c  d+(-) are estimates of the density along each characteristic path
c  during the time interval Dt.
c
c  Equations (1) and (2) would suggest that when estimating "emf2" for
c  example, that the interpolated values for "v1" and "b1" be upwinded
c  in both the 2- and 3- components of the characteristic velocity.  It
c  turns out that this is impractical numerically, and so only the
c  "partial" characteristics are tracked.  While estimating "emf2", "v1"
c  and "b1" are upwinded only in the 3-component of the characteristic
c  velocities.  Conversely, while estimating "emf3", "v1" and "b1" are
c  upwinded only in the 2-component of the characteristic velocities.
c  Since updating "b1" requires both "emf2" and "emf3", the evolution of
c  "b1" will ultimately depend upon the full characteristics.  This
c  amounts to nothing more than directionally splitting the full MoC
c  algorithm.  The effects of such a directionally split implementation
c  are not fully known.  What is known is:
c
c  1) A non-directionally split implementation of the MoC algorithm is
c     not possible without either relocating the emfs to the zone
c     corners or the magnetic field components to the zone centres.
c     The former has been tried (change deck mocemf) and was found to
c     generate intolerable diffusion of magnetic field.  In addition,
c     the algorithm is not unconditionally stable.  The latter has not
c     been tried, but is dismissed on the grounds that div(B) will be
c     determined by truncation errors rather than machine round-off
c     errors.
c
c  2) A directionally split algorithm that is not also operator split
c     (ie, performing the Lorentz updates of the velocities separately
c     from the MoC estimation of the emfs) does not allow stable Alfven
c     wave propagation in 2-D.  Operator splitting the MoC algorithm so
c     that the transverse Lorentz forces are computed *before* the
c     magnetic field update does allow Alfven waves to propagate stably
c     in multi-dimensions but appears to introduce more diffusion for
c     sub-Alfvenic flow.  On the other hand, super-Alfvenic flow does
c     *not* appear to be more diffusive in the operator split scheme.
c
c  INPUT VARIABLES:
c
c  OUTPUT VARIABLES:
c    emf1      emf along 1-edge computed using MoC estimates of v2, b2,
c              v3, and b3.
c    emf2      emf along 2-edge computed using MoC estimates of v3, b3,
c              v1, and b1.
c    emf3      emf along 3-edge computed using MoC estimates of v1, b1,
c              v2, and b2.
c
c  LOCAL VARIABLES:
c
c    1-D variables
c    bave      spatially averaged magnetic field at edge
c    srdp      sqrt(density) along the plus  characteristic (C+)
c    srdm      sqrt(density) along the minus characteristic (C-)
c    vchp      characteristic velocity along C+ (v - va)
c    vchm      characteristic velocity along C- (v + va)
c    vpch      velocity interpolated to the time-centred footpoint of C+
c    vmch      velocity interpolated to the time-centred footpoint of C-
c    bpch      B-field  interpolated to the time-centred footpoint of C+
c    bmch      B-field  interpolated to the time-centred footpoint of C-
c    vsnm1     MoC estimate of v[n-1] used to evaluate emf[n], n=1,2,3
c    bsnm1     MoC estimate of b[n-1] used to evaluate emf[n], n=1,2,3
c
c    2-D variables
c    v3intj   upwinded v3 in 2-direction
c    b3intj   upwinded b3 in 2-direction
c    etc..
c
c    3-D variables
c    srd[n]    sqrt of spatially averaged density at [n]-face n=1,2,3
c    vsnp1     MoC estimate of v[n+1] used to evaluate emf[n], n=1,2,3
c    bsnp1     MoC estimate of b[n+1] used to evaluate emf[n], n=1,2,3
c    vsnp1     is reused as the vsnp1*bsnm1 term in emf[n]
c    bsnp1     is reused as the vsnm1*bsnp1 term in emf[n]
c
c  EXTERNALS:
c    BVALEMF1, BVALEMF2, BVALEMF3
c
c I have inlined these M-MML 8.3.98
c    X1ZC1D  , X2ZC1D  , X3ZC1D
c    X1INT1D , X2INT1D , X3INT1D
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
       integer      i       , j       , k    

       real*8         absb    , sgnp    , sgnm
     1             , q1      , q2      , src
     &             , qv1, qv2, qb1, qb2, q3 ,dqm ,dqp ,xi ,fact 
     &              , dv(ijkn), db(ijkn)
c
       real*8          bave    (ijkn), srdp    (ijkn), srdm    (ijkn)
     1             , srdpi   (ijkn), srdmi   (ijkn), vchp    (ijkn)
     1             , vchm    (ijkn), vtmp    (ijkn), btmp    (ijkn)
     2             , vpch    (ijkn), vmch    (ijkn), bpch    (ijkn)
     3             , bmch    (ijkn), vsnm1   (ijkn), bsnm1   (ijkn)
     4             , vave    (ijkn), aave    (ijkn)
c
       real*8          vfl     (ijkn), vt      (ijkn), bt      (ijkn)
     1             , vint    (ijkn), bint    (ijkn)
c
       real*8          v3intj  (jn,kn),          b3intj(jn,kn)
     1             , v2intk  (jn,kn),          b2intk(jn,kn)
     2             , v1intk  (kn,in),          b1intk(kn,in)
     3             , v3inti  (kn,in),          b3inti(kn,in)
     4             , v2inti  (in,jn),          b2inti(in,jn)
     5             , v1intj  (in,jn),          b1intj(in,jn)
c
       real*8         emf1    (  in,  jn,  kn), emf2    (  in,  jn,  kn)
     1             , emf3    (  in,  jn,  kn)
       real*8         vsnp1   (  in,  jn,  kn), bsnp1   (  in,  jn,  kn)
     1             , srd1    (  in,  jn,  kn), srd2    (  in,  jn,  kn)
     1             , srd3    (  in,  jn,  kn)
c
c
       equivalence   ( bave    , w1da     ), ( srdp    , w1db     )
     1             , ( srdm    , w1dc     ), ( srdpi   , w1dd     )
     1             , ( srdmi   , w1de     ), ( vchp    , w1df     )
     1             , ( vchm    , w1dg     ), ( vpch    , w1dh     )
     1             , ( vmch    , w1di     ), ( bpch    , w1dj     )
     1             , ( bmch    , w1dk     )
     1             , ( vtmp    , w1dl     )
     1             , ( btmp    , w1dm     )
     1             , ( vave    , w1dn     )
     1             , ( aave    , w1do     )
     1             , ( vsnm1   , w1dp     )
     1             , ( bsnm1   , w1dq     )
     1             , ( vt      , w1dq     )
     1             , ( bt      , w1dr     )
     1             , ( vint    , w1ds     )
     1             , ( bint    , w1dt     )
     1             , ( vfl     , w1du     )
c
c there are no 2D scratch arrays like in ZEUS-3D, but these can all still 
c be equivalenced to each other. M-MML 4 Mar 98
       equivalence   ( v3intj, v2intk, v1intk, v3inti, v2inti
     2               , v1intj                          )
     2             , ( b3intj, b2intk, b1intk, b3inti, b2inti
     2               , b1intj                          )
c
c      Careful!  "wa3d" through "wc3d" are equivalenced in CT.
c                wa3d -> emf1    wb3d -> emf2    wc3d -> emf3
c
c      The worker arrays "we3d" and "wf3d" should still contain "srd2"
c  and "srd3" from LORENTZ. The worker array  "wd3d" will contain
c  "srd1", but "wd3d" is needed for "bsnp1" and "term2".  Thus "srd1"
c  will be recomputed once "srd3" is no longer needed.
c
cRAF We have more array space with wg3d, so don't recompute srd1.
cRAF SGIMP does not like equivalenced variables in the same loop nest,
cRAF so eliminate term1 and term2.
c
CPS
       equivalence   ( srd1              , w3di     )
     1             , ( srd2              , w3dj     )
     1             , ( srd3              , w3df     )
     1             , ( bsnp1             , w3dg     )
C
c
c      External statements
c
       external      bvalemf1, bvalemf2, bvalemf3
c
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c---- 1.  emf1 ---------------------------------------------------------
c-----------------------------------------------------------------------
c
c    BEGINNING OF I LOOPS
c
c
c      By following the characteristics of the flow in the 3-direction,
c  determine values for "v2" and "b2" ("vsnp1" and "bsnp1") to be used
c  in evaluating "emf1".
c
c     Compute upwinded b3 and v3 in the 2-direction and use them to
c     compute the wave speeds for the characteristic cones for the MoC.
c
CPS   Initialize vsnp1
       do k=1,kn
          do j=1,jn
             do i=1,in
                vsnp1(i,j,k)=0.
             enddo
          enddo
       enddo
C
       do 99 k=ks,ke+1
         do 9 i=is,ie
           do 7 j=js-2,je+2
             vfl   (j) = 0.5 * (v2(i,j,k) + v2(i,j,k-1)) - vg2(j)
             vt    (j) = v3(i,j,k) - vg3(k)
             bt    (j) = b3(i,j,k)
7          continue
c
c           call x2int1d ( bt, vfl, iordb3, istpb3, k, i
c     1                  , g2b, g2bi, bint )
c           call x2int1d ( vt, vfl, iords3, istps3, k, i
c     1                  , g2b, g2bi, vint )
c       subroutine x2int1d ( q, vp, iorder, isteep, k, i
c     1                   , g2, g2i , qp )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 807 j=js-1,je+1
             dqm      = ( vt(j  ) - vt(j-1) ) * dx2bi(j  )
             dqp      = ( vt(j+1) - vt(j  ) ) * dx2bi(j+1)
             dv (j  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm     = ( bt(j  ) - bt(j-1) ) * dx2bi(j  )
             dqp     = ( bt(j+1) - bt(j  ) ) * dx2bi(j+1)
             db (j  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 807       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g2bi(i)
           do 808 j=js,je+1
             qv1    = vt(j-1) + dx2a(j-1) * dv (j-1)
             qv2    = vt(j  ) - dx2a(j  ) * dv (j  )
             qb1    = bt(j-1) + dx2a(j-1) * db (j-1)
             qb2    = bt(j  ) - dx2a(j  ) * db (j  )
c
             xi     = vfl(j) * fact
             q3     = sign ( haf, xi )
             vint(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bint(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )
 808       continue

           do 8 j=js,je+1
             emf1(i,j,k) = vint(j)
             emf2(i,j,k) = bint(j)
 8        continue
 9      continue
 99    continue
c
c      Select an effective density and determine the characteristic
c  velocity using upwinded values for each characteristic in the
c  3-direction.
c
       do 50 j=js,je+1
         do 40 i=is,ie
           do 10 k=ks,ke+1
             vave (k) = emf1(i,j,k)
             bave (k) = emf2(i,j,k)
c            vave (k) = 0.5 * ( v3(i,j,k) + v3(i,jm1,k) ) - vg3(k)
c            bave (k) = 0.5 * ( b3(i,j,k) + b3(i,jm1,k) )
             absb     = abs ( bave(k) )
             aave(k)  = 0.5 * absb * ( srd2(i,j,k) + srd2(i,j,k-1) )
     1                             / ( srd2(i,j,k) * srd2(i,j,k-1) )
             sgnp     = sign ( haf, vave(k) - aave(k) )
             sgnm     = sign ( haf, vave(k) + aave(k) )
             srdp (k) = ( 0.5 + sgnp ) * srd2(i,j,k-1)
     1                + ( 0.5 - sgnp ) * srd2(i,j,k  )
             srdm (k) = ( 0.5 + sgnm ) * srd2(i,j,k-1)
     1                + ( 0.5 - sgnm ) * srd2(i,j,k  )
             srdpi(k) = 1.0 / srdp(k)
             srdmi(k) = 1.0 / srdm(k)
             vchp (k) = vave(k) - absb * srdpi(k)
             vchm (k) = vave(k) + absb * srdmi(k)
10         continue
c
c      Interpolate 1-D vectors of "v2" and "b2" in the 3-direction to
c  the footpoints of both characteristics.
c
           do 20 k=ks-2,ke+2
             vtmp(k) = v2(i,j,k) - vg2(j)
             btmp(k) = b2(i,j,k)
20         continue
c          call x3zc1d ( vtmp, vchp, vchm, iords2, istps2, i, j
c    1                 , g31b, g31bi, g32a, g32ai, vpch, vmch )
c          call x3zc1d ( btmp, vchp, vchm, iordb2, istpb2, i, j
c    1                 , g31b, g31bi, g32a, g32ai, bpch, bmch )
c       subroutine x3zc1d ( q, vp, vm, iorder, isteep, i, j, g31, g31i
c     1                   , g32, g32i, qp, qm )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 820 k=ks-1,ke+1
             dqm      = ( vtmp(k  ) - vtmp(k-1) ) * dx3bi(k  )
             dqp      = ( vtmp(k+1) - vtmp(k  ) ) * dx3bi(k+1)
             dv (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( btmp(k  ) - btmp(k-1) ) * dx3bi(k  )
             dqp      = ( btmp(k+1) - btmp(k  ) ) * dx3bi(k+1)
             db (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 820       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g31bi(i) * g32ai(j)
           do 830 k=ks,ke+1
             qv1    = vtmp(k-1) + dx3a(k-1) * dv (k-1)
             qv2    = vtmp(k  ) - dx3a(k  ) * dv (k  )
             qb1    = btmp(k-1) + dx3a(k-1) * db (k-1)
             qb2    = btmp(k  ) - dx3a(k  ) * db (k  )
c  
             xi     = vchp(k) * fact
             q3     = sign ( haf, xi )
             vpch(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bpch(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
c
             xi     = vchm(k) * fact
             q3     = sign ( haf, xi )
             vmch(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bmch(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
 830       continue
c
c
c      Evaluate "vsnp1" and "bsnp1" by solving the characteristic
c  equations.  There is no source term since the metric factor "g2" has
c  no explicit dependence on x3.
c
           do 30 k=ks,ke+1
             q2           = sign ( one, bave(k) )
             vsnp1(i,j,k) = ( vpch (k) * srdp (k) + vmch (k) * srdm (k)
     1                      + q2 * ( bpch(k) - bmch(k) ) )
     2                    / ( srdp (k) + srdm (k) )
             bsnp1(i,j,k) = ( bpch (k) * srdpi(k) + bmch (k) * srdmi(k)
     1                      + q2 * ( vpch(k) - vmch(k) ) )
     2                    / ( srdpi(k) + srdmi(k) )
             vsnp1(i,j,k) = vsnp1(i,j,k) * bave(k)
             bsnp1(i,j,k) = bsnp1(i,j,k) * vave(k)
30         continue
c
40       continue
c
50     continue
c
c-----------------------------------------------------------------------
c
c      By following the characteristics of the flow in the 2-direction,
c  determine values for "v3" and "b3" ("vsnm1" and "bsnm1") to be used
c  in evaluating "emf1".
c
       src = 0.0
c
c       Compute upwinded b2 and v2 in the 3-direction and use them to
c       compute the wave speeds for the chracteristic cones for the MoC
c
       do 199 j=js,je+1
         do 59 i=is,ie
           do 57 k=ks-2,ke+2
             vfl   (k) = 0.5 * (v3(i,j,k) + v3(i,j-1,k)) - vg3(k)
             vt    (k) = v2(i,j,k) - vg2(j)
             bt    (k) = b2(i,j,k)
57         continue
c           call x3int1d ( bt, vfl, iordb2, istpb2, i, j
c     1                 , g31b, g31bi, g32a, g32ai, bint )
c           call x3int1d ( vt, vfl, iords2, istps2, i, j
c     1                 , g31b, g31bi, g32a, g32ai, vint )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 857 k=ks-1,ke+1
             dqm      = ( vt(k  ) - vt(k-1) ) * dx3bi(k  )
             dqp      = ( vt(k+1) - vt(k  ) ) * dx3bi(k+1)
             dv (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( bt(k  ) - bt(k-1) ) * dx3bi(k  )
             dqp      = ( bt(k+1) - bt(k  ) ) * dx3bi(k+1)
             db (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 857       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g31bi(i) * g32ai(j)
           do 858 k=ks,ke+1
             qv1    = vt(k-1) + dx3a(k-1) * dv (k-1)
             qv2    = vt(k  ) - dx3a(k  ) * dv (k  )
             qb1    = bt(k-1) + dx3a(k-1) * db (k-1)
             qb2    = bt(k  ) - dx3a(k  ) * db (k  )
c  
             xi     = vfl(k) * fact
             q3     = sign ( haf, xi )
             vint(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bint(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
 858       continue
c
           do 58 k=ks,ke+1
             emf1(i,j,k) = vint(k)
             emf2(i,j,k) = bint(k)
 58       continue
 59    continue
 199   continue
c
c      Select an effective density and determine the characteristic
c  velocity using upwinded values for each characteristic in the
c  2-direction.
c
       do 299 k=ks,ke+1
         do 90 i=is,ie
           do 60 j=js,je+1
             vave (j) = emf1(i,j,k)
             bave (j) = emf2(i,j,k)
c            vave (j) = 0.5 * ( v2(i,j,k) + v2(i,j,km1) ) - vg2(j)
c            bave (j) = 0.5 * ( b2(i,j,k) + b2(i,j,km1) )
             absb     = abs ( bave(j) )
             aave (j) = 0.5 * absb * ( srd3(i,j,k) + srd3(i,j-1,k) )
     1                             / ( srd3(i,j,k) * srd3(i,j-1,k) )
             sgnp     = sign ( haf, vave(j) - aave(j) )
             sgnm     = sign ( haf, vave(j) + aave(j) )
             srdp (j) = ( 0.5 + sgnp ) * srd3(i,j-1,k)
     1                + ( 0.5 - sgnp ) * srd3(i,j,  k)
             srdm (j) = ( 0.5 + sgnm ) * srd3(i,j-1,k)
     1                + ( 0.5 - sgnm ) * srd3(i,j,  k)
             srdpi(j) = 1.0 / srdp(j)
             srdmi(j) = 1.0 / srdm(j)
             vchp (j) = vave(j) - absb * srdpi(j)
             vchm (j) = vave(j) + absb * srdmi(j)
60         continue
c
c      Interpolate 1-D vectors of "v3" and "b3" in the 2-direction to
c  the footpoints of both characteristics.
c
           do 70 j=js-2,je+2
             vtmp(j) = v3(i,j,k) - vg3(k)
             btmp(j) = b3(i,j,k)
70         continue
c           call x2zc1d ( vtmp, vchp, vchm, iords3, istps3, k, i
c     1                 , g2b, g2bi, vpch, vmch )
c           call x2zc1d ( btmp, vchp, vchm, iordb3, istpb3, k, i
c     1                 , g2b, g2bi, bpch, bmch )
c       subroutine x2zc1d ( q, vp, vm, iorder, isteep, k, i, g2, g2i
c     1                   , qp, qm )
c
           do 870 j=js-1,je+1
             dqm      = ( vtmp(j  ) - vtmp(j-1) ) * dx2bi(j  )
             dqp      = ( vtmp(j+1) - vtmp(j  ) ) * dx2bi(j+1)
             dv (j  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm     = ( btmp(j  ) - btmp(j-1) ) * dx2bi(j  )
             dqp     = ( btmp(j+1) - btmp(j  ) ) * dx2bi(j+1)
             db (j  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 870       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g2bi(i)
           do 880 j=js,je+1
             qv1    = vtmp(j-1) + dx2a(j-1) * dv (j-1)
             qv2    = vtmp(j  ) - dx2a(j  ) * dv (j  )
             qb1    = btmp(j-1) + dx2a(j-1) * db (j-1)
             qb2    = btmp(j  ) - dx2a(j  ) * db (j  )
c
             xi     = vchp(j) * fact
             q3     = sign ( haf, xi )
             vpch(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bpch(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )
c
             xi     = vchm(j) * fact
             q3     = sign ( haf, xi )
             vmch(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bmch(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )

 880       continue
c
c      Evaluate "vsnm1" and "bsnm1" by solving the characteristic
c  equations.  The source term is non-zero for RTP and ZRP coordinates
c  since dg32/dx2 .ne. 0.  Compute the two terms in "emf1".
c
           q1 = dt * g2bi(i)
           do 80 j=js,je+1
             q2           = sign ( one, bave(j) )

             vsnm1(  j  ) = ( vpch (j) * srdp (j) + vmch (j) * srdm (j)
     1                      + q2 * ( bpch(j) - bmch(j) ) )
     3                    / ( srdp (j) + srdm (j) ) + src
             bsnm1(  j  ) = ( bpch (j) * srdpi(j) + bmch (j) * srdmi(j)
     1                      + q2 * ( vpch(j) - vmch(j) ) )
     3                    / ( srdpi(j) + srdmi(j) )
             vsnm1(  j  ) = vsnm1(j) * bave(j)
             bsnm1(  j  ) = bsnm1(j) * vave(j)
c
             vsnp1(i,j,k) = 0.5*(  vsnp1(i,j,k) + bsnm1( j ) )
             bsnp1(i,j,k) = 0.5*(  vsnm1( j ) + bsnp1(i,j,k) )
 80       continue
 90     continue
 299  continue
c
c     END OF I LOOP
c
100    continue
c
c-----------------------------------------------------------------------
c
c      Set boundary values for "term1" and "term2".
c
C#ifdef MPI_USED
C       nreq = 0
C       nsub = nsub + 1
C#endif 

       call bvalemf1 ( vsnp1, bsnp1 )

c
c      Compute "emf1" for all 1-edges, including the ghost zones.
c
       do 130 k=ks-2,ke+3
         do 120 j=js-2,je+3
           do 110 i=is-2,ie+2
             emf1(i,j,k) = ( vsnp1(i,j,k) - bsnp1(i,j,k) )

     1                   * dx1a (i)

110        continue
120      continue
130    continue
c
c-----------------------------------------------------------------------
c---- 2.  emf2 ---------------------------------------------------------
c-----------------------------------------------------------------------
c
c     BEGINNING OF FIRST J LOOP
c
       do 180 j=js,je
c
c      By following the characteristics of the flow in the 1-direction,
c  determine values for "v3" and "b3" ("vsnp1" and "bsnp1") to be used
c  in evaluating "emf2".
c
         src = 0.0
c
c      Compute upwinded b1 and v1 in the 3-direction and use them to
c      compute the wave speeds for the chracteristic cones for the MoC.
c
         do 139 i=is,ie+1
           do 137 k=ks-2,ke+2
             vfl   (k) = 0.5*(v3(i,j,k) + v3(i-1,j,k)) - vg3(k)
             vt    (k) = v1(i,j,k) - vg1(i)
             bt    (k) = b1(i,j,k)
137        continue
c
c
c           call x3int1d ( bt, vfl, iordb1, istpb1, i, j
c     1                  , g31b, g31bi, g32a, g32ai, bint )
c           call x3int1d ( vt, vfl, iordb1, istpb1, i, j
c     1                  , g31b, g31bi, g32a, g32ai, vint )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 937 k=ks-1,ke+1
             dqm      = ( vt(k  ) - vt(k-1) ) * dx3bi(k  )
             dqp      = ( vt(k+1) - vt(k  ) ) * dx3bi(k+1)
             dv (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( bt(k  ) - bt(k-1) ) * dx3bi(k  )
             dqp      = ( bt(k+1) - bt(k  ) ) * dx3bi(k+1)
             db (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 937       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g31bi(i) * g32ai(j)
           do 938 k=ks,ke+1
             qv1    = vt(k-1) + dx3a(k-1) * dv (k-1)
             qv2    = vt(k  ) - dx3a(k  ) * dv (k  )
             qb1    = bt(k-1) + dx3a(k-1) * db (k-1)
             qb2    = bt(k  ) - dx3a(k  ) * db (k  )
c  
             xi     = vfl(k) * fact
             q3     = sign ( haf, xi )
             vint(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bint(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
 938       continue
c
           do 138 k=ks,ke+1
             v1intk(k,i) = vint(k)
             b1intk(k,i) = bint(k)
138        continue
139      continue
c
c      Select an effective density and determine the characteristic
c  velocity using upwinded values for each characteristic in the
c  1-direction.
c
         do 170 k=ks,ke+1
           do 140 i=is,ie+1
             vave (i) = v1intk(k,i)
             bave (i) = b1intk(k,i)
c            vave (i) = 0.5 * ( v1(i,j,k) + v1(i,j,km1) ) - vg1(i)
c            bave (i) = 0.5 * ( b1(i,j,k) + b1(i,j,km1) )
             absb     = abs ( bave(i) )
             aave (i) = 0.5 * absb * ( srd3(i,j,k) + srd3(i-1,j,k) )
     1                             / ( srd3(i,j,k) * srd3(i-1,j,k) )
             sgnp     = sign ( haf, vave(i) - aave(i) )
             sgnm     = sign ( haf, vave(i) + aave(i) )
             srdp (i) = ( 0.5 + sgnp ) * srd3(i-1,j,k)
     1                + ( 0.5 - sgnp ) * srd3(i  ,j,k)
             srdm (i) = ( 0.5 + sgnm ) * srd3(i-1,j,k)
     1                + ( 0.5 - sgnm ) * srd3(i  ,j,k)
             srdpi(i) = 1.0 / srdp(i)
             srdmi(i) = 1.0 / srdm(i)
             vchp (i) = vave(i) - absb * srdpi(i)
             vchm (i) = vave(i) + absb * srdmi(i)
140        continue
c
c      Interpolate 1-D vectors of "v3" and "b3" in the 1-direction to
c  the footpoints of both characteristics.
c
           do 150 i=is-2,ie+2
             vtmp(i) = v3(i,j,k) - vg3(k)
             btmp(i) = b3(i,j,k)
150        continue
c           call x1zc1d ( vtmp, vchp, vchm, iords3, istps3, j, k
c     1                 , vpch, vmch )
c           call x1zc1d ( btmp, vchp, vchm, iordb3, istpb3, j, k
c     1                 , bpch, bmch )
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 950 i=is-1,ie+1
             dqm      = ( vtmp(i  ) - vtmp(i-1) ) * dx1bi(i  )
             dqp      = ( vtmp(i+1) - vtmp(i  ) ) * dx1bi(i+1)
             dv (i  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( btmp(i  ) - btmp(i-1) ) * dx1bi(i  )
             dqp      = ( btmp(i+1) - btmp(i  ) ) * dx1bi(i+1)
             db (i  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 950       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           do 960 i=is,ie+1
             qv1    = vtmp(i-1) + dx1a(i-1) * dv (i-1)
             qv2    = vtmp(i  ) - dx1a(i  ) * dv (i  )
             qb1    = btmp(i-1) + dx1a(i-1) * db (i-1)
             qb2    = btmp(i  ) - dx1a(i  ) * db (i  )
c
             xi     = vchp(i) * dt
             q3     = sign ( haf, xi )
             vpch(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bpch(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
c
             xi     = vchm(i) * dt
             q3     = sign ( haf, xi )
             vmch(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bmch(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
 960       continue
c
c      Evaluate "vsnp1" and "bsnp1" by solving the characteristic
c  equations.  The source term is non-zero for RTP coordinates since
c  dg31/dx1 = 1.0.
c
           do 160 i=is,ie+1
             q2           = sign ( one, bave(i) )

             vsnp1(i,j,k) = ( vpch (i) * srdp (i) + vmch (i) * srdm (i)
     1                       + q2 * ( bpch(i) - bmch(i) ) )
     3                      / ( srdp (i) + srdm (i) ) + src
             bsnp1(i,j,k) = ( bpch (i) * srdpi(i) + bmch (i) * srdmi(i)
     1                       + q2 * ( vpch (i) - vmch (i) ) )
     3                      / ( srdpi(i) + srdmi(i) )
             vsnp1(i,j,k) = vsnp1(i,j,k) * bave(i)
             bsnp1(i,j,k) = bsnp1(i,j,k) * vave(i)
160        continue
c
170      continue
c
c        END OF FIRST J LOOP
c
180    continue
c
c-----------------------------------------------------------------------
c
c
c       BEGINNING OF SECOND J LOOP
c
c
       do 230 j=js,je
c
c      By following the characteristics of the flow in the 3-direction,
c  determine values for "v1" and "b1" ("vsnm1" and "bsnm1") to be used
c  in evaluating "emf2".
c
c       Compute upwinded b3 and v3 in the 1-direction and use them to
c       compute the wave speeds for the chracteristic cones for the MoC
c
         do 189 k=ks,ke+1
           do 188 i=is-2,ie+2
             vfl   (i) = 0.5*(v1(i,j,k) + v1(i,j,k-1)) - vg1(i)
             vt    (i) = v3(i,j,k) - vg3(k)
             bt    (i) = b3(i,j,k)
188        continue
c
c           call x1int1d ( bt, vfl, iordb1, istpb1, j, k
c     1                  , bint )
c           call x1int1d ( vt, vfl, iords1, istps1, j, k
c     1                  , vint )
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 988 i=is-1,ie+1
             dqm      = ( vt(i  ) - vt(i-1) ) * dx1bi(i  )
             dqp      = ( vt(i+1) - vt(i  ) ) * dx1bi(i+1)
             dv (i  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( bt(i  ) - bt(i-1) ) * dx1bi(i  )
             dqp      = ( bt(i+1) - bt(i  ) ) * dx1bi(i+1)
             db (i  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 988       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           do 987 i=is,ie+1
             qv1    = vt(i-1) + dx1a(i-1) * dv (i-1)
             qv2    = vt(i  ) - dx1a(i  ) * dv (i  )
             qb1    = bt(i-1) + dx1a(i-1) * db (i-1)
             qb2    = bt(i  ) - dx1a(i  ) * db (i  )
c
             xi     = vfl(i) * dt
             q3     = sign ( haf, xi )
             vint(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bint(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
 987       continue
c
           do 187 i=is,ie+1
             v3inti(k,i) = vint(i)
             b3inti(k,i) = bint(i)
187        continue
189      continue
c
c
c      Select an effective density and determine the characteristic
c  velocity using upwinded values for each characteristic in the
c  3-direction.
c
         do 220 i=is,ie+1
           do 190 k=ks,ke+1
             vave (k) = v3inti(k,i)
             bave (k) = b3inti(k,i)
c            vave (k) = 0.5 * ( v3(i,j,k) + v3(im1,j,k) ) - vg3(k)
c            bave (k) = 0.5 * ( b3(i,j,k) + b3(im1,j,k) )
             absb     = abs ( bave(k) )
             aave (k) = 0.5 * absb * ( srd1(i,j,k) + srd1(i,j,k-1) )
     1                             / ( srd1(i,j,k) * srd1(i,j,k-1) )
             sgnp     = sign ( haf, vave(k) - aave(k) )
             sgnm     = sign ( haf, vave(k) + aave(k) )
             srdp (k) = ( 0.5 + sgnp ) * srd1(i,j,k-1)
     1                + ( 0.5 - sgnp ) * srd1(i,j,k  )
             srdm (k) = ( 0.5 + sgnm ) * srd1(i,j,k-1)
     1                + ( 0.5 - sgnm ) * srd1(i,j,k  )
             srdpi(k) = 1.0 / srdp(k)
             srdmi(k) = 1.0 / srdm(k)
             vchp (k) = vave(k) - absb * srdpi(k)
             vchm (k) = vave(k) + absb * srdmi(k)
190        continue
c
c      Interpolate 1-D vectors of "v1" and "b1" in the 3-direction to
c  the footpoints of both characteristics.
c
           do 200 k=ks-2,ke+2
             vtmp(k) = v1(i,j,k) - vg1(i)
             btmp(k) = b1(i,j,k)
200        continue
c           call x3zc1d ( vtmp, vchp, vchm, iords1, istps1, i, j
c     1                 , g31a, g31ai, g32b, g32bi, vpch, vmch )
c           call x3zc1d ( btmp, vchp, vchm, iordb1, istpb1, i, j
c     1                 , g31a, g31ai, g32b, g32bi, bpch, bmch )
c       subroutine x3zc1d ( q, vp, vm, iorder, isteep, i, j, g31, g31i
c     1                   , g32, g32i, qp, qm )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 1000 k=ks-1,ke+1
             dqm      = ( vtmp(k  ) - vtmp(k-1) ) * dx3bi(k  )
             dqp      = ( vtmp(k+1) - vtmp(k  ) ) * dx3bi(k+1)
             dv (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( btmp(k  ) - btmp(k-1) ) * dx3bi(k  )
             dqp      = ( btmp(k+1) - btmp(k  ) ) * dx3bi(k+1)
             db (k  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 1000      continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g31ai(i) * g32bi(j)
           do 1010 k=ks,ke+1
             qv1    = vtmp(k-1) + dx3a(k-1) * dv (k-1)
             qv2    = vtmp(k  ) - dx3a(k  ) * dv (k  )
             qb1    = btmp(k-1) + dx3a(k-1) * db (k-1)
             qb2    = btmp(k  ) - dx3a(k  ) * db (k  )
c  
             xi     = vchp(k) * fact
             q3     = sign ( haf, xi )
             vpch(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bpch(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
c
             xi     = vchm(k) * fact
             q3     = sign ( haf, xi )
             vmch(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bmch(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
 1010      continue
c
c      Evaluate "vsnm1" and "bsnm1" by solving the characteristic
c  equations.  There is no source term since the metric factor "g1" has
c  no explicit dependence on x3.  Compute the two terms in "emf2".
c
           do 210 k=ks,ke+1
             q2           = sign ( one, bave(k) )
             vsnm1(    k) = ( vpch (k) * srdp (k) + vmch (k) * srdm (k)
     1                      + q2 * ( bpch(k) - bmch(k) ) )
     2                    / ( srdp (k) + srdm (k) )
             bsnm1(    k) = ( bpch (k) * srdpi(k) + bmch (k) * srdmi(k)
     1                      + q2 * ( vpch(k) - vmch(k) ) )
     2                    / ( srdpi(k) + srdmi(k) )
             vsnm1(    k) = vsnm1(k) * bave(k)
             bsnm1(    k) = bsnm1(k) * vave(k)
c
             vsnp1(i,j,k) = 0.5*(  vsnp1(i,j,k) + bsnm1(k) )
             bsnp1(i,j,k) = 0.5*(  vsnm1(    k) + bsnp1(i,j,k) )
210        continue
220      continue
c
c       END OF SECOND J LOOP
c
230    continue
c
c-----------------------------------------------------------------------
c
c      Set boundary values for "term1" and "term2".
c
C#ifdef MPI_USED
C       nreq = 0
C       nsub = nsub + 1
C#endif 
       call bvalemf2 ( vsnp1, bsnp1 )

c
c      Compute "emf2" for all 2-edges, including the ghost zones.
c
       do 260 k=ks-2,ke+3
         do 250 j=js-2,je+2
           do 240 i=is-2,ie+3
             emf2(i,j,k) = ( vsnp1(i,j,k) - bsnp1(i,j,k) )

     1                   * dx2a (j) * g2a (i)

240        continue
250      continue
260    continue
c
c-----------------------------------------------------------------------
c---- 3.  emf3 ---------------------------------------------------------
c-----------------------------------------------------------------------
c
c      BEGINNING OF K LOOP
c
       do 360 k=ks,ke
c
c      By following the characteristics of the flow in the 2-direction,
c  determine values for "v1" and "b1" ("vsnp1" and "bsnp1") to be used
c  in evaluating "emf3".
c
c      Compute upwinded b2 and v2 in the 1-direction and use them to
c      compute the wave speeds for the chracteristic cones for MoC.
c
         do 269 j=js,je+1
           do 267 i=is-2,ie+2
             vfl   (i) = 0.5 * (v1(i,j,k) + v1(i,j-1,k)) - vg1(i)
             vt    (i) = v2(i,j,k) - vg2(j)
             bt    (i) = b2(i,j,k)
267        continue
c           call x1int1d ( bt, vfl, iordb2, istpb2, j, k
c     1                  , bint )
c           call x1int1d ( vt, vfl, iords2, istps2, j, k
c     1                  , vint )
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 1067 i=is-1,ie+1
             dqm      = ( vt(i  ) - vt(i-1) ) * dx1bi(i  )
             dqp      = ( vt(i+1) - vt(i  ) ) * dx1bi(i+1)
             dv (i  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm      = ( bt(i  ) - bt(i-1) ) * dx1bi(i  )
             dqp      = ( bt(i+1) - bt(i  ) ) * dx1bi(i+1)
             db (i  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 1067      continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           do 1068 i=is,ie+1
             qv1    = vt(i-1) + dx1a(i-1) * dv (i-1)
             qv2    = vt(i  ) - dx1a(i  ) * dv (i  )
             qb1    = bt(i-1) + dx1a(i-1) * db (i-1)
             qb2    = bt(i  ) - dx1a(i  ) * db (i  )
c
             xi     = vfl(i) * dt
             q3     = sign ( haf, xi )
             vint(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bint(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
 1068      continue
c
           do 268 i=is,ie+1
             v2inti(i,j) = vint(i)
             b2inti(i,j) = bint(i)
268        continue
269      continue
c
c      Select an effective density and determine the characteristic
c  velocity using upwinded values for each characteristic in the
c  2-direction.
c
         do 300 i=is,ie+1
           do 270 j=js,je+1
             vave (j) = v2inti(i,j)
             bave (j) = b2inti(i,j)
c            vave (j) = 0.5 * ( v2(i,j,k) + v2(im1,j,k) ) - vg2(j)
c            bave (j) = 0.5 * ( b2(i,j,k) + b2(im1,j,k) )
             absb     = abs ( bave(j) )
             aave (j) = 0.5 * absb * ( srd1(i,j,k) + srd1(i,j-1,k) )
     1                             / ( srd1(i,j,k) * srd1(i,j-1,k) )
             sgnp     = sign ( haf, vave(j) - aave(j) )
             sgnm     = sign ( haf, vave(j) + aave(j) )
             srdp (j) = ( 0.5 + sgnp ) * srd1(i,j-1,k)
     1                + ( 0.5 - sgnp ) * srd1(i,j  ,k)
             srdm (j) = ( 0.5 + sgnm ) * srd1(i,j-1,k)
     1                + ( 0.5 - sgnm ) * srd1(i,j  ,k)
             srdpi(j) = 1.0 / srdp(j)
             srdmi(j) = 1.0 / srdm(j)
             vchp (j) = vave(j) - absb * srdpi(j)
             vchm (j) = vave(j) + absb * srdmi(j)
270        continue
c
c      Interpolate 1-D vectors of "v1" and "b1" in the 2-direction to
c  the footpoints of both characteristics.
c
           do 280 j=js-2,je+2
             vtmp(j) = v1(i,j,k) - vg1(i)
             btmp(j) = b1(i,j,k)
280        continue
c           call x2zc1d ( vtmp, vchp, vchm, iords1, istps1, k, i
c     1                 , g2a, g2ai, vpch, vmch )
c           call x2zc1d ( btmp, vchp, vchm, iordb1, istpb1, k, i
c     1                 , g2a, g2ai, bpch, bmch )
c       subroutine x2zc1d ( q, vp, vm, iorder, isteep, k, i, g2, g2i
c     1                   , qp, qm )
c
           do 1080 j=js-1,je+1
             dqm      = ( vtmp(j  ) - vtmp(j-1) ) * dx2bi(j  )
             dqp      = ( vtmp(j+1) - vtmp(j  ) ) * dx2bi(j+1)
             dv (j  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )

             dqm     = ( btmp(j  ) - btmp(j-1) ) * dx2bi(j  )
             dqp     = ( btmp(j+1) - btmp(j  ) ) * dx2bi(j+1)
             db (j  ) = max ( dqm * dqp, zro )
     1                * sign ( one, dqm + dqp )
     2                / max ( abs ( dqm + dqp ), tiny )
 1080      continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g2ai(i)
           do 1090 j=js,je+1
             qv1    = vtmp(j-1) + dx2a(j-1) * dv (j-1)
             qv2    = vtmp(j  ) - dx2a(j  ) * dv (j  )
             qb1    = btmp(j-1) + dx2a(j-1) * db (j-1)
             qb2    = btmp(j  ) - dx2a(j  ) * db (j  )
c
             xi     = vchp(j) * fact
             q3     = sign ( haf, xi )
             vpch(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bpch(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )
c
             xi     = vchm(j) * fact
             q3     = sign ( haf, xi )
             vmch(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bmch(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )

 1090      continue
c
c      Evaluate "vsnp1" and "bsnp1" by solving the characteristic
c  equations.  There is no source term since the metric factor "g1" has
c  no explicit dependence on x2.
c
           do 290 j=js,je+1
             q2           = sign ( one, bave(j) )
             vsnp1(i,j,k) = ( vpch (j) * srdp (j) + vmch (j) * srdm (j)
     1                      + q2 * ( bpch(j) - bmch(j) ) )
     2                    / ( srdp (j) + srdm (j) )
             bsnp1(i,j,k) = ( bpch (j) * srdpi(j) + bmch (j) * srdmi(j)
     1                      + q2 * ( vpch(j) - vmch(j) ) )
     2                    / ( srdpi(j) + srdmi(j) )
             vsnp1(i,j,k) = vsnp1(i,j,k) * bave(j)
             bsnp1(i,j,k) = bsnp1(i,j,k) * vave(j)
290        continue
c
300    continue
c
c-----------------------------------------------------------------------
c
c      By following the characteristics of the flow in the 1-direction,
c  determine values for "v2" and "b2" ("vsnm1" and "bsnm1") to be used
c  in evaluating "emf3".
c
       src = 0.0
c
c      Compute upwinded b1 and v1 in the 2-direction and use them to
c      compute the wave speeds for the chracteristic cones for Moc
c
       do 319 i=is,ie+1
         do 317 j=js-2,je+2
           vfl   (j) = 0.5 * (v2(i,j,k) + v2(i-1,j,k)) - vg2(j)
           vt    (j) = v1(i,j,k) - vg1(i)
           bt    (j) = b1(i,j,k)
317      continue
c         call x2int1d ( bt, vfl, iordb1, istpb1, k, i
c     1                , g2b, g2bi, bint )
c         call x2int1d ( vt, vfl, iords1, istps1, k, i
c     1                , g2b, g2bi, vint )
c
         do 1117 j=js-1,je+1
           dqm      = ( vt(j  ) - vt(j-1) ) * dx2bi(j  )
           dqp      = ( vt(j+1) - vt(j  ) ) * dx2bi(j+1)
           dv (j  ) = max ( dqm * dqp, zro )
     1              * sign ( one, dqm + dqp )
     2              / max ( abs ( dqm + dqp ), tiny )

           dqm     = ( bt(j  ) - bt(j-1) ) * dx2bi(j  )
           dqp     = ( bt(j+1) - bt(j  ) ) * dx2bi(j+1)
           db (j  ) = max ( dqm * dqp, zro )
     1              * sign ( one, dqm + dqp )
     2              / max ( abs ( dqm + dqp ), tiny )
 1117    continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
         fact = dt * g2bi(i)
         do 1118 j=js,je+1
           qv1    = vt(j-1) + dx2a(j-1) * dv (j-1)
           qv2    = vt(j  ) - dx2a(j  ) * dv (j  )
           qb1    = bt(j-1) + dx2a(j-1) * db (j-1)
           qb2    = bt(j  ) - dx2a(j  ) * db (j  )
c
           xi     = vfl(j) * fact
           q3     = sign ( haf, xi )
           vint(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1            + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
           bint(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1            + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )
 1118    continue
c
         do 318 j=js,je+1
           b1intj(i,j) = bint(j)
           v1intj(i,j) = vint(j)
318      continue
319    continue
c
c      Select an effective density and determine the characteristic
c  velocity using upwinded values for each characteristic in the
c  1-direction.
c
       do 350 j=js,je+1
         do 320 i=is,ie+1
           vave (i) = v1intj(i,j)
           bave (i) = b1intj(i,j)
c          vave (i) = 0.5 * ( v1(i,j,k) + v1(i,jm1,k) ) - vg1(i)
c          bave (i) = 0.5 * ( b1(i,j,k) + b1(i,jm1,k) )
           absb     = abs ( bave(i) )
           aave (i) = 0.5 * absb * ( srd2(i,j,k) + srd2(i-1,j,k) )
     1                           / ( srd2(i,j,k) * srd2(i-1,j,k) )
           sgnp     = sign ( haf, vave(i) - aave(i) )
           sgnm     = sign ( haf, vave(i) + aave(i) )
           srdp (i) = ( 0.5 + sgnp ) * srd2(i-1,j,k)
     1              + ( 0.5 - sgnp ) * srd2(i  ,j,k)
           srdm (i) = ( 0.5 + sgnm ) * srd2(i-1,j,k)
     1              + ( 0.5 - sgnm ) * srd2(i  ,j,k)
           srdpi(i) = 1.0 / srdp(i)
           srdmi(i) = 1.0 / srdm(i)
           vchp (i) = vave(i) - absb * srdpi(i)
           vchm (i) = vave(i) + absb * srdmi(i)
320      continue
c
c      Interpolate 1-D vectors of "v2" and "b2" in the 1-direction to
c  the footpoints of both characteristics.
c
         do 330 i=is-2,ie+2
           vtmp(i) = v2(i,j,k) - vg2(j)
           btmp(i) = b2(i,j,k)
330      continue
c         call x1zc1d ( vtmp, vchp, vchm, iords2, istps2, j, k
c     1               , vpch, vmch )
c         call x1zc1d ( btmp, vchp, vchm, iordb2, istpb2, j, k
c     1               , bpch, bmch )
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
         do 1130 i=is-1,ie+1
           dqm      = ( vtmp(i  ) - vtmp(i-1) ) * dx1bi(i  )
           dqp      = ( vtmp(i+1) - vtmp(i  ) ) * dx1bi(i+1)
           dv (i  ) = max ( dqm * dqp, zro )
     1              * sign ( one, dqm + dqp )
     2              / max ( abs ( dqm + dqp ), tiny )
           
           dqm      = ( btmp(i  ) - btmp(i-1) ) * dx1bi(i  )
           dqp      = ( btmp(i+1) - btmp(i  ) ) * dx1bi(i+1)
           db (i  ) = max ( dqm * dqp, zro )
     1              * sign ( one, dqm + dqp )
     2              / max ( abs ( dqm + dqp ), tiny )
 1130    continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
         do 1140 i=is,ie+1
           qv1    = vtmp(i-1) + dx1a(i-1) * dv (i-1)
           qv2    = vtmp(i  ) - dx1a(i  ) * dv (i  )
           qb1    = btmp(i-1) + dx1a(i-1) * db (i-1)
           qb2    = btmp(i  ) - dx1a(i  ) * db (i  )
c          
           xi     = vchp(i) * dt
           q3     = sign ( haf, xi )
           vpch(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1            + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
           bpch(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1            + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
c
           xi     = vchm(i) * dt
           q3     = sign ( haf, xi )
           vmch(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1            + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
           bmch(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1            + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
1140    continue
c
c      Evaluate "vsnm1" and "bsnm1" by solving the characteristic
c  equations.  The source term is non-zero for RTP coordinates since
c  dg2/dx1 = 1.0.  Compute the two terms in "emf3".
c
         do 340 i=is,ie+1
           q2           = sign ( one, bave(i) )

           vsnm1(i    ) = ( vpch (i) * srdp (i) + vmch (i) * srdm (i)
     1                    + q2 * ( bpch(i) - bmch(i) ) )
     3                  / ( srdp (i) + srdm (i) ) + src
           bsnm1(i    ) = ( bpch (i)* srdpi(i) + bmch (i)* srdmi(i)
     1                    + q2 * ( vpch(i) - vmch(i) ) )
     3                  / ( srdpi(i) + srdmi(i) )
c
           vsnm1(i    ) = vsnm1(i) * bave(i)
           bsnm1(i    ) = bsnm1(i) * vave(i)
c
           vsnp1(i,j,k) = 0.5*(  vsnp1(i,j,k) + bsnm1(i) )
           bsnp1(i,j,k) = 0.5*(  vsnm1(i)     + bsnp1(i,j,k) )
340      continue
350    continue
c
c       END OF K LOOP
c
360    continue
c
c-----------------------------------------------------------------------
c
c      Set boundary values for "term1" and "term2".
c
C#ifdef MPI_USED
C       nreq = 0
C       nsub = nsub + 1
C#endif 
       call bvalemf3 ( vsnp1, bsnp1 )

c
c      Compute "emf3" for all 3-edges, including the ghost zones.
c
       do 390 k=ks-2,ke+2
         do 380 j=js-2,je+3
           do 370 i=is-2,ie+3
             emf3(i,j,k) = ( vsnp1(i,j,k) - bsnp1(i,j,k) )

     1                   * dx3a (k) * g31a (i) * g32a (j)

370        continue
380      continue
390    continue

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 H S M O C                 \\\\\\\\\\
c
c=======================================================================
