












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                    C T                    \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine ct

c
c    dac:zeus3d.ct <-------- updates B-field using constrained transport
c    from jms:zeus2d.ct                                    october, 1989
c
c    written by: David Clarke
c    modified 1: May, 1990 by David Clarke; reworked to call the new
c                interpolation routines which need only be called once.
c    modified 2: August, 1990 by David Clarke; moved magnetic fields to
c                the face-centres (cospatial with the velocities).
c                Implemented a method of characteristics for evaluating
c                the emf's.  The transverse Lorentz accelerations are
c                now applied to the velocities during this step.
c    modified 3: November, 1990 by David Clarke; resurrected non-MoC
c                algorithm for evaluating emfs.  Added EDITOR alias MOC
c                which needs to be defined if MoC is to be used.
c    modified 4: June, 1992 by David Clarke; reworked singularity
c                formalism.
c    modified 5: December, 1992 by David Clarke (as suggested by John
c                Hawley); split velocity update from emf computation.
c                Velocities are now Lorentz-accelerated with *old*
c                magnetic field values. emf's are then estimated with
c                Lorentz-updated velocities.
c    modified 6: 3 March 1998, by Mordecai-Mark Mac Low; translated 
c                into ZEUS-MP form.
c
c  PURPOSE:  This routine transports the three components of the
c  magnetic field using a variation of the non-relativistic Constrained
c  Transport scheme (CT), developed by Chuck Evans and John Hawley (Ap.
c  J., 342, 700).  In this implementation, the magnetic field components
c  are face-centred, cospatial with the velocities and are updated using
c  edge-centred emf's which are cospatial with the current densities.
c
c  The emf's are evaluated by HSMOC in which the velocities and
c  magnetic field components required to compute the emf's are estimated
c  using the Method of Characteristics (MoC) algorithm developed by Jim
c  Stone et al. for 2-D.  For self-consistency, the transverse Lorentz
c  accelerations have been removed from STV* and are applied to the
c  velocities in LORENTZ using the pre-updated magnetic fields.  By
c  experimentation, it has been determined that performing the Lorentz
c  update after the magnetic field update is unstable.
c
c  LOCAL VARIABLES:
c    emf1      emf along the 2-3 edges of the grid (= v2*b3 - v3*b2)
c    emf2      emf along the 3-1 edges of the grid (= v3*b1 - v1*b3)
c    emf3      emf along the 1-2 edges of the grid (= v1*b2 - v2*b1)
c
c BOUNDARY VALUES USED:
c
c    var    ii    oi    ij    oj    ik    ok
c    ----  ----  ----  ----  ----  ----  ----
c    emf1  is-2  ie+2  js-2  je+3  ks-2  ke+3
c    emf2  is-2  ie+3  js-2  je+2  ks-2  ke+3
c    emf3  is-2  ie+3  js-2  je+3  ks-2  ke+2
c
c  EXTERNALS:
c    LORENTZ_D, HSMOC 
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
c
       integer       i       , ip1     , j       , jp1     , k
     1             , kp1
c
       real*8        qty1    (ijkn), qty1ni  (ijkn), qty2    (ijkn)
     1           , qty2ni  (ijkn)
c
       real*8        emf1    (  in,  jn,  kn), emf2    (  in,  jn,  kn)
     1           , emf3    (  in,  jn,  kn)
c
       equivalence   ( qty1    , w1da     ), ( qty1ni  , w1db     )
     1             , ( qty2    , w1dc     ), ( qty2ni  , w1dd     )
c
CPS
C       equivalence   ( emf1    , w3da     ), ( emf2    , w3db     )
C     2             , ( emf3    , w3dc     )
c
c      External statements
c
       external      lorentz_d, hsmoc
c
c-----------------------------------------------------------------------

c
c-----------------------------------------------------------------------
c-------------------------> Update velocities <-------------------------
c-----------------------------------------------------------------------
c
c      Compute the transverse Lorentz forces and accelerate the
c  velocities.
c

c modified for SPEC CPU2006
c       call lorentz_d (v1, v2, v3)
       call lorentz_d

c
c-----------------------------------------------------------------------
c-------------------------------> emfs <--------------------------------
c-----------------------------------------------------------------------
c
c  update all the boundary values of v before going into hsmoc to
c  compute the emfs.  Boy could this ever benefit from some
c  overlapping! think about this... (M-MML/MLN 25.3.98)

       nreq=0
       nsub = nsub + 1
       call bvalv1(3,3,0,0,0,0,v1)
       call bvalv2(3,3,0,0,0,0,v2)
       call bvalv3(3,3,0,0,0,0,v3)


       nreq=0
       nsub = nsub + 1
       call bvalv1(0,0,3,3,0,0,v1)
       call bvalv2(0,0,3,3,0,0,v2)
       call bvalv3(0,0,3,3,0,0,v3)


       nreq=0
       nsub = nsub + 1
       call bvalv1(0,0,0,0,3,3,v1)
       call bvalv2(0,0,0,0,3,3,v2)
       call bvalv3(0,0,0,0,3,3,v3)


c
       call hsmoc   ( emf1, emf2, emf3 )
c
c-----------------------------------------------------------------------
c
c      The emf's are finished.  Update b1, b2, and b3 using the emf's.
c  Since the same emf's are used throughout the grid, div(b) will be
c  conserved numerically to within truncation error.
c
c      Coordinate-imposed boundary conditions (e.g., reflecting at x2a=0
c  in ZRP, periodic at x3a=2*pi in RTP and ZRP) are communicated to the
c  magnetic field by the emfs.  Both the old and new zone face areas are
c  used to account for grid compression.
c
c-----------------------------------------------------------------------
c-----------------------------> Update b1 <-----------------------------
c-----------------------------------------------------------------------
c
       do 10 i=is,ie+1
         qty1  (i) = g2a   (i) * g31a  (i)


         qty1ni(i) = g2ai (i) * g31ai(i)

10     continue
       do 20 j=js-2,je+2
         qty2  (j) = g32b  (j) * dx2a  (j)


         qty2ni(j) = g32bi(j) * dx2ai(j)

20     continue

       do 50 k=ks-2,ke+2
         do 40 j=js-2,je+2
           do 30 i=is,ie+1
             b1(i,j,k) = ( b1(i,j,k) * qty1(i) * qty2(j) * dx3a(k)
     1                   + dt * ( emf3(i,j+1,k  ) - emf3(i,j,k)
     2                          - emf2(i,j  ,k+1) + emf2(i,j,k) ) )


     3                 * qty1ni(i) * qty2ni(j) * dx3ai(k)

30         continue
40       continue
50     continue
c
c-----------------------------------------------------------------------
c-----------------------------> Update b2 <-----------------------------
c-----------------------------------------------------------------------
c
       do 60 i=is-2,ie+2
         qty1  (i) = g31b  (i) * dx1a  (i)


         qty1ni(i) = g31bi(i) * dx1ai(i)

60     continue

       do 100 k=ks-2,ke+2
         do 90 j=js,je+1
           do 80 i=is-2,ie+2
             b2(i,j,k) = ( b2(i,j,k) * qty1(i) * g32a(j) * dx3a(k)
     1                   + dt * ( emf1(i  ,j,k+1) - emf1(i,j,k)
     2                          - emf3(i+1,j,k  ) + emf3(i,j,k) ) )


     3                 * qty1ni(i) * g32ai(j) * dx3ai(k)

80         continue
90       continue
100    continue
c
c-----------------------------------------------------------------------
c-----------------------------> Update b3 <-----------------------------
c-----------------------------------------------------------------------
c
       do 110 i=is-2,ie+2
         qty1  (i) = g2b   (i) * dx1a  (i)


         qty1ni(i) = g2bi (i) * dx1ai(i)

110    continue

       do 150 k=ks,ke+1
         do 140 j=js-2,je+2
           do 130 i=is-2,ie+2
             b3(i,j,k) = ( b3(i,j,k) * qty1(i) * dx2a(j)
     1                   + dt * ( emf2(i+1,j  ,k) - emf2(i,j,k)
     2                          - emf1(i  ,j+1,k) + emf1(i,j,k) ) )


     3                 * qty1ni(i) * dx2ai(j)

130        continue
140      continue
150    continue

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                    C T                    \\\\\\\\\\
c
c=======================================================================
c
c
