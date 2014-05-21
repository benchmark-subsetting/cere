












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 B L A S T                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine blast
c
c    mml:zeus3d.blast <----------- initialises spherical supernova blast
c                                                        september, 1987
c
c    written by: Mordecai-Mark Low
c    modified 1: November, 1987 by Mordecai-Mark Low; modified for
c                ZEUS04
c    modified 2: October, 1988 by Jim Stone; incorporated in ZEUS2D
c    modified 3: February, 1990 by David Clarke; incorporated into
c                ZEUS3D
c    modified 4: October, 1992 by David Clarke; modified to do multi-
c                dimensional shear alfven wave tests.
c    modified 5: Feb. 15, 1996 by Robert Fiedler; for ZEUS-MP
c    modified 6: Dec. 27, 1996 by Robert Fiedler; added radiation
c    modified 7: 18.3.98 by Mordecai-Mark Mac Low; corrected  (BC's and 
c       removed div B violating field spec in central region)
c
c  PURPOSE: Sets up a spherical/circular region at a specified point on
c  the grid (x10, x20, x30) with the specified radius (r) whose flow
c  variables differ from the rest of the initial grid.
c
c  LOCAL VARIABLES:
c    r            initial radius of overpressured region
c    x10,x20,x30  coordinates of centre of overpressured region.
c    drat         ratio of density  across blast front
c    prat         ratio of pressure across blast front
c    d0           density          in ambient medium (default = 1.0)
c    p0           pressure         in ambient medium (default = 0.6)
c    e0           internal energy  in ambient medium (default = 0.9)
c    er0          radiation energy in ambient medium (default = 1.0)
c    v10          1-velocity       in ambient medium
c    v20          2-velocity       in ambient medium
c    v30          3-velocity       in ambient medium
c    b10          1-magnetic field on entire grid
c    b20          2-magnetic field on entire grid
c    b30          3-magnetic field on entire grid
c    d1           density          in central region (default = 1.0)
c    p1           pressure         in central region (default = 0.6)
c    e1           internal energy  in central region (default = 0.9)
c    er1          radiation energy in central region (default = 30.)
c    v11          1-velocity       in central region
c    v21          2-velocity       in central region
c    v31          3-velocity       in central region
c    m,drs,drc    parameters for specifying a sphere whose surface is
c                 sinusoidally perturbed (spherical coordinates only
c                 For an unperturbed sphere, set all values to zero
c                 (default).
c
c  EXTERNALS:
c    OVERLAP      [ only]
c    BNDYALL
c    BSETMAG
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
       integer       i       , j       , k       , ip1     , jp1
     1             , kp1     , m
       real*8        r       , x10     , x20     , x30     , drat
     1             , prat    , d0      , p0      , e0      , v10
     1             , v20     , v30    , b10      , b20     , b30
     2             , d1      , p1      , e1      , v11     , v21
     1             , v31     , drs
     2             , drc     , rsq     , rin     , rout    , frac
     4             , cofrac  , mass

c
       integer       iin     (ijkn), iout    (ijkn), jin     (ijkn)
     1             , jout    (ijkn), kin     (ijkn), kout    (ijkn)
       real*8        massk   (ijkn)
c
       equivalence   ( massk   , w1dg     )
c     1             , ( iin     , w1da     ), ( iout    , w1db     )
c     2             , ( jin     , w1dc     ), ( jout    , w1dd     )
c     3             , ( kin     , w1de     ), ( kout    , w1df     )
c
c      External statements
c
       real*8   sasum
       external   sasum

       real*8 overlap
       external overlap

c
       namelist / pgen     /
     1               r       , x10     , x20     , x30     , drat
     2             , prat    , d0      , p0      , e0

     3             , v10     , v20     , v30     
     4             , b10     , b20     , b30
     5             , d1      , p1      , e1      

     6             , v11     , v21     , v31     
     8             , drs     , drc     , m
c
c-----------------------------------------------------------------------
c
       r    = 1.0
       x10  = 0.0
       x20  = 0.0
       x30  = 0.0
       drat = 0.0
       prat = 0.0
       d0   = 1.0
       p0   = 0.6
       e0   = 0.0
       v10  = 0.0
       v20  = 0.0
       v30  = 0.0
       b10  = 0.0
       b20  = 0.0
       b30  = 0.0
       d1   = 1.0
       p1   = 0.6
       e1   = 0.0
       v11  = 0.0
       v21  = 0.0
       v31  = 0.0
       drs  = 0.0
       drc  = 0.0
       m    = 0

c
       if (myid .eq. 0) then
         read (1, pgen)
cpu2006         write (2, pgen)

       endif
c
c      Set up atmosphere.
c
       if (e0 .ne. 0.0) p0 = e0 * gamm1
       if (e0 .eq. 0.0) e0 = p0 / gamm1

       k  = ks
       j  = js
       i  = is
       do 30 k=ks,ke
         do 20 j=js,je
           do 10 i=is,ie
             d (i,j,k) = d0
             v1(i,j,k) = v10
             v2(i,j,k) = v20
             v3(i,j,k) = v30

             e (i,j,k) = e0


10         continue
20       continue
30     continue

       do 31 k=ks-2,ke+3
         do 21 j=js-2,je+3
           do 11 i=is-2,ie+3
             b1(i,j,k) = b10
             b2(i,j,k) = b20
             b3(i,j,k) = b30
11         continue
21       continue
31     continue

c
c      Set up central region.
c
       do 40 i=is,ie
         ip1 = i + 1
         if ( abs(x1a(i)-x10) .lt. abs(x1a(ip1)-x10) ) then
           iin (i) = i
           iout(i) = ip1
         else
           iin (i) = ip1
           iout(i) = i
         endif
40     continue
c
       do 50 j=js,je
         jp1 = j + 1
         if ( abs(x2a(j)-x20) .lt. abs(x2a(jp1)-x20) ) then
           jin (j) = j
           jout(j) = jp1
         else
           jin (j) = jp1
           jout(j) = j
         endif
50     continue
c
       do 60 k=ks,ke
         kp1 = k + 1
         if ( abs(x3a(k)-x30) .lt. abs(x3a(kp1)-x30) ) then
           kin (k) = k
           kout(k) = kp1
         else
           kin (k) = kp1
           kout(k) = k
         endif
         massk(k) = 0.0
60     continue
c
       if (drat .ne. 0.0) d1 = d0 * drat
       if (prat .ne. 0.0) p1 = p0 * prat
       if (e1   .ne. 0.0) then
         p1 = e1 * gamm1
       else
         e1 = p1 / gamm1
       endif

c
       do 90 k=ks,ke
         do 80 j=js,je
           do 70 i=is,ie

             rsq  = r**2
             rin  = ( x1a(iin (i)) - x10 )**2
     1            + ( x2a(jin (j)) - x20 )**2
     2            + ( x3a(kin (k)) - x30 )**2
             rout = ( x1a(iout(i)) - x10 )**2
     1            + ( x2a(jout(j)) - x20 )**2
     2            + ( x3a(kout(k)) - x30 )**2



             if ( (rin .lt. rsq) .and. (rout .le. rsq) ) then
               d (i,j,k) = d1
               v1(i,j,k) = v11
               v2(i,j,k) = v21
               v3(i,j,k) = v31

               e (i,j,k) = e1


               massk(k) = massk(k) + d1 * dvl1a(i) * dvl2a(j) * dvl3a(k)
             endif
             if ( (rin .lt. rsq) .and. (rout .gt. rsq) ) then

               frac     = overlap ( 1, r, x10, x20, x30
     1                      , x1a(iin (i)), x2a(jin (j)), x3a(kin (k))
     2                      , x1a(iout(i)), x2a(jout(j)), x3a(kout(k)) )

               cofrac   = 1.0 - frac
               d(i,j,k) = d1 * frac + d0 * cofrac

               e(i,j,k) = e1 * frac + e0 * cofrac


               massk(k) = massk(k)
     1                  + d1 * frac * dvl1a(i) * dvl2a(j) * dvl3a(k)
             endif
70         continue
80       continue
90     continue
       mass =   sasum ( nx3z, massk(ks), 1 )


c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 B L A S T                 \\\\\\\\\\
c
c=======================================================================
c
c













































































c=======================================================================
c
c    \\\\\\\\\\        B E G I N   F U N C T I O N        //////////
c    //////////               O V E R L A P               \\\\\\\\\\
c
c=======================================================================
c

       real*8 function overlap ( ishp, rad, x0, y0, z0, xin, yin

     1                       , zin, xout, yout, zout )
c
c    dac:zeus3d.overlap <--------- overlap of region over Cartesian zone
c                                                            april, 1990
c
c    written by: David Clarke
c    modified 1:
c
c  PURPOSE:  Determines the fraction of a Cartesian zone that overlaps
c  the specified geometrical region (sphere or right cylinder).  This
c  is done by dividing the zone into 20**3 "subzones", and finding the
c  fraction of subzone centres lying inside the surface of the region.
c
c  INPUT VARIABLES:
c
c    ishp            =1 => sphere
c                    =2 => right cylinder
c    rad             radius of region
c    x0,y0,z0        coordinates of centre of curvature.
c    xin,yin,zin     coordinates of zone corner known to lie inside
c                    region.
c    xout,yout,zout  coordinates of zone corner diametrically opposed to
c                    zone corner known to lie inside region.
c
c  OUTPUT VARIABLES:
c
c  LOCAL VARIABLES:
c
c  EXTERNALS: [NONE]
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
c
       integer       i       , j       , k       , nx      , ny
     1             , nz      , ishp
c       real*8        overlap
       real*8        rad     , x0      , y0      , z0      , xin
     1             , yin     , zin     , xout    , yout    , zout
     2             , delx    , dely    , delz    , r       , fact
     3             , scount
c
       real*8        xsq     (  20), ysq     (  20), zsq     (  20)
     1             , count   (  20)
c
c-----------------------------------------------------------------------
c

c      Number of subzones in the x-direction is "nx", etc. for "ny" and
c  "nz".  Increment between subzones in x-direction is "delx", etc. for
c  "dely" and "delz".
c
       nx   = 20
       ny   = 20
       nz   = 20
       delx = ( xout - xin ) /  real( nx )
       dely = ( yout - yin ) /  real( ny )
       delz = ( zout - zin ) /  real( nz )
c
c      Set up subgrid inside zone.
c
       do 10 i=1,nx
         xsq  (i) = ( xin + ( 0.5 +  real(i-1) ) * delx - x0 )**2
         count(i) = 0.0
10     continue
       do 20 j=1,ny
         ysq  (j) = ( yin + ( 0.5 +  real(j-1) ) * dely - y0 )**2
20     continue
       do 30 k=1,nz
         zsq  (k) = ( zin + ( 0.5 +  real(k-1) ) * delz - z0 )**2
30     continue
c
c      Count the number of subzones lying inside the surface of the
c  region which passes through the zone.
c
       fact  = 1.0
       if (ishp .eq. 2) fact = 0.0
       do 60 k=1,nz
         do 50 j=1,ny
           do 40 i=1,nx
             r = sqrt ( fact * xsq(i) + ysq(j) + zsq(k) )
             if (r .le. rad) count(i) = count(i) + 1.0
40         continue
50       continue
60     continue
       scount = 0.0
       do 70 i=1,nx
         scount = scount + count(i)
70     continue
       scount =   max ( one, scount )
c
c      Set the fraction of the zone which overlaps the region.
c
       overlap = scount /  real( nx * ny * nz )

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\          E N D   F U N C T I O N          //////////
c    //////////               O V E R L A P               \\\\\\\\\\
c
c=======================================================================
c
c
