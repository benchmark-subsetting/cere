












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine setup
c
c  PURPOSE: Sets up execution of a new run by initializing all variables
c  according to the flags and values in the input deck.  Calls blast
c  a CPP macro) to initialize field variables for the particular
c  problem to be studied, otherwise field variables are set to "tiny"
c  (parameter defined to be smallest number possible on machine).  Note
c  user must define blast to be the appropriate subroutine name in the
c  file zeusmp.def or in the Make_zeusmp command line.
c
c  Order in which namelists are read has been changed from ZEUS-2D so 
c  that the boundary conditions are known before the MPI virtual 
c  topology is defined and before the grid is computed.  MPI needs to 
c  know if the grid is periodic, while the grid should be symmetric 
c  across reflecting boundaries but constant across flow in/out and 
c  periodic for periodic ones.
c
c  EXTERNALS:
c     ggen    -- initializes grid according to input deck
c     blast -- macroname which is defined to be the user supplied
c                subroutine name which initializes field variables for
c                the problem to be studied
c     bval*   -- boundary value routines
c     nudt    -- computes initial timestep
c     newgrid -- computes new grid position for moving grid
c     scopy
c
c  LOCALS:
c
c  LAST MODIFIED: JCH 3/13/97.
c-----------------------------------------------------------------------
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


      real*8    g,tgrav,ptmass,x1ptm,x2ptm,x3ptm,
     &        rt_fn_grv,rt_co_grv,wgt_grv

      integer mi_fn_grv,mi_co_grv,m_grv,nu_pr_grv,nu_po_grv,
     &        vv_grv,sl_fn_grv,sl_co_grv,smth_grv
CPS


      common /gravcomr/ g,tgrav,ptmass,x1ptm,x2ptm,x3ptm,
     &        rt_fn_grv,rt_co_grv,wgt_grv

      common /gravcomi/ mi_fn_grv,mi_co_grv,m_grv,nu_pr_grv,nu_po_grv,
     &        vv_grv,sl_fn_grv,sl_co_grv,smth_grv
CPS




      real*8    c,epsme,demax,dermax,dtotmax,radth,epsrad,
     .        dtimrdi,ernom,ennom,epsmaxd,epsmaxc,dtimpmxi2
      integer ifld,nmeiter,maxrad,ks0rad,iorad,cnvcrit
      common /radr/ c,epsme,demax,dermax,dtotmax,radth,epsrad
     .             ,ernom,ennom,epsmaxd,epsmaxc
      common /radi/ ifld,nmeiter,maxrad,ks0rad,iorad,cnvcrit
      common /impdt/ dtimrdi,dtimpmxi2
c#if defined RAD || defined EXP_DIFF
      real*8 f11 (in,jn,kn), f22 (in,jn,kn), f12 (in,jn,kn), 
     .     dr1 (in,jn,kn), dr2 (in,jn,kn), dr3 (in,jn,kn)
      real*8 dvl11(in,jn,kn), dvl22(in,jn,kn), dvl12(in,jn,kn),
     .     dvl21(in,jn,kn), divvl(in,jn,kn), dvl33(in,jn,kn),
     .     en  (in,jn,kn), ern (in,jn,kn), de  (in,jn,kn), 
     .     der (in,jn,kn),
     .     pn  (in,jn,kn),
     .     dpde(in,jn,kn),
     .     fr1 (in,jn,kn), fr2 (in,jn,kn),
     .     fr3 (in,jn,kn), p   (in,jn,kn)
      common /radr/
     .     f11 , f22, f12, dr1, dr2, dvl11, dvl22, dvl12, 
     .     dvl21, divvl,
     .     dvl33, en , ern, de , der, pn  , 
     .     dpde, fr1, fr2, fr3, dr3, p
c#endif 
      integer mi_fn_rad,mi_co_rad,m_rad,nu_pr_rad,nu_po_rad,
     &        vv_rad,sl_fn_rad,sl_co_rad,smth_rad
      real*8 rt_fn_rad,rt_co_rad,wgt_rad
      common /radcomr/ rt_fn_rad,rt_co_rad,wgt_rad

      common /radcomi/ mi_fn_rad,mi_co_rad,m_rad,nu_pr_rad,nu_po_rad,
     &        vv_rad,sl_fn_rad,sl_co_rad,smth_rad

      real*8 bb    (in,jn,kn), kap   (in,jn,kn),
     .     sig   (in,jn,kn), bbn   (in,jn,kn),
     .     kapn  (in,jn,kn), dbbde (in,jn,kn),
     .     dkapde(in,jn,kn), dkapdt(in,jn,kn),
     .     kapr  (in,jn,kn), eta   (in,jn,kn),
     .     detade(in,jn,kn), detadt(in,jn,kn)
c
      real*8 kpfrac
      common /planck/ kpfrac
c
      real*8 min_coef, max_coef
      common /kplim/ min_coef, max_coef
c

c
      common /rado/
     .     bb  , kap  , sig   , bbn,
     .     kapn, dbbde, dkapde, dkapdt,
     .     kapr, eta  , detade, detadt


       real*8 epsmin,epsmax,rmfp0,xnu,powr,rho0,t_0,rcourno
     1     ,rmfp0s,xnus,powrs,rho0s,t_0s,dtssi2
       common / radexpr / epsmin,epsmax,rmfp0,xnu,powr,rho0,t_0,rcourno
     1                  , rmfp0s,xnus,powrs,rho0s,t_0s,
     .                    dtssi2
       integer imx, jmx, kmx
       common / radexpi / imx, jmx, kmx


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

      real*8 clight,me,mp,mh,boltz,h,guniv
      real*8 msol,lsol
      real*8 cmau,cmpc,cmkpc,cmkm,everg
      real*8 rad_con,mmw,gasc,sbc
c   physical constants (cgs)
      data clight  /2.99792e10     /,
     &     me      /9.10953e-28    /,
     &     mp      /1.67265e-24    /,
     &     mh      /1.66053e-24    /,
     &     boltz   /1.38066e-16    /,
     &     rad_con /7.56000e-15    /,
     &     h       /6.62618e-27    /,
     &     guniv   /6.6720e-8      /,
     &     mmw     /1.0            /,
     &     gasc    /8.625e7        /,
     &     sbc     /1.8044e-5      /
c   astronomical constants (cgs)
      data msol    /1.989e33       /,
     &     lsol    /3.826e33       /
c   conversion factors
      data cmau    /1.49597e13     /,
     &     cmpc    /3.084e18       /,
     &     cmkpc   /3.084e21       /,
     &     cmkm    /1.0e5          /,
     &     everg   /1.60219e-12    /


C      #include "mgmpi.cpp"

C     -----------------------------------------------------------------
C     Boundary condition labels
C     -----------------------------------------------------------------

      INTEGER MM_DIRICHLET, MM_NEUMANN, MM_PERIODIC
      PARAMETER (MM_DIRICHLET = 1)
      PARAMETER (MM_NEUMANN   = 2)
      PARAMETER (MM_PERIODIC  = 3)

C     -----------------------------------------------------------------
C     Coordinate system labels
C     -----------------------------------------------------------------

      INTEGER MM_CARTESIAN, MM_CYLINDRICAL, MM_SPHERICAL
      PARAMETER (MM_CARTESIAN   = 1)
      PARAMETER (MM_CYLINDRICAL = 2)
      PARAMETER (MM_SPHERICAL   = 3)

C     -----------------------------------------------------------------
C     Smoother labels
C     -----------------------------------------------------------------

      INTEGER MM_POINT_JACOBI,MM_LOCAL_LINE_JACOBI,MM_PLANE_JACOBI
      INTEGER MM_LOCAL_POINT_RBGS
      PARAMETER (MM_POINT_JACOBI  = 0)
      PARAMETER (MM_LOCAL_LINE_JACOBI   = 1)
      PARAMETER (MM_PLANE_JACOBI  = 2)
      PARAMETER (MM_LOCAL_POINT_RBGS  = 3)

C     -----------------------------------------------------------------
C     Solver labels
C     -----------------------------------------------------------------

      INTEGER MM_MG_SOLVER, MM_CG_SOLVER, MM_BICG_SOLVER
      INTEGER MM_BICGMG_SOLVER, MM_FMG_SOLVER, MM_CGMG_SOLVER

      PARAMETER (MM_CG_SOLVER   = 1)
      PARAMETER (MM_MG_SOLVER   = 2)
      PARAMETER (MM_BICG_SOLVER = 3)
      PARAMETER (MM_BICGMG_SOLVER = 4)
      PARAMETER (MM_FMG_SOLVER = 5)
      PARAMETER (MM_CGMG_SOLVER = 6)

C     Solver for coarsest level
C     -------------------------

      INTEGER MM_CG_COARSE_SOLVER, MM_JACOBI_COARSE_SOLVER
      PARAMETER (MM_CG_COARSE_SOLVER     = 1)
      PARAMETER (MM_JACOBI_COARSE_SOLVER = 2)

C     Grid type
C     ---------

      INTEGER MM_IS_UNIFORM,MM_IS_NONUNIFORM
      PARAMETER (MM_IS_UNIFORM = 1)
      PARAMETER (MM_IS_NONUNIFORM = 2)

C     -----------------------------------------------------------------
C     mmTest* problem values
C     -----------------------------------------------------------------

      INTEGER POISSON, SYMMETRIC, NONSYMMETRIC
      PARAMETER (POISSON=1, SYMMETRIC=2, NONSYMMETRIC=3)

c
c#ifdef MPI_USED
c      integer npassit
c      parameter( npassit = mreq * ijkn * ijkn * 8)
c      real*8    passit(npassit)
c#endif 


c
      integer  i,j,k
      integer  iord,istp
      real*8     dtrat
c
      external empty,ggen
      external bvald,bvale,bvalv1,bvalv2,bvalv3

      external nudt,blast
c

c
      namelist /pcon/ nlim,tlim,cpulim,tsave,mbatch
      namelist /hycon/
     &  qcon,qlin,courno,dtrat,iord,istp
     & ,iordd,iorde,iords1,iords2,iords3,iordb1,iordb2,iordb3,iorder
     & ,istpd,istpe,istps1,istps2,istps3,istpb1,istpb2,istpb3,istper
     & ,dfloor,efloor,v1floor,v2floor,v3floor,b1floor,b2floor,b3floor
     & ,emf1floor,emf2floor,emf3floor,erfloor,gpfloor
c
c The BC namelists are now comprised of sets of 1-D arrays.  For more
c complicated BCs, boundary value arrays are to be initialized in
c the Problem Generator in the arrays diib(j,k,2), etc.
c
c Old:
c      namelist /iib/     niib, liib, ...
c     &,  diib,  eiib,   v1iib,   v2iib,   v3iib
c     &,                 b1iib,   b2iib,   b3iib
c     &,               emf1iib, emf2iib, emf3iib
c     &, eriib, gpiib
c ..and so on for the other 5 boundaries.
c New:
      namelist /iib/     niis, fiis
      namelist /oib/     nois, fois
      namelist /ijb/     nijs, fijs
      namelist /ojb/     nojs, fojs
      namelist /ikb/     niks, fiks
      namelist /okb/     noks, foks
c
c New namelist mpitop to read in the number of tiles in each direction
c after boundary conditions are set but before the grid is determined.
c
      namelist /mpitop/ ntiles, periodic
      namelist /grvcon/ g,tgrav,ptmass,x1ptm,x2ptm,x3ptm,
     &                  rt_fn_grv,rt_co_grv,wgt_grv,
     &                  mi_fn_grv,mi_co_grv,m_grv,nu_pr_grv,nu_po_grv,
     &                  vv_grv,sl_fn_grv,sl_co_grv,smth_grv

      namelist /radcon/ c,ifld,epsme,demax,dermax,dtotmax,nmeiter,radth
     &, epsrad,maxrad,ks0rad,iorad,cnvcrit,ernom,ennom,epsmaxd,epsmaxc
     &,                 rt_fn_rad,rt_co_rad,wgt_rad,
     &                  mi_fn_rad,mi_co_rad,m_rad,nu_pr_rad,nu_po_rad,
     &                  vv_rad,sl_fn_rad,sl_co_rad,smth_rad,
     .                  kpfrac
      namelist /radexp/ epsmin,epsmax,rmfp0,xnu,powr,rho0,t_0,rcourno
      namelist /eqos/ gamma, ciso
      namelist /gcon/ x1fac,x2fac,x3fac,ia,ja,ka,igcon
c=======================================================================
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////////
c
c------------------------  blast CONTROL  ---------------------------
c
c   nlim   = cycles to run                               (default    1M)
c   tlim   = physical (problem) time to stop calculation (default  0.0 )
c cpulim   = CPU time in seconds to stop the calculation (default  3.6M)
c  tsave   = CPU time to reserve for terminating the run (default 30.0s)
c mbatch   = 0 interactive mode                          (default     0)
c          = 1 batch mode (does not scan for keyboard input)
c
      nlim   = 1 000 000
      tlim   = 0.0
      cpulim = 3 600 000.0
      tsave  = 30.0
      mbatch = 0
c
      nred   = 0
      if (myid_w .eq. 0) then
        read (1,pcon)
cpu2006        write(2,pcon)
      endif
c
c------------------------  HYDRO CONTROL  ------------------------------
c
c  qcon   = quadratic artificial viscosity (q) constant (default 2.0)
c  qlin   = linear    artificial viscosity (q) constant (default 0.0)
c  courno = courant number                              (default 0.5)
c  dtrat  = ratio of initial dt to dtmin (used to compute dtmin below)
c  iord   = default order of advection scheme for all variables
c  istp   = default steepening flag for all variables
c  iord** = order of advection scheme to be used for variable **
c  iostp**= steepening flag for 3rd order advection.  When istp**=1,
c           use the discontinuity detection to steepen shocks during
c           interpolation for variable ** in X1INT,X1INTFC,X2INT,X2INTFC
c  **floor = smallest value desired for variable ** on grid
c            Can also be used to set a default value for initialization.
c            Note that no attempt is made to ensure that actual values 
c            stay above the floor values.  
c
      if (myid_w .eq. 0) then
        qcon   = 2.0
        qlin   = 0.0
        courno = 0.5
        dtrat  = 1.0e-3
        iord   = 2
        iordd  = 0
        iorde  = 0
        iords1 = 0
        iords2 = 0
        iords3 = 0
        iordb1 = 0
        iordb2 = 0
        iordb3 = 0
        iorder = 0
        istp   = 0
        istpd  = 2
        istpe  = 2
        istps1 = 2
        istps2 = 2
        istps3 = 2
        istpb1 = 2
        istpb2 = 2
        istpb3 = 2
        istper = 2
        d floor = tiny
        e floor = tiny
        v1floor = 0.0
        v2floor = 0.0
        v3floor = 0.0
        b1floor = 0.0
        b2floor = 0.0
        b3floor = 0.0
        emf1floor= 0.0
        emf2floor= 0.0
        emf3floor= 0.0
        erfloor = tiny
        gpfloor = 0.0
c
        read (1,hycon)
cpu2006        write(2,hycon)
c
c Set flags to default values unless they were set in the input deck.
c
        if(iordd  .eq. 0) iordd  = iord
        if(iorde  .eq. 0) iorde  = iord
        if(iords1 .eq. 0) iords1 = iord
        if(iords2 .eq. 0) iords2 = iord
        if(iords3 .eq. 0) iords3 = iord
        if(iordb1 .eq. 0) iordb1 = iord
        if(iordb2 .eq. 0) iordb2 = iord
        if(iordb3 .eq. 0) iordb3 = iord
        if(iorder .eq. 0) iorder = iord
c
        if(istpd  .eq. 2) istpd  = istp
        if(istpe  .eq. 2) istpe  = istp
        if(istps1 .eq. 2) istps1 = istp
        if(istps2 .eq. 2) istps2 = istp
        if(istps3 .eq. 2) istps3 = istp
        if(istps1 .eq. 2) istps1 = istp
        if(istps2 .eq. 2) istps2 = istp
        if(istps3 .eq. 2) istps3 = istp
        if(istper .eq. 2) istper = istp
c
c copy input flags to a buffer for later use and broadcasting.
c
         buf_in( 1) = qcon
         buf_in( 2) = qlin
         buf_in( 3) = courno
         buf_in( 4) = dtrat
         buf_in( 5) = dfloor
         buf_in( 6) = efloor
         buf_in( 7) = v1floor
         buf_in( 8) = v2floor
         buf_in( 9) = v3floor
         buf_in(10) = b1floor
         buf_in(11) = b2floor
         buf_in(12) = b3floor
         buf_in(13) = emf1floor
         buf_in(14) = emf2floor
         buf_in(15) = emf3floor
         buf_in(16) = erfloor
         buf_in(17) = gpfloor
         buf_in(18) = tlim
         buf_in(19) = cpulim
         buf_in(20) = tsave
c
        ibuf_in( 1) = iordd 
        ibuf_in( 2) = iorde 
        ibuf_in( 3) = iords1
        ibuf_in( 4) = iords2
        ibuf_in( 5) = iords3
        ibuf_in( 6) = iordb1
        ibuf_in( 7) = iordb2
        ibuf_in( 8) = iordb3
        ibuf_in( 9) = iorder
        ibuf_in(10) = istpd 
        ibuf_in(11) = istpe 
        ibuf_in(12) = istps1
        ibuf_in(13) = istps2
        ibuf_in(14) = istps3
        ibuf_in(15) = istpb1
        ibuf_in(16) = istpb2
        ibuf_in(17) = istpb3
        ibuf_in(18) = istper
        ibuf_in(19) = nlim
        ibuf_in(20) = mbatch
      endif

c
c------------------------  BOUNDARY CONTROL ----------------------------
c
c  The following points describecoords(1), how boundaries are handled:
c
c  1)  Any of 6 mhd boundary conditions may be specified independently
c  at every zone on the physical problem boundary.  The boundary type is
c  specified by nflo, where
c
c      nflo = 0  =>  interior boundary (get data from neighboring tile)
c           = 1  =>  reflecting (v(normal) = b(normal) = 0)
c           =-1  =>  reflecting (: same as 1; ZRP: same as 1 with
c                    inversion of 3-components at ijb; RTP: same as 1
c                    with inversion of 2- and 3-components at iib and
c                    inversion of 3-components at ijb and ojb.)
c           = 2  =>  flow out
c           = 3  =>  flow in
c           = 4  =>  periodic
c           = 5  =>  reflecting (v(normal) = 0, b(tangential) = 0)
c
c  Note that in ZRP and RTP, some boundary conditions are implied by
c  the choice of limits.  e.g., if 0 .le. x3a .le. 2*pi in either ZRP
c  or RTP, then periodic boundary conditions should be imposed.
c  Set "niib" to -1 (reflecting with inversion of 2- and 3-components) 
c  if the inner i boundary is at the   origin (RTP).
c  Set "nijb" to -1 (reflecting with inversion of 3-components) if
c     the inner j boundary is on the "Z" axis (ZRP or RTP).
c  Set "nojb" to -1 (reflecting with inversion of 3-components) if
c     the outer j boundary is on the "Z" axis (RTP).
c
c  Since the grid is staggered, the boundary conditions apply over
c  slightly different regions, depending on the centering of the 
c  variable c  and the type of boundary condition used.  Thus, "niib" 
c  is applied to zone or 1-face centered quantities (d, v1, e, gp, b1),
c  "niib2" is applied to 2-face centred quantities (v2, b2), 
c  "niib3" is applied to 3-face centred quantities (v3, b3), and 
c  "niib23" is appled to corner centred quantities (emf1, emf2, emf3).
c  Note that the secondary boundary integer flags are determined from 
c  "niib" automatically.
c
c  Since constant values are often used and are easy to input via 
c  namelists, a single scalar variable is read in for nflo on each 
c  boundary.  These are read in as:
c
c      niis(1),nois(1),nijs(1),nojs(1),niks(1),noks(1)
c
c  For more complicated boundary conditions, values of nflo at each 
c  boundary zone are stored in the 2-D arrays:
c
c      niib,noib,nijb,nojb,nikb,nokb
c
c  These arrays are automatically set to the constant input values.  For
c  more complicated problems, these arrays may be altered in the Problem
c  Generator.
c
c  2) Since the radiation boundary types may differ from the fluid 
c  boundary types, we may specify the former independently of the 
c  latter.  Thus, the radiation boundary types are specified by lflo,
c  where
c
c      lflo = 0  =>  interior boundary (get data from neighboring tile)
c           = 1  =>  reflecting
c           = 2  =>  flow out
c           = 3  =>  flow in
c           = 4  =>  periodic
c
c  Constant values for lflo are read in as:
c
c      niis(2),nois(2),nijs(2),nojs(2),niks(2),noks(2)
c
c  while the 2-D arrays are liib,loib,lijb,lojb,likb,lokb.
c
c  3) Boundary conditions on the gravitational potential are
c     specified by igr, where
c
c       igr = 0  =>  interior boundary (get data from neighboring tile)
c           = 1  =>  reflecting (dgp/d(normal) = 0 "von Neumann")
c           = 2  =>  use multipole expansion (Dirichlet)
c           = 3  =>  gp specified (Dirichlet)
c           = 4  =>  periodic
c
c     The flags are defined by analogy with the hydro nflo flag.  This
c     is quite different from ZEUS-2D.  The flags igr are read in as:
c
c      niis(3),nois(3),nijs(3),nojs(3),niks(3),noks(3)
c
c     No 2-D integer arrays for gravity BCs, I guess.
c
c  4) For flow-in boundaries, boundary values of d,e,v1,v2,[v3],...
c  must be input.  Since constant values are often used and are easy to 
c  input via namelists, a single scalar variable is read in for each 
c  field variable on each boundary, for example, the quantities
c
c    fiis(1),fois(1),fijs(1),fojs(1),fiks(1),foks(1) 
c
c  give the constant boundary values for d.  There is a set of f's
c  for e, v, b, er, and gp.
c
c  For more complicated inflow boundary conditions, arrays are used to 
c  store specified values at each boundary zone for each function; for 
c  example:
c
c    diib(j,k,2) is inner i boundary density for sweep j,k at ism2
c    diib(j,k,1) is inner i boundary density for sweep j,k at ism1
c    doib(j,k,1) is outer i boundary density for sweep j,k at iep1
c    doib(j,k,2) is outer i boundary density for sweep j,k at iep2
c
c    dijb(k,i,2) is inner j boundary density for sweep k,i at jsm2
c    dijb(k,i,1) is inner j boundary density for sweep k,i at jsm1
c    dojb(k,i,1) is outer j boundary density for sweep k,i at jep1
c    dojb(k,i,2) is outer j boundary density for sweep k,i at jep2
c
c    dikb(i,j,2) is inner k boundary density for sweep i,j at ksm2
c    dikb(i,j,1) is inner k boundary density for sweep i,j at ksm1
c    dokb(i,j,1) is outer k boundary density for sweep i,j at kep1
c    dokb(i,j,2) is outer k boundary density for sweep i,j at kep2
c
c    These arrays are initialized automatically to constant values;
c    the user's Problem Generator may alter these arrays, e.g. to
c    specify an inlet for a jet or for certain advection tests.
c
c 5) For corner zones [(ii-1,ji-1),(io+1,ji-1),etc] there is a
c    "pecking order" of precedence which attempts to pick the BC
c    that provides the most stable solution.
c    THE USER MAY WANT TO OVERIDE THIS CHOICE in the Problem Generator.
c
c  Set defaults and read in boundary values.  buf_in still contains
c  the "floor" or default values for each field variable.
c
      if (myid_w .eq. 0) then
        do 10 i=1,3
          niis(i) = 0
          nijs(i) = 0
          niks(i) = 0
          nois(i) = 0
          nojs(i) = 0
          noks(i) = 0
10      continue
        do 20 i=1,nbvar
          fiis(i) = buf_in(i+4)
          fois(i) = buf_in(i+4)
          fijs(i) = buf_in(i+4)
          fojs(i) = buf_in(i+4)
          fiks(i) = buf_in(i+4)
          foks(i) = buf_in(i+4)
20      continue
c
        read (1,iib)
cpu2006        write(2,iib)
c
        read (1,oib)
cpu2006        write(2,oib)
c
        read (1,ijb)
cpu2006        write(2,ijb)
c
        read (1,ojb)
cpu2006        write(2,ojb)
c
        read (1,ikb)
cpu2006        write(2,ikb)
c
        read (1,okb)
cpu2006        write(2,okb)
c
      endif
c

CJH -- MPI topology move to MSTART
cc------------------------  MPI TOPOLOGY  -------------------------------
cc
cc  ntiles:   elements equal the number of tiles in each direction.
cc  periodic: elements are true if grid is periodic in that direction --
cc            we check hydro BC flags for defaults, but can override.
cc
c      if (myid_w .eq. 0) then
c        ntiles(1) = 1
c        ntiles(2) = 1
c        ntiles(3) = 1
c        periodic(1) = niis(1).eq.4
c        periodic(2) = nijs(1).eq.4
c        periodic(3) = niks(1).eq.4
c        read (1,mpitop)
c        write(2,mpitop)
c      endif
cc
c#ifdef MPI_USED
cc
cc Tell the others what the master has read.
cc
c      call MPI_BCAST(ntiles  , 3,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
c      call MPI_BCAST(periodic, 3,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
cc
cc Quit if the number of processors indicated on the command line
cc differs from the number of tiles specified in the input file.
cc
c      if (nprocs_w .ne. ntiles(1)*ntiles(2)*ntiles(3)) then
c        if (myid_w .eq. 0)
c     1  write(*,"(/'SETUP: The number of threads ',i3,' does not match',
c     2            /'SETUP: the number input via ntiles ',i3,' in',
c     3            /'SETUP: input file zmp_inp; aborting the run...')")
c     4    nprocs_w, ntiles(1)*ntiles(2)*ntiles(3)
c        call MPI_FINALIZE( ierr )
c        stop
c      endif
cc
cc Create a virtual Cartesian topology for the domain decomposition.
cc
c      call MPI_CART_CREATE( MPI_COMM_WORLD, 3, ntiles, periodic
c     &                    , reorder, comm3d, ierr )
c      call MPI_COMM_RANK( comm3d, myid,     ierr )
c      call MPI_COMM_SIZE( comm3d, nprocs,   ierr )
cc
cc Find the ranks of my neighbors; find my virtual Cartesian coords.
cc
c      call MPI_CART_SHIFT( comm3d, 0, 1, n1m, n1p, ierr )
c      call MPI_CART_SHIFT( comm3d, 1, 1, n2m, n2p, ierr )
c      call MPI_CART_SHIFT( comm3d, 2, 1, n3m, n3p, ierr )
cc
c      call MPI_CART_COORDS( comm3d, myid, 3, coords, ierr )
c#ifdef DEBUG
cc
cc Identify this processor.
cc
c#ifdef DEC
c      pname(1:MPI_MAX_PROCESSOR_NAME) = ' '
c#else 
c      pname(1:MPI_MAX_PROCESSOR_NAME) = ""
c#endif 
c      call MPI_GET_PROCESSOR_NAME( pname, pnamel, ierr )
cc
cc Send processor names to the root process.
cc
c      call MPI_GATHER( pname,MPI_MAX_PROCESSOR_NAME
c     &,  MPI_CHARACTER
c     &,               pnames,MPI_MAX_PROCESSOR_NAME
c     &,  MPI_CHARACTER
c     &,  0, comm3d, ierr)
c      if (myid .eq. 0) then
c        do 25 k=0,min(nprocs-1,63)
c          kcoords(3) = k / ( ntiles(1) * ntiles(2) )
c          i          = k - kcoords(3) * ( ntiles(1) * ntiles(2) )
c          kcoords(2) = i / ntiles(1)
c          kcoords(1) = i - kcoords(2) * ntiles(1)
c          call MPI_CART_RANK( comm3d, kcoords, i, ierr )
c          write(*,"(' Process at (',i2.2,',',i2.2,',',i2.2,') is ',
c     1      'named ',a)") kcoords(1),kcoords(2),kcoords(3),
c     2      pnames(i)(1:MPI_MAX_PROCESSOR_NAME)
c25      continue
c      endif
c#endif 
cc
cc      call MPI_BUFFER_ATTACH ( passit, npassit, ierr )
c#endif 
cc
c-------------------------  SET BOUNDARY TYPES  ------------------------
c


c
      if (coords(1) .gt. 0) then
        niis(1) = 0
        niis(2) = 0
        niis(3) = 0
      endif
      if (coords(2) .gt. 0) then
        nijs(1) = 0
        nijs(2) = 0
        nijs(3) = 0
      endif
      if (coords(3) .gt. 0) then
        niks(1) = 0
        niks(2) = 0
        niks(3) = 0
      endif

      if (coords(1) .lt. ntiles(1) - 1) then
        nois(1) = 0
        nois(2) = 0
        nois(3) = 0
      endif
      if (coords(2) .lt. ntiles(2) - 1) then
        nojs(1) = 0
        nojs(2) = 0
        nojs(3) = 0
      endif
      if (coords(3) .lt. ntiles(3) - 1) then
        noks(1) = 0
        noks(2) = 0
        noks(3) = 0
      endif

c
c-------------------------  INITIALIZE GRID  ---------------------------
c
c  Routine ggen reads ggen1, ggen2, ggen3; computes the grid for each 
c  tile.
c


      call ggen


c
c------------------------  GRAVITY CONTROL  ----------------------------
c
c  Self-gravity can be included
c  by solving the Poisson equation in the GRAVITY module.
c  Point mass potentials are included directly in the momentum eqn
c  by using a non-zero value for the variable ptmass.  Point mass
c  potentials do not require defining GRAV, do not call the GRAVITY
c  module, and are not included in the array phi but are explicitly
c  added to the momentum eqn terms in FORCES.
c     g              gravitational constant
c     tgrav          time when gravitation is switched on
c     ptmass         mass of fixed point mass 
c     x1ptm          x1 coordinate of the point mass
c     x2ptm
c     x3ptm
c
c  Boundary flags more general than the ones below are input in the
c  BC namelists described above.  Keep these for now.  Strange, though.
c
c     igrijb = ijb flag (0 for     symmetric (Dirichlet) boundary      )
c                       (1 for non-symmetric             boundary whose
c                        value is calculated using multipole expansion )
c     igrojb = ojb flag ("  "      "          "         "              )
c
c  These are for the multigrid solver.  R: real, I: integer
c
c     wgt_grv    R   Weight factor for smoother (not an array)
c     rt_fn_grv  R   Error tolerance for fine grid  solver (CG, MG, etc.)
c     rt_co_grv  R   Error tolerance for coarse grid  solver
c     m_grv      I   Number of multigrid levels to use.
c     nu_pr_grv  I   Number of pre  smoothings to do (not an array)
c     nu_po_grv  I   Number of post smoothings to do (not an array)
c     sl_fn_grv  I   Type of fine-grid solver --1 CG, 2 MG, (3 CGMG)
c     sl_co_grv  I   Type of coarse grid solver --1 smoother, 2 CG
c     mi_fn_grv  I   Max iterations allowed for multigrid
c     mi_co_grv  I   Max iterations allowed for coarse grid solver
c     vv_grv     I   Cycle type: 0 standard V-cycle, 1 variable V-cycle
c     smth_grv   I   Smoother type: 0 point Jacobi, 1 local line Jacobi
c     gsup       I   Cycle number to calculate gravitational potential
      if (myid_w .eq. 0) then
        g         = 6.667e-8
        tgrav     = 0.0
        ptmass    = 0.0
        x1ptm     = x1a(is)
        x2ptm     = x2a(js)
        x3ptm     = x3a(ks)

        wgt_grv   = 0.9
        rt_fn_grv = 1.0E-7
        rt_co_grv = 1.0E-7
        m_grv     = 4
        nu_pr_grv = 3
        nu_po_grv = 3
        sl_fn_grv = 2
        sl_co_grv = 1
        mi_fn_grv = 10
        mi_co_grv = 100
        vv_grv    = 1
        smth_grv  = 0

c
        read (1,grvcon)
cpu2006        write(2,grvcon)
      endif
c
c------------------------  RADIATION CONTROL  --------------------------
c
c   c      = speed of light
c   ifld   = choice of flux limiters
c   epsme  = error criterion for NR convergence (WH80s uses 1.0e-5)
c demax,dermax = max relative change allowed in e,er in a single NR iter
c   dtotmax= max total relative change allowed over all NR iterations
c   nmeiter= max number of NR iterations allowed (WH80s uses 20)
c   radth  = "theta" time centering parameter
c     epsrad = error limit              for ICCGAF
c     maxrad = maximum iteration count  for ICCGAF
c     ks0rad = level of cyclic reduction in ICCGAF
c   iorad = 1 to dump convergence data for NR iterations
c
      if (myid_w .eq. 0) then
        c      = clight
        ifld   = 1
        epsme  = 1.0e-08
        de  max= 0.2
        der max= 0.2
        dtotmax= 0.2
        nmeiter= 20
        radth  = 0.55
        epsrad = 1.0e-12
        maxrad = 200
        ks0rad = 99
        iorad = 0
c
        wgt_rad   = 0.9
        rt_fn_rad = 1.0E-7
        rt_co_rad = 1.0E-7
        m_rad     = 4
        nu_pr_rad = 3
        nu_po_rad = 3
        sl_fn_rad = 2
        sl_co_rad = 1
        mi_fn_rad = 10
        mi_co_rad = 100
        vv_rad    = 1
c
        read (1,radcon)
cpu2006        write(2,radcon)
      endif
c
c------------------------  RAD_PF CONTROL  -----------------------------
c
c   epsmin   If max(abs(delta T))/Tmax < epsmin, increase time step.
c   epsmax   If max(abs(delta T))/Tmax > epsmax, decrease time step.
c            1/epsmax is max allowed time step.
c   rmfp0    Rosseland mean free path normalization constant.
c   xnu      Rosseland mean free path proportional to (rho0/d)**xnu
c   rho0     
c   powr     Rosseland mean free path proportional to (er/t_0)**(powr/4)
c   t_0
c   rcourno  Radiation diffusion Courant number (will be multiplied by
c            the hydro Courant number in nudt).
c
      if (myid_w .eq. 0) then
        epsmin  = 0.001
        epsmax  = 0.01
        rmfp0   = 1.0
        xnu     = 1.9
        powr    = 5.6/4.0
        rho0    = 1.0
        t_0     = 1.0
        rcourno = 1.0
c
        read (1,radexp)
cpu2006        write(2,radexp)
      endif
c
c------------------------  EQUATION OF STATE  --------------------------
c
c      gamma = ratio of specific heats
c      ciso  = isothermal sound speed
c
      if (myid_w .eq. 0) then
        gamma = 0.0
        ciso  = 0.0
c
        read (1,eqos)
cpu2006        write(2,eqos)
      endif

      gamm1 = gamma - 1.0
c

c-----------------------  SET CONSTANT VALUES IN BOUNDARY ARRAYS -------
c
c  For the loops below, set the boundary flags to zero for internal
c  boundaries.  Copy the constant values into the 2-D arrays.
c
c-----------------------  Constant values for IIB  ---------------------
c
       do 40 k=ks-2,ke+3
         do 30 j=js-2,je+3
           if (coords(1) .eq. 0) then
             niib    (j,k) = niis(1)

             niib2   (j,k) = niis(1)
             niib3   (j,k) = niis(1)
             niib23  (j,k) = niis(1)
           else
             niib    (j,k) = 0

             niib2   (j,k) = 0
             niib3   (j,k) = 0
             niib23  (j,k) = 0
           endif
           diib    (j,k,1) = fiis(1)
           diib    (j,k,2) = huge
           diib    (j,k,3) = huge

           eiib    (j,k,1) = fiis(2)
           eiib    (j,k,2) = huge

           v1iib   (j,k,1) = fiis(3)
           v1iib   (j,k,2) = huge
           v2iib   (j,k,1) = fiis(4)
           v2iib   (j,k,2) = huge
           v3iib   (j,k,1) = fiis(5)
           v3iib   (j,k,2) = huge

           b1iib   (j,k,1) = fiis(6)
           b1iib   (j,k,2) = huge
           b2iib   (j,k,1) = fiis(7)
           b2iib   (j,k,2) = huge
           b3iib   (j,k,1) = fiis(8)
           b3iib   (j,k,2) = huge
           emf1iib (j,k,1) = fiis(9)
           emf1iib (j,k,2) = huge
           emf1iib (j,k,3) = huge
           emf2iib (j,k,1) = fiis(10)
           emf2iib (j,k,2) = huge
           emf2iib (j,k,3) = huge
           emf3iib (j,k,1) = fiis(11)
           emf3iib (j,k,2) = huge
           emf3iib (j,k,3) = huge



30       continue
40     continue
c
c-----------------------  Constant values for OIB  ---------------------
c
       do 60 k=ks-2,ke+3
         do 50 j=js-2,je+3
           if (coords(1) .eq. ntiles(1) - 1) then
             noib    (j,k) = nois(1)

             noib2   (j,k) = nois(1)
             noib3   (j,k) = nois(1)
             noib23  (j,k) = nois(1)
           else
             noib    (j,k) = 0

             noib2   (j,k) = 0
             noib3   (j,k) = 0
             noib23  (j,k) = 0
           endif
           doib    (j,k,1) = fois(1)
           doib    (j,k,2) = huge
           doib    (j,k,3) = huge

           eoib    (j,k,1) = fois(2)
           eoib    (j,k,2) = huge

           v1oib   (j,k,1) = fois(3)
           v1oib   (j,k,2) = huge
           v2oib   (j,k,1) = fois(4)
           v2oib   (j,k,2) = huge
           v3oib   (j,k,1) = fois(5)
           v3oib   (j,k,2) = huge

           b1oib   (j,k,1) = fois(6)
           b1oib   (j,k,2) = huge
           b2oib   (j,k,1) = fois(7)
           b2oib   (j,k,2) = huge
           b3oib   (j,k,1) = fois(8)
           b3oib   (j,k,2) = huge
           emf1oib (j,k,1) = fois(9)
           emf1oib (j,k,2) = huge
           emf1oib (j,k,3) = huge
           emf2oib (j,k,1) = fois(10)
           emf2oib (j,k,2) = huge
           emf2oib (j,k,3) = huge
           emf3oib (j,k,1) = fois(11)
           emf3oib (j,k,2) = huge
           emf3oib (j,k,3) = huge



50       continue
60     continue
c
c-----------------------  Constant values for IJB  ---------------------
c
       do 80 k=ks-2,ke+3
         do 70 i=is-2,ie+3
           if (coords(2) .eq. 0) then
             nijb    (i,k) = nijs(1)

             nijb3   (i,k) = nijs(1)
             nijb1   (i,k) = nijs(1)
             nijb31  (i,k) = nijs(1)
           else
             nijb    (i,k) = 0

             nijb3   (i,k) = 0
             nijb1   (i,k) = 0
             nijb31  (i,k) = 0
           endif
           dijb    (i,k,1) = fijs(1)
           dijb    (i,k,2) = huge
           dijb    (i,k,3) = huge

           eijb    (i,k,1) = fijs(2)
           eijb    (i,k,2) = huge

           v1ijb   (i,k,1) = fijs(3)
           v1ijb   (i,k,2) = huge
           v2ijb   (i,k,1) = fijs(4)
           v2ijb   (i,k,2) = huge
           v3ijb   (i,k,1) = fijs(5)
           v3ijb   (i,k,2) = huge

           b1ijb   (i,k,1) = fijs(6)
           b1ijb   (i,k,2) = huge
           b2ijb   (i,k,1) = fijs(7)
           b2ijb   (i,k,2) = huge
           b3ijb   (i,k,1) = fijs(8)
           b3ijb   (i,k,2) = huge
           emf1ijb (i,k,1) = fijs(9)
           emf1ijb (i,k,2) = huge
           emf1ijb (i,k,3) = huge
           emf2ijb (i,k,1) = fijs(10)
           emf2ijb (i,k,2) = huge
           emf2ijb (i,k,3) = huge
           emf3ijb (i,k,1) = fijs(11)
           emf3ijb (i,k,2) = huge
           emf3ijb (i,k,3) = huge



70       continue
80     continue
c
c-----------------------  Constant values for OJB  ---------------------
c
       do 100 k=ks-2,ke+3
         do 90 i=is-2,ie+3
           if (coords(2) .eq. ntiles(2) - 1) then
             nojb    (i,k) = nojs(1)

             nojb3   (i,k) = nojs(1)
             nojb1   (i,k) = nojs(1)
             nojb31  (i,k) = nojs(1)
           else
             nojb    (i,k) = 0

             nojb3   (i,k) = 0
             nojb1   (i,k) = 0
             nojb31  (i,k) = 0
           endif
           dojb    (i,k,1) = fojs(1)
           dojb    (i,k,2) = huge
           dojb    (i,k,3) = huge

           eojb    (i,k,1) = fojs(2)
           eojb    (i,k,2) = huge

           v1ojb   (i,k,1) = fojs(3)
           v1ojb   (i,k,2) = huge
           v2ojb   (i,k,1) = fojs(4)
           v2ojb   (i,k,2) = huge
           v3ojb   (i,k,1) = fojs(5)
           v3ojb   (i,k,2) = huge

           b1ojb   (i,k,1) = fojs(6)
           b1ojb   (i,k,2) = huge
           b2ojb   (i,k,1) = fojs(7)
           b2ojb   (i,k,2) = huge
           b3ojb   (i,k,1) = fojs(8)
           b3ojb   (i,k,2) = huge
           emf1ojb (i,k,1) = fojs(9)
           emf1ojb (i,k,2) = huge
           emf1ojb (i,k,3) = huge
           emf2ojb (i,k,1) = fojs(10)
           emf2ojb (i,k,2) = huge
           emf2ojb (i,k,3) = huge
           emf3ojb (i,k,1) = fojs(11)
           emf3ojb (i,k,2) = huge
           emf3ojb (i,k,3) = huge



90       continue
100    continue
c
c-----------------------  Constant values for IKB  ---------------------
c
       do 120 j=js-2,je+3
         do 110 i=is-2,ie+3
           if (coords(3) .eq. 0) then
             nikb    (i,j) = niks(1)

             nikb1   (i,j) = niks(1)
             nikb2   (i,j) = niks(1)
             nikb12  (i,j) = niks(1)
           else
             nikb    (i,j) = 0

             nikb1   (i,j) = 0
             nikb2   (i,j) = 0
             nikb12  (i,j) = 0
           endif
           dikb    (i,j,1) = fiks(1)
           dikb    (i,j,2) = huge
           dikb    (i,j,3) = huge

           eikb    (i,j,1) = fiks(2)
           eikb    (i,j,2) = huge

           v1ikb   (i,j,1) = fiks(3)
           v1ikb   (i,j,2) = huge
           v2ikb   (i,j,1) = fiks(4)
           v2ikb   (i,j,2) = huge
           v3ikb   (i,j,1) = fiks(5)
           v3ikb   (i,j,2) = huge

           b1ikb   (i,j,1) = fiks(6)
           b1ikb   (i,j,2) = huge
           b2ikb   (i,j,1) = fiks(7)
           b2ikb   (i,j,2) = huge
           b3ikb   (i,j,1) = fiks(8)
           b3ikb   (i,j,2) = huge
           emf1ikb (i,j,1) = fiks(9)
           emf1ikb (i,j,2) = huge
           emf1ikb (i,j,3) = huge
           emf2ikb (i,j,1) = fiks(10)
           emf2ikb (i,j,2) = huge
           emf2ikb (i,j,3) = huge
           emf3ikb (i,j,1) = fiks(11)
           emf3ikb (i,j,2) = huge
           emf3ikb (i,j,3) = huge



110      continue
120    continue
c
c-----------------------  Constant values for OKB  ---------------------
c
       do 140 j=js-2,je+3
         do 130 i=is-2,ie+3
           if (coords(3) .eq. ntiles(3) - 1) then
             nokb    (i,j) = noks(1)

             nokb1   (i,j) = noks(1)
             nokb2   (i,j) = noks(1)
             nokb12  (i,j) = noks(1)
           else
             nokb    (i,j) = 0

             nokb1   (i,j) = 0
             nokb2   (i,j) = 0
             nokb12  (i,j) = 0
           endif
           dokb    (i,j,1) = foks(1)
           dokb    (i,j,2) = huge
           dokb    (i,j,3) = huge

           eokb    (i,j,1) = foks(2)
           eokb    (i,j,2) = huge

           v1okb   (i,j,1) = foks(3)
           v1okb   (i,j,2) = huge
           v2okb   (i,j,1) = foks(4)
           v2okb   (i,j,2) = huge
           v3okb   (i,j,1) = foks(5)
           v3okb   (i,j,2) = huge

           b1okb   (i,j,1) = foks(6)
           b1okb   (i,j,2) = huge
           b2okb   (i,j,1) = foks(7)
           b2okb   (i,j,2) = huge
           b3okb   (i,j,1) = foks(8)
           b3okb   (i,j,2) = huge
           emf1okb (i,j,1) = foks(9)
           emf1okb (i,j,2) = huge
           emf1okb (i,j,3) = huge
           emf2okb (i,j,1) = foks(10)
           emf2okb (i,j,2) = huge
           emf2okb (i,j,3) = huge
           emf3okb (i,j,1) = foks(11)
           emf3okb (i,j,2) = huge
           emf3okb (i,j,3) = huge



130      continue
140    continue
c
c-------------------------  blast GENERATOR  -------------------------
c
c  blast is a user-defined cpp macro name representing a subroutine
c  which intializes all field variables for the particular problem to
c  be studied.  blast should initialize the field variable arrays 
c  for both active zones and at least the first layer of boundary zones,
c  unless the default or input constant values already specify the 
c  desired problem.

c
c  For non-uniform initial magnetic field configurations, to satisfy 
c  the constraint DIV(B)=0 blast should initialize
c  b1, b2, b3 by differencing a vector potential.

c
c  First initialize all field variables to default (input) values.
c
      do 170 k=ks-2,ke+3
        do 160 j=js-2,je+3
          do 150 i=is-2,ie+3
            d  (i,j,k) = dfloor

            e  (i,j,k) = efloor

            v1 (i,j,k) = v1floor
            v2 (i,j,k) = v2floor
            v3 (i,j,k) = v3floor

            b1 (i,j,k) = b1floor * g2bi(i) * g31bi(i)
            b2 (i,j,k) = b2floor * g32bi(j)
            b3 (i,j,k) = b3floor * g2bi(i)


150       continue
160     continue
170   continue
c
c Set the bvstat array to 0, indicating that the boundary values
c will need to be updated.
c
      do 176 j=1,nbvar
        do 175 i=1,6
          bvstat(i,j) = 0
 175   continue
 176  continue
      dt     = huge
c


      call blast


c
c Note that the 2-D boundary arrays were set to constant values above,
c which should be overwritten with any desired spatially-dependent 
c functions in the Problem Generator.
c
c Set the scalar boundary value flags niis, etc. to nonzero values if 
c the corresponding boundaries are physical ones.
c

       if (niib(js,ks).ne.0 .and. coords(1).eq.0        ) 
     &                              niis(1) = niib(js,ks)
       if (noib(js,ks).ne.0 .and. coords(1).eq.ntiles(1) - 1) 
     &                              nois(1) = noib(js,ks)
       if (nijb(is,ks).ne.0 .and. coords(2).eq.0        ) 
     &                              nijs(1) = nijb(is,ks)
       if (nojb(is,ks).ne.0 .and. coords(2).eq.ntiles(2) - 1) 
     &                              nojs(1) = nojb(is,ks)
       if (nikb(is,js).ne.0 .and. coords(3).eq.0        ) 
     &                              niks(1) = nikb(is,js)
       if (nokb(is,js).ne.0 .and. coords(3).eq.ntiles(3) - 1) 
     &                              noks(1) = nokb(is,js)
c
c-----------------------  INITIALIZE ADDITIONAL BOUNDARY ARRAYS --------
c
c      Set the secondary boundary flags ("niib2", "niib3", "niib23",
c  etc.) from the primary boundary flags ("niib", etc.).
c
       call bndyflgs
c
c      If the second (and third) layer boundaries were not set by the
c  user, set them equal to the first layer boundary values.
c
       do 190 k=ks-2,ke+3
         do 180 j=js-2,je+3
           if (   diib(j,k,2) .eq. huge)    diib(j,k,2) =    diib(j,k,1)
           if (   diib(j,k,3) .eq. huge)    diib(j,k,3) =    diib(j,k,2)

           if (   eiib(j,k,2) .eq. huge)    eiib(j,k,2) =    eiib(j,k,1)

           if (  v1iib(j,k,2) .eq. huge)   v1iib(j,k,2) =   v1iib(j,k,1)
           if (  v2iib(j,k,2) .eq. huge)   v2iib(j,k,2) =   v2iib(j,k,1)
           if (  v3iib(j,k,2) .eq. huge)   v3iib(j,k,2) =   v3iib(j,k,1)

           if (  b1iib(j,k,2) .eq. huge)   b1iib(j,k,2) =   b1iib(j,k,1)
           if (  b2iib(j,k,2) .eq. huge)   b2iib(j,k,2) =   b2iib(j,k,1)
           if (  b3iib(j,k,2) .eq. huge)   b3iib(j,k,2) =   b3iib(j,k,1)
           if (emf1iib(j,k,2) .eq. huge) emf1iib(j,k,2) = emf1iib(j,k,1)
           if (emf1iib(j,k,3) .eq. huge) emf1iib(j,k,3) = emf1iib(j,k,2)
           if (emf2iib(j,k,2) .eq. huge) emf2iib(j,k,2) = emf2iib(j,k,1)
           if (emf2iib(j,k,3) .eq. huge) emf2iib(j,k,3) = emf2iib(j,k,2)
           if (emf3iib(j,k,2) .eq. huge) emf3iib(j,k,2) = emf3iib(j,k,1)
           if (emf3iib(j,k,3) .eq. huge) emf3iib(j,k,3) = emf3iib(j,k,2)


           if (   doib(j,k,2) .eq. huge)    doib(j,k,2) =    doib(j,k,1)
           if (   doib(j,k,3) .eq. huge)    doib(j,k,3) =    doib(j,k,2)

           if (   eoib(j,k,2) .eq. huge)    eoib(j,k,2) =    eoib(j,k,1)

           if (  v1oib(j,k,2) .eq. huge)   v1oib(j,k,2) =   v1oib(j,k,1)
           if (  v2oib(j,k,2) .eq. huge)   v2oib(j,k,2) =   v2oib(j,k,1)
           if (  v3oib(j,k,2) .eq. huge)   v3oib(j,k,2) =   v3oib(j,k,1)

           if (  b1oib(j,k,2) .eq. huge)   b1oib(j,k,2) =   b1oib(j,k,1)
           if (  b2oib(j,k,2) .eq. huge)   b2oib(j,k,2) =   b2oib(j,k,1)
           if (  b3oib(j,k,2) .eq. huge)   b3oib(j,k,2) =   b3oib(j,k,1)
           if (emf1oib(j,k,2) .eq. huge) emf1oib(j,k,2) = emf1oib(j,k,1)
           if (emf1oib(j,k,3) .eq. huge) emf1oib(j,k,3) = emf1oib(j,k,2)
           if (emf2oib(j,k,2) .eq. huge) emf2oib(j,k,2) = emf2oib(j,k,1)
           if (emf2oib(j,k,3) .eq. huge) emf2oib(j,k,3) = emf2oib(j,k,2)
           if (emf3oib(j,k,2) .eq. huge) emf3oib(j,k,2) = emf3oib(j,k,1)
           if (emf3oib(j,k,3) .eq. huge) emf3oib(j,k,3) = emf3oib(j,k,2)


180      continue
190    continue
c
       do 210 k=ks-2,ke+3
         do 200 i=is-2,ie+3
           if (   dijb(i,k,2) .eq. huge)    dijb(i,k,2) =    dijb(i,k,1)
           if (   dijb(i,k,3) .eq. huge)    dijb(i,k,3) =    dijb(i,k,2)

           if (   eijb(i,k,2) .eq. huge)    eijb(i,k,2) =    eijb(i,k,1)

           if (  v1ijb(i,k,2) .eq. huge)   v1ijb(i,k,2) =   v1ijb(i,k,1)
           if (  v2ijb(i,k,2) .eq. huge)   v2ijb(i,k,2) =   v2ijb(i,k,1)
           if (  v3ijb(i,k,2) .eq. huge)   v3ijb(i,k,2) =   v3ijb(i,k,1)

           if (  b1ijb(i,k,2) .eq. huge)   b1ijb(i,k,2) =   b1ijb(i,k,1)
           if (  b2ijb(i,k,2) .eq. huge)   b2ijb(i,k,2) =   b2ijb(i,k,1)
           if (  b3ijb(i,k,2) .eq. huge)   b3ijb(i,k,2) =   b3ijb(i,k,1)
           if (emf1ijb(i,k,2) .eq. huge) emf1ijb(i,k,2) = emf1ijb(i,k,1)
           if (emf1ijb(i,k,3) .eq. huge) emf1ijb(i,k,3) = emf1ijb(i,k,2)
           if (emf2ijb(i,k,2) .eq. huge) emf2ijb(i,k,2) = emf2ijb(i,k,1)
           if (emf2ijb(i,k,3) .eq. huge) emf2ijb(i,k,3) = emf2ijb(i,k,2)
           if (emf3ijb(i,k,2) .eq. huge) emf3ijb(i,k,2) = emf3ijb(i,k,1)
           if (emf3ijb(i,k,3) .eq. huge) emf3ijb(i,k,3) = emf3ijb(i,k,2)


           if (   dojb(i,k,2) .eq. huge)    dojb(i,k,2) =    dojb(i,k,1)
           if (   dojb(i,k,3) .eq. huge)    dojb(i,k,3) =    dojb(i,k,2)

           if (   eojb(i,k,2) .eq. huge)    eojb(i,k,2) =    eojb(i,k,1)

           if (  v1ojb(i,k,2) .eq. huge)   v1ojb(i,k,2) =   v1ojb(i,k,1)
           if (  v2ojb(i,k,2) .eq. huge)   v2ojb(i,k,2) =   v2ojb(i,k,1)
           if (  v3ojb(i,k,2) .eq. huge)   v3ojb(i,k,2) =   v3ojb(i,k,1)

           if (  b1ojb(i,k,2) .eq. huge)   b1ojb(i,k,2) =   b1ojb(i,k,1)
           if (  b2ojb(i,k,2) .eq. huge)   b2ojb(i,k,2) =   b2ojb(i,k,1)
           if (  b3ojb(i,k,2) .eq. huge)   b3ojb(i,k,2) =   b3ojb(i,k,1)
           if (emf1ojb(i,k,2) .eq. huge) emf1ojb(i,k,2) = emf1ojb(i,k,1)
           if (emf1ojb(i,k,3) .eq. huge) emf1ojb(i,k,3) = emf1ojb(i,k,2)
           if (emf2ojb(i,k,2) .eq. huge) emf2ojb(i,k,2) = emf2ojb(i,k,1)
           if (emf2ojb(i,k,3) .eq. huge) emf2ojb(i,k,3) = emf2ojb(i,k,2)
           if (emf3ojb(i,k,2) .eq. huge) emf3ojb(i,k,2) = emf3ojb(i,k,1)
           if (emf3ojb(i,k,3) .eq. huge) emf3ojb(i,k,3) = emf3ojb(i,k,2)


200      continue
210    continue
c
       do 230 j=js-2,je+3
         do 220 i=is-2,ie+3
           if (   dikb(i,j,2) .eq. huge)    dikb(i,j,2) =    dikb(i,j,1)
           if (   dikb(i,j,3) .eq. huge)    dikb(i,j,3) =    dikb(i,j,2)

           if (   eikb(i,j,2) .eq. huge)    eikb(i,j,2) =    eikb(i,j,1)

           if (  v1ikb(i,j,2) .eq. huge)   v1ikb(i,j,2) =   v1ikb(i,j,1)
           if (  v2ikb(i,j,2) .eq. huge)   v2ikb(i,j,2) =   v2ikb(i,j,1)
           if (  v3ikb(i,j,2) .eq. huge)   v3ikb(i,j,2) =   v3ikb(i,j,1)

           if (  b1ikb(i,j,2) .eq. huge)   b1ikb(i,j,2) =   b1ikb(i,j,1)
           if (  b2ikb(i,j,2) .eq. huge)   b2ikb(i,j,2) =   b2ikb(i,j,1)
           if (  b3ikb(i,j,2) .eq. huge)   b3ikb(i,j,2) =   b3ikb(i,j,1)
           if (emf1ikb(i,j,2) .eq. huge) emf1ikb(i,j,2) = emf1ikb(i,j,1)
           if (emf1ikb(i,j,3) .eq. huge) emf1ikb(i,j,3) = emf1ikb(i,j,2)
           if (emf2ikb(i,j,2) .eq. huge) emf2ikb(i,j,2) = emf2ikb(i,j,1)
           if (emf2ikb(i,j,3) .eq. huge) emf2ikb(i,j,3) = emf2ikb(i,j,2)
           if (emf3ikb(i,j,2) .eq. huge) emf3ikb(i,j,2) = emf3ikb(i,j,1)
           if (emf3ikb(i,j,3) .eq. huge) emf3ikb(i,j,3) = emf3ikb(i,j,2)


           if (   dokb(i,j,2) .eq. huge)    dokb(i,j,2) =    dokb(i,j,1)
           if (   dokb(i,j,3) .eq. huge)    dokb(i,j,3) =    dokb(i,j,2)

           if (   eokb(i,j,2) .eq. huge)    eokb(i,j,2) =    eokb(i,j,1)

           if (  v1okb(i,j,2) .eq. huge)   v1okb(i,j,2) =   v1okb(i,j,1)
           if (  v2okb(i,j,2) .eq. huge)   v2okb(i,j,2) =   v2okb(i,j,1)
           if (  v3okb(i,j,2) .eq. huge)   v3okb(i,j,2) =   v3okb(i,j,1)

           if (  b1okb(i,j,2) .eq. huge)   b1okb(i,j,2) =   b1okb(i,j,1)
           if (  b2okb(i,j,2) .eq. huge)   b2okb(i,j,2) =   b2okb(i,j,1)
           if (  b3okb(i,j,2) .eq. huge)   b3okb(i,j,2) =   b3okb(i,j,1)
           if (emf1okb(i,j,2) .eq. huge) emf1okb(i,j,2) = emf1okb(i,j,1)
           if (emf1okb(i,j,3) .eq. huge) emf1okb(i,j,3) = emf1okb(i,j,2)
           if (emf2okb(i,j,2) .eq. huge) emf2okb(i,j,2) = emf2okb(i,j,1)
           if (emf2okb(i,j,3) .eq. huge) emf2okb(i,j,3) = emf2okb(i,j,2)
           if (emf3okb(i,j,2) .eq. huge) emf3okb(i,j,2) = emf3okb(i,j,1)
           if (emf3okb(i,j,3) .eq. huge) emf3okb(i,j,3) = emf3okb(i,j,2)


220      continue
230    continue


c
c-------------------------  GRID MOTION CONTROL  -----------------------
c
c  x1fac     x1 motion factor
c            < 0 gives "Lagrangian" tracking in x1 lines
c  x2fac     x2 motion factor
c            < 0 gives "Lagrangian" tracking in x2 lines
c  x3fac     x3 motion factor
c            < 0 gives "Lagrangian" tracking in x3 lines
c  ia        i<ia => zone ratio is preserved in x1 lines
c  ja        j<ja => zone ratio is preserved in x2 lines
c  ka        k<ka => zone ratio is preserved in x3 lines
c  igcon     selects grid treatment:
c            =0 => separate motion
c            =1 => averaged motion
c            =2 => tracking x1, x2, and x3 boundaries
c            =3 => averaged boundary tracking
c            =4 => input grid boundary speeds
c                  vg1(io) = x1fac * central sound speed
c                  vg2(jo) = x2fac * central sound speed
c                  vg3(ko) = x3fac * central sound speed
c
      if (myid_w .eq. 0) then
        x1fac = 0.0
        x2fac = 0.0
        x3fac = 0.0
        ia    = is
        ja    = js
        ka    = ks
        igcon = 0
c
        read (1,gcon)
cpu2006        write(2,gcon)
c
        ia =  min (  max ( ia, is ), ie )
        ja =  min (  max ( ja, js ), je )
        ka =  min (  max ( ka, ks ), ke )
c
      endif

      do 320 i=is-2,ie+3
        vg1(i) = 0.0
320   continue
      do 330 j=js-2,je+3
        vg2(j) = 0.0
330   continue
      do 340 k=ks-2,ke+3
        vg3(k) = 0.0
340   continue
c
c----------------------  INITIAL TIMESTEP  -----------------------------
c


       nreq = 0
c
c Compute the momentum densities from the initial conditions.  We
c need some density boundary values, but we won't try to be efficient
c here.  Be sure to call bvald in sequence so that the values on edges
c get set properly.  
c

       nreq = 0
       nsub = nsub + 1
       call bvald(1,0,0,0,0,0,d)

       nreq = 0
       nsub = nsub + 1
       call bvald(0,0,1,0,0,0,d)

       nreq = 0
       nsub = nsub + 1
       call bvald(0,0,0,0,1,0,d)

       do 370 k=ks,ke
         do 360 j=js,je
           do 350 i=is,ie
             w3da(i,j,k) = v1(i,j,k) * 0.5 * (d(i-1,j  ,k  ) + d(i,j,k))
             w3db(i,j,k) = v2(i,j,k) * 0.5 * (d(i  ,j-1,k  ) + d(i,j,k))
     1                   * g2b(i)
             w3dc(i,j,k) = v3(i,j,k) * 0.5 * (d(i  ,j  ,k-1) + d(i,j,k))
     1                   * g31b(i) * g32b(j)

350        continue
360      continue
370    continue
c
c Set the artificial viscosity time step dtqqi2 to zero.
c
       dtqqi2 = 0.0
       dtmin  = tiny
c

c


       call nudt

       dtmin = dtrat * dt
       dt    = 10.0 * dtmin


c
c-----------------------  INITIALIZE NEW GRID  -------------------------
c
c  Compute grid velocities, and new grid positions [in routine newgrid].
c  Note newgrid will recompute "n+1/2" and "n+1" grid lines, but only
c  if x1fac or x2fac .ne. 0.  The "n+1/2" and "n+1" grid lines are
c  initialized in ggen to the old values in case the grid never moves.  Note
c  newgrid must be called after nudt since it needs the timestep.  Thus,
c  the initial timestep computed above did not account for grid motion.
c

c
c------------------------  Initialize everything else  -----------------
c
      ix1x2x3 = 1
c  For rad_pf* routines
      jx1x2x3 = 1  
c
      return
      end
