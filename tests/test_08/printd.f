












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine printd
c
c  FORMATTED WRITE OF SELECTED VARIABLES
c
c     written by: Jim Stone
c     date:       February,1993
c     modified1:  DAC from ZEUS-3D
c     modified2:  RAF 1/12/96 for ZEUS-MP
c     modified3:  RAF 3/12/97, for ablation test problem
c     modified4:  JCH 4/17/97, cons.h included even when RAD_EXP
c                 not defined
c     modified5:  M-MML 5/18/98, expanded to include the full ZEUS-3D 
c         suite, including max and min value, as well as things like 
c         rms velocity, and to run under MPI
c
c  PURPOSE:  Dumps scalar "history" variables in a formatted write
c  for analysis.  Currently implemented variables are:
c
c   scal( 1) = time
c   scal( 2) = dt
c   scal( 3) = mass
c   scal( 4) = total energy
c   scal( 5) = kinetic energy
c   scal( 6) = magnetic energy
c   scal( 7) = internal energy
c   scal( 8) = radiation energy
c   scal( 9) = gravitational potential energy
c   scal(10) = 0.5*d*v1**2
c   scal(11) = 0.5*d*v2**2
c   scal(12) = 0.5*d*v3**2
c   scal(13) = b1**2
c   scal(14) = b2**2
c   scal(15) = b3**2
c   scal(16) = b1 flux
c   scal(17) = b2 flux
c   scal(18) = b3 flux
c   scal(19) = 1 angular momentum
c   scal(20) = 2 angular momentum
c   scal(21) = 3 angular momentum
c   scal(22) = 1 center of mass
c   scal(23) = 2 center of mass
c   scal(24) = 3 center of mass
c
c
c   scal(25) =    dmin   - minimum density
c   scal(26) =    dmax   - maximum density
c   scal(27) =    emin   - minimum specific energy
c   scal(28) =    emax   - maximum specific energy
c   scal(29) =    pmin   - minimum pressure
c   scal(30) =    pmax   - maximum pressure
c   scal(31) =    v1min  - minimum velocity in 1-direction
c   scal(32) =    v1max  - maximum velocity in 1-direction
c   scal(33) =    v2min  - minimum velocity in 2-direction
c   scal(34) =    v2max  - maximum velocity in 2-direction
c   scal(35) =    v3min  - minimum velocity in 3-direction
c   scal(36) =    v3max  - maximum velocity in 3-direction
c   scal(37) =    vmin   - minimum speed (magnitude of velocity vector)
c   scal(38) =    vmax   - maximum speed (magnitude of velocity vector)
c   scal(39) =    b1min  - minimum mag. field in 1-direction
c   scal(40) =    b1max  - maximum mag. field in 1-direction
c   scal(41) =    b2min  - minimum mag. field in 2-direction
c   scal(42) =    b2max  - maximum mag. field in 2-direction
c   scal(43) =    b3min  - minimum mag. field in 3-direction
c   scal(44) =    b3max  - maximum mag. field in 3-direction
c   scal(45) =    bmin   - minimum mag. field strength (magnitude of B-vector)
c   scal(46) =    bmax   - maximum mag. field strength (magnitude of B-vector)
c   scal(47) =    dvbmin - minimum div(B) (normalised)
c   scal(48) =    dvbmax - maximum div(B) (normalised)
c   scal(49) =    dtcs   - sound speed time stop
c      dt**   - timestep limit caused by ** (eg., dtcs, dtqq, etc.)
c  LOCALS:
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
      integer i,j,k,nscal
      integer id_ctr
      parameter(nscal=24)
      real*8 scal(nscal)
      real*8 mass,x1cm,x2cm,x3cm,eint,ek1,ek2,ek3,epot
     .   ,emag1,emag2,emag3,b1flx,b2flx,b3flx
     .   ,l1,l2,l3,erad,dvb
       real*8        dmin    , dmax    , semin   , semax   , pmin
     1             , pmax    , v1min   , v1max   , v2min   , v2max
     2             , v3min   , v3max   , vsqrmin , vsqrmax , vmin
     3             , vmax    , b1min   , b1max   , b2min   , b2max
     4             , b3min   , b3max   , bsqrmin , bsqrmax , bmin
     5             , bmax    , dvbmin  , dvbmax
       integer       imin    (  12), jmin    (  12), kmin    (  12)
     1             , imax    (  12), jmax    (  12), kmax    (  12)
      real*8 rv,dvola,dm,g3b,q1,q2,q3,dar1,dar2,dar3
     .   , v1av,v2av,v3av,b1av,b2av,b3av
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////
c=======================================================================
c
c
c  Integrate quantities
c
c      Zero totals.
c
       id_ctr = 99999
       mass  = tiny
       x1cm  = 0.0
       x2cm  = 0.0
       x3cm  = 0.0
       eint  = 0.0
       ek1   = 0.0
       ek2   = 0.0
       ek3   = 0.0
       epot  = 0.0
       emag1 = 0.0
       emag2 = 0.0
       emag3 = 0.0
       b1flx = 0.0
       b2flx = 0.0
       b3flx = 0.0
       l1    = 0.0
       l2    = 0.0
       l3    = 0.0
       erad  = 0.0
c
c      Compute totals over k-sweeps
c
       do 40 k=ks,ke
         do 30 j=js,je
           do 20 i=is,ie
             dvola     = dvl1a(i) * dvl2a(j) * dvl3a(k)
             dm        = d(i,j,k) * dvola
             g3b       = g31b(i) * g32b(j)
             q1        = x1b(i)
             q2        = x2b(j) * g2b(i)
             q3        = x3b(k) * g3b
             dar1      = g2a(i) * dx2b(j) * g3b    * dx3b(k)
             dar2      = g3b    * dx3b(k)          * dx1b(i)
             dar3      =          dx1b(i) * g2a(i) * dx2b(j)
             v1av      = v1(i,j,k) + v1(i+1,j  ,k  )
             v2av      = v2(i,j,k) + v2(i  ,j+1,k  )
             v3av      = v3(i,j,k) + v3(i  ,j  ,k+1)

             b1av      = b1(i,j,k) + b1(i+1,j  ,k  )
             b2av      = b2(i,j,k) + b2(i  ,j+1,k  )
             b3av      = b3(i,j,k) + b3(i  ,j  ,k+1)

             mass      = mass      + dm
             x1cm      = x1cm      + dm * q1
             x2cm      = x2cm      + dm * q2
             x3cm      = x3cm      + dm * q3
             ek1       = ek1       + dm * v1av**2
             ek2       = ek2       + dm * v2av**2
             ek3       = ek3       + dm * v3av**2

CJH

             eint      = eint      + e(i,j,k) * dvola





             rv        = sqrt ( ( x1b(i) - x1ptm )**2
     1                        + ( x2b(j) - x2ptm )**2
     2                        + ( x3b(k) - x3ptm )**2 )



             epot      = epot      - dm * g * ptmass / rv

             emag1     = emag1     + b1av**2 * dvola
             emag2     = emag2     + b2av**2 * dvola
             emag3     = emag3     + b3av**2 * dvola
             b1flx     = b1flx     + b1(i,j,k) * dar1
             b2flx     = b2flx     + b2(i,j,k) * dar2
             b3flx     = b3flx     + b3(i,j,k) * dar3

             l1        = l1        + dm * ( q2 * v3av - q3 * v2av )
             l2        = l2        + dm * ( q3 * v1av - q1 * v3av )
             l3        = l3        + dm * ( q1 * v2av - q2 * v1av )


c
20         continue
30       continue
40     continue
c
       scal(1)  = time
       scal(2)  = dt
       scal(3)  = mass
c
       scal(7)  = eint
       scal(8)  = erad
       scal(9)  = epot
       scal(10) = ek1   / 8.0
       scal(11) = ek2   / 8.0
       scal(12) = ek3   / 8.0
       scal(13) = emag1 / 8.0
       scal(14) = emag2 / 8.0
       scal(15) = emag3 / 8.0
       scal(16) = b1flx
       scal(17) = b2flx
       scal(18) = b3flx
c

c
       scal(19) = l1    / 2.0
       scal(20) = l2    / 2.0
       scal(21) = l3    / 2.0
c
       scal(22) = x1cm  / mass
       scal(23) = x2cm  / mass
       scal(24) = x3cm  / mass
c

c
c Total up various forms of energy.
c
c kinetic
       scal(5)  = scal(10) + scal(11) + scal(12)             
c magnetic
       scal(6)  = scal(13) + scal(14) + scal(15)             
       scal(4)  = scal( 5) + scal( 6) + scal( 7) + scal( 8) + scal( 9)
c
c  Write out variables to file connected to unit 3 opened in MAIN
c  program unit (zeusmp.src)
c

      write(3,2001) (scal(i),i=1,nscal)

2001  format(1p,6e13.5/1p,6e13.5/1p,6e13.5/1p,6e13.5/)
c
      return
      end
