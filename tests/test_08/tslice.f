












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine tslice
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
c     modified6:  efh 04/15/99, see modified5. Control mechanism modified
c                 as well (intchk, dataio), as printd works for each tile.
c     modified7:  PSLi 11/19/99, correct a mistake in getting extrema by
c                 the root process.
c     modified8:  PSLi 03/12/02, add back MPI_USED directive and some
c                 minor changes so that the routine works when MPI_USED
c                 is OFF in definition file.
c
c  PURPOSE:  Dumps global scalar "history" variables in a formatted write
c  for analysis.  
c  Currently implemented variables are:
c
c "sum up" variables
c   sumscal( 1) = mass
c   sumscal( 2) = total energy
c   sumscal( 3) = kinetic energy
c   sumscal( 4) = magnetic energy
c   sumscal( 5) = internal energy
c   sumscal( 6) = radiation energy
c   sumscal( 7) = gravitational potential energy
c   sumscal( 8) = 0.5*d*v1**2
c   sumscal( 9) = 0.5*d*v2**2
c   sumscal(10) = 0.5*d*v3**2
c   sumscal(11) = b1**2
c   sumscal(12) = b2**2
c   sumscal(13) = b3**2
c   sumscal(14) = b1 flux
c   sumscal(15) = b2 flux
c   sumscal(16) = b3 flux
c   sumscal(17) = 1 angular momentum
c   sumscal(18) = 2 angular momentum
c   sumscal(19) = 3 angular momentum
c   sumscal(20) = 1 center of mass
c   sumscal(21) = 2 center of mass
c   sumscal(22) = 3 center of mass
c   sumscal(13) = 1 rms velocity
c   sumscal(14) = 2 rms velocity
c   sumscal(15) = 3 rms velocity
c   sumscal(16) = rms velocity
c
c "minmax" variables
c   minscal(1)  =    dmin   - minimum density
c   maxscal(1)  =    dmax   - maximum density
c   minscal(2)  =    emin   - minimum specific energy
c   maxscal(2)  =    emax   - maximum specific energy
c   minscal(3)  =    pmin   - minimum pressure
c   maxscal(3)  =    pmax   - maximum pressure
c   minscal(4)  =    v1min  - minimum velocity in 1-direction
c   maxscal(4)  =    v1max  - maximum velocity in 1-direction
c   minscal(5)  =    v2min  - minimum velocity in 2-direction
c   maxscal(5)  =    v2max  - maximum velocity in 2-direction
c   minscal(6)  =    v3min  - minimum velocity in 3-direction
c   maxscal(6)  =    v3max  - maximum velocity in 3-direction
c   minscal(7)  =    vmin   - minimum speed (magnitude of velocity vector)
c   maxscal(7)  =    vmax   - maximum speed (magnitude of velocity vector)
c   minscal(8)  =    b1min  - minimum mag. field in 1-direction
c   maxscal(8)  =    b1max  - maximum mag. field in 1-direction
c   minscal(9)  =    b2min  - minimum mag. field in 2-direction
c   maxscal(9)  =    b2max  - maximum mag. field in 2-direction
c   minscal(10) =    b3min  - minimum mag. field in 3-direction
c   maxscal(10) =    b3max  - maximum mag. field in 3-direction
c   minscal(11) =    bmin   - minimum mag. field strength (magnitude of B-vector)
c   maxscal(11) =    bmax   - maximum mag. field strength (magnitude of B-vector)
c   minscal(12) =    dvbmin - minimum div(B) (normalised)
c   maxscal(12) =    dvbmax - maximum div(B) (normalised)
c
c
c  LOCALS:  REAL sumscal(nscal)     sendbuf in MPI_Reduce
c           REAL totsumscal(nscal)  recvbuf in MPI_Reduce 
c 
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

      integer i,j,k,ip1,jp1,kp1,
     &        nsumscal,nminscal,nmaxscal
      integer id_ctr, my_rank
      parameter(nsumscal=27)

      parameter(nminscal=12)
      parameter(nmaxscal=12)


      real*8 sumscal(nsumscal)    
     &    ,totsumscal(nsumscal)
c 2d for process rank used in MAXLOC
     &    ,minscal(2,nminscal)     
     &    ,maxscal(2,nmaxscal)    
     &    ,totminscal(2,nminscal)
     &    ,totmaxscal(2,nmaxscal)
      integer
c for the coordinates of extrema
c     &    ,totxmin(nprocs,nminscal) 
c     &    ,totxmax(nprocs,nmaxscal)
c for the coordinates of extrema
     &     totxmin(nprocs*nminscal) 
     &    ,totxmax(nprocs*nmaxscal)
      integer  
     &         imin(nminscal)    , imax(nmaxscal)
     &       , jmin(nminscal)    , jmax(nmaxscal)
     &       , kmin(nminscal)    , kmax(nmaxscal)

      real*8 mass,x1cm,x2cm,x3cm,eint,ek1,ek2,ek3,epot
     .   ,emag1,emag2,emag3,b1flx,b2flx,b3flx
     .   ,l1,l2,l3,erad,dvb,totdvb
CPS

     .   ,mdtal


     .   ,mdtcs,mdtv1,mdtv2,mdtv3,mdtqqi2
C
       real*8        dmin    , dmax    , semin   , semax   , pmin
     &             , pmax    , v1min   , v1max   , v2min   , v2max
     &             , v3min   , v3max   , vsqrmin , vsqrmax , vmin
     &             , vmax    , b1min   , b1max   , b2min   , b2max
     &             , b3min   , b3max   , bsqrmin , bsqrmax , bmin
     &             , bmax    , dvbmin  , dvbmax
     &             , vrms1   , vrms2   , vrms3   , vrms
      real*8 rv,dvola,vola,dm,g3b,q1,q2,q3,dar1,dar2,dar3
     .   , v1av,v2av,v3av,b1av,b2av,b3av
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////
c=======================================================================
c
c
c  Integrate quantities for each tile
c
c      Zero totals.
c

       id_ctr = 99999
       mass  = tiny
       vola  = 0.0
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
       vrms1 = 0.0
       vrms2 = 0.0
       vrms3 = 0.0
       vrms  = 0.0
       erad  = 0.0
c
c      Compute totals over k-sweeps
c
       do k=ks,ke
         kp1 = k + 1
         do j=js,je
           jp1 = j + 1
           do i=is,ie
             ip1 = i + 1
             dvola     = dvl1a(i) * dvl2a(j) * dvl3a(k)
             vola      = vola + dvola
             dm        = d(i,j,k) * dvola
             g3b       = g31b(i) * g32b(j)
             q1        = x1b(i)
             q2        = x2b(j) * g2b(i)
             q3        = x3b(k) * g3b
             dar1      = g2a(i) * dx2b(j) * g3b    * dx3b(k)
             dar2      = g3b    * dx3b(k)          * dx1b(i)
             dar3      =          dx1b(i) * g2a(i) * dx2b(j)
             v1av      = v1(i,j,k) + v1(ip1,j  ,k  )
             v2av      = v2(i,j,k) + v2(i  ,jp1,k  )
             v3av      = v3(i,j,k) + v3(i  ,j  ,kp1)

             b1av      = b1(i,j,k) + b1(ip1,j  ,k  )
             b2av      = b2(i,j,k) + b2(i  ,jp1,k  )
             b3av      = b3(i,j,k) + b3(i  ,j  ,kp1)

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
             
             vrms1     = vrms1     + dvola*v1(i,j,k)**2
             vrms2     = vrms2     + dvola*v2(i,j,k)**2
             vrms3     = vrms3     + dvola*v3(i,j,k)**2
             vrms      = vrms      + dvola
     &                             *(   v1(i,j,k)**2
     &                                + v2(i,j,k)**2
     &                                + v3(i,j,k)**2)



           enddo
         enddo
       enddo


       sumscal(1)  = mass
       sumscal(5)  = eint
       sumscal(6)  = erad
       sumscal(7)  = epot
       sumscal(8)  = ek1   / 8.0
       sumscal(9)  = ek2   / 8.0
       sumscal(10) = ek3   / 8.0
       sumscal(11) = emag1 / 8.0
       sumscal(12) = emag2 / 8.0
       sumscal(13) = emag3 / 8.0
       sumscal(14) = b1flx
       sumscal(15) = b2flx
       sumscal(16) = b3flx
c


       sumscal(17) = l1    / 2.0
       sumscal(18) = l2    / 2.0
       sumscal(19) = l3    / 2.0

c no division by mass because of 
       sumscal(20) = x1cm  
c other tiles
       sumscal(21) = x2cm  
       sumscal(22) = x3cm

       sumscal(23) = vrms1 
       sumscal(24) = vrms2
       sumscal(25) = vrms3
       sumscal(26) = vrms 
       sumscal(27) = vola



c
c Total up various forms of energy.
c
c kinetic
       sumscal(3)  = sumscal(8)  + sumscal(9)  + sumscal(10)    
c magnetic
       sumscal(4)  = sumscal(11) + sumscal(12) + sumscal(13)    
c total
       sumscal(2)  = sumscal(3)  + sumscal(4)  + sumscal(5)     
     &             + sumscal(6) + sumscal(7)

c
c.......................................................................
c  Now reduce all tiles to root process
c


CPS
      DO i=1,27
        totsumscal(i)=sumscal(i)
      ENDDO
c center of mass
      totsumscal(20) = totsumscal(20) / totsumscal(1)         
      totsumscal(21) = totsumscal(21) / totsumscal(1)
      totsumscal(22) = totsumscal(22) / totsumscal(1)  
c rms-velocities
      totsumscal(23) = sqrt(totsumscal(23) / totsumscal(27))  
      totsumscal(24) = sqrt(totsumscal(24) / totsumscal(27))
      totsumscal(25) = sqrt(totsumscal(25) / totsumscal(27))
      totsumscal(26) = sqrt(totsumscal(26) / totsumscal(27))
C

c
c.......................................................................
c Calculate extrema for each tile.
c

c-- 1.  density --------------------------------------------------------
c
       call mnmx ( d, is, js, ks, ie, je, ke
     &           , minscal(1,1), imin(1), jmin(1), kmin(1)
     &           , maxscal(1,1), imax(1), jmax(1), kmax(1) )
       

c
c-- 2.  specific internal energy (proportional to T) -------------------
c

       call spenergy ( is, ie, js, je, ks, ke, w3da )
       call mnmx ( w3da, is, js, ks, ie, je, ke
     1           , minscal(1,2), imin(2), jmin(2), kmin(2)
     2           , maxscal(1,2), imax(2), jmax(2), kmax(2) )
C#else
C       minscal(1,2)=0.0
C       maxscal(1,2)=0.0
C       imin(2)=0
C       imax(2)=0
C       jmin(2)=0
C       jmax(2)=0
C       kmin(2)=0
C       kmax(2)=0

c
c-- 3.  pressure -------------------------------------------------------
c

       call pressure ( w3da, 1 )
       call mnmx ( w3da, is, js, ks, ie, je, ke
     1           , minscal(1,3), imin(3), jmin(3), kmin(3)
     2           , maxscal(1,3), imax(3), jmax(3), kmax(3) )
c
c-- 4.  1-velocity -----------------------------------------------------
c

       call mnmx ( v1, is+1, js, ks, ie, je, ke
     1           , minscal(1,4), imin(4), jmin(4), kmin(4)
     2           , maxscal(1,4), imax(4), jmax(4), kmax(4) )
c
c-- 5.  2-velocity -----------------------------------------------------
c
       call mnmx ( v2, is, js+1, ks, ie, je, ke
     1           , minscal(1,5), imin(5), jmin(5), kmin(5)
     2           , maxscal(1,5), imax(5), jmax(5), kmax(5) )
c
c-- 6.  3-velocity -----------------------------------------------------
c
       call mnmx ( v3, is, js, ks+1, ie, je, ke
     1           , minscal(1,6), imin(6), jmin(6), kmin(6)
     2           , maxscal(1,6), imax(6), jmax(6), kmax(6) )
c
c-- 7.  speed (magnitude of velocity) ----------------------------------
c

       do k=ks,ke


         do j=js,je


           do i=is,ie

             w3da(i,j,k) = 0.25 * ( v1(i+1,j  ,k  ) + v1(i,j,k) )**2
     1                   + 0.25 * ( v2(i  ,j+1,k  ) + v2(i,j,k) )**2
     2                   + 0.25 * ( v3(i  ,j  ,k+1) + v3(i,j,k) )**2
           enddo
         enddo
       enddo
       call mnmx ( w3da, is, js, ks, ie, je, ke
     1           , vsqrmin, imin(7), jmin(7), kmin(7)
     2           , vsqrmax, imax(7), jmax(7), kmax(7) )
       minscal(1,7) = sqrt ( vsqrmin )
       maxscal(1,7) = sqrt ( vsqrmax )

c
c-- 8.  1-magnetic field -----------------------------------------------
c

       call mnmx ( b1, is+1, js, ks, ie, je, ke
     1           , minscal(1,8), imin(8), jmin(8), kmin(8)
     2           , maxscal(1,8), imax(8), jmax(8), kmax(8) )
c
c-- 9. 2-magnetic field ------------------------------------------------
c
       call mnmx ( b2, is, js+1, ks, ie, je, ke
     1           , minscal(1,9), imin(9), jmin(9), kmin(9)
     2           , maxscal(1,9), imax(9), jmax(9), kmax(9) )
c
c-- 10. 3-magnetic field -----------------------------------------------
c
       call mnmx ( b3, is, js, ks+1, ie, je, ke
     1           , minscal(1,10), imin(10), jmin(10), kmin(10)
     2           , maxscal(1,10), imax(10), jmax(10), kmax(10) )
c
c-- 11. magnetic field strength (magnitude of magnetic field vector) ---
c
       call pressure ( w3da, 2 )
       call mnmx ( w3da, is, js, ks, ie, je, ke
     1           , bsqrmin, imin(11), jmin(11), kmin(11)
     2           , bsqrmax, imax(11), jmax(11), kmax(11) )
       minscal(1,11) = sqrt ( 2.0 * bsqrmin )
       maxscal(1,11) = sqrt ( 2.0 * bsqrmax )
c
c-- 12.  div(B) --------------------------------------------------------
c
       call diverg ( b1, b2, b3, 1, 1, w3da, dvb )
       call mnmx ( w3da, is+1, js+1, ks+1, ie-1, je-1, ke-1
     1           , minscal(1,12), imin(12), jmin(12), kmin(12)
     2           , maxscal(1,12), imax(12), jmax(12), kmax(12) )



c
c get the extrema and their coordinates
c   tot[min/max]scal contains the processID (2,i) and the
c   extemum value of that process (1,i) for all i=1,n[min/max]scal 
c


      do i=1,nminscal
        minscal(2,i) = 0
        maxscal(2,i) = 0
      enddo



c
c we still need the coordinates ...
c These can be stored in [i-k][min-max] of process 0, as
c 0 does the output.



c x-coord



c y-coord



c z-coord


CPS


       mdtal=dtal


       mdtcs= dtcs
       mdtv1=dtv1
       mdtv2=dtv2
       mdtv3=dtv3
       mdtqqi2=dtqqi2

       do i = 1, 12
          totminscal(1,i) = minscal(1,i)
          totminscal(2,i) = minscal(2,i)
          totmaxscal(1,i) = maxscal(1,i)
          totmaxscal(2,i) = maxscal(2,i)
       enddo

c
c  Write out variables to file connected to unit iotsl opened in MAIN
c  program unit (zeusmp.f)


c-----------------------------------------------------------------------
c------------------------- DUMP TO ASCII FILE --------------------------
c-----------------------------------------------------------------------
c

      if (myid .eq. 0) then
         write (31, 2040) 'time = ', time, ', nhy = ', nhy,
     &         ', dt = ', dt
         write (31, 2050) 'total mass                      =', 
     &         totsumscal(1)

         write (31, 2050) 'centre of mass in 1-direction   =', 
     &         totsumscal(20)
         write (31, 2050) 'centre of mass in 2-direction   =', 
     &         totsumscal(21)
         write (31, 2050) 'centre of mass in 3-direction   =', 
     &         totsumscal(22)

         write (31, 2050) 'total internal energy           =', 
     &         totsumscal(5)
         write (31, 2050) 'kinetic energy in 1-direction   =', 
     &         totsumscal(8)
         write (31, 2050) 'kinetic energy in 2-direction   =', 
     &         totsumscal(9)
         write (31, 2050) 'kinetic energy in 3-direction   =', 
     &         totsumscal(10)
         write (31, 2050) 'total kinetic energy            =', 
     &         totsumscal(3)
         write (31, 2050) 'gravitational potential energy  =', 
     &         totsumscal(7)

         write (31, 2050) 'magnetic energy of 1-component  =', 
     &         totsumscal(11)
         write (31, 2050) 'magnetic energy of 2-component  =', 
     &         totsumscal(12)
         write (31, 2050) 'magnetic energy of 3-component  =', 
     &         totsumscal(13)
         write (31, 2050) 'total magnetic energy           =', 
     &         totsumscal(4)
         write (31, 2050) 'magnetic flux of 1-component    =', 
     &         totsumscal(14)
         write (31, 2050) 'magnetic flux of 2-component    =', 
     &         totsumscal(15)
         write (31, 2050) 'magnetic flux of 3-component    =', 
     &         totsumscal(16)
         write (31, 2050) 'averaged and normalised div(B)  =', 
     &         dvb

         write (31, 2050) 'total energy                    =', 
     &         totsumscal(2)

         write (31, 2050) '1-component of angular momentum =', 
     &         totsumscal(17)
         write (31, 2050) '2-component of angular momentum =', 
     &         totsumscal(18)
         write (31, 2050) '3-component of angular momentum =', 
     &         totsumscal(19)

         write (31, 2050) 'rms velocity in 1-direction     =',
     &         totsumscal(23)
         write (31, 2050) 'rms velocity in 2-direction     =',
     &         totsumscal(24)
         write (31, 2050) 'rms velocity in 3-direction     =',
     &         totsumscal(25)
         write (31, 2050) 'rms velocity                    =',
     &         totsumscal(26)
         write (31, 2060) 'minimum density                 =', 
     &         totminscal(1,1)
         write (31, 2060) 'maximum density                 =', 
     &         totmaxscal(1,1)

         write (31, 2060) 'minimum internal energy density =', 
     &         totminscal(1,2)
         write (31, 2060) 'maximum internal energy density =', 
     &         totmaxscal(1,2)

c         write (31, 2060) 'minimum thermal pressure        =', 
c     &         totminscal(1,3), imin(3), jmin(3), kmin(3)
c         write (31, 2060) 'maximum thermal pressure        =', 
c     &         totmaxscal(1,3), imax(3), jmax(3), kmax(3)
         write (31, 2060) 'minimum 1-velocity              =', 
     &         totminscal(1,4)
         write (31, 2060) 'maximum 1-velocity              =', 
     &         totmaxscal(1,4)
         write (31, 2060) 'minimum 2-velocity              =', 
     &         totminscal(1,5)
         write (31, 2060) 'maximum 2-velocity              =', 
     &         totmaxscal(1,5)
         write (31, 2060) 'minimum 3-velocity              =', 
     &         totminscal(1,6)
         write (31, 2060) 'maximum 3-velocity              =', 
     &         totmaxscal(1,6)
         write (31, 2060) 'minimum speed (1-2 plane)       =', 
     &         totminscal(1,7)
         write (31, 2060) 'maximum speed (1-2 plane)       =', 
     &         totmaxscal(1,7)

         write (31, 2060) 'minimum 1-magnetic field        =', 
     &         totminscal(1,8)
         write (31, 2060) 'maximum 1-magnetic field        =', 
     &         totmaxscal(1,8)
         write (31, 2060) 'minimum 2-magnetic field        =', 
     &         totminscal(1,9)
         write (31, 2060) 'maximum 2-magnetic field        =', 
     &         totmaxscal(1,9)
         write (31, 2060) 'minimum 3-magnetic field        =', 
     &         totminscal(1,10)
         write (31, 2060) 'maximum 3-magnetic field        =', 
     &         totmaxscal(1,10)
         write (31, 2060) 'minimum magnetic field strength =', 
     &         totminscal(1,11)
         write (31, 2060) 'maximum magnetic field strength =', 
     &         totmaxscal(1,11)
         write (31, 2060) 'minimum normalised div(B)       =', 
     &         totminscal(1,12)
         write (31, 2060) 'maximum normalised div(B)       =', 
     &         totmaxscal(1,12)
         write (31, 2050) 'Alfven time step                =', mdtal


         write (31, 2050) 'sound speed time step           =', mdtcs
         write (31, 2050) 'v1 time step                    =', mdtv1
         write (31, 2050) 'v2 time step                    =', mdtv2
         write (31, 2050) 'v3 time step                    =', mdtv3
         write (31, 2050) 'artificial viscosity time step  =',mdtqqi2
         write (31, 2050) 'time step for next cycle        =', dtnew
      endif
c
c-----------------------------------------------------------------------
c----------------------- Write format statements -----------------------
c-----------------------------------------------------------------------
c

2040   format('TSLICE  :',/
     1       ,'TSLICE  : ----------------------------------------------'
     2       ,'------------------------',/
     3       ,'TSLICE  : ------- ',a7,1pe12.5,a10,i5,a7,e12.5
     4       ,' --------',/
     5       ,'TSLICE  : ----------------------------------------------'
     6       ,'------------------------')
2050   format('TSLICE  : ',a33,1pe13.5e3)
2060   format('TSLICE  : ',a33,1pe13.5e3)
2070   format('TSLICE  : Data appended to file ',a8,'  at time ='
     1       ,1pe12.5,', nhy =',i6)
2080   format('TSLICE  : **** WARNING **** >',i6,' plot entries '
     1       ,'requested at time =',1pe12.5,/
     2       ,'TSLICE  : nhy =',i6,'.  Data thinned, resetting dttslp '
     3       ,'to ',e12.5,'.')
2090   format('TSLICE  : Data appended to plot arrays    at time ='
     1       ,1pe12.5,', nhy =',i6)

c
c-----------------------------------------------------------------------
c



      return
      end
