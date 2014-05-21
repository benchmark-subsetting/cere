












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                F O R C E S                \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine forces (ibeg,iend,jbeg,jend,kbeg,kend
     &                   ,u1,u2,u3,w1,w2,w3)

c
c
c Computes Pressure, Gravity, and Pseudo-Rotational Forces.

c   and includes magnetic pressure forces (longitudinal Lorentz forces)

c
c Arrays u1 , u2 , u3  hold the old velocity values, while
c        w1 , w2 , w3  receive the updated values.
c
c BOUNDARY VALUES USED:
c
c  Macro defined  var   ii    oi    ij    oj    ik    ok
c  -------------  ---  ----  ----  ----  ----  ----  ----
c                  d   is-1        js-1        ks-1
c  ZRP             d   is-1        js-1        ks-1  ke+1
c  RTP             d   is-1        js-1  je+1  ks-1  ke+1
c                  e   is-1        js-1        ks-1
c  TOTAL_ENERGY    u1  is-1  ie+1  js-1        ks-1
c  RTP             u2  is-1              je+1
c  TOTAL_ENERGY    u2  is-1        js-1  je+1  ks-1
c  TOTAL_ENERGY    u3  is-1        js-1        ks-1  ke+1
c  ZRP             u3              js-1              ke+1
c  RTP             u3  is-1        js-1              ke+1
c  RAD || RAD_EXP  er  is-1        js-1        ks-1
c               b1  is-1  ie+1  js-1  je+1  ks-1  ke+1
c               b2  is-1  ie+1  js-1  je+1  ks-1  ke+1
c               b3  is-1  ie+1  js-1  je+1  ks-1  ke+1
c Written by RAF; modified 3/13/97 (JCH); 
c     modified 2 Mar 1998 to add  (M-MML)
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

      real*8   d (in,jn,kn), e (in,jn,kn),
     1       v1(in,jn,kn), v2(in,jn,kn), v3(in,jn,kn)

      real*8   b1(in,jn,kn), b2(in,jn,kn), b3(in,jn,kn)




      common /fieldr/  d, e, v1, v2, v3

      common /fieldr/  b1, b2, b3




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

       integer ibeg,iend,jbeg,jend,kbeg,kend
       integer i,j,k
       real*8 u1 (in,jn,kn),u2 (in,jn,kn),u3 (in,jn,kn)
     &     ,w1 (in,jn,kn),w2 (in,jn,kn),w3 (in,jn,kn)
       real*8 poo(in),pmo(in),pom(in)
       real*8 st,rhoi




       real*8 d1b2oo(in), d1b3oo(in), d2b1oo(in), d2b3oo(in)
     &     ,d3b1oo(in), d3b2oo(in)
       real*8 d1b2po(in), d1b3op(in), d2b1po(in), d2b3op(in)
     &     ,d3b1po(in), d3b2op(in)




c
c-----------------------------------------------------------------------
c
c Main loop for pressure, magnetic pressure, self-gravity, and
c     rotational pseudo-forces.  
c
c Active zones for all velocity components are (is:ie, js:je, ks:ke).
c
c In its most general configuration, this loop needs 1 layer of 
c boundary data for each of d, e, u1, u2, u3, and gp.
c
c If GRAV or GRAV_FFT is not defined, we don't need gp.
c If ISO  is     defined, we don't need e .
c
c For  defined and TOTAL_ENERGY not defined, we don't need u1,u2,u3
c boundary values.  All we do need in this case is the inner boundary 
c data for d (and e).
c
c Of course, TOTAL_ENERGY and ISO are mutually exclusive. 
c
c **> at the moment, TOTAL_ENERGY and  also can't be used together
c       (M-MML 4 Mar 98)
c
c#if defined RAD || defined RAD_EXP
c       call opacity(e, d, gamma, kap, sig, dkapdt,
c     .              ibeg-1, iend, jbeg, jend, kbeg, kend)
c#endif 
c
       do 40 k=kbeg,kend
         do 30 j=jbeg,jend
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
           do 10 i=ibeg-1,iend
c
c Compute thermal pressure
c


               poo(i)         = gamm1 * ( e(i  ,j  ,k  ) + efloor )
               pmo(i)         = gamm1 * ( e(i  ,j-1,k  ) + efloor )
               pom(i)         = gamm1 * ( e(i  ,j  ,k-1) + efloor )



c.......................................................................
c   


 10        continue
c.......................................................................

c
c        Compute differences in the squares of the magnetic field
c        components for the longitudinal Lorentz forces.
c
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
           do 11 i=ibeg-1,iend
              d1b2oo(i) = ( ( g2b (i  ) * b2(i  ,j  ,k  ) )**2
     1                    - ( g2b (i-1) * b2(i-1,j  ,k  ) )**2 )
     2                     * g2ai (i) * g2ai (i)
              d1b2po(i) = ( ( g2b (i  ) * b2(i  ,j+1,k  ) )**2
     1                    - ( g2b (i-1) * b2(i-1,j+1,k  ) )**2 )
     2                     * g2ai (i) * g2ai (i)
              d1b3oo(i) = ( ( g31b(i  ) * b3(i  ,j  ,k  ) )**2
     1                    - ( g31b(i-1) * b3(i-1,j  ,k  ) )**2 )
     2                     * g31ai(i) * g31ai(i)
              d1b3op(i) = ( ( g31b(i  ) * b3(i  ,j  ,k+1) )**2
     1                    - ( g31b(i-1) * b3(i-1,j  ,k+1) )**2 )
     2                     * g31ai(i) * g31ai(i)
              d2b3oo(i) = ( ( g32b(j  ) * b3(i  ,j  ,k  ) )**2
     1                    - ( g32b(j-1) * b3(i  ,j-1,k  ) )**2 )
     2                     * g32ai(j) * g32ai(j)
              d2b3op(i) = ( ( g32b(j  ) * b3(i  ,j  ,k+1) )**2
     1                    - ( g32b(j-1) * b3(i  ,j-1,k+1) )**2 )
     2                     * g32ai(j) * g32ai(j)
              d2b1oo(i) = b1(i  ,j  ,k  ) * b1(i  ,j  ,k  )
     1                  - b1(i  ,j-1,k  ) * b1(i  ,j-1,k  )
              d2b1po(i) = b1(i+1,j  ,k  ) * b1(i+1,j  ,k  )
     1                  - b1(i+1,j-1,k  ) * b1(i+1,j-1,k  )
              d3b1oo(i) = b1(i  ,j  ,k  ) * b1(i  ,j  ,k  )
     1                  - b1(i  ,j  ,k-1) * b1(i  ,j  ,k-1)
              d3b1po(i) = b1(i+1,j  ,k  ) * b1(i+1,j  ,k  )
     1                  - b1(i+1,j  ,k-1) * b1(i+1,j  ,k-1)
              d3b2oo(i) = b2(i  ,j  ,k  ) * b2(i  ,j  ,k  )
     1                  - b2(i  ,j  ,k-1) * b2(i  ,j  ,k-1)
              d3b2op(i) = b2(i  ,j+1,k  ) * b2(i  ,j+1,k  )
     1                  - b2(i  ,j+1,k-1) * b2(i  ,j+1,k-1)
11         continue


CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
           do 20 i=ibeg,iend
c.......................................................................
c
c Perform an explicit update for v1
c
             rhoi    = 2.0 / ( d(i-1,j,k) + d(i,j,k) )
c
c  1.  pressure gradient
c
             st      = - rhoi
     1               * ( poo(i)    - poo(i-1)    ) * dx1bi(i)




c

c
c  6. Magnetic pressure (longitudinal Lorentz force)
c
             st = st - (d1b2oo(i) + d1b2po(i) + d1b3oo(i) + d1b3op(i))
     3                 * rhoi * dx1bi(i) * 0.25


c
             w1(i,j,k) = u1(i,j,k) + dt * st
c.......................................................................
c
c Perform an explicit update for v2 
c
             rhoi    = 2.0 / ( d(i,j-1,k) + d(i,j,k) )
c
c  1.  pressure gradient
c
             st      = - rhoi
     1               * ( poo(i)     - pmo(i)     ) * dx2bi(j)
     2               * g2bi(i)




c

c
c  6. Magnetic pressure (longitudinal Lorentz force)
c
             st = st - (d2b3oo(i) + d2b3op(i) + d2b1oo(i) + d2b1po(i))
     3               * rhoi * dx2bi(j) * g2bi(i) * 0.25


c
             w2(i,j,k) = u2(i,j,k) + dt * st
c
c.......................................................................
c
c Perform an explicit update for v3
c
             rhoi    = 2.0 / ( d(i,j,k-1) + d(i,j,k) )
c
c  1.  pressure gradient
c
             st      = - rhoi
     1               * ( poo(i)    - pom(i)      ) * dx3bi(k)
     2               * g31bi(i) * g32bi(j)



c

c
c  6. Magnetic pressure (longitudinal Lorentz force)
c
             st = st - (d3b1oo(i) + d3b1po(i) + d3b2oo(i) + d3b2op(i))
     3               * rhoi * dx3bi(k) * g31bi(i) * g32bi(j) * 0.25


c
             w3(i,j,k) = u3(i,j,k) + dt * st
20         continue
30       continue
40     continue
c

       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                F O R C E S                \\\\\\\\\\
c
c=======================================================================
c
c
