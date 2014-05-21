












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 M O M X 3                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine momx3 ( ibeg,iend,jbeg,jend,kbeg,kend
     &                  , s1,s2,s3,mflx )
c
c    dac:zeus3d.momx3 <--------------- transports momenta in 3-direction
c                                                         february, 1990
c
c    written by: David Clarke
c    modified 1: November, 1992 by David Clarke; momenta are now updated
c                between and including i=is,ie, j=js,je, and k=ks,ke to
c                allow for proper treatment of periodic boundaries.
c    modified 2: Feb. 22, 1996 by Robert Fiedler; completely rewritten
c                for ZEUS-MP.
c    modified 3: July 6, 1996 by Robert Fiedler; blocked the k-loop
c                to improve performance on systems with a small 128.
c    modified 4: Aug. 2, 1996 by Robert Fiedler; unrolled the i-loop
c                to improve performance on systems with a small cache.
c
c  PURPOSE:  Transports the three components of the momentum density in
c  the 3-direction using the consistent transport algorithm, including
c  the effects of grid compression.  The transported fluxes are thus
c  given by the mass fluxes times the time centred area of the control
c  volume faces times the interpolated velocities.  Interpolations are
c  performed in-line.
c
c  INPUT VARIABLES:
c    mflx    mass flux in 3-direction
c    s1      momentum density in 1-direction
c    s2      momentum density in 2-direction
c    s3      momentum density in 3-direction
c
c BOUNDARY VALUES USED:
c
c    Macro defined  var   ii    oi    ij    oj    ik    ok
c    -------------  ---  ----  ----  ----  ----  ----  ----
c                  mflx  is-1        js-1        ks-1  ke+1
c                    u1                          ks-2  ke+2
c                    u2                          ks-2  ke+2
c                    u3  is-1        js-1        ks-2  ke+2
c
c  OUTPUT VARIABLES:
c    s1      momentum density in 1-direction updated in the 3-direction
c    s2      momentum density in 2-direction updated in the 3-direction
c    s3      momentum density in 3-direction updated in the 3-direction
c
c  LOCAL VARIABLES:
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

c
       integer  i,j,k, ibeg,iend,jbeg,jend,kbeg,kend
c
       real*8  xi, q1, q2, vel, dqm,dqp
c
       real*8        atwid1  (ijkn), atwid2 (ijkn), atwid3 (ijkn)
     &           , atwidj1 (ijkn), atwidj2(ijkn), atwidj3(ijkn)
c

       real*8        sflx  (ijkn,4), dq   (ijkn,4)

       integer  iriter

c
       real*8        mflx    (  in,  jn,  kn), s1      (  in,  jn,  kn)
     1           , s2      (  in,  jn,  kn), s3      (  in,  jn,  kn)
c
       equivalence   ( atwid1  , w1da     ), ( atwidj1 , w1dd     )
     1             , ( atwid2  , w1db     ), ( atwidj2 , w1de     )
     2             , ( atwid3  , w1dc     ), ( atwidj3 , w1df     )
c


c.......................................................................
c
c      Compute time-centred area factors.
c
       do 10 i=ibeg,iend
         atwid1(i) = 0.5 * g2a (i) * dx1b(i) * dvl1bi(i)
         atwid2(i) = 0.5 * g2b (i) * g2b (i) * dx1a  (i) * dvl1ai(i)
         atwid3(i) = 0.5 * g31b(i) * g2b (i) * dx1a  (i) * dvl1ai(i)
10     continue
       do 20 j=jbeg,jend
         atwidj1(j) = dx2a(j) * dvl2ai(j)
         atwidj2(j) = dx2b(j) * dvl2bi(j)
         atwidj3(j) = g32b(j) * dx2a  (j) * dvl2ai(j)
20     continue
c
c.......................................................................
c
       do 3000 j=jbeg,jend

c
c Do the iterations which will not be done in unrolled loops first.
c
         iriter = mod(iend-ibeg+1,4)
         do 2000 i=ibeg,ibeg+iriter-1

c
c------------------------------ TRANSPORT S1 ---------------------------
c
c      Interpolate "v1" at the 3-interfaces.
c
c       call x3zc3d ( v1, vel3, is, js, ie, je, iords1, istps1
c     1             , g31a, g31ai, g32b, g32bi, sflx 3, p      )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 1070 k=kbeg-1,kend+1
             dqm        = (v1 (i  ,j,k  ) - v1 (i  ,j,k-1)) * dx3bi(k  )
             dqp        = (v1 (i  ,j,k+1) - v1 (i  ,j,k  )) * dx3bi(k+1)
             dq(k,1)    = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
1070       continue
c
c  2.  Choose time averaged, upwinded interface value.
c
c      Construct an i-average of "v3" to be used for interpolation.
c
c      Construct the 1-momentum flux at the 3-interfaces and perform
c  1-momentum advection.  Note that the timestep "dt" is hidden in the
c  mass flux.
c
           do 1100 k=kbeg,kend+1
             vel         = 0.5 * ( v3(i-1,j,k  ) + v3(i  ,j,k  ) )
              q2         = dt * g31ai(i  ) * g32bi(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,1) = ( 0.5 + q1 ) * ( v1 (i  ,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,1) )
     2                   + ( 0.5 - q1 ) * ( v1 (i  ,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,1) )
              sflx (k,1) = ( mflx (i-1,j,k  ) + mflx (i  ,j,k  ) )
     1                   * sflx (k,1)    * atwid1(i  ) * atwidj1(j)
1100       continue
c
           do 1170 k=kbeg,kend
             s1(i  ,j,k)  = ( s1(i  ,j,k) * dvl3a(k)

     1                    - sflx(k+1,1) + sflx(k,1) ) * dvl3a i(k)

1170       continue
c
c------------------------------ TRANSPORT S2 ---------------------------
c
c      Interpolate "v2" at the 3-interfaces.
c
c       call x3zc3d ( v2, vel3, is, js, ie, je, iords2, istps2
c     1             , g31b, g31bi, g32a, g32ai, sflx 3, p      )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 1270 k=kbeg-1,kend+1
             dqm       = (v2 (i  ,j,k  ) - v2 (i  ,j,k-1)) * dx3bi(k  )
             dqp       = (v2 (i  ,j,k+1) - v2 (i  ,j,k  )) * dx3bi(k+1)
             dq(k,1)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
1270       continue
c
c  2.  Choose time averaged, upwinded interface value.
c
c      Construct a j-average of "v3" to be used for interpolation.
c
c      Construct the 2-momentum flux at the 3-interfaces and perform
c  2-momentum advection.  Note that the timestep "dt" is hidden in the
c  mass flux.
c
           do 1300 k=kbeg,kend+1
             vel         = 0.5 * ( v3(i  ,j-1,k  ) + v3(i  ,j,k  ) )
              q2         = dt * g31bi(i  ) * g32ai(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,1) = ( 0.5 + q1 ) * ( v2 (i  ,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,1) )
     2                   + ( 0.5 - q1 ) * ( v2 (i  ,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,1) )
              sflx (k,1) = ( mflx (i  ,j-1,k  ) + mflx (i  ,j  ,k  ) )
     1                   * sflx (k,1) * atwid2(i  ) * atwidj2(j)
1300       continue
c
           do 1370 k=kbeg,kend
             s2(i  ,j,k) = ( s2(i  ,j,k) * dvl3a(k)

     1                   - sflx(k+1,1) + sflx(k  ,1) ) * dvl3a i(k)

1370       continue
c
c------------------------------ TRANSPORT S3 ---------------------------
c
c
c      Interpolate "v3" at the zone centers.
c
c       call x3fc3d ( v3, vel3, is, js, ie, je, iords3, sflx 3 )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 1470 k=kbeg-1,kend+1
             dqm       = (v3 (i  ,j,k  ) - v3 (i  ,j,k-1)) * dx3ai(k-1)
             dqp       = (v3 (i  ,j,k+1) - v3 (i  ,j,k  )) * dx3ai(k  )
             dq(k,1)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
1470       continue
c
c  2.  Choose time averaged, upwinded interface value.
c
c      Construct a k-average of "v3-vg3" to be used for interpolation.
c
c      Construct the 3-momentum flux at the 3-interfaces and perform
c  3-momentum advection.  Note that the timestep "dt" is hidden in the
c  mass flux.
c
           do 1500 k=kbeg-1,kend
             vel         = 0.5 * ( v3(i  ,j,k  ) - vg3(k  )
     1                           + v3(i  ,j,k+1) - vg3(k+1) )
              xi         = vel         * dt * g31bi(i  ) * g32bi(j)
              q1         = sign ( haf, xi )
              sflx (k,1) = ( 0.5 + q1 ) * ( v3(i  ,j,k  )
     1                   + ( dx3b(k  ) - xi ) * dq(k  ,1) )
     2                   + ( 0.5 - q1 ) * ( v3(i  ,j,k+1)
     3                   - ( dx3b(k+1) + xi ) * dq(k+1,1) )
              sflx (k,1) = ( mflx (i  ,j,k  ) + mflx (i  ,j,k+1) )
     1                   * sflx (k,1) * atwid3(i  ) * atwidj3(j)
1500       continue
c
           do 1570 k=kbeg,kend
             s3(i  ,j,k) = ( s3(i  ,j,k) * dvl3b(k)

     1                   - sflx(k  ,1) + sflx(k-1,1) ) * dvl3b i(k)

1570       continue
2000     continue

c
c Do the bulk of the iterations in unrolled loops.
c
           do 2900 i=ibeg+iriter,iend,4
c
c------------------------------ TRANSPORT S1 ---------------------------
c
c      Interpolate "v1" at the 3-interfaces.
c
c       call x3zc3d ( v1, vel3, is, js, ie, je, iords1, istps1
c     1             , g31a, g31ai, g32b, g32bi, sflx 3, p      )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 2070 k=kbeg-1,kend+1
             dqm        = (v1 (i  ,j,k  ) - v1 (i  ,j,k-1)) * dx3bi(k  )
             dqp        = (v1 (i  ,j,k+1) - v1 (i  ,j,k  )) * dx3bi(k+1)
             dq(k,1)    = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c
             dqm        = (v1 (i+1,j,k  ) - v1 (i+1,j,k-1)) * dx3bi(k  )
             dqp        = (v1 (i+1,j,k+1) - v1 (i+1,j,k  )) * dx3bi(k+1)
             dq(k,2)    = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c

             dqm        = (v1 (i+2,j,k  ) - v1 (i+2,j,k-1)) * dx3bi(k  )
             dqp        = (v1 (i+2,j,k+1) - v1 (i+2,j,k  )) * dx3bi(k+1)
             dq(k,3)    = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c


             dqm        = (v1 (i+3,j,k  ) - v1 (i+3,j,k-1)) * dx3bi(k  )
             dqp        = (v1 (i+3,j,k+1) - v1 (i+3,j,k  )) * dx3bi(k+1)
             dq(k,4)    = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c

2070       continue
c
c  2.  Choose time averaged, upwinded interface value.
c
c      Construct an i-average of "v3" to be used for interpolation.
c
c      Construct the 1-momentum flux at the 3-interfaces and perform
c  1-momentum advection.  Note that the timestep "dt" is hidden in the
c  mass flux.
c
           do 2100 k=kbeg,kend+1
             vel         = 0.5 * ( v3(i-1,j,k  ) + v3(i  ,j,k  ) )
              q2         = dt * g31ai(i  ) * g32bi(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,1) = ( 0.5 + q1 ) * ( v1 (i  ,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,1) )
     2                   + ( 0.5 - q1 ) * ( v1 (i  ,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,1) )
              sflx (k,1) = ( mflx (i-1,j,k  ) + mflx (i  ,j,k  ) )
     1                   * sflx (k,1)    * atwid1(i  ) * atwidj1(j)
c
             vel         = 0.5 * ( v3(i  ,j,k  ) + v3(i+1,j,k  ) )
              q2         = dt * g31ai(i+1) * g32bi(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,2) = ( 0.5 + q1 ) * ( v1 (i+1,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,2) )
     2                   + ( 0.5 - q1 ) * ( v1 (i+1,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,2) )
              sflx (k,2) = ( mflx (i  ,j,k  ) + mflx (i+1,j,k  ) )
     1                   * sflx (k,2)    * atwid1(i+1) * atwidj1(j)
c

             vel         = 0.5 * ( v3(i+1,j,k  ) + v3(i+2,j,k  ) )
              q2         = dt * g31ai(i+2) * g32bi(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,3) = ( 0.5 + q1 ) * ( v1 (i+2,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,3) )
     2                   + ( 0.5 - q1 ) * ( v1 (i+2,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,3) )
              sflx (k,3) = ( mflx (i+1,j,k  ) + mflx (i+2,j,k  ) )
     1                   * sflx (k,3)    * atwid1(i+2) * atwidj1(j)
c


             vel         = 0.5 * ( v3(i+2,j,k  ) + v3(i+3,j,k  ) )
              q2         = dt * g31ai(i+3) * g32bi(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,4) = ( 0.5 + q1 ) * ( v1 (i+3,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,4) )
     2                   + ( 0.5 - q1 ) * ( v1 (i+3,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,4) )
              sflx (k,4) = ( mflx (i+2,j,k  ) + mflx (i+3,j,k  ) )
     1                   * sflx (k,4)    * atwid1(i+3) * atwidj1(j)
c

2100       continue
c
           do 2170 k=kbeg,kend
             s1(i  ,j,k)  = ( s1(i  ,j,k) * dvl3a(k)

     1                    - sflx(k+1,1) + sflx(k,1) ) * dvl3a i(k)

c
             s1(i+1,j,k)  = ( s1(i+1,j,k) * dvl3a(k)

     1                    - sflx(k+1,2) + sflx(k,2) ) * dvl3a i(k)

c

             s1(i+2,j,k)  = ( s1(i+2,j,k) * dvl3a(k)

     1                    - sflx(k+1,3) + sflx(k,3) ) * dvl3a i(k)

c


             s1(i+3,j,k)  = ( s1(i+3,j,k) * dvl3a(k)

     1                    - sflx(k+1,4) + sflx(k,4) ) * dvl3a i(k)

c

2170       continue
c
c------------------------------ TRANSPORT S2 ---------------------------
c
c      Interpolate "v2" at the 3-interfaces.
c
c       call x3zc3d ( v2, vel3, is, js, ie, je, iords2, istps2
c     1             , g31b, g31bi, g32a, g32ai, sflx 3, p      )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 2270 k=kbeg-1,kend+1
             dqm       = (v2 (i  ,j,k  ) - v2 (i  ,j,k-1)) * dx3bi(k  )
             dqp       = (v2 (i  ,j,k+1) - v2 (i  ,j,k  )) * dx3bi(k+1)
             dq(k,1)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c
             dqm       = (v2 (i+1,j,k  ) - v2 (i+1,j,k-1)) * dx3bi(k  )
             dqp       = (v2 (i+1,j,k+1) - v2 (i+1,j,k  )) * dx3bi(k+1)
             dq(k,2)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c

             dqm       = (v2 (i+2,j,k  ) - v2 (i+2,j,k-1)) * dx3bi(k  )
             dqp       = (v2 (i+2,j,k+1) - v2 (i+2,j,k  )) * dx3bi(k+1)
             dq(k,3)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c


             dqm       = (v2 (i+3,j,k  ) - v2 (i+3,j,k-1)) * dx3bi(k  )
             dqp       = (v2 (i+3,j,k+1) - v2 (i+3,j,k  )) * dx3bi(k+1)
             dq(k,4)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c

2270       continue
c
c  2.  Choose time averaged, upwinded interface value.
c
c      Construct a j-average of "v3" to be used for interpolation.
c      Construct the 2-momentum flux at the 3-interfaces and perform
c
c  2-momentum advection.  Note that the timestep "dt" is hidden in the
c  mass flux.
c
           do 2300 k=kbeg,kend+1
             vel         = 0.5 * ( v3(i  ,j-1,k  ) + v3(i  ,j,k  ) )
              q2         = dt * g31bi(i  ) * g32ai(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,1) = ( 0.5 + q1 ) * ( v2 (i  ,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,1) )
     2                   + ( 0.5 - q1 ) * ( v2 (i  ,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,1) )
              sflx (k,1) = ( mflx (i  ,j-1,k  ) + mflx (i  ,j  ,k  ) )
     1                   * sflx (k,1) * atwid2(i  ) * atwidj2(j)
c
             vel         = 0.5 * ( v3(i+1,j-1,k  ) + v3(i+1,j,k  ) )
              q2         = dt * g31bi(i+1) * g32ai(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,2) = ( 0.5 + q1 ) * ( v2 (i+1,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,2) )
     2                   + ( 0.5 - q1 ) * ( v2 (i+1,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,2) )
              sflx (k,2) = ( mflx (i+1,j-1,k  ) + mflx (i+1,j  ,k  ) )
     1                   * sflx (k,2) * atwid2(i+1) * atwidj2(j)
c

             vel         = 0.5 * ( v3(i+2,j-1,k  ) + v3(i+2,j,k  ) )
              q2         = dt * g31bi(i+2) * g32ai(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,3) = ( 0.5 + q1 ) * ( v2 (i+2,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,3) )
     2                   + ( 0.5 - q1 ) * ( v2 (i+2,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,3) )
              sflx (k,3) = ( mflx (i+2,j-1,k  ) + mflx (i+2,j  ,k  ) )
     1                   * sflx (k,3) * atwid2(i+2) * atwidj2(j)
c


             vel         = 0.5 * ( v3(i+3,j-1,k  ) + v3(i+3,j,k  ) )
              q2         = dt * g31bi(i+3) * g32ai(j)
              xi         = ( vel           - vg3(k  ) ) * q2
              q1         = sign ( haf, xi )
              sflx (k,4) = ( 0.5 + q1 ) * ( v2 (i+3,j,k-1)
     1                   + ( dx3a(k-1) - xi ) * dq (k-1,4) )
     2                   + ( 0.5 - q1 ) * ( v2 (i+3,j,k  )
     3                   - ( dx3a(k  ) + xi ) * dq (k  ,4) )
              sflx (k,4) = ( mflx (i+3,j-1,k  ) + mflx (i+3,j  ,k  ) )
     1                   * sflx (k,4) * atwid2(i+3) * atwidj2(j)
c

2300       continue
c
           do 2370 k=kbeg,kend
             s2(i  ,j,k) = ( s2(i  ,j,k) * dvl3a(k)

     1                   - sflx(k+1,1) + sflx(k  ,1) ) * dvl3a i(k)

c
             s2(i+1,j,k) = ( s2(i+1,j,k) * dvl3a(k)

     1                   - sflx(k+1,2) + sflx(k  ,2) ) * dvl3a i(k)

c

             s2(i+2,j,k) = ( s2(i+2,j,k) * dvl3a(k)

     1                   - sflx(k+1,3) + sflx(k  ,3) ) * dvl3a i(k)

c


             s2(i+3,j,k) = ( s2(i+3,j,k) * dvl3a(k)

     1                   - sflx(k+1,4) + sflx(k  ,4) ) * dvl3a i(k)

c

2370       continue
c
c------------------------------ TRANSPORT S3 ---------------------------
c
c
c      Interpolate "v3" at the zone centers.
c
c       call x3fc3d ( v3, vel3, is, js, ie, je, iords3, sflx 3 )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 2470 k=kbeg-1,kend+1
             dqm       = (v3 (i  ,j,k  ) - v3 (i  ,j,k-1)) * dx3ai(k-1)
             dqp       = (v3 (i  ,j,k+1) - v3 (i  ,j,k  )) * dx3ai(k  )
             dq(k,1)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c
             dqm       = (v3 (i+1,j,k  ) - v3 (i+1,j,k-1)) * dx3ai(k-1)
             dqp       = (v3 (i+1,j,k+1) - v3 (i+1,j,k  )) * dx3ai(k  )
             dq(k,2)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c

             dqm       = (v3 (i+2,j,k  ) - v3 (i+2,j,k-1)) * dx3ai(k-1)
             dqp       = (v3 (i+2,j,k+1) - v3 (i+2,j,k  )) * dx3ai(k  )
             dq(k,3)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c


             dqm       = (v3 (i+3,j,k  ) - v3 (i+3,j,k-1)) * dx3ai(k-1)
             dqp       = (v3 (i+3,j,k+1) - v3 (i+3,j,k  )) * dx3ai(k  )
             dq(k,4)   = max ( dqm * dqp, zro )
     1                 * sign ( one, dqm + dqp )
     2                 / max ( abs ( dqm + dqp ), tiny )
c

2470       continue
c
c  2.  Choose time averaged, upwinded interface value.
c
c      Construct a k-average of "v3-vg3" to be used for interpolation.
c
c      Construct the 3-momentum flux at the 3-interfaces and perform
c  3-momentum advection.  Note that the timestep "dt" is hidden in the
c  mass flux.
c
           do 2500 k=kbeg-1,kend
             vel         = 0.5 * ( v3(i  ,j,k  ) - vg3(k  )
     1                           + v3(i  ,j,k+1) - vg3(k+1) )
              xi         = vel         * dt * g31bi(i  ) * g32bi(j)
              q1         = sign ( haf, xi )
              sflx (k,1) = ( 0.5 + q1 ) * ( v3(i  ,j,k  )
     1                   + ( dx3b(k  ) - xi ) * dq(k  ,1) )
     2                   + ( 0.5 - q1 ) * ( v3(i  ,j,k+1)
     3                   - ( dx3b(k+1) + xi ) * dq(k+1,1) )
              sflx (k,1) = ( mflx (i  ,j,k  ) + mflx (i  ,j,k+1) )
     1                   * sflx (k,1) * atwid3(i  ) * atwidj3(j)
c
             vel         = 0.5 * ( v3(i+1,j,k  ) - vg3(k  )
     1                           + v3(i+1,j,k+1) - vg3(k+1) )
              xi         = vel         * dt * g31bi(i+1) * g32bi(j)
              q1         = sign ( haf, xi )
              sflx (k,2) = ( 0.5 + q1 ) * ( v3(i+1,j,k  )
     1                   + ( dx3b(k  ) - xi ) * dq(k  ,2) )
     2                   + ( 0.5 - q1 ) * ( v3(i+1,j,k+1)
     3                   - ( dx3b(k+1) + xi ) * dq(k+1,2) )
              sflx (k,2) = ( mflx (i+1,j,k  ) + mflx (i+1,j,k+1) )
     1                   * sflx (k,2) * atwid3(i+1) * atwidj3(j)
c

             vel         = 0.5 * ( v3(i+2,j,k  ) - vg3(k  )
     1                           + v3(i+2,j,k+1) - vg3(k+1) )
              xi         = vel         * dt * g31bi(i+2) * g32bi(j)
              q1         = sign ( haf, xi )
              sflx (k,3) = ( 0.5 + q1 ) * ( v3(i+2,j,k  )
     1                   + ( dx3b(k  ) - xi ) * dq(k  ,3) )
     2                   + ( 0.5 - q1 ) * ( v3(i+2,j,k+1)
     3                   - ( dx3b(k+1) + xi ) * dq(k+1,3) )
              sflx (k,3) = ( mflx (i+2,j,k  ) + mflx (i+2,j,k+1) )
     1                   * sflx (k,3) * atwid3(i+2) * atwidj3(j)
c


             vel         = 0.5 * ( v3(i+3,j,k  ) - vg3(k  )
     1                           + v3(i+3,j,k+1) - vg3(k+1) )
              xi         = vel         * dt * g31bi(i+3) * g32bi(j)
              q1         = sign ( haf, xi )
              sflx (k,4) = ( 0.5 + q1 ) * ( v3(i+3,j,k  )
     1                   + ( dx3b(k  ) - xi ) * dq(k  ,4) )
     2                   + ( 0.5 - q1 ) * ( v3(i+3,j,k+1)
     3                   - ( dx3b(k+1) + xi ) * dq(k+1,4) )
              sflx (k,4) = ( mflx (i+3,j,k  ) + mflx (i+3,j,k+1) )
     1                   * sflx (k,4) * atwid3(i+3) * atwidj3(j)
c

2500       continue
c
           do 2570 k=kbeg,kend
             s3(i  ,j,k) = ( s3(i  ,j,k) * dvl3b(k)

     1                   - sflx(k  ,1) + sflx(k-1,1) ) * dvl3b i(k)

c
             s3(i+1,j,k) = ( s3(i+1,j,k) * dvl3b(k)

     1                   - sflx(k  ,2) + sflx(k-1,2) ) * dvl3b i(k)

c

             s3(i+2,j,k) = ( s3(i+2,j,k) * dvl3b(k)

     1                   - sflx(k  ,3) + sflx(k-1,3) ) * dvl3b i(k)

c


             s3(i+3,j,k) = ( s3(i+3,j,k) * dvl3b(k)

     1                   - sflx(k  ,4) + sflx(k-1,4) ) * dvl3b i(k)

c

2570       continue
2900     continue

c
3000   continue
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 M O M X 3                 \\\\\\\\\\
c
c=======================================================================
c
c
