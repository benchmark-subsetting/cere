












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 A V I S C                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine avisc (ibeg,iend,jbeg,jend,kbeg,kend,dvdxmn
     &                  ,w1,w2,w3,u1,u2,u3,s1,s2,s3)
c
c    mln:zeus3d.viscous <------------- artificial viscosity source terms
c                                                        ?????????, 19??
c
c    written by: Mike Norman
c    modified 1: June, 1988 by Jim Stone; incorporated into ZEUS2D
c    modified 2: February, 1990 by David Clarke; incorporated into
c                ZEUS3D
c    modified 3: June, 1992 by David Clarke; expunged "ISMIN", thereby
c                decreasing execution time by 30%.
c    modified 4: Oct., 1994 by Robert Fiedler to run in parallel on SGIs
c    modified 5: Totally rewritten 2/27/96 by RAF for ZEUS-MP.
c
c  PURPOSE: Computes the artificial viscosity source terms in the
c  momentum and energy equations.  i.e., it computes
c
c             dv / dt = -DIV(Q) / rho             for v1, v2, and v3
c      and    de / dt = -Q * delta(v) / delta(x)
c
c  This routine uses the von Neumann-Richtmyer form of the artificial
c  viscosity.  This means that geometric terms are not included in
c  DIV(Q).
c
c
c  LOCAL VARIABLES:
c    qqs        diagonal elements of viscous tensor.  Thus, this is a
c               linear treatment of the artificial viscosity.
c    dvelb      v1(i+1,j,k)-v1(i,j,k) for i-sweep
c               v2(i,j+1,k)-v2(i,j,k) for j-sweep
c               v3(i,j,k+1)-v3(i,j,k) for k-sweep
c    dvela      min ( zro, dvelb ) - ensures that only compressional
c               waves (shocks) are affected.
c    dvdxmn     min ( ( delta(v1) / delta(x1) ),
c                     ( delta(v2) / delta(x2) ),
c                     ( delta(v3) / delta(x3) ) )
c    w1,w2,w3   velocity values prior to viscosity update.
c    u1,u2,u3   velocity values after    viscosity update.
c    s1,s2,s3   updated momentum densities for transport step.
c
c BOUNDARY VALUES USED:
c
c  Macro defined  var   ii    oi    ij    oj    ik    ok
c  -------------  ---  ----  ----  ----  ----  ----  ----
c                  d   is-1        js-1        ks-1
c                  u1  is-1  ie+1
c                  u2              js-1  je+1
c                  u3                          ks-1  ke+1
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
       integer i,j,k, ibeg,iend,jbeg,jend,kbeg,kend
       real*8    q1,q3, g3i
       real*8    qqs,qqsm, dvelas,dvelasm, dvdxmn
       real*8    w1(in,jn,kn),w2(in,jn,kn),w3(in,jn,kn)
     &        ,u1(in,jn,kn),u2(in,jn,kn),u3(in,jn,kn)
     &        ,s1(in,jn,kn),s2(in,jn,kn),s3(in,jn,kn)

c
c-----------------------------------------------------------------------
c
c      Start von Neumann-Richtmyer artificial viscosity update.
c

       q1     = dt * qcon

c
c Do all the work in one big loop so that d and e will stay in cache.
c
         do 160 k=kbeg,kend
           do 150 j=jbeg,jend
cremoved for SPEC CPU2006 CDIR$ UNROLL 4
             do 140 i=ibeg,iend
c
c      Do v1.
c
                 dvelasm = min ( zro, w1(i  ,j,k) - w1  (i-1,j,k) )
                 dvelas  = min ( zro, w1(i+1,j,k) - w1  (i  ,j,k) )
                 qqsm    = q1 * d(i-1,j,k) * dvelasm * dvelasm
                 qqs     = q1 * d(i  ,j,k) * dvelas  * dvelas
                 q3      = dvelas  * dx1ai(i)
                 dvdxmn  = min ( dvdxmn, q3 )

                 e  (i,j,k) = e(i,j,k) - q3 * qqs

                 u1 (i,j,k) = w1(i,j,k)
     1                      - ( qqs  - qqsm   ) * dx1bi(i)
     2                      * 2.0 / ( d(i-1,j,k) + d(i,j,k) )
c
c      Do v2.
c
                 dvelasm = min ( zro, w2(i,j  ,k) - w2  (i,j-1,k) )
                 dvelas  = min ( zro, w2(i,j+1,k) - w2  (i,j  ,k) )
                 qqsm    = q1 * d(i,j-1,k) * dvelasm  * dvelasm
                 qqs     = q1 * d(i,j  ,k) * dvelas   * dvelas
                 q3      = dvelas   * dx2ai(j) * g2bi(i)
                 dvdxmn  = min ( dvdxmn, q3 )

                 e  (i,j,k) = e(i,j,k) - q3 * qqs

                 u2 (i,j,k) = w2(i,j,k)
     1                      - ( qqs   - qqsm    ) * dx2bi(j) * g2bi(i)
     2                      * 2.0 / ( d(i,j-1,k) + d(i,j,k) )
c
c      Do v3.
c
                 g3i     = g31bi(i) * g32bi(j)
                 dvelasm = min ( zro, w3(i,j,k  ) - w3  (i,j,k-1) )
                 dvelas  = min ( zro, w3(i,j,k+1) - w3  (i,j,k  ) )
                 qqsm    = q1 * d(i,j,k-1) * dvelasm  * dvelasm
                 qqs     = q1 * d(i,j,k  ) * dvelas   * dvelas
                 q3      = dvelas   * dx3ai(k) * g3i
                 dvdxmn  = min ( dvdxmn, q3 )

                 e  (i,j,k) = e(i,j,k) - q3 * qqs

                 u3 (i,j,k) = w3(i,j,k)
     1                      - ( qqs   - qqsm    ) * dx3bi(k) * g3i
     2                      *  2.0 / ( d(i,j,k-1) + d(i,j,k) )
c
c Save the updated momentum densities for the transport step, since 
c everything should be in the cache already.
c
               s1(i,j,k) = u1(i,j,k) * 0.5 * (d(i-1,j  ,k  ) + d(i,j,k))
               s2(i,j,k) = u2(i,j,k) * 0.5 * (d(i  ,j-1,k  ) + d(i,j,k))
     1                   * g2b(i)
               s3(i,j,k) = u3(i,j,k) * 0.5 * (d(i  ,j  ,k-1) + d(i,j,k))
     1                   * g31b(i) * g32b(j)
140          continue
150        continue
160      continue
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 A V I S C                 \\\\\\\\\\
c
c=======================================================================
c
c
