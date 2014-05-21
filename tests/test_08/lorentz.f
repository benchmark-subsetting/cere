












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////               L O R E N T Z               \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
c modified for SPEC CPU2006
c       subroutine lorentz (ibeg,iend,jbeg,jend,kbeg,kend
c     &                   ,u1,u2,u3,w1,w2,w3)
       subroutine lorentz (ibeg,iend,jbeg,jend,kbeg,kend
     &                   ,u1,u2,u3)


c
c    dac:zeus3d.lorentz <----- MoC estimate of transverse lorentz forces
c                                                          october, 1992
c
c    written by: David Clarke
c    modified 1: Mordecai-Mark Mac Low, December 1997 - March 1998
c                rewritten for ZEUS-MP 
c
c  PURPOSE:  Uses the Method of Characteristics (MoC) to compute the
c  transverse components of the Lorentz force and then accelerate the
c  velocities accordingly.  After a suggestion by John Hawley, these
c  source terms are computed separately from the MoC estimate of the
c  emfs (MOCEMFS) and before the magnetic field update.  This improves
c  stability of multi-dimensional Alfven waves.
c
c  MOCEMFS solves the induction equation without the use of operator
c  splitting.  Thus, the relevant characteristic velocities are v+/-va
c  and includes the flow velocity v (from the advection term v grad(B))
c  and the alfven velocity va (from the induction term va grad(B)).
c  Conversely, Euler's equation is operator split, and the advection
c  term (v grad(v)) is handled elsewhere (MOMX*).  Thus, the effective
c  characteristic velocities when estimating the transverse Lorentz
c  terms is simply +/-va (from the induction term va grad(v)).
c
c  See comments in MOCEMFS for further ideas regarding the Method of
c  Characteristics.
c
c  INPUT VARIABLES:
c
c    u[1,2,3] = old velocities
c
c  OUTPUT VARIABLES:
c
c    w[1,2,3] = velocities from ibeg to iend, jbeg to jend, kbeg to kend
c
c  LOCAL VARIABLES:
c    srd[n]    sqrt of spatially averaged density at [n]-face n=1,2,3
c    bave      spatially averaged density at edge
c    srdpi     1.0/srd[n] along the plus  alfven characteristic (A+)
c    srdmi     1.0/srd[n] along the minus alfven characteristic (A-)
c    valp      characteristic velocity along A+ (-va)
c    valm      characteristic velocity along A- (+va)
c    vpal      velocity interpolated to the time-centred footpoint of A+
c    vmal      velocity interpolated to the time-centred footpoint of A-
c    bpal      B-field  interpolated to the time-centred footpoint of A+
c    bmal      B-field  interpolated to the time-centred footpoint of A-
c    bstar     MoC estimate of b[n] used to evaluate st[n], n=1,2,3
c    st[n]     Transverse Lorentz force on the [n]-velocity, n=1,2,3
c
c  EXTERNALS:
c    
c   -----> these are now inlined (M-MML): X1ZC1D  , X2ZC1D  , X3ZC1D
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

c
       integer       i       , j       , k      
       integer       ibeg,iend,jbeg,jend,kbeg,kend
       real*8          u1 (in,jn,kn),u2 (in,jn,kn),u3 (in,jn,kn)
commented out for SPEC CPU2006
c     &              ,w1 (in,jn,kn),w2 (in,jn,kn),w3 (in,jn,kn)

       real*8          absb ,q1, qv1, qv2, qb1, qb2, q3, dqm, dqp
     &              , xi, fact 
     &              , dv(ijkn), db(ijkn)
c
       real*8          bave    (ijkn), srdpi   (ijkn), srdmi   (ijkn)
     1             , valp    (ijkn), valm    (ijkn), vtmp    (ijkn)
     1             , btmp    (ijkn), vpal    (ijkn), vmal    (ijkn)
     1             , bpal    (ijkn), bmal    (ijkn), bstar   (ijkn)
c
       real*8          st1     (  in,  jn,  kn), st2  (  in,  jn,  kn)
     1             , st3     (  in,  jn,  kn), srd1 (  in,  jn,  kn)
     1             , srd2    (  in,  jn,  kn), srd3 (  in,  jn,  kn)
c
       equivalence   ( bave    , w1da     ), ( srdpi   , w1db     )
     1             , ( srdmi   , w1dc     ), ( vtmp    , w1dd     )
     2             , ( vpal    , w1de     ), ( vmal    , w1df     )
     2             , ( valp    , bpal    , w1dg     )
     1             , ( valm    , bmal    , w1dh     )
     8             , ( btmp    , bstar   , w1di     )
     9             , ( db      , w1dj     ),  ( dv     , w1dk     )
c
c      Routine MOCEMFS is expecting to find "srd1", "srd2", and "srd3" 
c  intact in worker arrays "wd3d", "we3d", and "wf3d" respectively.
c  [but MOCEMFS is in ZEUS-3D. does HSMOC also need these? M-MML 9.12.97 
c       so far, yes 4.3.98]
c
CPS: prevent overwritten of momenta.
C       equivalence   ( st1     , w3da     ), ( st2     , w3db     )
C     1             , ( st3     , w3dc     )
       equivalence   ( srd1    , w3di     ), ( srd2    , w3dj     )
     1             , ( srd3    , w3df     )
c
c      External statements
c
c
c
c-----------------------------------------------------------------------
c
c      Compute face-centred averages of density, initialise source terms
c  to zero.
c
       do 3 k=kbeg-1,kend+1
         do 2 j=jbeg-1,jend+1
           do 1 i=ibeg-1,iend+1
             srd1(i,j,k) = sqrt ( 0.5 * ( d (i,j,k) + d (i-1,j,k) ) )
             srd2(i,j,k) = sqrt ( 0.5 * ( d (i,j,k) + d (i,j-1,k) ) )
             srd3(i,j,k) = sqrt ( 0.5 * ( d (i,j,k) + d (i,j,k-1) ) )
             st1 (i,j,k) = 0.0
             st2 (i,j,k) = 0.0
             st3 (i,j,k) = 0.0
1          continue
2        continue
3      continue
c
c-----------------------------------------------------------------------
c---- 1-force ----------------------------------------------------------
c-----------------------------------------------------------------------
c
c      By following the Alfven velocity in the 2-direction, evaluate
c  "bstar" from "b1" to estimate the 2-transverse Lorentz force.
c
       do 50 k=kbeg,kend
         do 45 i=ibeg,iend+1
c
c      Select an effective density and determine the Alfven speed for
c  each Alfven characteristic in the 2-direction.
c
           do 10 j=jbeg,jend+1
             bave (j) = 0.5 * ( b2(i,j,k) + b2(i-1,j,k) )
             absb     = abs ( bave(j) )
             srdpi(j) = 1.0 / srd1(i,j  ,k)
             srdmi(j) = 1.0 / srd1(i,j-1,k)
             valp (j) =-absb * srdpi(j)
             valm (j) = absb * srdmi(j)
10         continue
c
c      Interpolate 1-D vectors of "u1" and "b1" in the 2-direction to
c  the footpoints of both Alfven characteristics.
c
C modified by PS
           do 20 j=jbeg-2,jend+2
             vtmp(j) = v1(i,j,k) - vg1(i)
             btmp(j) = b1(i,j,k)
20         continue


c          call x2zc1d ( vtmp, valp, valm, iords1, istps1, k, i
c    1                 , g2a, g2ai, vpal, vmal )
c          call x2zc1d ( btmp, valp, valm, iordb1, istpb1, k, i
c    1                 , g2a, g2ai, bpal, bmal )
c       subroutine x2zc1d ( q, vp, vm, iorder, isteep, k, i, g2, g2i
c     1                   , qp, qm )
c
c  Inline interpolation (following tranx*)
c

           do 22 j=jbeg-1,jend+1
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
22         continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g2ai(i)
           do 24 j=jbeg,jend+1
             qv1    = vtmp(j-1) + dx2a(j-1) * dv (j-1)
             qv2    = vtmp(j  ) - dx2a(j  ) * dv (j  )
             qb1    = btmp(j-1) + dx2a(j-1) * db (j-1)
             qb2    = btmp(j  ) - dx2a(j  ) * db (j  )
c
             xi     = valp(j) * fact
             q3     = sign ( haf, xi )
             vpal(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bpal(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )
c
             xi     = valm(j) * fact
             q3     = sign ( haf, xi )
             vmal(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bmal(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )

 24       continue
c
c      Evaluate "bstar" by solving the characteristic equations.
c
           do 30 j=jbeg,jend+1
             q1       = sign ( one, bave(j) )
             bstar(j) = ( bpal(j) * srdpi(j) + bmal(j) * srdmi(j)
     1                  + q1 * ( vpal(j) - vmal(j) ) )
     2                / ( srdpi(j) + srdmi(j) )
30         continue
c
c      Evaluate transverse Lorentz force.
c
           do 40 j=jbeg,jend
             st1(i,j,k) = ( bave (j+1) + bave (j) ) * g2ai(i)
     4                  * ( bstar(j+1) - bstar(j) ) * dx2ai(j)
40         continue
45       continue
50     continue
c
c-----------------------------------------------------------------------
c
c      By following the Alfven velocity in the 3-direction, evaluate
c  "bstar" from "b1" to estimate the 3-transverse Lorentz force.
c
       do 100 j=jbeg,jend
         do 95 i=ibeg,iend+1
c
c      Select an effective density and determine the Alfven speed for
c  each Alfven characteristic in the 3-direction.
c
           do 60 k=kbeg,kend+1
             bave (k) = 0.5 * ( b3(i,j,k) + b3(i-1,j,k) )
             absb     = abs ( bave(k) )
             srdpi(k) = 1.0 / srd1(i,j,k  )
             srdmi(k) = 1.0 / srd1(i,j,k-1)
             valp (k) =-absb * srdpi(k)
             valm (k) = absb * srdmi(k)
60         continue
c
c      Interpolate 1-D vectors of "u1" and "b1" in the 3-direction to
c  the footpoints of both Alfven characteristics.
c
C modified by PS
           do 70 k=kbeg-2,kend+2
             vtmp(k) = v1(i,j,k) - vg1(i)
             btmp(k) = b1(i,j,k)
70         continue

c           call x3zc1d ( vtmp, valp, valm, iords1, istps1, i, j
c     1                 , g31a, g31ai, g32b, g32bi, vpal, vmal )
c           call x3zc1d ( btmp, valp, valm, iordb1, istpb1, i, j
c     1                 , g31a, g31ai, g32b, g32bi, bpal, bmal )
c       subroutine x3zc1d ( q, vp, vm, iorder, isteep, i, j, g31, g31i
c     1                   , g32, g32i, qp, qm )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 72 k=kbeg-1,kend+1
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
72         continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g31ai(i) * g32bi(j)
           do 74 k=kbeg,kend+1
             qv1    = vtmp(k-1) + dx3a(k-1) * dv (k-1)
             qv2    = vtmp(k  ) - dx3a(k  ) * dv (k  )
             qb1    = btmp(k-1) + dx3a(k-1) * db (k-1)
             qb2    = btmp(k  ) - dx3a(k  ) * db (k  )
c  
             xi     = valp(k) * fact
             q3     = sign ( haf, xi )
             vpal(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bpal(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
c
             xi     = valm(k) * fact
             q3     = sign ( haf, xi )
             vmal(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bmal(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
74         continue
c
c      Evaluate "bstar" by solving the characteristic equations.
c
           do 80 k=kbeg,kend+1
             q1       = sign ( one, bave(k) )
             bstar(k) = ( bpal(k) * srdpi(k) + bmal(k) * srdmi(k)
     1                  + q1 * ( vpal(k) - vmal(k) ) )
     2                / ( srdpi(k) + srdmi(k) )
80         continue
c
c      Evaluate transverse Lorentz force.
c
           q1 = g31ai(i) * g32bi(j)
           do 90 k=kbeg,kend
             st1(i,j,k) = st1(i,j,k)
     1                  + ( bave (k+1) + bave (k) ) * q1
     4                  * ( bstar(k+1) - bstar(k) ) * dx3ai(k)
90         continue
95       continue
100      continue
c
110    continue
c
c-----------------------------------------------------------------------
c---- 2-force ----------------------------------------------------------
c-----------------------------------------------------------------------
c

       do 165 j=jbeg,jend+1
c
c      By following the Alfven velocity in the 3-direction, evaluate
c  "bstar" from "b2" to estimate the 3-transverse Lorentz force.
c
         do 160 i=ibeg,iend
c
c      Select an effective density and determine the Alfven speed for
c  each Alfven characteristic in the 3-direction.
c
           do 120 k=kbeg,kend+1
             bave (k) = 0.5 * ( b3(i,j,k) + b3(i,j-1,k) )
             absb     = abs ( bave(k) )
             srdpi(k) = 1.0 / srd2(i,j,k  )
             srdmi(k) = 1.0 / srd2(i,j,k-1)
             valp (k) =-absb * srdpi(k)
             valm (k) = absb * srdmi(k)
120        continue
c
c      Interpolate 1-D vectors of "u2" and "b2" in the 3-direction to
c  the footpoints of both Alfven characteristics.
c
C modified by PS
           do 130 k=kbeg-2,kend+2
             vtmp(k) = v2(i,j,k) - vg2(j)
             btmp(k) = b2(i,j,k)
130        continue
c          call x3zc1d ( vtmp, valp, valm, iords2, istps2, i, j
c    1                 , g31b, g31bi, g32a, g32ai, vpal, vmal )
c          call x3zc1d ( btmp, valp, valm, iordb2, istpb2, i, j
c    1                 , g31b, g31bi, g32a, g32ai, bpal, bmal )
c
c       subroutine x3zc1d ( q, vp, vm, iorder, isteep, i, j, g31, g31i
c     1                   , g32, g32i, qp, qm )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 132 k=kbeg-1,kend+1
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
132         continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g31bi(i) * g32ai(j)
           do 134 k=kbeg,kend+1
             qv1    = vtmp(k-1) + dx3a(k-1) * dv (k-1)
             qv2    = vtmp(k  ) - dx3a(k  ) * dv (k  )
             qb1    = btmp(k-1) + dx3a(k-1) * db (k-1)
             qb2    = btmp(k  ) - dx3a(k  ) * db (k  )
c  
             xi     = valp(k) * fact
             q3     = sign ( haf, xi )
             vpal(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bpal(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
c
             xi     = valm(k) * fact
             q3     = sign ( haf, xi )
             vmal(k)= ( 0.5 + q3 ) * ( qv1 - xi * dv (k-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (k  ) )
             bmal(k)= ( 0.5 + q3 ) * ( qb1 - xi * db (k-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (k  ) )
134         continue
c
c      Evaluate "bstar" by solving the characteristic equations.
c
           do 140 k=kbeg,kend+1
             q1       = sign ( one, bave(k) )
             bstar(k) = ( bpal(k) * srdpi(k) + bmal(k) * srdmi(k)
     1                  + q1 * ( vpal(k) - vmal(k) ) )
     2                / ( srdpi(k) + srdmi(k) )
140        continue
c
c      Evaluate transverse Lorentz force.
c
           q1 = g31bi(i) * g32ai(j)
           do 150 k=kbeg,kend
             st2(i,j,k) = ( bave (k+1) + bave (k) ) * q1
     4                  * ( bstar(k+1) - bstar(k) ) * dx3ai(k)
150        continue
160      continue
165    continue
c
c-----------------------------------------------------------------------
c
c      By following the Alfven velocity in the 1-direction, evaluate
c  "bstar" from "b2" to estimate the 1-transverse Lorentz force.
c
c *if def,IRIX.and.SGIMP
c C*$*ASSERT CONCURRENT CALL
c *endif IRIX.and.SGIMP
       do 210 k=kbeg,kend
         do 205 j=jbeg,jend+1
c
c      Select an effective density and determine the Alfven speed for
c  each Alfven characteristic in the 1-direction.
c
           do 170 i=ibeg,iend+1
             bave (i) = 0.5 * ( b1(i,j,k) + b1(i,j-1,k) )
             absb     = abs ( bave(i) )
             srdpi(i) = 1.0 / srd2(i  ,j,k)
             srdmi(i) = 1.0 / srd2(i-1,j,k)
             valp (i) =-absb * srdpi(i)
             valm (i) = absb * srdmi(i)
170        continue
c
c      Interpolate 1-D vectors of "u2" and "b2" in the 1-direction to
c  the footpoints of both Alfven characteristics.
c
C modified by PS
           do 180 i=ibeg-2,iend+2
             vtmp(i) = v2(i,j,k) - vg2(j)
             btmp(i) = b2(i,j,k)
180        continue
c          call x1zc1d ( vtmp, valp, valm, iords2, istps2, j, k
c    1                 , vpal, vmal )
c          call x1zc1d ( btmp, valp, valm, iordb2, istpb2, j, k
c    1                 , bpal, bmal )
c       subroutine x1zc1d ( q, vp, vm, iorder, isteep, j, k, qp, qm )

c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 182 i=ibeg-1,iend+1
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
 182       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           do 184 i=ibeg,iend+1
             qv1    = vtmp(i-1) + dx1a(i-1) * dv (i-1)
             qv2    = vtmp(i  ) - dx1a(i  ) * dv (i  )
             qb1    = btmp(i-1) + dx1a(i-1) * db (i-1)
             qb2    = btmp(i  ) - dx1a(i  ) * db (i  )
c
             xi     = valp(i) * dt
             q3     = sign ( haf, xi )
             vpal(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bpal(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
c
             xi     = valm(i) * dt
             q3     = sign ( haf, xi )
             vmal(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bmal(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
184        continue

c
c      Evaluate "bstar" by solving the characteristic equations.
c
           do 190 i=ibeg,iend+1
             q1       = sign ( one, bave(i) )
             bstar(i) = ( bpal(i) * srdpi(i) + bmal(i) * srdmi(i)
     1                  + q1 * ( vpal(i) - vmal(i) ) ) * g2a(i)
     2                / ( srdpi(i) + srdmi(i) )
190        continue
c
c      Evaluate transverse Lorentz force.  Note that the metric
c  factor "g2a" has been absorbed into "bstar".
c
           do 200 i=ibeg,iend
             st2(i,j,k) = st2(i,j,k)
     2                  + ( bave (i+1) + bave (i) ) * g2bi(i)
     4                  * ( bstar(i+1) - bstar(i) ) * dx1ai(i)
200        continue
205      continue
210    continue
c
220    continue
c
c-----------------------------------------------------------------------
c---- 3-force ----------------------------------------------------------
c-----------------------------------------------------------------------
c
c *if def,IRIX.and.SGIMP
c C*$*ASSERT CONCURRENT CALL
c *endif IRIX.and.SGIMP
       do 330 k=kbeg,kend+1
c
c      By following the Alfven velocity in the 1-direction, evaluate
c  "bstar" from "b3" to estimate the 1-transverse Lorentz force.
c
         do 270 j=jbeg,jend
c
c      Select an effective density and determine the Alfven speed for
c  each Alfven characteristic in the 1-direction.
c
           do 230 i=ibeg,iend+1
             bave (i) = 0.5 * ( b1(i,j,k) + b1(i,j,k-1) )
             absb     = abs ( bave(i) )
             srdpi(i) = 1.0 / srd3(i  ,j,k)
             srdmi(i) = 1.0 / srd3(i-1,j,k)
             valp (i) =-absb * srdpi(i)
             valm (i) = absb * srdmi(i)
230        continue
c
c      Interpolate 1-D vectors of "u3" and "b3" in the 1-direction to
c  the footpoints of both Alfven characteristics.
c
C modified by PS
           do 240 i=ibeg-2,iend+2
             vtmp(i) = v3(i,j,k) - vg3(k)
             btmp(i) = b3(i,j,k)
240        continue
c           call x1zc1d ( vtmp, valp, valm, iords3, istps3, j, k
c     1                 , vpal, vmal )
c           call x1zc1d ( btmp, valp, valm, iordb3, istpb3, j, k
c     1                 , bpal, bmal )
c       subroutine x1zc1d ( q, vp, vm, iorder, isteep, j, k, qp, qm )
c
c  1.  Evaluate monotonised, van Leer difference in "q" across the zone.
c
           do 242 i=ibeg-1,iend+1
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
 242       continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           do 244 i=ibeg,iend+1
             qv1    = vtmp(i-1) + dx1a(i-1) * dv (i-1)
             qv2    = vtmp(i  ) - dx1a(i  ) * dv (i  )
             qb1    = btmp(i-1) + dx1a(i-1) * db (i-1)
             qb2    = btmp(i  ) - dx1a(i  ) * db (i  )
c
             xi     = valp(i) * dt
             q3     = sign ( haf, xi )
             vpal(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bpal(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
c
             xi     = valm(i) * dt
             q3     = sign ( haf, xi )
             vmal(i)= ( 0.5 + q3 ) * ( qv1 - xi * dv (i-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (i  ) )
             bmal(i)= ( 0.5 + q3 ) * ( qb1 - xi * db (i-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (i  ) )
244        continue
c
c      Evaluate "bstar" by solving the characteristic equations.
c
           do 250 i=ibeg,iend+1
             q1       = sign ( one, bave(i) )
             bstar(i) = ( bpal(i) * srdpi(i) + bmal(i) * srdmi(i)
     1                  + q1 * ( vpal(i) - vmal(i) ) ) * g31a(i)
     2                / ( srdpi(i) + srdmi(i) )
250        continue
c
c      Evaluate transverse Lorentz force.  Note that the metric
c  factor "g31a" has been absorbed into "bstar".
c
           do 260 i=ibeg,iend
             st3(i,j,k) = ( bave (i+1) + bave (i) ) * g31bi(i)
     4                  * ( bstar(i+1) - bstar(i) ) * dx1ai(i)
260        continue
270      continue
c
c-----------------------------------------------------------------------
c
c      By following the Alfven velocity in the 2-direction, evaluate
c  "bstar" from "b3" to estimate the 2-transverse Lorentz force.
c
         do 320 i=ibeg,iend
c
c      Select an effective density and determine the Alfven speed for
c  each Alfven characteristic in the 2-direction.
c
           do 280 j=jbeg,jend+1
             bave (j) = 0.5 * ( b2(i,j,k) + b2(i,j,k-1) )
             absb     = abs ( bave(j) )
             srdpi(j) = 1.0 / srd3(i,j  ,k)
             srdmi(j) = 1.0 / srd3(i,j-1,k)
             valp (j) =-absb * srdpi(j)
             valm (j) = absb * srdmi(j)
280        continue
c
c      Interpolate 1-D vectors of "u3" and "b3" in the 2-direction to
c  the footpoints of both Alfven characteristics.
c
C modified by PS
           do 290 j=jbeg-2,jend+2
             vtmp(j) = v3(i,j,k) - vg3(k)
             btmp(j) = b3(i,j,k)
290        continue
c           call x2zc1d ( vtmp, valp, valm, iords3, istps3, k, i
c     1                 , g2b, g2bi, vpal, vmal )
c           call x2zc1d ( btmp, valp, valm, iordb3, istpb3, k, i
c     1                 , g2b, g2bi, bpal, bmal )
c       subroutine x2zc1d ( q, vp, vm, iorder, isteep, k, i, g2, g2i
c     1                   , qp, qm )
c
            do 292 j=jbeg-1,jend+1
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
292         continue
c
c  2.  Perform an upwinded interpolation of "q" to the time-centred
c      bases of the characteristics.
c
           fact = dt * g2bi(i)
           do 294 j=jbeg,jend+1
             qv1    = vtmp(j-1) + dx2a(j-1) * dv (j-1)
             qv2    = vtmp(j  ) - dx2a(j  ) * dv (j  )
             qb1    = btmp(j-1) + dx2a(j-1) * db (j-1)
             qb2    = btmp(j  ) - dx2a(j  ) * db (j  )
c
             xi     = valp(j) * fact
             q3     = sign ( haf, xi )
             vpal(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bpal(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )
c
             xi     = valm(j) * fact
             q3     = sign ( haf, xi )
             vmal(j)= ( 0.5 + q3 ) * ( qv1 - xi * dv (j-1) )
     1              + ( 0.5 - q3 ) * ( qv2 - xi * dv (j  ) )
             bmal(j)= ( 0.5 + q3 ) * ( qb1 - xi * db (j-1) )
     1              + ( 0.5 - q3 ) * ( qb2 - xi * db (j  ) )

 294       continue
c
c      Evaluate "bstar" by solving the characteristic equations.
c
           do 300 j=jbeg,jend+1
             q1       = sign ( one, bave(j) )
             bstar(j) = ( bpal(j) * srdpi(j) + bmal(j) * srdmi(j)
     1                  + q1 * ( vpal(j) - vmal(j) ) ) * g32a(j)
     1                / ( srdpi(j) + srdmi(j) )
300        continue
c
c      Evaluate transverse Lorentz force.  Note that the metric
c  factor "g32a" has been absorbed into "bstar".
c
           do 310 j=jbeg,jend
             st3(i,j,k) = st3(i,j,k)
     2                  + ( bave (j+1) + bave (j) ) * g2ai(i) * g32bi(j)
     4                  * ( bstar(j+1) - bstar(j) ) * dx2ai(j)
310        continue
320      continue
c
330    continue
c
c-----------------------------------------------------------------------
c
c      Accelerate velocities and set boundaries.
c
       q1 = 0.5 * dt

c modified for SPEC CPU2006
c       do 360 k=kbeg,kend
c         do 350 j=jbeg,jend
c           do 340 i=ibeg,iend
c             w1(i,j,k) = u1(i,j,k) + q1 * st1(i,j,k) / srd1(i,j,k)**2
c             w2(i,j,k) = u2(i,j,k) + q1 * st2(i,j,k) / srd2(i,j,k)**2
c             w3(i,j,k) = u3(i,j,k) + q1 * st3(i,j,k) / srd3(i,j,k)**2
c340        continue
c350      continue
c360    continue
       do 360 k=kbeg,kend
         do 350 j=jbeg,jend
           do 340 i=ibeg,iend
             u1(i,j,k) = v1(i,j,k) + q1 * st1(i,j,k) / srd1(i,j,k)**2
             u2(i,j,k) = v2(i,j,k) + q1 * st2(i,j,k) / srd2(i,j,k)**2
             u3(i,j,k) = v3(i,j,k) + q1 * st3(i,j,k) / srd3(i,j,k)**2
340        continue
350      continue
360    continue
c


c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////               L O R E N T Z               \\\\\\\\\\
c
c=======================================================================
c








