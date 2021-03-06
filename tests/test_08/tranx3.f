












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                T R A N X 3                \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine tranx3 (ibeg,iend,jbeg,jend,kbeg,kend
     &                   ,dlo,den

     &                   ,eod,edn


     &                   ,mflx)
c
c    dac:zeus3d.tranx3 <----- transports zone-centred variables along x3
c    from jms:zeus2d.tranx2, mln:zeus04.tranz                  may, 1990
c
c    written by: David Clarke
c    modified 1: June 1992, by David Clarke; added the total energy
c                option originally designed by Byung-IL Jun.
c    modified 2: Feb. 22, 1996 by Robert Fiedler; completely rewritten
c                for ZEUS-MP.
c    modified 3: Aug. 6,  1996 by Robert Fiedler; rearranged for
c                maximum efficiency, and unrolled i-loops.
c    modified 3: Dec. 18,  1996 by Robert Fiedler; added radiation.
c
c  PURPOSE:  Transports all zone centred variables in the 3-direction
c  only.  Currently transported are:
c
c                      mass   density
c                      energy density
c
c  The consistent advection algorithm, in which mass fluxes are used to
c  construct the fluxes of all variables across the interfaces, is used
c  for all hydrodynamical variables.  Thus, the mass fluxes are passed
c  to MOMX3 in order to transport the momenta as well.  The magnetic
c  field components are treated separately from the mass fluxes in CT.
c  Interpolations are done in-line.
c
c  INPUT VARIABLES: 
c    ibeg,iend,jbeg,jend,kbeg,kend  index ranges to cover.
c    dlo      mass            density at previous substep.
c    eod      specific energy density at previous substep; equals
c             (e+p)/d  if TOTAL_ENERGY is defined.
c
c BOUNDARY VALUES USED:
c
c    Macro defined  var   ii    oi    ij    oj    ik    ok
c    -------------  ---  ----  ----  ----  ----  ----  ----
c                    d   is-1        js-1        ks-3  ke+2
c                   e/d                          ks-2  ke+2
c    TOTAL_ENERGY    u1        ie+1
c    TOTAL_ENERGY    u2                    je+1
c                    u3  is-1        js-1        ks-1  ke+1
c
c  OUTPUT VARIABLES:
c    den      updated mass            density.
c    edn      updated specific energy density.
c    mflx     mass flux (in the 3-direction)
c
c  LOCAL VARIABLES:
c    atwid    effective cross sectional area of the 1-interfaces
c    atwid2   effective cross sectional area of the 2-interfaces
c    etwid    interpolated specific energy densities (e/d) at all
c             3-interfaces
c    eflx     energy density flux across all 3-interfaces (reuse etwid)
c    dtwid    interpolated mass densities at all 3-interfaces
c    dflx     mass density flux across all 3-interfaces   (reuse dtwid)
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
       integer i, j, k, ibeg,iend,jbeg,jend,kbeg,kend
c
       real*8    dqm,dqp, xi,q1
c
       real*8        atwid2(ijkn),  atwid (ijkn)


       integer iriter

       real*8        mflux (ijkn,4)
     &           , dtwid (ijkn,4),  dd    (ijkn,4)

     &           , etwid (ijkn,4),  deod  (ijkn,4)


c
c
       real*8  mflx (in,jn,kn)
       real*8  dlo  (in,jn,kn), den  (in,jn,kn)

     &      ,eod  (in,jn,kn), edn  (in,jn,kn)


c
       equivalence (atwid , w1da), (atwid2, w1db)


c
c
c-----------------------------------------------------------------------
c
c Compute time-centered area factors.
c
       do 10 i=ibeg-1,iend+1
         atwid (i)           = g2b(i) * dx1a(i) * dvl1ai(i)
10     continue
       do 20 j=jbeg-1,jend+1
         atwid2(j)           = dx2a(j) * dvl2ai(j)
20     continue
c
c Transport all zone-centered quantities in the 3 direction.
c
       do 2100 j=jbeg-1,jend
         if ( (j.ge.jbeg) .or. (j.eq.js-1) ) then
c.......................................................................
c
c Split off the i=ibeg-1 iteration to ease unrolling.
c
           i = ibeg - 1
c
c   Interpolate to obtain zone-centered quantities at zone faces.
c
c     1.  Evaluate monotonised, van Leer differences across the zone.
c
             if (kbeg .eq. ks) then  
c  Need d(ks-3) from neighbor.
               k        = ks - 2
               dqm      = (dlo(i  ,j,k  ) - dikb (i  ,j,3)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
             endif
             do 30 k=max(kbeg-2,ks-1),kend+1
               dqm      = (dlo(i  ,j,k  ) - dlo(i  ,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
30         continue
c
c     2.  Choose time averaged, upwinded interface value.
c
c  For the purposes of consistent advection, construct the mass
c  flux across each 1-interface.  The mass flux will be used to create
c  the fluxes of all variables, including the momenta which are updated
c  in MOMX1.
c
             do 40 k=kbeg-1,kend+1
               xi          = ( v3  (i  ,j,k  ) - vg3(k  ) ) * dt
               q1          = sign ( haf, xi )
               dtwid (k,1) = ( 0.5 + q1 ) * ( dlo(i  ,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,1) )
     2                     + ( 0.5 - q1 ) * ( dlo(i  ,j,k  )
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,1) )
c
               mflux (k,1) = dtwid (k,1) * ( v3(i  ,j,k) - vg3(k) ) * dt
40           continue
c
c  Save the mass flux outside (ibeg:iend,jbeg:jend,kbeg:kend)
c  only for zones along the inner borders.
c
             if (kbeg .eq. ks) mflx(i  ,j,ks-1) = mflux (ks-1,1)
             do 60 k=kbeg,kend
               mflx(i  ,j,k) = mflux (k   ,1)
60           continue
             if (kend .eq. ke) mflx(i  ,j,ke+1) = mflux (ke+1,1)
c
c.......................................................................
c

c Do the iterations which will not be done in unrolled loops next.
c
           iriter = mod(iend-ibeg+1,4)
           do 1090 i=ibeg,ibeg+iriter-1

c
c   Interpolate to obtain zone-centered quantities at zone faces.
c
c     1.  Evaluate monotonised, van Leer differences across the zone.
c
             if (kbeg .eq. ks) then  
c  Need d(ks-3) from neighbor.
               k        = ks - 2
               dqm      = (dlo(i  ,j,k  ) - dikb (i  ,j,3)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c Not valid, but we don't use it.
               deod(k,1) = zro  


             endif
             do 1030 k=max(kbeg-2,ks-1),kend+1
               dqm      = (dlo(i  ,j,k  ) - dlo(i  ,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )

               dqm      = (eod(i  ,j,k  ) - eod(i  ,j,k-1)) * dx3bi(k  )
               dqp      = (eod(i  ,j,k+1) - eod(i  ,j,k  )) * dx3bi(k+1)
               deod(k,1)= max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )


1030       continue
c
c     2.  Choose time averaged, upwinded interface value.
c
c  For the purposes of consistent advection, construct the mass
c  flux across each 1-interface.  The mass flux will be used to create
c  the fluxes of all variables, including the momenta which are updated
c  in MOMX1.
c
             do 1040 k=kbeg-1,kend+1
               xi          = ( v3  (i  ,j,k  ) - vg3(k  ) ) * dt
               q1          = sign ( haf, xi )
               dtwid (k,1) = ( 0.5 + q1 ) * ( dlo(i  ,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,1) )
     2                     + ( 0.5 - q1 ) * ( dlo(i  ,j,k  )
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,1) )
c
               mflux (k,1) = dtwid (k,1) * ( v3(i  ,j,k) - vg3(k) ) * dt
               dtwid (k,1) = mflux (k,1) * atwid (i  ) * atwid2(j)

               etwid (k,1) = ( 0.5 + q1 ) * ( eod(i  ,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * deod (k-1,1) )
     2                     + ( 0.5 - q1 ) * ( eod(i  ,j,k  )
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,1) )
c
               etwid (k,1) = dtwid (k,1) * etwid (k,1)


1040         continue
c
c  Save the mass flux outside (ibeg:iend,jbeg:jend,kbeg:kend)
c  only for zones along the inner borders.
c
             if (j.eq.js-1) then
c
               if (kbeg .eq. ks) mflx(i  ,j,ks-1) = mflux (ks-1,1)
               do 1060 k=kbeg,kend
                 mflx(i  ,j,k) = mflux (k   ,1)
1060           continue
               if (kend .eq. ke) mflx(i  ,j,ke+1) = mflux (ke+1,1)
c
             endif
c
c  Perform mass density and energy density advection.  Note that
c  the timestep "dt" is hidden the fluxes "dtwid" and "etwid".
c
             if (j.ge.jbeg) then
c
               if (kbeg .eq. ks) mflx(i  ,j,ks-1) = mflux (ks-1,1)
               do 1080 k=kbeg,kend
               mflx(i  ,j,k)= mflux (k   ,1)
               den(i  ,j,k) = ( dlo(i  ,j,k) * dvl3a(k)

     1                      - dtwid (k+1,1) + dtwid (k,1) ) * dvl3a i(k)


               e  (i  ,j,k) = ( e  (i  ,j,k) * dvl3a(k)

     1                      - etwid (k+1,1) + etwid (k,1) ) * dvl3a i(k)

c
c Compute e/d for the next substep.
c

               edn(i  ,j,k) =         e(i  ,j,k) / den(i  ,j,k)



1080           continue
               if (kend .eq. ke) mflx(i  ,j,ke+1) = mflux (ke+1,1)
             endif
1090       continue

c.......................................................................
c
c Do the bulk of the iterations in unrolled loops.
c
           do 2090 i=ibeg+iriter,iend,4
c
c   Interpolate to obtain zone-centered quantities at zone faces.
c
c     1.  Evaluate monotonised, van Leer differences across the zone.
c
             if (kbeg .eq. ks) then  
c  Need d(ks-3) from neighbor.
               k        = ks - 2
               dqm      = (dlo(i  ,j,k  ) - dikb (i  ,j,3)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c Not valid, but we don't use it.
               deod(k,1) = zro  


c
               dqm      = (dlo(i+1,j,k  ) - dikb (i+1,j,3)) * dx3bi(k  )
               dqp      = (dlo(i+1,j,k+1) - dlo(i+1,j,k  )) * dx3bi(k+1)
               dd(k,2)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c Not valid, but we don't use it.
               deod(k,2) = zro  


c

               dqm      = (dlo(i+2,j,k  ) - dikb (i+2,j,3)) * dx3bi(k  )
               dqp      = (dlo(i+2,j,k+1) - dlo(i+2,j,k  )) * dx3bi(k+1)
               dd(k,3)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c Not valid, but we don't use it.
               deod(k,3) = zro  


c


               dqm      = (dlo(i+3,j,k  ) - dikb (i+3,j,3)) * dx3bi(k  )
               dqp      = (dlo(i+3,j,k+1) - dlo(i+3,j,k  )) * dx3bi(k+1)
               dd(k,4)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )
c Not valid, but we don't use it.
               deod(k,4) = zro  


c

             endif
             do 2030 k=max(kbeg-2,ks-1),kend+1
               dqm      = (dlo(i  ,j,k  ) - dlo(i  ,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )

               dqm      = (eod(i  ,j,k  ) - eod(i  ,j,k-1)) * dx3bi(k  )
               dqp      = (eod(i  ,j,k+1) - eod(i  ,j,k  )) * dx3bi(k+1)
               deod(k,1)= max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )


c
               dqm      = (dlo(i+1,j,k  ) - dlo(i+1,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i+1,j,k+1) - dlo(i+1,j,k  )) * dx3bi(k+1)
               dd(k,2)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )

               dqm      = (eod(i+1,j,k  ) - eod(i+1,j,k-1)) * dx3bi(k  )
               dqp      = (eod(i+1,j,k+1) - eod(i+1,j,k  )) * dx3bi(k+1)
               deod(k,2)= max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )


c

               dqm      = (dlo(i+2,j,k  ) - dlo(i+2,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i+2,j,k+1) - dlo(i+2,j,k  )) * dx3bi(k+1)
               dd(k,3)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )

               dqm      = (eod(i+2,j,k  ) - eod(i+2,j,k-1)) * dx3bi(k  )
               dqp      = (eod(i+2,j,k+1) - eod(i+2,j,k  )) * dx3bi(k+1)
               deod(k,3)= max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )


c


               dqm      = (dlo(i+3,j,k  ) - dlo(i+3,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i+3,j,k+1) - dlo(i+3,j,k  )) * dx3bi(k+1)
               dd(k,4)  = max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )

               dqm      = (eod(i+3,j,k  ) - eod(i+3,j,k-1)) * dx3bi(k  )
               dqp      = (eod(i+3,j,k+1) - eod(i+3,j,k  )) * dx3bi(k+1)
               deod(k,4)= max ( dqm * dqp, zro )
     1                  * sign ( one, dqm + dqp )
     2                  / max ( abs ( dqm + dqp ), tiny )


c

2030       continue
c
c     2.  Choose time averaged, upwinded interface value.
c
c  For the purposes of consistent advection, construct the mass
c  flux across each 1-interface.  The mass flux will be used to create
c  the fluxes of all variables, including the momenta which are updated
c  in MOMX1.
c
             do 2040 k=kbeg-1,kend+1
               xi          = ( v3  (i  ,j,k  ) - vg3(k  ) ) * dt
               q1          = sign ( haf, xi )
               dtwid (k,1) = ( 0.5 + q1 ) * ( dlo(i  ,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,1) )
     2                     + ( 0.5 - q1 ) * ( dlo(i  ,j,k  )
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,1) )
c
               mflux (k,1) = dtwid (k,1) * ( v3(i  ,j,k) - vg3(k) ) * dt
               dtwid (k,1) = mflux (k,1) * atwid (i  ) * atwid2(j)

               etwid (k,1) = ( 0.5 + q1 ) * ( eod(i  ,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * deod (k-1,1) )
     2                     + ( 0.5 - q1 ) * ( eod(i  ,j,k  )
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,1) )
c
               etwid (k,1) = dtwid (k,1) * etwid (k,1)


c
               xi          = ( v3  (i+1,j,k  ) - vg3(k  ) ) * dt
               q1          = sign ( haf, xi )
               dtwid (k,2) = ( 0.5 + q1 ) * ( dlo(i+1,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,2) )
     2                     + ( 0.5 - q1 ) * ( dlo(i+1,j,k  )
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,2) )
c
               mflux (k,2) = dtwid (k,2) * ( v3(i+1,j,k) - vg3(k) ) * dt
               dtwid (k,2) = mflux (k,2) * atwid (i+1) * atwid2(j)

               etwid (k,2) = ( 0.5 + q1 ) * ( eod(i+1,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * deod (k-1,2) )
     2                     + ( 0.5 - q1 ) * ( eod(i+1,j,k  )
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,2) )
c
               etwid (k,2) = dtwid (k,2) * etwid (k,2)


c

               xi          = ( v3  (i+2,j,k  ) - vg3(k  ) ) * dt
               q1          = sign ( haf, xi )
               dtwid (k,3) = ( 0.5 + q1 ) * ( dlo(i+2,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,3) )
     2                     + ( 0.5 - q1 ) * ( dlo(i+2,j,k  )
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,3) )
c
               mflux (k,3) = dtwid (k,3) * ( v3(i+2,j,k) - vg3(k) ) * dt
               dtwid (k,3) = mflux (k,3) * atwid (i+2) * atwid2(j)

               etwid (k,3) = ( 0.5 + q1 ) * ( eod(i+2,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * deod (k-1,3) )
     2                     + ( 0.5 - q1 ) * ( eod(i+2,j,k  )
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,3) )
c
               etwid (k,3) = dtwid (k,3) * etwid (k,3)


c


               xi          = ( v3  (i+3,j,k  ) - vg3(k  ) ) * dt
               q1          = sign ( haf, xi )
               dtwid (k,4) = ( 0.5 + q1 ) * ( dlo(i+3,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,4) )
     2                     + ( 0.5 - q1 ) * ( dlo(i+3,j,k  )
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,4) )
c
               mflux (k,4) = dtwid (k,4) * ( v3(i+3,j,k) - vg3(k) ) * dt
               dtwid (k,4) = mflux (k,4) * atwid (i+3) * atwid2(j)

               etwid (k,4) = ( 0.5 + q1 ) * ( eod(i+3,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * deod (k-1,4) )
     2                     + ( 0.5 - q1 ) * ( eod(i+3,j,k  )
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,4) )
c
               etwid (k,4) = dtwid (k,4) * etwid (k,4)


c

2040         continue
c
c  Save the mass flux outside (ibeg:iend,jbeg:jend,kbeg:kend)
c  only for zones along the inner borders.
c
             if (j.eq.js-1) then
c
               if (kbeg .eq. ks) then
                 mflx(i  ,j,ks-1) = mflux (ks-1,1)
                 mflx(i+1,j,ks-1) = mflux (ks-1,2)

                 mflx(i+2,j,ks-1) = mflux (ks-1,3)


                 mflx(i+3,j,ks-1) = mflux (ks-1,4)

               endif
               do 2060 k=kbeg,kend
                 mflx(i  ,j,k) = mflux (k   ,1)
                 mflx(i+1,j,k) = mflux (k   ,2)

                 mflx(i+2,j,k) = mflux (k   ,3)


                 mflx(i+3,j,k) = mflux (k   ,4)

2060           continue
               if (kend .eq. ke) then
                 mflx(i  ,j,ke+1) = mflux (ke+1,1)
                 mflx(i+1,j,ke+1) = mflux (ke+1,2)

                 mflx(i+2,j,ke+1) = mflux (ke+1,3)


                 mflx(i+3,j,ke+1) = mflux (ke+1,4)

               endif
c
             endif
c
c  Perform mass density and energy density advection.  Note that
c  the timestep "dt" is hidden the fluxes "dtwid" and "etwid".
c
             if (j.ge.jbeg) then
c
               if (kbeg .eq. ks) then
                 mflx(i  ,j,ks-1) = mflux (ks-1,1)
                 mflx(i+1,j,ks-1) = mflux (ks-1,2)

                 mflx(i+2,j,ks-1) = mflux (ks-1,3)


                 mflx(i+3,j,ks-1) = mflux (ks-1,4)

               endif
               do 2080 k=kbeg,kend
               mflx(i  ,j,k)= mflux (k   ,1)
               den(i  ,j,k) = ( dlo(i  ,j,k) * dvl3a(k)

     1                      - dtwid (k+1,1) + dtwid (k,1) ) * dvl3a i(k)


               e  (i  ,j,k) = ( e  (i  ,j,k) * dvl3a(k)

     1                      - etwid (k+1,1) + etwid (k,1) ) * dvl3a i(k)

c
c Compute e/d for the next substep.
c

               edn(i  ,j,k) =         e(i  ,j,k) / den(i  ,j,k)



c
               mflx(i+1,j,k)= mflux (k   ,2)
               den(i+1,j,k) = ( dlo(i+1,j,k) * dvl3a(k)

     1                      - dtwid (k+1,2) + dtwid (k,2) ) * dvl3a i(k)


               e  (i+1,j,k) = ( e  (i+1,j,k) * dvl3a(k)

     1                      - etwid (k+1,2) + etwid (k,2) ) * dvl3a i(k)

c
c Compute e/d for the next substep.
c

               edn(i+1,j,k) =         e(i+1,j,k) / den(i+1,j,k)



c

               mflx(i+2,j,k)= mflux (k   ,3)
               den(i+2,j,k) = ( dlo(i+2,j,k) * dvl3a(k)

     1                      - dtwid (k+1,3) + dtwid (k,3) ) * dvl3a i(k)


               e  (i+2,j,k) = ( e  (i+2,j,k) * dvl3a(k)

     1                      - etwid (k+1,3) + etwid (k,3) ) * dvl3a i(k)

c
c Compute e/d for the next substep.
c

               edn(i+2,j,k) =         e(i+2,j,k) / den(i+2,j,k)



c


               mflx(i+3,j,k)= mflux (k   ,4)
               den(i+3,j,k) = ( dlo(i+3,j,k) * dvl3a(k)

     1                      - dtwid (k+1,4) + dtwid (k,4) ) * dvl3a i(k)


               e  (i+3,j,k) = ( e  (i+3,j,k) * dvl3a(k)

     1                      - etwid (k+1,4) + etwid (k,4) ) * dvl3a i(k)

c
c Compute e/d for the next substep.
c

               edn(i+3,j,k) =         e(i+3,j,k) / den(i+3,j,k)



c

2080           continue
               if (kend .eq. ke) then
                 mflx(i  ,j,ke+1) = mflux (ke+1,1)
                 mflx(i+1,j,ke+1) = mflux (ke+1,2)

                 mflx(i+2,j,ke+1) = mflux (ke+1,3)


                 mflx(i+3,j,ke+1) = mflux (ke+1,4)

               endif
c  j>=jbeg
             endif  
2090       continue
c  j>=jbeg || j=js-1
         endif  
2100   continue
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                T R A N X 3                \\\\\\\\\\
c
c=======================================================================
c
c
