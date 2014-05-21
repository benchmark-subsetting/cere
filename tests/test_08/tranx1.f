












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                T R A N X 1                \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine tranx1 (ibeg,iend,jbeg,jend,kbeg,kend
     &                   ,dlo,den

     &                   ,eod,edn


     &                   ,mflx)
c
c    dac:zeus3d.tranx1 <----- transports zone-centred variables along x1
c    from jms:zeus2d.tranx1, mln:zeus04.tranz                  may, 1990
c
c    written by: David Clarke
c    modified 1: June 1992, by David Clarke; added the total energy
c                option originally designed by Byung-IL Jun.
c    modified 2: Feb. 20, 1996 by Robert Fiedler; completely rewritten
c                for ZEUS-MP.
c    modified 3: Dec. 19, 1996 by Robert Fiedler; added radiation
c
c  PURPOSE:  Transports all zone centred variables in the 1-direction
c  only.  Currently transported are:
c
c                      mass   density
c                      energy density
c
c  The consistent advection algorithm, in which mass fluxes are used to
c  construct the fluxes of all variables across the interfaces, is used
c  for all hydrodynamical variables.  Thus, the mass fluxes are passed
c  to MOMX1 on order to transport the momenta as well.  The magnetic
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
c                    d   is-3  ie+2  js-1        ks-1
c                   e/d  is-2  ie+2
c                    u1  is-1  ie+1  js-1        ks-1
c    TOTAL_ENERGY    u2                    je+1
c    TOTAL_ENERGY    u3                                ke+1
c
c  OUTPUT VARIABLES:
c    den      updated mass            density.
c    edn      updated specific energy density.
c    mflx     mass flux (in the 1-direction)
c
c  LOCAL VARIABLES:
c    atwid    effective cross sectional area of the 1-interfaces
c    etwid    interpolated specific energy densities (e/d) at all
c             1-interfaces
c    eflx     energy density flux across all 1-interfaces  (reuse etwid)
c    dtwid    interpolated mass densities at all 1-interfaces
c    dflx     mass density flux across all 1-interfaces    (reuse dtwid)
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
       real*8        atwid (ijkn),  mflux (ijkn)
     &           , dtwid (ijkn),  dd    (ijkn)

     &           , etwid (ijkn),  deod  (ijkn)


c
       real*8        mflx    (  in,  jn,  kn)
       real*8  dlo(in,jn,kn), den(in,jn,kn)

     &      ,eod(in,jn,kn), edn(in,jn,kn)


c
       equivalence (atwid , w1da), (mflux , w1db)
     &           , (dtwid , w1dc), (dd    , w1dd)

     &           , (etwid , w1de), (deod  , w1df)


c
c-----------------------------------------------------------------------
c
c Compute time-centered area factors.
c
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
       do 10 i=ibeg-1,iend+1

         atwid (i)  =       g2a (i) * g31a (i)

10     continue
c
c Transport all zone-centered quantities in the 1 direction.
c Note that transporting v1 in MOMX1 will require the mass flux at 
c x1a(is-1).  To get it from the field variables, we need d at is-3.
c We also need mflx at js-1 and ks-1 for i=is,ie+1 for v2 and v3.
c Extend loop indices to compute mass fluxes beyond inner borders.
c Be careful to assign values to mflx, (and den, edn) only
c within the range (ibeg:iend,jbeg:jend,kbeg:kend) when these
c indices are not on the borders, so that they can't get overwritten
c when this routine is called with various index ranges.
c
         do 100 k=kbeg-1,kend
           do 90 j=jbeg-1,jend
c
c   Interpolate to obtain zone-centered quantities at zone faces.
c
c     1.  Evaluate monotonised, van Leer differences across the zone.
c
             if (ibeg .eq. is) then  
c  Need d(is-3) from neighbor.
               i         = is - 2
               dqm       = (dlo(i  ,j,k) - diib (j,k,3)) * dx1bi(i  )
               dqp       = (dlo(i+1,j,k) - dlo(i  ,j,k)) * dx1bi(i+1)
               dd(i)     = max ( dqm * dqp, zro )
     1                   * sign ( one, dqm + dqp )
     2                   / max ( abs ( dqm + dqp ), tiny )
c Not valid, but we don't use it.
               deod  (i) = zro  


             endif
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
             do 30 i=max(ibeg-2,is-1),iend+1
               dqm       = (dlo(i  ,j,k) - dlo(i-1,j,k)) * dx1bi(i  )
               dqp       = (dlo(i+1,j,k) - dlo(i  ,j,k)) * dx1bi(i+1)
               dd(i)     = max ( dqm * dqp, zro )
     1                   * sign ( one, dqm + dqp )
     2                   / max ( abs ( dqm + dqp ), tiny )

               dqm       = (eod(i  ,j,k) - eod(i-1,j,k)) * dx1bi(i  )
               dqp       = (eod(i+1,j,k) - eod(i  ,j,k)) * dx1bi(i+1)
               deod(i)   = max ( dqm * dqp, zro )
     1                   * sign ( one, dqm + dqp )
     2                   / max ( abs ( dqm + dqp ), tiny )


30           continue
c
c     2.  Choose time averaged, upwinded interface values.
c
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
             do 40 i=ibeg-1,iend+1
               xi        = ( v1  (i,j,k) - vg1(i) ) * dt
               q1        = sign ( haf, xi )
               dtwid (i) = ( 0.5 + q1 ) * ( dlo(i-1,j,k)
     1                   + ( dx1a(i-1) - xi ) * dd   (i-1) )
     2                   + ( 0.5 - q1 ) * ( dlo(i  ,j,k)
     3                   - ( dx1a(i  ) + xi ) * dd   (i  ) )

               etwid (i) = ( 0.5 + q1 ) * ( eod(i-1,j,k)
     1                   + ( dx1a(i-1) - xi ) * deod (i-1) )
     2                   + ( 0.5 - q1 ) * ( eod(i  ,j,k)
     3                   - ( dx1a(i  ) + xi ) * deod (i  ) )


40           continue
c
c  For the purposes of consistent advection, construct the mass
c  flux across each 1-interface.  The mass flux will be used to create
c  the fluxes of all variables, including the momenta which are updated
c  in MOMX1.
c
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
           do 50 i=ibeg-1,iend+1
             mflux (i    ) = dtwid (i    ) * ( v1(i,j,k) - vg1(i) ) * dt
             dtwid (i    ) = mflux (i    ) * atwid (i    )

             etwid (i    ) = dtwid (i    ) * etwid (i    )


50         continue
c
c  Save the mass flux outside (ibeg:iend,jbeg:jend,kbeg:kend)
c  only for zones next to the inner borders.
c
           if ( (j.eq.js-1 .and. k.ge.kbeg)   .or.
     &          (j.ge.jbeg .and. k.eq.ks-1)   .or.
     &          (j.eq.js-1 .and. k.eq.ks-1) ) then
             if (ibeg.eq.is) mflx(is-1,j,k) = mflux (is-1)
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
             do 60 i=ibeg,iend
               mflx(i,j,k) = mflux (i      )
60           continue
             if (iend.eq.ie) mflx(ie+1,j,k) = mflux (ie+1)
           endif
c
c  Perform mass density and energy density advection.  Note that
c  the timestep "dt" is hidden the fluxes "dflx" and "eflx".
c  Do only zones inside (ibeg:iend,jbeg:jend,kbeg:kend).
c
           if (j.ge.jbeg .and. k.ge.kbeg) then
             if (ibeg.eq.is) mflx(is-1,j,k) = mflux (is-1)
CREMOVED FOR SPEC CPU2006 CDIR$ UNROLL 4
             do 80 i=ibeg,iend
             mflx(i,j,k)= mflux (i      )
             den(i,j,k) = ( dlo(i,j,k) * dvl1a(i)

     1                  - dtwid (i+1    ) + dtwid (i    ) ) * dvl1a i(i)


             e  (i,j,k) = ( e  (i,j,k) * dvl1a(i)

     1                  - etwid (i+1    ) + etwid (i    ) ) * dvl1a i(i)

c
c Compute e/d for the next substep.
c

             edn(i,j,k) = e(i,j,k) / den(i,j,k)



80         continue
           if (iend.eq.ie) mflx(ie+1,j,k) = mflux (ie+1)
         endif
90       continue
100    continue
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                T R A N X 1                \\\\\\\\\\
c
c=======================================================================
c
c
