












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 B V A L D                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine bvald ( rl1, ru1, rl2, ru2, rl3, ru3, d )
c
c    dac:zeus3d.bvald <------------------------- density boundary values
c    from mln:zeus04.bval; jms:zeus2d.bvald               february, 1990
c
c    written by: David Clarke, February, 1990.
c    modified 1: RAF, 3/5/96; completely rewritten for ZEUS-MP
c    modified 2: RAF, 8/27/96; correction for periodic BCs iib, no MPI.
c
c  PURPOSE: This routine sets boundary values for the density.  The
c  active zones for "d" are "is" to "ie" in the 1-direction, "js" to
c  "je" in the 2-direction, and "ks" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for third order
c  interpolation. The ranges for HD boundary value calculations are:
c
c    i-boundaries:                    j = js  , je     k = ks  , ke
c    j-boundaries:   i = is  , ie                      k = ks  , ke
c    k-boundaries:   i = is  , ie     j = js  , je
c
c  However, edge and corner boundary values are required the MOC 
c  algorithm.
c
c  Boundary values are set for the first two zones beyond the boundary
c  to allow for third order interpolations.
c
c  Boundary values are set according to the the basic flow types:
c
c      nflo = 1  =>  reflecting [v(normal) = b(normal) = 0]
c           =-1  =>  reflecting (: same as 1; ZRP: same as 1 with
c                    inversion of 3-components at ijb; RTP: same as 1
c                    with inversion of 2- and 3-components at iib and
c                    inversion of 3-components at ijb and ojb.)
c           = 2  =>  flow out
c           = 3  =>  flow in
c           = 4  =>  periodic
c           = 5  =>  reflecting [v(normal) = 0, b(tangential) = 0]
c
c  If desired, every boundary zone may be given a different boundary
c  type.  These types are stored in the following six arrays:
c
c      niib(j,k) = nflo of inner i boundary on sweep j,k
c      noib(j,k) = nflo of outer i boundary on sweep j,k
c      nijb(k,i) = nflo of inner j boundary on sweep k,i
c      nojb(k,i) = nflo of outer j boundary on sweep k,i
c      nikb(i,j) = nflo of inner k boundary on sweep i,j
c      nokb(i,j) = nflo of outer k boundary on sweep i,j
c
c  In addition, there are "niib2", "niib3", and "niib23" to account for
c  the different centring of the variables.  Similar arrays are defined
c  for all other boundaries (see discussion in BNDYFLGS).
c
c  Note that there is no point in setting the boundaries if the grid
c  stretching routine (EXTEND) is used, until the solution actually
c  reaches the edge of the grid.
c
c  Flags rl1, ru1, rl2, ru2, rl3, and ru3 request boundary data:
c      = 0  => do nothing
c      = 1  => pass 1st         slab
c      = 3  => pass 1st and 2nd slab
c
c  Array bvstat in /bndryi/ records the status of boundary values:
c      = 0  => needs updating
c      = 1  => 1st         slab  is  up to date, but not the second.
c      = 3  => 1st and 2nd slabs are up to date.
c
c  The values of bvstat must be reset to 0 when the corresponding
c  function is updated in some external routine.
c
c  For the mass density only, if an "rl" = 3, we pass an extra layer to 
c  the "m" tile, allowing us to compute the mass flux at the point
c  x[123]a([ijk]s-1) in TRANX[123].
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
       real*8    d(in,jn,kn)
c
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l1 = rl1 - bvstat(1,1)
       u1 = ru1 - bvstat(2,1)
c
c      Inner i boundary.
c

         if (l1 .gt. 0) then
         do 30 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 25 j=js-1,je+1
             if ( abs(niib(j,k)) .eq. 1) then
               d(is-1,j,k) = d(is  ,j,k)
               d(is-2,j,k) = d(is+1,j,k)
               diib(j,k,3) = d(is+2,j,k)
             endif
             if (niib(j,k) .eq. 2) then
               d(is-1,j,k) = d(is  ,j,k)
               d(is-2,j,k) = d(is-1,j,k)
               diib(j,k,3) = d(is-2,j,k)
             endif
             if (niib(j,k) .eq. 3) then
               d(is-1,j,k) = diib (j,k,1)
               d(is-2,j,k) = diib (j,k,2)
c              do nothing for diib(j,k,3)
             endif

             if (niib(j,k) .eq. 4) then
               d(is-1,j,k) = d(ie  ,j,k)
               d(is-2,j,k) = d(ie-1,j,k)
               diib(j,k,3) = d(ie-2,j,k)
             endif

             if (niib(j,k) .eq. 5) then
               d(is-1,j,k) = d(is  ,j,k)
               d(is-2,j,k) = d(is+1,j,k)
               diib(j,k,3) = d(is+2,j,k)
             endif
25         continue
30       continue
         bvstat(1,1) = rl1
         endif

c
c      Outer i boundary.
c

         if (u1 .gt. 0) then
         do 60 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 55 j=js-1,je+1
             if ( abs(noib(j,k)) .eq. 1) then
               d(ie+1,j,k) = d(ie,j,k)
               d(ie+2,j,k) = d(ie-1,j,k)
             endif
             if (noib(j,k) .eq. 2) then
               d(ie+1,j,k) = d(ie,j,k)
               d(ie+2,j,k) = d(ie+1,j,k)
             endif
             if (noib(j,k) .eq. 3) then
               d(ie+1,j,k) = doib (j,k,1)
               d(ie+2,j,k) = doib (j,k,2)
             endif

             if (noib(j,k) .eq. 4) then
               d(ie+1,j,k) = d(is  ,j,k)
               d(ie+2,j,k) = d(is+1,j,k)
             endif

             if (noib(j,k) .eq. 5) then
               d(ie+1,j,k) = d(ie  ,j,k)
               d(ie+2,j,k) = d(ie-1,j,k)
             endif
55         continue
60       continue
         bvstat(2,1) = ru1
         endif

c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l2 = rl2 - bvstat(3,1)
       u2 = ru2 - bvstat(4,1)
c
c      Inner j boundary.
c

         if (l2 .gt. 0) then
         do 90 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 85 i=is-1,ie+1
             if ( abs(nijb(i,k)) .eq. 1) then
               d(i,js-1,k) = d(i,js  ,k)
               d(i,js-2,k) = d(i,js+1,k)
               dijb(i,k,3) = d(i,js+2,k)
             endif
             if (nijb(i,k) .eq. 2) then
               d(i,js-1,k) = d(i,js  ,k)
               d(i,js-2,k) = d(i,js-1,k)
               dijb(i,k,3) = d(i,js-2,k)
             endif
             if (nijb(i,k) .eq. 3) then
               d(i,js-1,k) = dijb (i,k,1)
               d(i,js-2,k) = dijb (i,k,2)
c              do nothing for dijb(i,k,3)
             endif

             if (nijb(i,k) .eq. 4) then
               d(i,js-1,k) = d(i,je  ,k)
               d(i,js-2,k) = d(i,je-1,k)
               dijb(i,k,3) = d(i,je-2,k)
             endif

             if (nijb(i,k) .eq. 5) then
               d(i,js-1,k) = d(i,js  ,k)
               d(i,js-2,k) = d(i,js+1,k)
               dijb(i,k,3) = d(i,js+2,k)
             endif
85         continue
90       continue
         bvstat(3,1) = rl2
         endif

c
c      Outer j boundary.
c

         if (u2 .gt. 0) then
         do 120 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 115 i=is-1,ie+1
             if ( abs(nojb(i,k)) .eq. 1) then
               d(i,je+1,k) = d(i,je  ,k)
               d(i,je+2,k) = d(i,je-1,k)
             endif
             if (nojb(i,k) .eq. 2) then
               d(i,je+1,k) = d(i,je  ,k)
               d(i,je+2,k) = d(i,je+1,k)
             endif
             if (nojb(i,k) .eq. 3) then
               d(i,je+1,k) = dojb (i,k,1)
               d(i,je+2,k) = dojb (i,k,2)
             endif

             if (nojb(i,k) .eq. 4) then
               d(i,je+1,k) = d(i,js  ,k)
               d(i,je+2,k) = d(i,js+1,k)
             endif

             if (nojb(i,k) .eq. 5) then
               d(i,je+1,k) = d(i,je  ,k)
               d(i,je+2,k) = d(i,je-1,k)
             endif
115        continue
120      continue
         bvstat(4,1) = ru2
         endif

c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l3 = rl3 - bvstat(5,1)
       u3 = ru3 - bvstat(6,1)
c
c      Inner k boundary.
c

         if (l3 .gt. 0) then
         do 150 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 145 i=is-1,ie+1
             if ( abs(nikb(i,j)) .eq. 1) then
               d(i,j,ks-1) = d(i,j,ks  )
               d(i,j,ks-2) = d(i,j,ks+1)
               dikb(i,j,3) = d(i,j,ks+2)
             endif
             if (nikb(i,j) .eq. 2) then
               d(i,j,ks-1) = d(i,j,ks  )
               d(i,j,ks-2) = d(i,j,ks-1)
               dikb(i,j,3) = d(i,j,ks-2)
             endif
             if (nikb(i,j) .eq. 3) then
               d(i,j,ks-1) = dikb (i,j,1)
               d(i,j,ks-2) = dikb (i,j,2)
c              do nothing for dikb(i,j,3)
             endif

             if (nikb(i,j) .eq. 4) then
               d(i,j,ks-1) = d(i,j,ke  )
               d(i,j,ks-2) = d(i,j,ke-1)
               dikb(i,j,3) = d(i,j,ke-2)
             endif

             if (nikb(i,j) .eq. 5) then
               d(i,j,ks-1) = d(i,j,ks  )
               d(i,j,ks-2) = d(i,j,ks+1)
               dikb(i,j,3) = d(i,j,ks+2)
             endif
145        continue
150      continue
         bvstat(5,1) = rl3
         endif

c
c      Outer k boundary.
c

         if (u3 .gt. 0) then
         do 180 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 175 i=is-1,ie+1
             if ( abs(nokb(i,j)) .eq. 1) then
               d(i,j,ke+1) = d(i,j,ke  )
               d(i,j,ke+2) = d(i,j,ke-1)
             endif
             if (nokb(i,j) .eq. 2) then
               d(i,j,ke+1) = d(i,j,ke  )
               d(i,j,ke+2) = d(i,j,ke+1)
             endif
             if (nokb(i,j) .eq. 3) then
               d(i,j,ke+1) = dokb (i,j,1)
               d(i,j,ke+2) = dokb (i,j,2)
             endif

             if (nokb(i,j) .eq. 4) then
               d(i,j,ke+1) = d(i,j,ks  )
               d(i,j,ke+2) = d(i,j,ks+1)
             endif

             if (nokb(i,j) .eq. 5) then
               d(i,j,ke+1) = d(i,j,ke  )
               d(i,j,ke+2) = d(i,j,ke-1)
             endif
175        continue
180      continue
         bvstat(6,1) = ru3
         endif

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 B V A L D                 \\\\\\\\\\
c
c=======================================================================
c
c













































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 B V A L E                 \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvale ( rl1, ru1, rl2, ru2, rl3, ru3, e )

c
c    dac:zeus3d.bvale <--------- internal energy density boundary values
c    from mln:zeus04.bval; jms:zeus2d.bvale               february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: RAF, 3/5/96 for ZEUS-MP
c    modified 2: RAF, 8/27/96; correction for periodic BCs iib, no MPI.
c
c  PURPOSE: This routine sets boundary values for the internal energy.
c  The active zones for "e" are "is" to "ie" in the 1-direction, "js" to
c  "je" in the 2-direction, and "ks" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for third order
c  interpolation.  No edge or corner boundary values are required.
c  Thus, the ranges for the boundary value calculations are:
c
c    i-boundaries:                    j = js  , je     k = ks  , ke
c    j-boundaries:   i = is  , ie                      k = ks  , ke
c    k-boundaries:   i = is  , ie     j = js  , je
c
c  See comments in BVALD.
c
c  Flags l1, l2, l3, activate the 1-, 2-, and 3- loops when nonzero.
c  Their values give the number of layers to pass.
c  Flag iupper activates enables sends    in the "m" direction and
c                                receives in the "p" direction when
c  nonzero. 
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
       real*8    e(in,jn,kn)
c
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l1 = rl1 - bvstat(1,2)
       u1 = ru1 - bvstat(2,2)
c
c      Inner i boundary.
c

         if (l1 .gt. 0) then
         do 230 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 225 j=js-1,je+1
             if ( abs(niib(j,k)) .eq. 1) then
               e(is-1,j,k) = e(is  ,j,k)
               e(is-2,j,k) = e(is+1,j,k)
             endif
             if (niib(j,k) .eq. 2) then
               e(is-1,j,k) = e(is  ,j,k)
               e(is-2,j,k) = e(is-1,j,k)
             endif
             if (niib(j,k) .eq. 3) then
               if (l1.gt.1 ) then   
c  PASS e/d (NOT TOTAL_ENERGY)
                 e(is-1,j,k) = eiib (j,k,1) / diib (j,k,1)
                 e(is-2,j,k) = eiib (j,k,2) / diib (j,k,2)
               else                 
c  PASS e
                 e(is-1,j,k) = eiib (j,k,1)
                 e(is-2,j,k) = eiib (j,k,2)
               endif
             endif

             if (niib(j,k) .eq. 4) then
               e(is-1,j,k) = e(ie  ,j,k)
               e(is-2,j,k) = e(ie-1,j,k)
             endif

             if (niib(j,k) .eq. 5) then
               e(is-1,j,k) = e(is  ,j,k)
               e(is-2,j,k) = e(is+1,j,k)
             endif
225        continue
230      continue
         bvstat(1,2) = rl1
         endif

c
c      Outer i boundary.
c

         if (u1 .gt. 0) then
         do 260 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 255 j=js-1,je+1
             if ( abs(noib(j,k)) .eq. 1) then
               e(ie+1,j,k) = e(ie,j,k)
               e(ie+2,j,k) = e(ie-1,j,k)
             endif
             if (noib(j,k) .eq. 2) then
               e(ie+1,j,k) = e(ie,j,k)
               e(ie+2,j,k) = e(ie+1,j,k)
             endif
             if (noib(j,k) .eq. 3) then
               if (l1.gt.1 ) then   
c  Pass e/d (not for TOTAL_ENERGY)
                 e(ie+1,j,k) = eoib (j,k,1) / doib (j,k,1)
                 e(ie+2,j,k) = eoib (j,k,2) / doib (j,k,2)
               else                 
c  Pass e
                 e(ie+1,j,k) = eoib (j,k,1)
                 e(ie+2,j,k) = eoib (j,k,2)
               endif
             endif

             if (noib(j,k) .eq. 4) then
               e(ie+1,j,k) = e(is  ,j,k)
               e(ie+2,j,k) = e(is+1,j,k)
             endif

             if (noib(j,k) .eq. 5) then
               e(ie+1,j,k) = e(ie  ,j,k)
               e(ie+2,j,k) = e(ie-1,j,k)
             endif
255        continue
260      continue
         bvstat(2,2) = ru1
         endif

c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l2 = rl2 - bvstat(3,2)
       u2 = ru2 - bvstat(4,2)
c
c      Inner j boundary.
c

         if (l2 .gt. 0) then
         do 290 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 285 i=is-1,ie+1
             if ( abs(nijb(i,k)) .eq. 1) then
               e(i,js-1,k) = e(i,js  ,k)
               e(i,js-2,k) = e(i,js+1,k)
             endif
             if (nijb(i,k) .eq. 2) then
               e(i,js-1,k) = e(i,js  ,k)
               e(i,js-2,k) = e(i,js-1,k)
             endif
             if (nijb(i,k) .eq. 3) then
               if (l2.gt.1 ) then   
c  Pass e/d (not for TOTAL_ENERGY)
                 e(i,js-1,k) = eijb (i,k,1) / dijb (i,k,1)
                 e(i,js-2,k) = eijb (i,k,2) / dijb (i,k,2)
               else                 
c  Pass e
                 e(i,js-1,k) = eijb (i,k,1)
                 e(i,js-2,k) = eijb (i,k,2)
               endif
             endif

             if (nijb(i,k) .eq. 4) then
               e(i,js-1,k) = e(i,je  ,k)
               e(i,js-2,k) = e(i,je-1,k)
             endif

             if (nijb(i,k) .eq. 5) then
               e(i,js-1,k) = e(i,js  ,k)
               e(i,js-2,k) = e(i,js+1,k)
             endif
285        continue
290      continue
         bvstat(3,2) = rl2
         endif

c
c      Outer j boundary.
c

         if (u2 .gt. 0) then
         do 320 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 315 i=is-1,ie+1
             if ( abs(nojb(i,k)) .eq. 1) then
               e(i,je+1,k) = e(i,je  ,k)
               e(i,je+2,k) = e(i,je-1,k)
             endif
             if (nojb(i,k) .eq. 2) then
               e(i,je+1,k) = e(i,je  ,k)
               e(i,je+2,k) = e(i,je+1,k)
             endif
             if (nojb(i,k) .eq. 3) then
               if (l2.gt.1 ) then   
c  Pass e/d (not for TOTAL_ENERGY)
                 e(i,je+1,k) = eojb (i,k,1) / dojb (i,k,1)
                 e(i,je+2,k) = eojb (i,k,2) / dojb (i,k,2)
               else                 
c  Pass e
                 e(i,je+1,k) = eojb (i,k,1)
                 e(i,je+2,k) = eojb (i,k,2)
               endif
             endif

             if (nojb(i,k) .eq. 4) then
               e(i,je+1,k) = e(i,js  ,k)
               e(i,je+2,k) = e(i,js+1,k)
             endif

             if (nojb(i,k) .eq. 5) then
               e(i,je+1,k) = e(i,je  ,k)
               e(i,je+2,k) = e(i,je-1,k)
             endif
315        continue
320      continue
         bvstat(4,2) = ru2
         endif

c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l3 = rl3 - bvstat(5,2)
       u3 = ru3 - bvstat(6,2)
c
c      Inner k boundary.
c

         if (l3 .gt. 0) then
         do 350 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 345 i=is-1,ie+1
             if ( abs(nikb(i,j)) .eq. 1) then
               e(i,j,ks-1) = e(i,j,ks  )
               e(i,j,ks-2) = e(i,j,ks+1)
             endif
             if (nikb(i,j) .eq. 2) then
               e(i,j,ks-1) = e(i,j,ks  )
               e(i,j,ks-2) = e(i,j,ks-1)
             endif
             if (nikb(i,j) .eq. 3) then
               if (l3.gt.1 ) then   
c  Pass e/d (not for TOTAL_ENERGY)
                 e(i,j,ks-1) = eikb (i,j,1) / dikb (i,j,1)
                 e(i,j,ks-2) = eikb (i,j,2) / dikb (i,j,2)
               else                 
c  Pass e
                 e(i,j,ks-1) = eikb (i,j,1)
                 e(i,j,ks-2) = eikb (i,j,2)
               endif
             endif

             if (nikb(i,j) .eq. 4) then
               e(i,j,ks-1) = e(i,j,ke  )
               e(i,j,ks-2) = e(i,j,ke-1)
             endif

             if (nikb(i,j) .eq. 5) then
               e(i,j,ks-1) = e(i,j,ks  )
               e(i,j,ks-2) = e(i,j,ks+1)
             endif
345        continue
350      continue
         bvstat(5,2) = rl3
         endif

c
c      Outer k boundary.
c

         if (u3 .gt. 0) then
         do 380 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 375 i=is-1,ie+1
             if ( abs(nokb(i,j)) .eq. 1) then
               e(i,j,ke+1) = e(i,j,ke  )
               e(i,j,ke+2) = e(i,j,ke-1)
             endif
             if (nokb(i,j) .eq. 2) then
               e(i,j,ke+1) = e(i,j,ke  )
               e(i,j,ke+2) = e(i,j,ke+1)
             endif
             if (nokb(i,j) .eq. 3) then
               if (l3.gt.1 ) then   
c  Pass e/d (not for TOTAL_ENERGY)
                 e(i,j,ke+1) = eokb (i,j,1) / dokb (i,j,1)
                 e(i,j,ke+2) = eokb (i,j,2) / dokb (i,j,2)
               else                 
c  Pass e
                 e(i,j,ke+1) = eokb (i,j,1)
                 e(i,j,ke+2) = eokb (i,j,2)
               endif
             endif

             if (nokb(i,j) .eq. 4) then
               e(i,j,ke+1) = e(i,j,ks  )
               e(i,j,ke+2) = e(i,j,ks+1)
             endif

             if (nokb(i,j) .eq. 5) then
               e(i,j,ke+1) = e(i,j,ke  )
               e(i,j,ke+2) = e(i,j,ke-1)
             endif
375        continue
380      continue
         bvstat(6,2) = ru3
         endif

c

       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                B V A L E                  \\\\\\\\\\
c
c=======================================================================
c
c













































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 B V A L E R S             \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalers ( rl1, ru1, rl2, ru2, rl3, ru3, er )
c
c    Written : RAF, 9/23/96 for ZEUS-MP
c    Last modified: 2/17/97
c
c  PURPOSE: This routine sets boundary values for the raditation
c  energy density.
c
c  This version is to be called from a source step routine.  If er is
c  specified on a boundary, er**b is used for boundary values.
c
c  The active zones for "t" are "is" to "ie" in the 1-direction, "js" to
c  "je" in the 2-direction, and "ks" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for third order
c  interpolation.  No edge or corner boundary values are required.
c  Thus, the ranges for the boundary value calculations are:
c
c    i-boundaries:                    j = js  , je     k = ks  , ke
c    j-boundaries:   i = is  , ie                      k = ks  , ke
c    k-boundaries:   i = is  , ie     j = js  , je
c
c  See comments in BVALD.
c
c  Flags l1, l2, l3, activate the 1-, 2-, and 3- loops when nonzero.
c  Their values give the number of layers to pass.
c  Flag iupper activates enables sends    in the "m" direction and
c                                receives in the "p" direction when
c  nonzero. 
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
       real*8    er(in,jn,kn)
c
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                B V A L E R S              \\\\\\\\\\
c
c=======================================================================
c
c













































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 B V A L E R T             \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalert ( rl1, ru1, rl2, ru2, rl3, ru3, er )
c
c    Written : RAF, 9/23/96 for ZEUS-MP
c    Last modified: 2/17/97
c
c  PURPOSE: This routine sets boundary values for the raditation
c  energy density.
c
c  This version is to be called from a transport step routine.  If er is
c  specified on a boundary, er**b/d**b is used for boundary values.
c
c  The active zones for "t" are "is" to "ie" in the 1-direction, "js" to
c  "je" in the 2-direction, and "ks" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for third order
c  interpolation.  No edge or corner boundary values are required.
c  Thus, the ranges for the boundary value calculations are:
c
c    i-boundaries:                    j = js  , je     k = ks  , ke
c    j-boundaries:   i = is  , ie                      k = ks  , ke
c    k-boundaries:   i = is  , ie     j = js  , je
c
c  See comments in BVALD.
c
c  Flags l1, l2, l3, activate the 1-, 2-, and 3- loops when nonzero.
c  Their values give the number of layers to pass.
c  Flag iupper activates enables sends    in the "m" direction and
c                                receives in the "p" direction when
c  nonzero. 
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
       real*8    er(in,jn,kn)
c
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                B V A L E R                \\\\\\\\\\
c
c=======================================================================
c
c













































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                B V A L V 1                \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalv1 ( rl1, ru1, rl2, ru2, rl3, ru3, v1 )
c
c    dac:zeus3d.bvalv1 <----------- 1-direction velocity boundary values
c    from mln:zeus04.bflo; jms:zeus2d.bvalv1              february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: RAF, 3/5/96 for ZEUS-MP
c
c  PURPOSE: This routine sets boundary values for the 1-velocity.  The
c  active zones for "v1" are "is+1" to "ie" in the 1-direction, "js" to
c  "je" in the 2-direction, and "ks" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for third order
c  interpolation.  No edge or corner boundary values are required.
c  Thus, the ranges for the boundary value calculations are:
c
c    i-boundaries:                    j = js  , je     k = ks  , ke
c    j-boundaries:   i = is+1, ie                      k = ks  , ke
c    k-boundaries:   i = is+1, ie     j = js  , je
c
c  Note that for periodic or tile-tile boundaries, "is" is also active.
c
c  The flow out boundary uses a switch to ensure fluid can only flow OUT
c  of the i boundary (boundary value set to 0 if it tries to flow in).
c
c  See comments in BVALD.
c
c  Flags l1, l2, l3 activate the 1-, 2-, and 3- loops when nonzero.
c  Their values give the number of layers to pass.
c
c  Array v1 is input so that velocity values at old time levels
c  as well as momentum components can be passed.
c
c  NOTE: Need to know whether to pass velocity or momentum density
c        for inflow boundaries.
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
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

       real*8    v1(in,jn,kn)
       real*8    q1
c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l1 = rl1 - bvstat(1,3)
       u1 = ru1 - bvstat(2,3)
c
c      Inner i boundary.
c


         if (l1 .gt. 0) then
         do 430 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 425 j=js-1,je+1
             if ( abs(niib(j,k)) .eq. 1) then
               v1(is  ,j,k) =       vg1(is)
               v1(is-1,j,k) = 2.0 * vg1(is) - v1(is+1,j,k)
               v1(is-2,j,k) = 2.0 * vg1(is) - v1(is+2,j,k)
             endif
             if (niib(j,k) .eq. 2) then
               q1           = sign ( haf, v1(is+1,j,k) - vg1(is) )
               v1(is  ,j,k) = v1(is+1,j,k) * ( 0.5 - q1 )
               v1(is-1,j,k) = v1(is  ,j,k)
               v1(is-2,j,k) = v1(is  ,j,k)
             endif
             if (niib(j,k) .eq. 3) then
               v1(is  ,j,k) = v1iib (j,k,1)
               v1(is-1,j,k) = v1iib (j,k,2)
               v1(is-2,j,k) = 2.0 * v1iib (j,k,2) - v1iib (j,k,1)
             endif

             if (niib(j,k) .eq. 4) then
               v1(is-1,j,k) = v1(ie  ,j,k)
               v1(is-2,j,k) = v1(ie-1,j,k)
             endif

             if (niib(j,k) .eq. 5) then
               v1(is  ,j,k) =       vg1(is)
               v1(is-1,j,k) = 2.0 * vg1(is) - v1(is+1,j,k)
               v1(is-2,j,k) = 2.0 * vg1(is) - v1(is+2,j,k)
             endif
425        continue
430      continue
         bvstat(1,3) = rl1
         endif

c
c      Outer i boundary.
c
c 1-face-centered quantities need to be evolved on only one end
c (we have chosen to evolve them on the inner boundary), hence
c no change in subscripts compared to zone-centered quantities.
c

         if (u1 .gt. 0) then
         do 460 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 455 j=js-1,je+1
             if ( abs(noib(j,k)) .eq. 1) then
               v1(ie+1,j,k) =       vg1(ie+1)
               v1(ie+2,j,k) = 2.0 * vg1(ie+1) - v1(ie  ,j,k)
             endif
             if (noib(j,k) .eq. 2) then
               q1           = sign ( haf, v1(ie,j,k) - vg1(ie+1) )
               v1(ie+1,j,k) = v1(ie,j,k) * ( 0.5 + q1 )
               v1(ie+2,j,k) = v1(ie+1,j,k)
             endif
             if (noib(j,k) .eq. 3) then
               v1(ie+1,j,k) = v1oib (j,k,1)
               v1(ie+2,j,k) = v1oib (j,k,2)
             endif

             if (noib(j,k) .eq. 4) then
               v1(ie+1,j,k) = v1(is  ,j,k)
               v1(ie+2,j,k) = v1(is+1,j,k)
             endif

             if (noib(j,k) .eq. 5) then
               v1(ie+1,j,k) =       vg1(ie+1)
               v1(ie+2,j,k) = 2.0 * vg1(ie+1) - v1(ie,j,k)
             endif
455        continue
460      continue
         bvstat(2,3) = ru1
         endif

c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l2 = rl2 - bvstat(3,3)
       u2 = ru2 - bvstat(4,3)
c
c      Inner j boundary.
c

         if (l2 .gt. 0) then
         do 490 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 485 i=is-1,ie+1
             if ( abs(nijb1(i,k)) .eq. 1) then
               v1(i,js-1,k) = v1(i,js  ,k)
               v1(i,js-2,k) = v1(i,js+1,k)
             endif
             if (nijb1(i,k) .eq. 2) then
               v1(i,js-1,k) = v1(i,js  ,k)
               v1(i,js-2,k) = v1(i,js-1,k)
             endif
             if (nijb1(i,k) .eq. 3) then
               v1(i,js-1,k) = v1ijb (i,k,1)
               v1(i,js-2,k) = v1ijb (i,k,2)
             endif

             if (nijb1(i,k) .eq. 4) then
               v1(i,js-1,k) = v1(i,je  ,k)
               v1(i,js-2,k) = v1(i,je-1,k)
             endif

             if (nijb1(i,k) .eq. 5) then
               v1(i,js-1,k) = v1(i,js  ,k)
               v1(i,js-2,k) = v1(i,js+1,k)
             endif
485        continue
490      continue
         bvstat(3,3) = rl2
         endif

c
c      Outer j boundary.
c

         if (u2 .gt. 0) then
         do 520 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 515 i=is-1,ie+1
             if ( abs(nojb1(i,k)) .eq. 1) then
               v1(i,je+1,k) = v1(i,je  ,k)
               v1(i,je+2,k) = v1(i,je-1,k)
             endif
             if (nojb1(i,k) .eq. 2) then
               v1(i,je+1,k) = v1(i,je  ,k)
               v1(i,je+2,k) = v1(i,je+1,k)
             endif
             if (nojb1(i,k) .eq. 3) then
               v1(i,je+1,k) = v1ojb (i,k,1)
               v1(i,je+2,k) = v1ojb (i,k,2)
             endif

             if (nojb1(i,k) .eq. 4) then
               v1(i,je+1,k) = v1(i,js  ,k)
               v1(i,je+2,k) = v1(i,js+1,k)
             endif

             if (nojb1(i,k) .eq. 5) then
               v1(i,je+1,k) = v1(i,je  ,k)
               v1(i,je+2,k) = v1(i,je-1,k)
             endif
515        continue
520      continue
         bvstat(4,3) = ru2
         endif

c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l3 = rl3 - bvstat(5,3)
       u3 = ru3 - bvstat(6,3)
c
c      Inner k boundary.
c

       if (l3 .gt. 0) then
       do 550 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 545 i=is-1,ie+1
             if ( abs(nikb1(i,j)) .eq. 1) then
               v1(i,j,ks-1) = v1(i,j,ks  )
               v1(i,j,ks-2) = v1(i,j,ks+1)
             endif
             if (nikb1(i,j) .eq. 2) then
               v1(i,j,ks-1) = v1(i,j,ks  )
               v1(i,j,ks-2) = v1(i,j,ks-1)
             endif
             if (nikb1(i,j) .eq. 3) then
               v1(i,j,ks-1) = v1ikb (i,j,1)
               v1(i,j,ks-2) = v1ikb (i,j,2)
             endif

             if (nikb1(i,j) .eq. 4) then
               v1(i,j,ks-1) = v1(i,j,ke  )
               v1(i,j,ks-2) = v1(i,j,ke-1)
             endif

             if (nikb1(i,j) .eq. 5) then
               v1(i,j,ks-1) = v1(i,j,ks  )
               v1(i,j,ks-2) = v1(i,j,ks+1)
             endif
545        continue
550      continue
         bvstat(5,3) = rl3
         endif

c
c      Outer k boundary.
c

         if (u3 .gt. 0) then
         do 580 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 575 i=is-1,ie+1
             if ( abs(nokb1(i,j)) .eq. 1) then
               v1(i,j,ke+1) = v1(i,j,ke  )
               v1(i,j,ke+2) = v1(i,j,ke-1)
             endif
             if (nokb1(i,j) .eq. 2) then
               v1(i,j,ke+1) = v1(i,j,ke  )
               v1(i,j,ke+2) = v1(i,j,ke+1)
             endif
             if (nokb1(i,j) .eq. 3) then
               v1(i,j,ke+1) = v1okb (i,j,1)
               v1(i,j,ke+2) = v1okb (i,j,2)
             endif

             if (nokb1(i,j) .eq. 4) then
               v1(i,j,ke+1) = v1(i,j,ks  )
               v1(i,j,ke+2) = v1(i,j,ks+1)
             endif

             if (nokb1(i,j) .eq. 5) then
               v1(i,j,ke+1) = v1(i,j,ke  )
               v1(i,j,ke+2) = v1(i,j,ke-1)
             endif
575        continue
580      continue
         bvstat(6,3) = ru3
         endif

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                B V A L V 1                \\\\\\\\\\
c
c=======================================================================
c
c













































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                B V A L V 2                \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalv2 ( rl1, ru1, rl2, ru2, rl3, ru3, v2 )
c
c    dac:zeus3d.bvalv2 <----------- 2-direction velocity boundary values
c    from mln:zeus04.bflo; jms:zeus2d.bvalv2              february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: RAF, 3/5/96 for ZEUS-MP
c
c  PURPOSE: This routine sets boundary values for the 2-velocity.  The
c  active zones for "v2" are "is" to "ie" in the 1-direction, "js+1" to
c  "je" in the 2-direction, and "ks" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for third order
c  interpolation.  No edge or corner boundary values are required.
c  Thus, the ranges for the boundary value calculations are:
c
c    i-boundaries:                    j = js+1, je     k = ks  , ke
c    j-boundaries:   i = is  , ie                      k = ks  , ke
c    k-boundaries:   i = is  , ie     j = js+1, je
c
c  Note that for periodic or tile-tile boundaries, "js" is also active.
c
c  The flow out boundary uses a switch to ensure fluid can only flow OUT
c  of the j boundary (boundary value set to 0 if it tries to flow in).
c
c  See comments in BVALD.
c
c  Flags l1, l2, l3, activate the 1-, 2-, and 3- loops when nonzero.
c  Their values give the number of layers to pass.
c
c  Array v2 is input so that velocity values at old time levels
c  and momenta can be passed.
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
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

       real*8    v2(in,jn,kn)
       real*8    q1
c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l1 = rl1 - bvstat(1,4)
       u1 = ru1 - bvstat(2,4)
c
c      Inner i boundary.
c

c
         if (l1 .gt. 0) then
         do 630 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 625 j=js-1,je+1
             if (niib2(j,k) .eq. 1) then
               v2(is-1,j,k) = v2(is  ,j,k)
               v2(is-2,j,k) = v2(is+1,j,k)
             endif
             if (niib2(j,k) .eq.-1) then

               v2(is-1,j,k) = v2(is  ,j,k)
               v2(is-2,j,k) = v2(is+1,j,k)

             endif
             if (niib2(j,k) .eq. 2) then
               v2(is-1,j,k) = v2(is  ,j,k)
               v2(is-2,j,k) = v2(is-1,j,k)
             endif
             if (niib2(j,k) .eq. 3) then
               v2(is-1,j,k) = v2iib (j,k,1)
               v2(is-2,j,k) = v2iib (j,k,2)
             endif

             if (niib2(j,k) .eq. 4) then
               v2(is-1,j,k) = v2(ie  ,j,k)
               v2(is-2,j,k) = v2(ie-1,j,k)
             endif

             if (niib2(j,k) .eq. 5) then
               v2(is-1,j,k) = v2(is  ,j,k)
               v2(is-2,j,k) = v2(is+1,j,k)
             endif
625        continue
630      continue
         bvstat(1,4) = rl1
         endif

c
c      Outer i boundary.
c

         if (u1 .gt. 0) then
         do 660 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 655 j=js-1,je+1
             if ( abs(noib2(j,k)) .eq. 1) then
               v2(ie+1,j,k) = v2(ie,j,k)
               v2(ie+2,j,k) = v2(ie-1,j,k)
             endif
             if (noib2(j,k) .eq. 2) then
c#if blast == advect  --   I wish cpp could do this!
c               v2(ie+1,j,k) = 2.0 * v2(ie  ,j,k) - v2(ie-1,j,k)
c               v2(ie+2,j,k) = 2.0 * v2(ie+1,j,k) - v2(ie  ,j,k)
c#else
               v2(ie+1,j,k) = v2(ie,j,k)
               v2(ie+2,j,k) = v2(ie+1,j,k)
c#endif
             endif
             if (noib2(j,k) .eq. 3) then
               v2(ie+1,j,k) = v2oib (j,k,1)
               v2(ie+2,j,k) = v2oib (j,k,2)
             endif

             if (noib2(j,k) .eq. 4) then
               v2(ie+1,j,k) = v2(is  ,j,k)
               v2(ie+2,j,k) = v2(is+1,j,k)
             endif

             if (noib2(j,k) .eq. 5) then
               v2(ie+1,j,k) = v2(ie  ,j,k)
               v2(ie+2,j,k) = v2(ie-1,j,k)
             endif
655        continue
660      continue
         bvstat(2,4) = ru1
         endif

c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l2 = rl2 - bvstat(3,4)
       u2 = ru2 - bvstat(4,4)
c
c      Inner j boundary.
c

         if (l2 .gt. 0) then
         do 690 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 685 i=is-1,ie+1
             if ( abs(nijb(i,k)) .eq. 1) then
               v2(i,js  ,k) =       vg2(js)
               v2(i,js-1,k) = 2.0 * vg2(js) - v2(i,js+1,k)
               v2(i,js-2,k) = 2.0 * vg2(js) - v2(i,js+2,k)
             endif
             if (nijb(i,k) .eq. 2) then
               q1           = sign ( haf, v2(i,js+1,k) - vg2(js) )
               v2(i,js  ,k) = v2(i,js+1,k) * ( 0.5 - q1 )
               v2(i,js-1,k) = v2(i,js  ,k)
               v2(i,js-2,k) = v2(i,js  ,k)
             endif
             if (nijb(i,k) .eq. 3) then
               v2(i,js  ,k) = v2ijb (i,k,1)
               v2(i,js-1,k) = v2ijb (i,k,2)
               v2(i,js-2,k) = 2.0 * v2ijb (i,k,2) - v2ijb (i,k,1)
             endif

             if (nijb(i,k) .eq. 4) then
               v2(i,js-1,k) = v2(i,je  ,k)
               v2(i,js-2,k) = v2(i,je-1,k)
             endif

             if (nijb(i,k) .eq. 5) then
               v2(i,js  ,k) =       vg2(js)
               v2(i,js-1,k) = 2.0 * vg2(js) - v2(i,js+1,k)
               v2(i,js-2,k) = 2.0 * vg2(js) - v2(i,js+2,k)
             endif
685        continue
690      continue
         bvstat(3,4) = rl2
         endif

c
c      Outer j boundary.
c

         if (u2 .gt. 0) then
         do 720 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 715 i=is-1,ie+1
             if ( abs(nojb(i,k)) .eq. 1) then
               v2(i,je+1,k) =       vg2(je+1)
               v2(i,je+2,k) = 2.0 * vg2(je+1) - v2(i,je  ,k)
             endif
             if (nojb(i,k) .eq. 2) then
               q1           = sign ( haf, v2(i,je,k) - vg2(je+1) )
               v2(i,je+1,k) = v2(i,je  ,k) * ( 0.5 + q1 )
               v2(i,je+2,k) = v2(i,je+1,k)
             endif
             if (nojb(i,k) .eq. 3) then
               v2(i,je+1,k) = v2ojb (i,k,1)
               v2(i,je+2,k) = v2ojb (i,k,2)
             endif

             if (nojb(i,k) .eq. 4) then
               v2(i,je+1,k) = v2(i,js  ,k)
               v2(i,je+2,k) = v2(i,js+1,k)
             endif

             if (nojb(i,k) .eq. 5) then
               v2(i,je+1,k) =       vg2(je+1)
               v2(i,je+2,k) = 2.0 * vg2(je+1) - v2(i,je  ,k)
             endif
715        continue
720      continue
         bvstat(4,4) = ru2
         endif

c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l3 = rl3 - bvstat(5,4)
       u3 = ru3 - bvstat(6,4)
c
c      Inner k boundary.
c

         if (l3 .gt. 0) then
         do 750 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 745 i=is-1,ie+1
             if ( abs(nikb2(i,j)) .eq. 1) then
               v2(i,j,ks-1) = v2(i,j,ks  )
               v2(i,j,ks-2) = v2(i,j,ks+1)
             endif
             if (nikb2(i,j) .eq. 2) then
               v2(i,j,ks-1) = v2(i,j,ks  )
               v2(i,j,ks-2) = v2(i,j,ks-1)
             endif
             if (nikb2(i,j) .eq. 3) then
               v2(i,j,ks-1) = v2ikb (i,j,1)
               v2(i,j,ks-2) = v2ikb (i,j,2)
             endif

             if (nikb2(i,j) .eq. 4) then
               v2(i,j,ks-1) = v2(i,j,ke  )
               v2(i,j,ks-2) = v2(i,j,ke-1)
             endif

             if (nikb2(i,j) .eq. 5) then
               v2(i,j,ks-1) = v2(i,j,ks  )
               v2(i,j,ks-2) = v2(i,j,ks+1)
             endif
745        continue
750      continue
         bvstat(5,4) = rl3
         endif

c
c      Outer k boundary.
c

         if (u3 .gt. 0) then
         do 780 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 775 i=is-1,ie+1
             if ( abs(nokb2(i,j)) .eq. 1) then
               v2(i,j,ke+1) = v2(i,j,ke  )
               v2(i,j,ke+2) = v2(i,j,ke-1)
             endif
             if (nokb2(i,j) .eq. 2) then
               v2(i,j,ke+1) = v2(i,j,ke  )
               v2(i,j,ke+2) = v2(i,j,ke+1)
             endif
             if (nokb2(i,j) .eq. 3) then
               v2(i,j,ke+1) = v2okb (i,j,1)
               v2(i,j,ke+2) = v2okb (i,j,2)
             endif

             if (nokb2(i,j) .eq. 4) then
               v2(i,j,ke+1) = v2(i,j,ks  )
               v2(i,j,ke+2) = v2(i,j,ks+1)
             endif

             if (nokb2(i,j) .eq. 5) then
               v2(i,j,ke+1) = v2(i,j,ke  )
               v2(i,j,ke+2) = v2(i,j,ke-1)
             endif
775        continue
780      continue
         bvstat(6,4) = ru3
         endif

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                B V A L V 2                \\\\\\\\\\
c
c=======================================================================
c
c
*dk bvalv3
c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                B V A L V 3                \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalv3 ( rl1, ru1, rl2, ru2, rl3, ru3, v3 )
c
c    dac:zeus3d.bvalv3 <----------- 3-direction velocity boundary values
c    from mln:zeus04.bflo; jms:zeus2d.bvalv3              february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: RAF, 3/5/96 for ZEUS-MP
c
c  PURPOSE: This routine sets boundary values for the 3-velocity.  The
c  active zones for "v3" are "is" to "ie" in the 1-direction, "js" to
c  "je" in the 2-direction, and "ks+1" to "ke" in the 3-direction.  Two
c  layers of boundary values at each face are needed for interpolation.
c  No edge or corner boundary values are required.  Thus, the ranges for
c  the boundary value calculations are:
c
c    i-boundaries:                    j = js  , je     k = ks+1, ke
c    j-boundaries:   i = is  , ie                      k = ks+1, ke
c    k-boundaries:   i = is  , ie     j = js  , je
c
c  Note that for periodic or tile-tile boundaries, "ks" is also active.
c
c  The flow out boundary uses a switch to ensure fluid can only flow OUT
c  of the k boundary (boundary value set to 0 if it tries to flow in).
c
c  See comments in BVALD.
c
c  Flags l1, l2, l3, activate the 1-, 2-, and 3- loops when nonzero.
c  Their values give the number of layers to pass.
c
c  Array v3 is input so that velocity values at old time levels
c  can be passed.
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
       integer i,j,k,l1,l2,l3,u1,u2,u3
     &       , rl1,rl2,rl3,ru1,ru2,ru3

       real*8    v3(in,jn,kn)
       real*8    q1
c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l1 = rl1 - bvstat(1,5)
       u1 = ru1 - bvstat(2,5)
c
c      Inner i boundary.
c

         if (l1 .gt. 0) then
         do 830 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 825 j=js-1,je+1
             if (niib3(j,k) .eq. 1) then
               v3(is-1,j,k) = v3(is  ,j,k)
               v3(is-2,j,k) = v3(is+1,j,k)
             endif
             if (niib3(j,k) .eq.-1) then

               v3(is-1,j,k) = v3(is  ,j,k)
               v3(is-2,j,k) = v3(is+1,j,k)

             endif
             if (niib3(j,k) .eq. 2) then
               v3(is-1,j,k) = v3(is  ,j,k)
               v3(is-2,j,k) = v3(is-1,j,k)
             endif
             if (niib3(j,k) .eq. 3) then
               v3(is-1,j,k) = v3iib (j,k,1)
               v3(is-2,j,k) = v3iib (j,k,2)
             endif

             if (niib3(j,k) .eq. 4) then
               v3(is-1,j,k) = v3(ie  ,j,k)
               v3(is-2,j,k) = v3(ie-1,j,k)
             endif

             if (niib3(j,k) .eq. 5) then
               v3(is-1,j,k) = v3(is  ,j,k)
               v3(is-2,j,k) = v3(is+1,j,k)
             endif
825        continue
830      continue
         bvstat(1,5) = rl1
         endif

c
c      Outer i boundary.
c

         if (u1 .gt. 0) then
         do 860 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 855 j=js-1,je+1
             if ( abs(noib3(j,k)) .eq. 1) then
               v3(ie+1,j,k) = v3(ie,j,k)
               v3(ie+2,j,k) = v3(ie-1,j,k)
             endif
             if (noib3(j,k) .eq. 2) then
c#if blast == advect  --  I wish!
c               v3(ie+1,j,k) = 2.0 * v3(ie  ,j,k) - v3(ie-1,j,k)
c               v3(ie+2,j,k) = 2.0 * v3(ie+1,j,k) - v3(ie  ,j,k)
c#else
               v3(ie+1,j,k) = v3(ie,j,k)
               v3(ie+2,j,k) = v3(ie+1,j,k)
c#endif
             endif
             if (noib3(j,k) .eq. 3) then
               v3(ie+1,j,k) = v3oib (j,k,1)
               v3(ie+2,j,k) = v3oib (j,k,2)
             endif

             if (noib3(j,k) .eq. 4) then
               v3(ie+1,j,k) = v3(is  ,j,k)
               v3(ie+2,j,k) = v3(is+1,j,k)
             endif

             if (noib3(j,k) .eq. 5) then
               v3(ie+1,j,k) = v3(ie  ,j,k)
               v3(ie+2,j,k) = v3(ie-1,j,k)
             endif
855        continue
860      continue
         bvstat(2,5) = ru1
         endif

c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l2 = rl2 - bvstat(3,5)
       u2 = ru2 - bvstat(4,5)
c
c      Inner j boundary.
c

         if (l2 .gt. 0) then
         do 890 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 885 i=is-1,ie+1
             if (nijb3(i,k) .eq. 1) then
               v3(i,js-1,k) = v3(i,js  ,k)
               v3(i,js-2,k) = v3(i,js+1,k)
             endif
             if (nijb3(i,k) .eq.-1) then

               v3(i,js-1,k) = v3(i,js  ,k)
               v3(i,js-2,k) = v3(i,js+1,k)

             endif
             if (nijb3(i,k) .eq. 2) then
               v3(i,js-1,k) = v3(i,js  ,k)
               v3(i,js-2,k) = v3(i,js-1,k)
             endif
             if (nijb3(i,k) .eq. 3) then
               v3(i,js-1,k) = v3ijb (i,k,1)
               v3(i,js-2,k) = v3ijb (i,k,2)
             endif

             if (nijb3(i,k) .eq. 4) then
               v3(i,js-1,k) = v3(i,je  ,k)
               v3(i,js-2,k) = v3(i,je-1,k)
             endif

             if (nijb3(i,k) .eq. 5) then
               v3(i,js-1,k) = v3(i,js  ,k)
               v3(i,js-2,k) = v3(i,js+1,k)
             endif
885      continue
890    continue
       bvstat(3,5) = rl2
       endif

c
c      Outer j boundary.
c

         if (u2 .gt. 0) then
         do 920 k=ks-1,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 915 i=is-1,ie+1
             if (nojb3(i,k) .eq. 1) then
               v3(i,je+1,k) = v3(i,je  ,k)
               v3(i,je+2,k) = v3(i,je-1,k)
             endif
             if (nojb3(i,k) .eq.-1) then

               v3(i,je+1,k) = v3(i,je  ,k)
               v3(i,je+2,k) = v3(i,je-1,k)

             endif
             if (nojb3(i,k) .eq. 2) then
c#if alias blast.eq.advect  -- I wish!
               v3(i,je+1,k) = 2.0 * v3(i,je  ,k) - v3(i,je-1,k)
               v3(i,je+2,k) = 2.0 * v3(i,je+1,k) - v3(i,je  ,k)
c#else
               v3(i,je+1,k) = v3(i,je  ,k)
               v3(i,je+2,k) = v3(i,je+1,k)
c#endif
             endif
             if (nojb3(i,k) .eq. 3) then
               v3(i,je+1,k) = v3ojb (i,k,1)
               v3(i,je+2,k) = v3ojb (i,k,2)
             endif

             if (nojb3(i,k) .eq. 4) then
               v3(i,je+1,k) = v3(i,js  ,k)
               v3(i,je+2,k) = v3(i,js+1,k)
             endif

             if (nojb3(i,k) .eq. 5) then
               v3(i,je+1,k) = v3(i,je  ,k)
               v3(i,je+2,k) = v3(i,je-1,k)
             endif
915        continue
920      continue
         bvstat(4,5) = ru2
         endif

c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       l3 = rl3 - bvstat(5,5)
       u3 = ru3 - bvstat(6,5)
c
c      Inner k boundary.
c

         if (l3 .gt. 0) then
         do 950 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 945 i=is-1,ie+1
             if ( abs(nikb(i,j)) .eq. 1) then
               v3(i,j,ks  ) =       vg3(ks)
               v3(i,j,ks-1) = 2.0 * vg3(ks) - v3(i,j,ks+1)
               v3(i,j,ks-2) = 2.0 * vg3(ks) - v3(i,j,ks+2)
             endif
             if (nikb(i,j) .eq. 2) then
               q1           = sign ( haf, v3(i,j,ks+1) - vg3(ks) )
               v3(i,j,ks  ) = v3(i,j,ks+1) * ( 0.5 - q1 )
               v3(i,j,ks-1) = v3(i,j,ks  )
               v3(i,j,ks-2) = v3(i,j,ks  )
             endif
             if (nikb(i,j) .eq. 3) then
               v3(i,j,ks  ) = v3ikb (i,j,1)
               v3(i,j,ks-1) = v3ikb (i,j,2)
               v3(i,j,ks-2) = 2.0 * v3ikb (i,j,2) - v3ikb (i,j,1)
             endif

             if (nikb(i,j) .eq. 4) then
               v3(i,j,ks-1) = v3(i,j,ke  )
               v3(i,j,ks-2) = v3(i,j,ke-1)
             endif

             if (nikb(i,j) .eq. 5) then
               v3(i,j,ks  ) =       vg3(ks)
               v3(i,j,ks-1) = 2.0 * vg3(ks) - v3(i,j,ks+1)
               v3(i,j,ks-2) = 2.0 * vg3(ks) - v3(i,j,ks+2)
             endif
945        continue
950      continue
         bvstat(5,5) = rl3
         endif

c
c      Outer k boundary.
c

         if (u3 .gt. 0) then
         do 980 j=js-1,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 975 i=is-1,ie+1
             if ( abs(nokb(i,j)) .eq. 1) then
               v3(i,j,ke+1) =       vg3(ke+1)
               v3(i,j,ke+2) = 2.0 * vg3(ke+1) - v3(i,j,ke  )
             endif
             if (nokb(i,j) .eq. 2) then
               q1           = sign ( haf, v3(i,j,ke) - vg3(ke+1) )
               v3(i,j,ke+1) = v3(i,j,ke  ) * ( 0.5 + q1 )
               v3(i,j,ke+2) = v3(i,j,ke+1)
             endif
             if (nokb(i,j) .eq. 3) then
               v3(i,j,ke+1) = v3okb (i,j,1)
               v3(i,j,ke+2) = v3okb (i,j,2)
             endif

             if (nokb(i,j) .eq. 4) then
               v3(i,j,ke+1) = v3(i,j,ks  )
               v3(i,j,ke+2) = v3(i,j,ks+1)
             endif

             if (nokb(i,j) .eq. 5) then
               v3(i,j,ke+1) =       vg3(ke+1)
               v3(i,j,ke+2) = 2.0 * vg3(ke+1) - v3(i,j,ke)
             endif
975        continue
980      continue
         bvstat(6,5) = ru3
         endif

c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                B V A L V 3                \\\\\\\\\\
c
c=======================================================================
c
c
