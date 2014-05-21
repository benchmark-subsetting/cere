












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////              B V A L E M F 1              \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine bvalemf1 ( v2b3, v3b2 )

c
c    dac:zeus3d.bvalemf1 <-------------- boundary values for 1-emf terms
c                                                         february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: September, 1990 by David Clarke; moved magnetic fields
c                to face-centres.
c    modified 2: minimal rewrite for ZEUS-MP by M-MML 9.3.98
c
c  PURPOSE: This routine sets boundary values for the two terms in the
c  1-emf (centred on the 1-edges).  The "active" zones for "emf1" are:
c
c    i = is to ie;  j = js+1 to je;  k = ks+1 to ke
c
c  In order to update both the active and ghost zones of the 2- and
c  3-magnetic field components, all edges in the boundary regions are
c  required.  This gives a complete grid of values for "emf1".  Thus,
c  the ranges for the boundary values are:
c
c    i-boundaries:                    j = js  , je+1   k = ks  , ke+1
c    j-boundaries:   i = is-2, ie+2                    k = ks  , ke+1
c    k-boundaries:   i = is-2, ie+2   j = js-2, je+3
c
c  Note that the boundary values must be set even if is > ismn, etc.
c  because the emfs are stored in worker arrays and it is likely that
c  the boundary values have been overwritten.
c
c  See comments in BVALD.
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
       integer       i       , j       , k,      ii,    jj,   kk
c
       real*8        v2b3    (  in,  jn,  kn), v3b2    (  in,  jn,  kn)
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c

c
c      Inner i boundary.
c
         do 20 k=ks,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 10 j=js,je+1
             if ( abs(niib23(j,k)) .eq. 1) then
               v2b3(is-1,j,k) = v2b3(is  ,j,k)
               v2b3(is-2,j,k) = v2b3(is+1,j,k)
               v3b2(is-1,j,k) = v3b2(is  ,j,k)
               v3b2(is-2,j,k) = v3b2(is+1,j,k)
             endif
             if (niib23(j,k) .eq. 2) then
               v2b3(is-1,j,k) = v2b3(is  ,j,k)
               v2b3(is-2,j,k) = v2b3(is  ,j,k)
               v3b2(is-1,j,k) = v3b2(is  ,j,k)
               v3b2(is-2,j,k) = v3b2(is  ,j,k)
             endif
             if (niib23(j,k) .eq. 3) then
               v2b3(is-1,j,k) = emf1iib(j,k,1)
               v2b3(is-2,j,k) = emf1iib(j,k,2)
               v3b2(is-1,j,k) = 0.0
               v3b2(is-2,j,k) = 0.0
             endif

             if (niib23(j,k) .eq. 4) then
               v2b3(is-1,j,k) = v2b3(ie  ,j,k)
               v2b3(is-2,j,k) = v2b3(ie-1,j,k)
               v3b2(is-1,j,k) = v3b2(ie  ,j,k)
               v3b2(is-2,j,k) = v3b2(ie-1,j,k)
             endif

             if (niib23(j,k) .eq. 5) then
               v2b3(is-1,j,k) =-v2b3(is  ,j,k)
               v2b3(is-2,j,k) =-v2b3(is+1,j,k)
               v3b2(is-1,j,k) =-v3b2(is  ,j,k)
               v3b2(is-2,j,k) =-v3b2(is+1,j,k)
             endif
10         continue
20       continue

c
c      Outer i boundary.
c

c
         do 25 k=ks,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 15 j=js,je+1
             if ( abs(noib23(j,k)) .eq. 1) then
               v2b3(ie+1,j,k) = v2b3(ie  ,j,k)
               v2b3(ie+2,j,k) = v2b3(ie-1,j,k)
               v3b2(ie+1,j,k) = v3b2(ie  ,j,k)
               v3b2(ie+2,j,k) = v3b2(ie-1,j,k)
             endif
             if (noib23(j,k) .eq. 2) then
               v2b3(ie+1,j,k) = v2b3(ie  ,j,k)
               v2b3(ie+2,j,k) = v2b3(ie  ,j,k)
               v3b2(ie+1,j,k) = v3b2(ie  ,j,k)
               v3b2(ie+2,j,k) = v3b2(ie  ,j,k)
             endif
             if (noib23(j,k) .eq. 3) then
               v2b3(ie+1,j,k) = emf1oib(j,k,1)
               v2b3(ie+2,j,k) = emf1oib(j,k,2)
               v3b2(ie+1,j,k) = 0.0
               v3b2(ie+2,j,k) = 0.0
             endif

             if (noib23(j,k) .eq. 4) then
               v2b3(ie+1,j,k) = v2b3(is  ,j,k)
               v2b3(ie+2,j,k) = v2b3(is+1,j,k)
               v3b2(ie+1,j,k) = v3b2(is  ,j,k)
               v3b2(ie+2,j,k) = v3b2(is+1,j,k)
             endif

             if (noib23(j,k) .eq. 5) then
               v2b3(ie+1,j,k) =-v2b3(ie  ,j,k)
               v2b3(ie+2,j,k) =-v2b3(ie-1,j,k)
               v3b2(ie+1,j,k) =-v3b2(ie  ,j,k)
               v3b2(ie+2,j,k) =-v3b2(ie-1,j,k)
             endif
c
15         continue
25       continue

c
c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner j boundary.
c

         do 50 k=ks,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 40 i=is-2,ie+2
             if (nijb3(i,k) .eq. 1) then
               v2b3(i,js  ,k) = 0.0
               v2b3(i,js-1,k) =-v2b3(i,js+1,k)
               v2b3(i,js-2,k) =-v2b3(i,js+2,k)
               v3b2(i,js  ,k) = 0.0
               v3b2(i,js-1,k) =-v3b2(i,js+1,k)
               v3b2(i,js-2,k) =-v3b2(i,js+2,k)
             endif
             if (nijb3(i,k) .eq.-1) then

               v2b3(i,js  ,k) = 0.0
               v2b3(i,js-1,k) =-v2b3(i,js+1,k)
               v2b3(i,js-2,k) =-v2b3(i,js+2,k)
               v3b2(i,js  ,k) = 0.0
               v3b2(i,js-1,k) =-v3b2(i,js+1,k)
               v3b2(i,js-2,k) =-v3b2(i,js+2,k)


             endif
             if (nijb3(i,k) .eq. 2) then
               v2b3(i,js-1,k) = v2b3(i,js  ,k)
               v2b3(i,js-2,k) = v2b3(i,js  ,k)
               v3b2(i,js-1,k) = v3b2(i,js  ,k)
               v3b2(i,js-2,k) = v3b2(i,js  ,k)
             endif
             if (nijb3(i,k) .eq. 3) then
               v2b3(i,js  ,k) = emf1ijb(i,k,1)
               v2b3(i,js-1,k) = emf1ijb(i,k,2)
               v2b3(i,js-2,k) = emf1ijb(i,k,3)
               v3b2(i,js  ,k) = 0.0
               v3b2(i,js-1,k) = 0.0
               v3b2(i,js-2,k) = 0.0
             endif

             if (nijb3(i,k) .eq. 4) then
               v2b3(i,js-1,k) = v2b3(i,je  ,k)
               v2b3(i,js-2,k) = v2b3(i,je-1,k)
               v3b2(i,js-1,k) = v3b2(i,je  ,k)
               v3b2(i,js-2,k) = v3b2(i,je-1,k)
             endif

             if (nijb3(i,k) .eq. 5) then
               v2b3(i,js  ,k) = 0.0
               v2b3(i,js-1,k) = v2b3(i,js+1,k)
               v2b3(i,js-2,k) = v2b3(i,js+2,k)
               v3b2(i,js-1,k) = v3b2(i,js+1,k)
               v3b2(i,js-2,k) = v3b2(i,js+2,k)
             endif
40         continue
50       continue

c
c      Outer j boundary.
c

         do 55 k=ks,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 45 i=is-2,ie+2
             if (nojb3(i,k) .eq. 1) then
               v2b3(i,je+1,k) = 0.0
               v2b3(i,je+2,k) =-v2b3(i,je  ,k)
               v2b3(i,je+3,k) =-v2b3(i,je-1,k)
               v3b2(i,je+1,k) = 0.0
               v3b2(i,je+2,k) =-v3b2(i,je  ,k)
               v3b2(i,je+3,k) =-v3b2(i,je-1,k)
             endif
             if (nojb3(i,k) .eq.-1) then


               v2b3(i,je+1,k) = 0.0
               v2b3(i,je+2,k) =-v2b3(i,je  ,k)
               v2b3(i,je+3,k) =-v2b3(i,je-1,k)
               v3b2(i,je+1,k) = 0.0
               v3b2(i,je+2,k) =-v3b2(i,je  ,k)
               v3b2(i,je+3,k) =-v3b2(i,je-1,k)

             endif
             if (nojb3(i,k) .eq. 2) then
c*if alias blast.eq.advect
c               v2b3(i,je+2,k) = 3.0 * ( v2b3(i,je+1,k) - v2b3(i,je  ,k) )
c       1                      + v2b3(i,je-1,k)
c               v2b3(i,je+3,k) = 3.0 * ( v2b3(i,je+2,k) - v2b3(i,je+1,k) )
c       1                      + v2b3(i,je  ,k)
c*else
               v2b3(i,je+2,k) = v2b3(i,je+1,k)
               v2b3(i,je+3,k) = v2b3(i,je+1,k)
               v3b2(i,je+2,k) = v3b2(i,je+1,k)
               v3b2(i,je+3,k) = v3b2(i,je+1,k)
             endif
             if (nojb3(i,k) .eq. 3) then
               v2b3(i,je+1,k) = emf1ojb(i,k,1)
               v2b3(i,je+2,k) = emf1ojb(i,k,2)
               v2b3(i,je+3,k) = emf1ojb(i,k,3)
               v3b2(i,je+1,k) = 0.0
               v3b2(i,je+2,k) = 0.0
               v3b2(i,je+3,k) = 0.0
             endif

             if (nojb3(i,k) .eq. 4) then
               v2b3(i,je+2,k) = v2b3(i,js+1,k)
               v2b3(i,je+3,k) = v2b3(i,js+2,k)
               v3b2(i,je+2,k) = v3b2(i,js+1,k)
               v3b2(i,je+3,k) = v3b2(i,js+2,k)
             endif

             if (nojb3(i,k) .eq. 5) then
               v2b3(i,je+1,k) = 0.0
               v2b3(i,je+2,k) = v2b3(i,je  ,k)
               v2b3(i,je+3,k) = v2b3(i,je-1,k)
               v3b2(i,je+2,k) = v3b2(i,je  ,k)
               v3b2(i,je+3,k) = v3b2(i,je-1,k)
             endif
c
45         continue
55       continue

C       if(myid_w .eq.1) then
C          print *, v2b3(is,js,ks),v2b3(16,js,ks),v2b3(ie,js,ks)
C          print *, v2b3(is,js+1,ks),v2b3(16,js+1,ks),v2b3(ie,js+1,ks)
C          print *, v2b3(is,js+2,ks),v2b3(16,js+2,ks),v2b3(ie,js+2,ks)
C          print *, v2b3(is,je+1,ks),v2b3(16,je+1,ks),v2b3(ie,je+1,ks)
C          print *, v2b3(is,je+2,ks),v2b3(16,je+2,ks),v2b3(ie,je+2,ks)
C          print *, v2b3(is,je+3,ks),v2b3(16,je+3,ks),v2b3(ie,je+3,ks)
C          print *
C       endif
c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner k boundary.
c

         do 80 j=js-2,je+3
cremoved for SPEC CPU2006 cdir$ ivdep
           do 70 i=is-2,ie+2
c 
c      Inner k boundary.
c 
             if ( abs(nikb2(i,j)) .eq. 1) then
               v2b3(i,j,ks  ) = 0.0
               v2b3(i,j,ks-1) =-v2b3(i,j,ks+1)
               v2b3(i,j,ks-2) =-v2b3(i,j,ks+2)
               v3b2(i,j,ks  ) = 0.0
               v3b2(i,j,ks-1) =-v3b2(i,j,ks+1)
               v3b2(i,j,ks-2) =-v3b2(i,j,ks+2)
             endif
             if (nikb2(i,j) .eq. 2) then
               v2b3(i,j,ks-1) = v2b3(i,j,ks  )
               v2b3(i,j,ks-2) = v2b3(i,j,ks  )
               v3b2(i,j,ks-1) = v3b2(i,j,ks  )
               v3b2(i,j,ks-2) = v3b2(i,j,ks  )
             endif
             if (nikb2(i,j) .eq. 3) then
               v2b3(i,j,ks  ) = 0.0
               v2b3(i,j,ks-1) = 0.0
               v2b3(i,j,ks-2) = 0.0
               v3b2(i,j,ks  ) =-emf1ikb(i,j,1)
               v3b2(i,j,ks-1) =-emf1ikb(i,j,2)
               v3b2(i,j,ks-2) =-emf1ikb(i,j,3)
             endif

             if (nikb2(i,j) .eq. 4) then
               v2b3(i,j,ks-1) = v2b3(i,j,ke  )
               v2b3(i,j,ks-2) = v2b3(i,j,ke-1)
               v3b2(i,j,ks-1) = v3b2(i,j,ke  )
               v3b2(i,j,ks-2) = v3b2(i,j,ke-1)
             endif

            if (nikb2(i,j) .eq. 5) then
               v2b3(i,j,ks-1) = v2b3(i,j,ks+1)
               v2b3(i,j,ks-2) = v2b3(i,j,ks+2)
               v3b2(i,j,ks  ) = 0.0
               v3b2(i,j,ks-1) = v3b2(i,j,ks+1)
               v3b2(i,j,ks-2) = v3b2(i,j,ks+2)
             endif
c
70         continue
80       continue

c 
c      Outer k boundary.
c 

         do 85 j=js-2,je+3
cremoved for SPEC CPU2006 cdir$ ivdep
           do 75 i=is-2,ie+2
             if ( abs(nokb2(i,j)) .eq. 1) then
               v2b3(i,j,ke+1) = 0.0
               v2b3(i,j,ke+2) =-v2b3(i,j,ke  )
               v2b3(i,j,ke+3) =-v2b3(i,j,ke-1)
               v3b2(i,j,ke+1) = 0.0
               v3b2(i,j,ke+2) =-v3b2(i,j,ke  )
               v3b2(i,j,ke+3) =-v3b2(i,j,ke-1)
             endif
             if (nokb2(i,j) .eq. 2) then
               v2b3(i,j,ke+2) = v2b3(i,j,ke+1)
               v2b3(i,j,ke+3) = v2b3(i,j,ke+1)
               v3b2(i,j,ke+2) = v3b2(i,j,ke+1)
               v3b2(i,j,ke+3) = v3b2(i,j,ke+1)
             endif
             if (nokb2(i,j) .eq. 3) then
               v2b3(i,j,ke+1) = 0.0
               v2b3(i,j,ke+2) = 0.0
               v2b3(i,j,ke+3) = 0.0
               v3b2(i,j,ke+1) =-emf1okb(i,j,1)
               v3b2(i,j,ke+2) =-emf1okb(i,j,2)
               v3b2(i,j,ke+3) =-emf1okb(i,j,3)
             endif

             if (nokb2(i,j) .eq. 4) then
               v2b3(i,j,ke+2) = v2b3(i,j,ks+1)
               v2b3(i,j,ke+3) = v2b3(i,j,ks+2)
               v3b2(i,j,ke+2) = v3b2(i,j,ks+1)
               v3b2(i,j,ke+3) = v3b2(i,j,ks+2)
             endif

             if (nokb2(i,j) .eq. 5) then
               v2b3(i,j,ke+2) = v2b3(i,j,ke  )
               v2b3(i,j,ke+3) = v2b3(i,j,ke-1)
               v3b2(i,j,ke+1) = 0.0
               v3b2(i,j,ke+2) = v3b2(i,j,ke  )
               v3b2(i,j,ke+3) = v3b2(i,j,ke-1)
             endif
c 
75         continue
85       continue


c
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.0) then
C          print *, v2b3(is,js,ks),v2b3(16,js,ks),v2b3(ie,js,ks)
C          print *, v2b3(is,js,ks+1),v2b3(16,js,ks+1),v2b3(ie,js,ks+1)
C          print *, v2b3(is,js,ks+2),v2b3(16,js,ks+2),v2b3(ie,js,ks+2)
C          print *, v2b3(is,js,ke+1),v2b3(16,js,ke+1),v2b3(ie,js,ke+1)
C          print *, v2b3(is,js,ke+2),v2b3(16,js,ke+2),v2b3(ie,js,ke+2)
C          print *, v2b3(is,js,ke+3),v2b3(16,js,ke+3),v2b3(ie,js,ke+3)
C          print *
C       endif
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.1) then
C          print *, v2b3(is,js,ks),v2b3(16,js,ks),v2b3(ie,js,ks)
C          print *, v2b3(is,js,ks+1),v2b3(16,js,ks+1),v2b3(ie,js,ks+1)
C          print *, v2b3(is,js,ks+2),v2b3(16,js,ks+2),v2b3(ie,js,ks+2)
C          print *
C       endif
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.3) then
C          print *, v2b3(is,js,ke+1),v2b3(16,js,ke+1),v2b3(ie,js,ke+1)
C          print *, v2b3(is,js,ke+2),v2b3(16,js,ke+2),v2b3(ie,js,ke+2)
C          print *, v2b3(is,js,ke+3),v2b3(16,js,ke+3),v2b3(ie,js,ke+3)
C          print *
C       endif
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////              B V A L E M F 1              \\\\\\\\\\
c
c=======================================================================
c













































































c
c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////              B V A L E M F 2              \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalemf2 ( v3b1, v1b3 )

c
c    dac:zeus3d.bvalemf2 <-------------- boundary values for 2-emf terms
c                                                         february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: September, 1990 by David Clarke; moved magnetic fields
c                to face-centres.
c    modified 2: minimal rewrite for ZEUS-MP by M-MML 10.3.98
c
c  PURPOSE: This routine sets boundary values for the two terms in the
c  2-emf (centred on the 2-edges).  The active zones for "emf2" are:
c
c    i = is to ie+1;  j = js to je;  k = ks tp ke+1
c
c  In order to update both the active and ghost zones of the 3- and
c  1-magnetic field components, all edges in the boundary regions are
c  required.  This gives a complete grid of values for "emf2".  Thus,
c  the ranges for the boundary values are:
c
c    j-boundaries:   i = is  , ie+1                    k = ks  , ke+1
c    k-boundaries:   i = is  , ie+1   j = js-2, je+2
c    i-boundaries:                    j = js-2, je+2   k = ks-2, ke+3
c
c  Note that the boundary values must be set even if js > jsmn, etc.
c  because the emfs are stored in worker arrays and it is likely that
c  the boundary values have been overwritten.

c  See comments in BVALD.
c
c  EXTERNALS: [NONE}
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
       integer       i       , j       , k     , ii  ,   jj,   kk
c
       real*8        v3b1    (  in,  jn,  kn), v1b3    (  in,  jn,  kn)
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner j boundary.
c

         do 20 k=ks,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 10 i=is,ie+1
             if (nijb31(i,k) .eq. 1) then
               v3b1(i,js-1,k) = v3b1(i,js  ,k)
               v3b1(i,js-2,k) = v3b1(i,js+1,k)
               v1b3(i,js-1,k) = v1b3(i,js  ,k)
               v1b3(i,js-2,k) = v1b3(i,js+1,k)
             endif
             if (nijb31(i,k) .eq.-1) then

               v3b1(i,js-1,k) = v3b1(i,js  ,k)
               v3b1(i,js-2,k) = v3b1(i,js+1,k)
               v1b3(i,js-1,k) = v1b3(i,js  ,k)
               v1b3(i,js-2,k) = v1b3(i,js+1,k)


             endif
             if (nijb31(i,k) .eq. 2) then
               v3b1(i,js-1,k) = v3b1(i,js  ,k)
               v3b1(i,js-2,k) = v3b1(i,js  ,k)
               v1b3(i,js-1,k) = v1b3(i,js  ,k)
               v1b3(i,js-2,k) = v1b3(i,js  ,k)
             endif
             if (nijb31(i,k) .eq. 3) then
               v3b1(i,js-1,k) = emf2ijb(i,k,1)
               v3b1(i,js-2,k) = emf2ijb(i,k,2)
               v1b3(i,js-1,k) = 0.0
               v1b3(i,js-2,k) = 0.0
             endif

             if (nijb31(i,k) .eq. 4) then
               v3b1(i,js-1,k) = v3b1(i,je  ,k)
               v3b1(i,js-2,k) = v3b1(i,je-1,k)
               v1b3(i,js-1,k) = v1b3(i,je  ,k)
               v1b3(i,js-2,k) = v1b3(i,je-1,k)
             endif

             if (nijb31(i,k) .eq. 5) then
               v3b1(i,js-1,k) =-v3b1(i,js  ,k)
               v3b1(i,js-2,k) =-v3b1(i,js+1,k)
               v1b3(i,js-1,k) =-v1b3(i,js  ,k)
               v1b3(i,js-2,k) =-v1b3(i,js+1,k)
             endif
10         continue
20       continue

c 
c      Outer j boundary.
c 

         do 25 k=ks,ke+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 15 i=is,ie+1
             if (nojb31(i,k) .eq. 1) then
               v3b1(i,je+1,k) = v3b1(i,je  ,k)
               v3b1(i,je+2,k) = v3b1(i,je-1,k)
               v1b3(i,je+1,k) = v1b3(i,je  ,k)
               v1b3(i,je+2,k) = v1b3(i,je-1,k)
             endif
             if (nojb31(i,k) .eq.-1) then


               v3b1(i,je+1,k) = v3b1(i,je  ,k)
               v3b1(i,je+2,k) = v3b1(i,je-1,k)
               v1b3(i,je+1,k) = v1b3(i,je  ,k)
               v1b3(i,je+2,k) = v1b3(i,je-1,k)

             endif
             if (nojb31(i,k) .eq. 2) then
               v3b1(i,je+1,k) = v3b1(i,je  ,k)
               v3b1(i,je+2,k) = v3b1(i,je  ,k)
               v1b3(i,je+1,k) = v1b3(i,je  ,k)
               v1b3(i,je+2,k) = v1b3(i,je  ,k)
             endif
             if (nojb31(i,k) .eq. 3) then
               v3b1(i,je+1,k) = emf2ojb(i,k,1)
               v3b1(i,je+2,k) = emf2ojb(i,k,2)
               v1b3(i,je+1,k) = 0.0
               v1b3(i,je+2,k) = 0.0
             endif

             if (nojb31(i,k) .eq. 4) then
               v3b1(i,je+1,k) = v3b1(i,js  ,k)
               v3b1(i,je+2,k) = v3b1(i,js+1,k)
               v1b3(i,je+1,k) = v1b3(i,js  ,k)
               v1b3(i,je+2,k) = v1b3(i,js+1,k)
             endif

             if (nojb31(i,k) .eq. 5) then
               v3b1(i,je+1,k) =-v3b1(i,je  ,k)
               v3b1(i,je+2,k) =-v3b1(i,je-1,k)
               v1b3(i,je+1,k) =-v1b3(i,je  ,k)
               v1b3(i,je+2,k) =-v1b3(i,je-1,k)
             endif
c 
15         continue
25       continue

c
c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner k boundary.
c

         do 50 j=js-2,je+2
cremoved for SPEC CPU2006 cdir$ ivdep
           do 40 i=is,ie+1
             if ( abs(nikb1(i,j)) .eq. 1) then
               v3b1(i,j,ks  ) = 0.0
               v3b1(i,j,ks-1) =-v3b1(i,j,ks+1)
               v3b1(i,j,ks-2) =-v3b1(i,j,ks+2)
               v1b3(i,j,ks  ) = 0.0
               v1b3(i,j,ks-1) =-v1b3(i,j,ks+1)
               v1b3(i,j,ks-2) =-v1b3(i,j,ks+2)
             endif
             if (nikb1(i,j) .eq. 2) then
               v3b1(i,j,ks-1) = v3b1(i,j,ks  )
               v3b1(i,j,ks-2) = v3b1(i,j,ks  )
               v1b3(i,j,ks-1) = v1b3(i,j,ks  )
               v1b3(i,j,ks-2) = v1b3(i,j,ks  )
             endif
             if (nikb1(i,j) .eq. 3) then
               v3b1(i,j,ks  ) = emf2ikb(i,j,1)
               v3b1(i,j,ks-1) = emf2ikb(i,j,2)
               v3b1(i,j,ks-2) = emf2ikb(i,j,3)
               v1b3(i,j,ks  ) = 0.0
               v1b3(i,j,ks-1) = 0.0
               v1b3(i,j,ks-2) = 0.0
             endif

             if (nikb1(i,j) .eq. 4) then
               v3b1(i,j,ks-1) = v3b1(i,j,ke  )
               v3b1(i,j,ks-2) = v3b1(i,j,ke-1)
               v1b3(i,j,ks-1) = v1b3(i,j,ke  )
               v1b3(i,j,ks-2) = v1b3(i,j,ke-1)
             endif

             if (nikb1(i,j) .eq. 5) then
               v3b1(i,j,ks  ) = 0.0
               v3b1(i,j,ks-1) = v3b1(i,j,ks+1)
               v3b1(i,j,ks-2) = v3b1(i,j,ks+2)
               v1b3(i,j,ks-1) = v1b3(i,j,ks+1)
               v1b3(i,j,ks-2) = v1b3(i,j,ks+2)
             endif
40         continue
50       continue

c 
c      Outer k boundary.
c 

         do 55 j=js-2,je+2
cremoved for SPEC CPU2006 cdir$ ivdep
           do 45 i=is,ie+1
             if ( abs(nokb1(i,j)) .eq. 1) then
               v3b1(i,j,ke+1) = 0.0
               v3b1(i,j,ke+2) =-v3b1(i,j,ke  )
               v3b1(i,j,ke+3) =-v3b1(i,j,ke-1)
               v1b3(i,j,ke+1) = 0.0
               v1b3(i,j,ke+2) =-v1b3(i,j,ke  )
               v1b3(i,j,ke+3) =-v1b3(i,j,ke-1)
             endif
             if (nokb1(i,j) .eq. 2) then
               v3b1(i,j,ke+2) = v3b1(i,j,ke+1)
               v3b1(i,j,ke+3) = v3b1(i,j,ke+1)
               v1b3(i,j,ke+2) = v1b3(i,j,ke+1)
               v1b3(i,j,ke+3) = v1b3(i,j,ke+1)
             endif
             if (nokb1(i,j) .eq. 3) then
               v3b1(i,j,ke+1) = emf2okb(i,j,1)
               v3b1(i,j,ke+2) = emf2okb(i,j,2)
               v3b1(i,j,ke+3) = emf2okb(i,j,3)
               v1b3(i,j,ke+1) = 0.0
               v1b3(i,j,ke+2) = 0.0
               v1b3(i,j,ke+3) = 0.0
             endif

             if (nokb1(i,j) .eq. 4) then
               v3b1(i,j,ke+2) = v3b1(i,j,ks+1)
               v3b1(i,j,ke+3) = v3b1(i,j,ks+2)
               v1b3(i,j,ke+2) = v1b3(i,j,ks+1)
               v1b3(i,j,ke+3) = v1b3(i,j,ks+2)
             endif

             if (nokb1(i,j) .eq. 5) then
               v3b1(i,j,ke+1) = 0.0
               v3b1(i,j,ke+2) = v3b1(i,j,ke  )
               v3b1(i,j,ke+3) = v3b1(i,j,ke-1)
               v1b3(i,j,ke+2) = v1b3(i,j,ke  )
               v1b3(i,j,ke+3) = v1b3(i,j,ke-1)
             endif
c 
45        continue
55       continue

c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c      Inner i boundary.
c

         do 80 k=ks-2,ke+3
cremoved for SPEC CPU2006 cdir$ ivdep
           do 70 j=js-2,je+2
             if (niib3(j,k) .eq. 1) then
               v3b1(is  ,j,k) = 0.0
               v3b1(is-1,j,k) =-v3b1(is+1,j,k)
               v3b1(is-2,j,k) =-v3b1(is+2,j,k)
               v1b3(is  ,j,k) = 0.0
               v1b3(is-1,j,k) =-v1b3(is+1,j,k)
               v1b3(is-2,j,k) =-v1b3(is+2,j,k)
             endif
             if (niib3(j,k) .eq.-1) then


               v3b1(is  ,j,k) = 0.0
               v3b1(is-1,j,k) =-v3b1(is+1,j,k)
               v3b1(is-2,j,k) =-v3b1(is+2,j,k)
               v1b3(is  ,j,k) = 0.0
               v1b3(is-1,j,k) =-v1b3(is+1,j,k)
               v1b3(is-2,j,k) =-v1b3(is+2,j,k)

             endif
             if (niib3(j,k) .eq. 2) then
               v3b1(is-1,j,k) = v3b1(is  ,j,k)
               v3b1(is-2,j,k) = v3b1(is  ,j,k)
               v1b3(is-1,j,k) = v1b3(is  ,j,k)
               v1b3(is-2,j,k) = v1b3(is  ,j,k)
             endif
             if (niib3(j,k) .eq. 3) then
               v3b1(is  ,j,k) = 0.0
               v3b1(is-1,j,k) = 0.0
               v3b1(is-2,j,k) = 0.0
               v1b3(is  ,j,k) =-emf2iib(j,k,1)
               v1b3(is-1,j,k) =-emf2iib(j,k,2)
               v1b3(is-2,j,k) =-emf2iib(j,k,3)
             endif

             if (niib3(j,k) .eq. 4) then
               v3b1(is-1,j,k) = v3b1(ie  ,j,k)
               v3b1(is-2,j,k) = v3b1(ie-1,j,k)
               v1b3(is-1,j,k) = v1b3(ie  ,j,k)
               v1b3(is-2,j,k) = v1b3(ie-1,j,k)
             endif

             if (niib3(j,k) .eq. 5) then
               v3b1(is-1,j,k) = v3b1(is+1,j,k)
               v3b1(is-2,j,k) = v3b1(is+2,j,k)
               v1b3(is  ,j,k) = 0.0
               v1b3(is-1,j,k) = v1b3(is+1,j,k)
               v1b3(is-2,j,k) = v1b3(is+2,j,k)
             endif
70         continue
80       continue

c 
c      Outer i boundary.
c 

         do 85 k=ks-2,ke+3
cremoved for SPEC CPU2006 cdir$ ivdep
           do 75 j=js-2,je+2
             if ( abs(noib3(j,k)) .eq. 1) then
               v3b1(ie+1,j,k) = 0.0
               v3b1(ie+2,j,k) =-v3b1(ie  ,j,k)
               v3b1(ie+3,j,k) =-v3b1(ie-1,j,k)
               v1b3(ie+1,j,k) = 0.0
               v1b3(ie+2,j,k) =-v1b3(ie  ,j,k)
               v1b3(ie+3,j,k) =-v1b3(ie-1,j,k)
             endif
             if (noib3(j,k) .eq. 2) then
               v3b1(ie+2,j,k) = v3b1(ie+1,j,k)
               v3b1(ie+3,j,k) = v3b1(ie+1,j,k)
c*if alias blast.eq.advect
c               v1b3(ie+2,j,k) = 3.0 * ( v1b3(ie+1,j,k) - v1b3(ie  ,j,k) )
c       1                      + v1b3(ie-1,j,k)
c               v1b3(ie+3,j,k) = 3.0 * ( v1b3(ie+2,j,k) - v1b3(ie+1,j,k) )
c       1                      + v1b3(ie  ,j,k)
c*else
               v1b3(ie+2,j,k) = v1b3(ie+1,j,k)
               v1b3(ie+3,j,k) = v1b3(ie+1,j,k)
c*endif
             endif
             if (noib3(j,k) .eq. 3) then
               v3b1(ie+1,j,k) = 0.0
               v3b1(ie+2,j,k) = 0.0
               v3b1(ie+3,j,k) = 0.0
               v1b3(ie+1,j,k) =-emf2oib(j,k,1)
               v1b3(ie+2,j,k) =-emf2oib(j,k,2)
               v1b3(ie+3,j,k) =-emf2oib(j,k,3)
             endif

             if (noib3(j,k) .eq. 4) then
               v3b1(ie+2,j,k) = v3b1(is+1,j,k)
               v3b1(ie+3,j,k) = v3b1(is+2,j,k)
               v1b3(ie+2,j,k) = v1b3(is+1,j,k)
               v1b3(ie+3,j,k) = v1b3(is+2,j,k)
             endif

             if (noib3(j,k) .eq. 5) then
               v3b1(ie+2,j,k) = v3b1(ie  ,j,k)
               v3b1(ie+3,j,k) = v3b1(ie-1,j,k)
               v1b3(ie+1,j,k) = 0.0
               v1b3(ie+2,j,k) = v1b3(ie  ,j,k)
               v1b3(ie+3,j,k) = v1b3(ie-1,j,k)
             endif
c
75         continue
85       continue
c


C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.0) then
C          print *, v3b1(is,js,ks),v3b1(16,js,ks),v3b1(ie,js,ks)
C          print *, v3b1(is,js,ks+1),v3b1(16,js,ks+1),v3b1(ie,js,ks+1)
C          print *, v3b1(is,js,ks+2),v3b1(16,js,ks+2),v3b1(ie,js,ks+2)
C          print *, v3b1(is,js,ke+1),v3b1(16,js,ke+1),v3b1(ie,js,ke+1)
C          print *, v3b1(is,js,ke+2),v3b1(16,js,ke+2),v3b1(ie,js,ke+2)
C          print *, v3b1(is,js,ke+3),v3b1(16,js,ke+3),v3b1(ie,js,ke+3)
C          print *
C       endif
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.1) then
C          print *, v3b1(is,js,ks),v3b1(16,js,ks),v3b1(ie,js,ks)
C          print *, v3b1(is,js,ks+1),v3b1(16,js,ks+1),v3b1(ie,js,ks+1)
C          print *, v3b1(is,js,ks+2),v3b1(16,js,ks+2),v3b1(ie,js,ks+2)
C          print *
C       endif
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.3) then
C          print *, v3b1(is,js,ke+1),v3b1(16,js,ke+1),v3b1(ie,js,ke+1)
C          print *, v3b1(is,js,ke+2),v3b1(16,js,ke+2),v3b1(ie,js,ke+2)
C          print *, v3b1(is,js,ke+3),v3b1(16,js,ke+3),v3b1(ie,js,ke+3)
C          print *
C       endif
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////              B V A L E M F 2              \\\\\\\\\\
c
c=======================================================================
c













































































c
c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////              B V A L E M F 3              \\\\\\\\\\
c
c=======================================================================
c
       subroutine bvalemf3 ( v1b2, v2b1 )

c
c    dac:zeus3d.bvalemf3 <-------------- boundary values for 3-emf terms
c                                                         february, 1990
c
c    written by: David Clarke, February 1990
c    modified 1: September, 1990 by David Clarke; moved magnetic fields
c                to face-centres.
c    modified 2: minimal rewrite for ZEUS-MP by M-MML 10.3.98
c
c  PURPOSE: This routine sets boundary values for the two terms in the
c  3-emf (centred on the 3-edges).  The active zones for "emf3" are:
c
c    i = is to ie+1;  j = js to je+1;  k = ks to ke
c
c  In order to update both the active and ghost zones of the 1- and
c  2-magnetic field components, all edges in the boundary regions are
c  required.  This gives a complete grid of values for "emf3".  Thus,
c  the ranges for the boundary values are:
c
c    k-boundaries:   i = is  , ie+1   j = js  , je+1
c    i-boundaries:                    j = js  , je+1   k = ks-2, ke+2
c    j-boundaries:   i = is-2, ie+3                    k = ks-2, ke+2
c
c  Note that the boundary values must be set even if ks > ksmn, etc.
c  because the emfs are stored in worker arrays and it is likely that
c  the boundary values have been overwritten.
c
c  See comments in BVALD.
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
       integer       i       , j       , k     ,   ii,   jj,   kk
c
       real*8        v1b2    (  in,  jn,  kn), v2b1    (  in,  jn,  kn)
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner k boundary.
c

         do 20 j=js,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 10 i=is,ie+1
             if ( abs(nikb12(i,j)) .eq. 1) then
               v1b2(i,j,ks-1) = v1b2(i,j,ks  )
               v1b2(i,j,ks-2) = v1b2(i,j,ks+1)
               v2b1(i,j,ks-1) = v2b1(i,j,ks  )
               v2b1(i,j,ks-2) = v2b1(i,j,ks+1)
             endif
             if (nikb12(i,j) .eq. 2) then
               v1b2(i,j,ks-1) = v1b2(i,j,ks  )
               v1b2(i,j,ks-2) = v1b2(i,j,ks  )
               v2b1(i,j,ks-1) = v2b1(i,j,ks  )
               v2b1(i,j,ks-2) = v2b1(i,j,ks  )
             endif
             if (nikb12(i,j) .eq. 3) then
               v1b2(i,j,ks-1) = emf3ikb(i,j,1)
               v1b2(i,j,ks-2) = emf3ikb(i,j,2)
               v2b1(i,j,ks-1) = 0.0
               v2b1(i,j,ks-2) = 0.0
             endif

             if (nikb12(i,j) .eq. 4) then
               v1b2(i,j,ks-1) = v1b2(i,j,ke  )
               v1b2(i,j,ks-2) = v1b2(i,j,ke-1)
               v2b1(i,j,ks-1) = v2b1(i,j,ke  )
               v2b1(i,j,ks-2) = v2b1(i,j,ke-1)
             endif

             if (nikb12(i,j) .eq. 5) then
               v1b2(i,j,ks-1) =-v1b2(i,j,ks  )
               v1b2(i,j,ks-2) =-v1b2(i,j,ks+1)
               v2b1(i,j,ks-1) =-v2b1(i,j,ks  )
               v2b1(i,j,ks-2) =-v2b1(i,j,ks+1)
             endif
10         continue
20       continue

c 
c      Outer k boundary.
c 

         do 25 j=js,je+1
cremoved for SPEC CPU2006 cdir$ ivdep
           do 15 i=is,ie+1
             if ( abs(nokb12(i,j)) .eq. 1) then
               v1b2(i,j,ke+1) = v1b2(i,j,ke  )
               v1b2(i,j,ke+2) = v1b2(i,j,ke-1)
               v2b1(i,j,ke+1) = v2b1(i,j,ke  )
               v2b1(i,j,ke+2) = v2b1(i,j,ke-1)
             endif
             if (nokb12(i,j) .eq. 2) then
               v1b2(i,j,ke+1) = v1b2(i,j,ke  )
               v1b2(i,j,ke+2) = v1b2(i,j,ke  )
               v2b1(i,j,ke+1) = v2b1(i,j,ke  )
               v2b1(i,j,ke+2) = v2b1(i,j,ke  )
             endif
             if (nokb12(i,j) .eq. 3) then
               v1b2(i,j,ke+1) = emf3okb(i,j,1)
               v1b2(i,j,ke+2) = emf3okb(i,j,2)
               v2b1(i,j,ke+1) = 0.0
               v2b1(i,j,ke+2) = 0.0
             endif

             if (nokb12(i,j) .eq. 4) then
               v1b2(i,j,ke+1) = v1b2(i,j,ks  )
               v1b2(i,j,ke+2) = v1b2(i,j,ks+1)
               v2b1(i,j,ke+1) = v2b1(i,j,ks  )
               v2b1(i,j,ke+2) = v2b1(i,j,ks+1)
             endif

             if (nokb12(i,j) .eq. 5) then
               v1b2(i,j,ke+1) =-v1b2(i,j,ke  )
               v1b2(i,j,ke+2) =-v1b2(i,j,ke-1)
               v2b1(i,j,ke+1) =-v2b1(i,j,ke  )
               v2b1(i,j,ke+2) =-v2b1(i,j,ke-1)
             endif
c 
15         continue
25       continue

c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner i boundary.
c

         do 50 k=ks-2,ke+2
cremoved for SPEC CPU2006 cdir$ ivdep
           do 40 j=js,je+1
             if (niib2(j,k) .eq. 1) then
               v1b2(is  ,j,k) = 0.0
               v1b2(is-1,j,k) =-v1b2(is+1,j,k)
               v1b2(is-2,j,k) =-v1b2(is+2,j,k)
               v2b1(is  ,j,k) = 0.0
               v2b1(is-1,j,k) =-v2b1(is+1,j,k)
               v2b1(is-2,j,k) =-v2b1(is+2,j,k)
             endif
             if (niib2(j,k) .eq.-1) then


               v1b2(is  ,j,k) = 0.0
               v1b2(is-1,j,k) =-v1b2(is+1,j,k)
               v1b2(is-2,j,k) =-v1b2(is+2,j,k)
               v2b1(is  ,j,k) = 0.0
               v2b1(is-1,j,k) =-v2b1(is+1,j,k)
               v2b1(is-2,j,k) =-v2b1(is+2,j,k)

             endif
             if (niib2(j,k) .eq. 2) then
               v1b2(is-1,j,k) = v1b2(is  ,j,k)
               v1b2(is-2,j,k) = v1b2(is  ,j,k)
               v2b1(is-1,j,k) = v2b1(is  ,j,k)
               v2b1(is-2,j,k) = v2b1(is  ,j,k)
             endif
             if (niib2(j,k) .eq. 3) then
               v1b2(is  ,j,k) = emf3iib(j,k,1)
               v1b2(is-1,j,k) = emf3iib(j,k,2)
               v1b2(is-2,j,k) = emf3iib(j,k,3)
               v2b1(is  ,j,k) = 0.0
               v2b1(is-1,j,k) = 0.0
               v2b1(is-2,j,k) = 0.0
             endif

             if (niib2(j,k) .eq. 4) then
               v1b2(is-1,j,k) = v1b2(ie  ,j,k)
               v1b2(is-2,j,k) = v1b2(ie-1,j,k)
               v2b1(is-1,j,k) = v2b1(ie  ,j,k)
               v2b1(is-2,j,k) = v2b1(ie-1,j,k)
             endif

            if (niib2(j,k) .eq. 5) then
               v1b2(is  ,j,k) = 0.0
               v1b2(is-1,j,k) = v1b2(is+1,j,k)
               v1b2(is-2,j,k) = v1b2(is+2,j,k)
               v2b1(is-1,j,k) = v2b1(is+1,j,k)
               v2b1(is-2,j,k) = v2b1(is+2,j,k)
             endif
40         continue
50       continue

c 
c      Outer i boundary.
c 

         do 55 k=ks-2,ke+2
cremoved for SPEC CPU2006 cdir$ ivdep
           do 45 j=js,je+1
             if ( abs(noib2(j,k)) .eq. 1) then
               v1b2(ie+1,j,k) = 0.0
               v1b2(ie+2,j,k) =-v1b2(ie  ,j,k)
               v1b2(ie+3,j,k) =-v1b2(ie-1,j,k)
               v2b1(ie+1,j,k) = 0.0
               v2b1(ie+2,j,k) =-v2b1(ie  ,j,k)
               v2b1(ie+3,j,k) =-v2b1(ie-1,j,k)
             endif
             if (noib2(j,k) .eq. 2) then
c*if alias blast.eq.advect
c               v1b2(ie+2,j,k) = 3.0 * ( v1b2(ie+1,j,k) - v1b2(ie  ,j,k) )
c       1                      + v1b2(ie-1,j,k)
c               v1b2(ie+3,j,k) = 3.0 * ( v1b2(ie+2,j,k) - v1b2(ie+1,j,k) )
c       1                      + v1b2(ie  ,j,k)
c*else
               v1b2(ie+2,j,k) = v1b2(ie+1,j,k)
               v1b2(ie+3,j,k) = v1b2(ie+1,j,k)
c*endif
               v2b1(ie+2,j,k) = v2b1(ie+1,j,k)
               v2b1(ie+3,j,k) = v2b1(ie+1,j,k)
             endif
             if (noib2(j,k) .eq. 3) then
               v1b2(ie+1,j,k) = emf3oib(j,k,1)
               v1b2(ie+2,j,k) = emf3oib(j,k,2)
               v1b2(ie+3,j,k) = emf3oib(j,k,3)
               v2b1(ie+1,j,k) = 0.0
               v2b1(ie+2,j,k) = 0.0
               v2b1(ie+3,j,k) = 0.0
             endif

             if (noib2(j,k) .eq. 4) then
               v1b2(ie+2,j,k) = v1b2(is+1,j,k)
               v1b2(ie+3,j,k) = v1b2(is+2,j,k)
               v2b1(ie+2,j,k) = v2b1(is+1,j,k)
               v2b1(ie+3,j,k) = v2b1(is+2,j,k)
             endif

             if (noib2(j,k) .eq. 5) then
               v1b2(ie+1,j,k) = 0.0
               v1b2(ie+2,j,k) = v1b2(ie  ,j,k)
               v1b2(ie+3,j,k) = v1b2(ie-1,j,k)
               v2b1(ie+2,j,k) = v2b1(ie  ,j,k)
               v2b1(ie+3,j,k) = v2b1(ie-1,j,k)
             endif
c c 
45         continue
55       continue


c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
c
c      Inner j boundary.
c

         do 80 k=ks-2,ke+2
cremoved for SPEC CPU2006 cdir$ ivdep
           do 70 i=is-2,ie+3
             if ( abs(nijb1(i,k)) .eq. 1) then
               v1b2(i,js  ,k) = 0.0
               v1b2(i,js-1,k) =-v1b2(i,js+1,k)
               v1b2(i,js-2,k) =-v1b2(i,js+2,k)
               v2b1(i,js  ,k) = 0.0
               v2b1(i,js-1,k) =-v2b1(i,js+1,k)
               v2b1(i,js-2,k) =-v2b1(i,js+2,k)
             endif
             if (nijb1(i,k) .eq. 2) then
               v1b2(i,js-1,k) = v1b2(i,js  ,k)
               v1b2(i,js-2,k) = v1b2(i,js  ,k)
               v2b1(i,js-1,k) = v2b1(i,js  ,k)
               v2b1(i,js-2,k) = v2b1(i,js  ,k)
             endif
             if (nijb1(i,k) .eq. 3) then
               v1b2(i,js  ,k) = 0.0
               v1b2(i,js-1,k) = 0.0
               v1b2(i,js-2,k) = 0.0
               v2b1(i,js  ,k) =-emf3ijb(i,k,1)
               v2b1(i,js-1,k) =-emf3ijb(i,k,2)
               v2b1(i,js-2,k) =-emf3ijb(i,k,3)
             endif

             if (nijb1(i,k) .eq. 4) then
               v1b2(i,js-1,k) = v1b2(i,je  ,k)
               v1b2(i,js-2,k) = v1b2(i,je-1,k)
               v2b1(i,js-1,k) = v2b1(i,je  ,k)
               v2b1(i,js-2,k) = v2b1(i,je-1,k)
             endif

             if (nijb1(i,k) .eq. 5) then
               v1b2(i,js-1,k) = v1b2(i,js+1,k)
               v1b2(i,js-2,k) = v1b2(i,js+2,k)
               v2b1(i,js  ,k) = 0.0
               v2b1(i,js-1,k) = v2b1(i,js+1,k)
               v2b1(i,js-2,k) = v2b1(i,js+2,k)
             endif
70         continue
80       continue

c 
c      Outer j boundary.
c 

         do 85 k=ks-2,ke+2
cremoved for SPEC CPU2006 cdir$ ivdep
           do 75 i=is-2,ie+3
             if ( abs(nojb1(i,k)) .eq. 1) then
               v1b2(i,je+1,k) = 0.0
               v1b2(i,je+2,k) =-v1b2(i,je  ,k)
               v1b2(i,je+3,k) =-v1b2(i,je-1,k)
               v2b1(i,je+1,k) = 0.0
               v2b1(i,je+2,k) =-v2b1(i,je  ,k)
               v2b1(i,je+3,k) =-v2b1(i,je-1,k)
             endif
             if (nojb1(i,k) .eq. 2) then
               v1b2(i,je+2,k) = v1b2(i,je+1,k)
               v1b2(i,je+3,k) = v1b2(i,je+1,k)
c*if alias blast.eq.advect
c               v2b1(i,je+2,k) = 2.0 * v2b1(i,je+1,k) - v2b1(i,je  ,k)
c               v2b1(i,je+3,k) = 2.0 * v2b1(i,je+2,k) - v2b1(i,je+1,k)
c*else
               v2b1(i,je+2,k) = v2b1(i,je+1,k)
               v2b1(i,je+3,k) = v2b1(i,je+1,k)
c*endif
             endif
             if (nojb1(i,k) .eq. 3) then
               v1b2(i,je+1,k) = 0.0
               v1b2(i,je+2,k) = 0.0
               v1b2(i,je+3,k) = 0.0
               v2b1(i,je+1,k) =-emf3ojb(i,k,1)
               v2b1(i,je+2,k) =-emf3ojb(i,k,2)
               v2b1(i,je+3,k) =-emf3ojb(i,k,3)
             endif

             if (nojb1(i,k) .eq. 4) then
               v1b2(i,je+2,k) = v1b2(i,js+1,k)
               v1b2(i,je+3,k) = v1b2(i,js+2,k)
               v2b1(i,je+2,k) = v2b1(i,js+1,k)
               v2b1(i,je+3,k) = v2b1(i,js+2,k)
             endif

             if (nojb1(i,k) .eq. 5) then
               v1b2(i,je+2,k) = v1b2(i,je  ,k)
               v1b2(i,je+3,k) = v1b2(i,je-1,k)
               v2b1(i,je+1,k) = 0.0
               v2b1(i,je+2,k) = v2b1(i,je  ,k)
               v2b1(i,je+3,k) = v2b1(i,je-1,k)
             endif
c   
75         continue
85       continue


C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.0) then
C          print *, v2b1(is,js,ks),v2b1(16,js,ks),v2b1(ie,js,ks)
C          print *, v2b1(is,js+1,ks),v2b1(16,js+1,ks),v2b1(ie,js+1,ks)
C          print *, v2b1(is,js+2,ks),v2b1(16,js+2,ks),v2b1(ie,js+2,ks)
C          print *, v2b1(is,je+1,ks),v2b1(16,je+1,ks),v2b1(ie,je+1,ke)
C          print *, v2b1(is,je+2,ks),v2b1(16,je+2,ks),v2b1(ie,je+2,ke)
C          print *, v2b1(is,je+3,ks),v2b1(16,je+3,ks),v2b1(ie,je+3,ke)
C          print *
C       endif
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.1) then
C          print *, v2b1(is,js,ks),v2b1(16,js,ks),v2b1(ie,js,ks)
C          print *, v2b1(is,js+1,ks),v2b1(16,js+1,ks),v2b1(ie,js+1,ks)
C          print *, v2b1(is,js+2,ks),v2b1(16,js+2,ks),v2b1(ie,js+2,ks)
C          print *
C       endif
C       call mpi_barrier(mpi_comm_world,ierr)
C       if(myid_w .eq.3) then
C          print *, v2b1(is,je+1,ks),v2b1(16,je+1,ks),v2b1(ie,je+1,ke)
C          print *, v2b1(is,je+2,ks),v2b1(16,je+2,ks),v2b1(ie,je+2,ke)
C          print *, v2b1(is,je+3,ks),v2b1(16,je+3,ks),v2b1(ie,je+3,ke)
C          print *
C       endif
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////              B V A L E M F 3              \\\\\\\\\\
c
c=======================================================================
c
