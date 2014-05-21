












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////              B N D Y F L G S              \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine bndyflgs
c
c    dac:zeus3d.bndyflgs <---------------- sets secondary boundary flags
c                                                          october, 1990
c
c    written by: David Clarke
c    modified 1: by RAF 2/13/96 for ZEUS-MP
c
c  PURPOSE:  This subroutine sets the secondary integer boundary flags
c  ("niib2", "niib3", "niib23", etc.) which are used in addition to the
c  primary flags ("niib", etc.) to account for the staggered grid.
c  Thus, the following chart indicates the integer flag which sets the
c  various variables at the various boundaries:
c
c      boundary            variable(s)         integer flag
c
c      inner (outer) i     d, e, v1, b1        niib   (noib  )
c                          v2, b2              niib2  (noib2 )
c                          v3, b3              niib3  (noib3 )
c                          emf1, emf2, emf3    niib23 (noib23)
c      inner (outer) j     d, e, v2, b2        nijb   (nojb  )
c                          v3, b3              nijb3  (nojb3 )
c                          v1, b1              nijb1  (nojb1 )
c                          emf1, emf2, emf3    nijb31 (nojb31)
c      inner (outer) k     d, e, v3, b3        nikb   (nokb  )
c                          v1, b1              nikb1  (nokb1 )
c                          v2, b2              nikb2  (nokb2 )
c                          emf1, emf2, emf3    nikb12 (nokb12)
c
c  Note that there is a "pecking order" for the boundary types.  If two
c  adjacent 1s have different values for the primary integer flag
c  (see discussion in BVALD), the secondary integer flag is set
c  according to this order, which is currently:
c
c      3, 5, 1,-1, 4, 2
c
c  Thus, if niib(j,k) = 1 while niib(j-1,k) = 4, then niib2(j,k) is set
c  to 1.  But if niib(j,k) = 1 while niib(j-1,k) = 5, then niib2(j,k) is
c  set to 5.  The pecking order is determined by the order in which the
c  "if tests" are made in the loops below.
c
c  LOCAL VARIABLES:
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

c
       integer       i, j, k, im1, jm1, km1
c
c-----------------------------------------------------------------------
c------------------------  I - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       do 20 k=ks-2,ke+3
         km1 =  max ( ks-2, k - 1 )
         do 10 j=js-2,je+3
           jm1 =  max ( js-2, j - 1 )
c
c      Inner i boundary
c
           if ( (niib(j,k) .eq. 4) .or. (niib(jm1,k) .eq. 4) )
     1       niib2(j,k) = 4
           if ( (niib(j,k) .eq. 1) .or. (niib(jm1,k) .eq. 1) )
     1       niib2(j,k) = 1
           if ( (niib(j,k) .eq.-1) .or. (niib(jm1,k) .eq.-1) )
     1       niib2(j,k) =-1
           if ( (niib(j,k) .eq. 5) .or. (niib(jm1,k) .eq. 5) )
     1       niib2(j,k) = 5
           if ( (niib(j,k) .eq. 3) .or. (niib(jm1,k) .eq. 3) )
     1       niib2(j,k) = 3
c
           if ( (niib(j,k) .eq. 4) .or. (niib(j,km1) .eq. 4) )
     1       niib3(j,k) = 4
           if ( (niib(j,k) .eq. 1) .or. (niib(j,km1) .eq. 1) )
     1       niib3(j,k) = 1
           if ( (niib(j,k) .eq.-1) .or. (niib(j,km1) .eq.-1) )
     1       niib3(j,k) =-1
           if ( (niib(j,k) .eq. 5) .or. (niib(j,km1) .eq. 5) )
     1       niib3(j,k) = 5
           if ( (niib(j,k) .eq. 3) .or. (niib(j,km1) .eq. 3) )
     1       niib3(j,k) = 3
c
           if ( (niib2(j,k) .eq. 4) .or. (niib2(j,km1) .eq. 4) )
     1       niib23(j,k) = 4
           if ( (niib2(j,k) .eq. 1) .or. (niib2(j,km1) .eq. 1) )
     1       niib23(j,k) = 1
           if ( (niib2(j,k) .eq.-1) .or. (niib2(j,km1) .eq.-1) )
     1       niib23(j,k) =-1
           if ( (niib2(j,k) .eq. 5) .or. (niib2(j,km1) .eq. 5) )
     1       niib23(j,k) = 5
           if ( (niib2(j,k) .eq. 3) .or. (niib2(j,km1) .eq. 3) )
     1       niib23(j,k) = 3
c
c      Outer i boundary
c
           if ( (noib(j,k) .eq. 4) .or. (noib(jm1,k) .eq. 4) )
     1       noib2(j,k) = 4
           if ( (noib(j,k) .eq. 1) .or. (noib(jm1,k) .eq. 1) )
     1       noib2(j,k) = 1
           if ( (noib(j,k) .eq.-1) .or. (noib(jm1,k) .eq.-1) )
     1       noib2(j,k) =-1
           if ( (noib(j,k) .eq. 5) .or. (noib(jm1,k) .eq. 5) )
     1       noib2(j,k) = 5
           if ( (noib(j,k) .eq. 3) .or. (noib(jm1,k) .eq. 3) )
     1       noib2(j,k) = 3
c
           if ( (noib(j,k) .eq. 4) .or. (noib(j,km1) .eq. 4) )
     1       noib3(j,k) = 4
           if ( (noib(j,k) .eq. 1) .or. (noib(j,km1) .eq. 1) )
     1       noib3(j,k) = 1
           if ( (noib(j,k) .eq.-1) .or. (noib(j,km1) .eq.-1) )
     1       noib3(j,k) =-1
           if ( (noib(j,k) .eq. 5) .or. (noib(j,km1) .eq. 5) )
     1       noib3(j,k) = 5
           if ( (noib(j,k) .eq. 3) .or. (noib(j,km1) .eq. 3) )
     1       noib3(j,k) = 3
c
           if ( (noib2(j,k) .eq. 4) .or. (noib2(j,km1) .eq. 4) )
     1       noib23(j,k) = 4
           if ( (noib2(j,k) .eq. 1) .or. (noib2(j,km1) .eq. 1) )
     1       noib23(j,k) = 1
           if ( (noib2(j,k) .eq.-1) .or. (noib2(j,km1) .eq.-1) )
     1       noib23(j,k) =-1
           if ( (noib2(j,k) .eq. 5) .or. (noib2(j,km1) .eq. 5) )
     1       noib23(j,k) = 5
           if ( (noib2(j,k) .eq. 3) .or. (noib2(j,km1) .eq. 3) )
     1       noib23(j,k) = 3
c
10       continue
20     continue
c
c-----------------------------------------------------------------------
c------------------------  J - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       do 40 k=ks-2,ke+3
         km1 =  max ( ks-2, k - 1 )
         do 30 i=is-2,ie+3
           im1 =  max ( is-2, i - 1 )
c
c      Inner j boundary
c
           if ( (nijb(i,k) .eq. 4) .or. (nijb(i,km1) .eq. 4) )
     1       nijb3(i,k) = 4
           if ( (nijb(i,k) .eq. 1) .or. (nijb(i,km1) .eq. 1) )
     1       nijb3(i,k) = 1
           if ( (nijb(i,k) .eq.-1) .or. (nijb(i,km1) .eq.-1) )
     1       nijb3(i,k) =-1
           if ( (nijb(i,k) .eq. 5) .or. (nijb(i,km1) .eq. 5) )
     1       nijb3(i,k) = 5
           if ( (nijb(i,k) .eq. 3) .or. (nijb(i,km1) .eq. 3) )
     1       nijb3(i,k) = 3
c
           if ( (nijb(i,k) .eq. 4) .or. (nijb(im1,k) .eq. 4) )
     1       nijb1(i,k) = 4
           if ( (nijb(i,k) .eq. 1) .or. (nijb(im1,k) .eq. 1) )
     1       nijb1(i,k) = 1
           if ( (nijb(i,k) .eq.-1) .or. (nijb(im1,k) .eq.-1) )
     1       nijb1(i,k) =-1
           if ( (nijb(i,k) .eq. 5) .or. (nijb(im1,k) .eq. 5) )
     1       nijb1(i,k) = 5
           if ( (nijb(i,k) .eq. 3) .or. (nijb(im1,k) .eq. 3) )
     1       nijb1(i,k) = 3
c
           if ( (nijb3(i,k) .eq. 4) .or. (nijb3(im1,k) .eq. 4) )
     1       nijb31(i,k) = 4
           if ( (nijb3(i,k) .eq. 1) .or. (nijb3(im1,k) .eq. 1) )
     1       nijb31(i,k) = 1
           if ( (nijb3(i,k) .eq.-1) .or. (nijb3(im1,k) .eq.-1) )
     1       nijb31(i,k) =-1
           if ( (nijb3(i,k) .eq. 5) .or. (nijb3(im1,k) .eq. 5) )
     1       nijb31(i,k) = 5
           if ( (nijb3(i,k) .eq. 3) .or. (nijb3(im1,k) .eq. 3) )
     1       nijb31(i,k) = 3
c
c      Outer j boundary
c
           if ( (nojb(i,k) .eq. 4) .or. (nojb(i,km1) .eq. 4) )
     1       nojb3(i,k) = 4
           if ( (nojb(i,k) .eq. 1) .or. (nojb(i,km1) .eq. 1) )
     1       nojb3(i,k) = 1
           if ( (nojb(i,k) .eq.-1) .or. (nojb(i,km1) .eq.-1) )
     1       nojb3(i,k) =-1
           if ( (nojb(i,k) .eq. 5) .or. (nojb(i,km1) .eq. 5) )
     1       nojb3(i,k) = 5
           if ( (nojb(i,k) .eq. 3) .or. (nojb(i,km1) .eq. 3) )
     1       nojb3(i,k) = 3
c
           if ( (nojb(i,k) .eq. 4) .or. (nojb(im1,k) .eq. 4) )
     1       nojb1(i,k) = 4
           if ( (nojb(i,k) .eq. 1) .or. (nojb(im1,k) .eq. 1) )
     1       nojb1(i,k) = 1
           if ( (nojb(i,k) .eq.-1) .or. (nojb(im1,k) .eq.-1) )
     1       nojb1(i,k) =-1
           if ( (nojb(i,k) .eq. 5) .or. (nojb(im1,k) .eq. 5) )
     1       nojb1(i,k) = 5
           if ( (nojb(i,k) .eq. 3) .or. (nojb(im1,k) .eq. 3) )
     1       nojb1(i,k) = 3
c
           if ( (nojb3(i,k) .eq. 4) .or. (nojb3(im1,k) .eq. 4) )
     1       nojb31(i,k) = 4
           if ( (nojb3(i,k) .eq. 1) .or. (nojb3(im1,k) .eq. 1) )
     1       nojb31(i,k) = 1
           if ( (nojb3(i,k) .eq.-1) .or. (nojb3(im1,k) .eq.-1) )
     1       nojb31(i,k) =-1
           if ( (nojb3(i,k) .eq. 5) .or. (nojb3(im1,k) .eq. 5) )
     1       nojb31(i,k) = 5
           if ( (nojb3(i,k) .eq. 3) .or. (nojb3(im1,k) .eq. 3) )
     1       nojb31(i,k) = 3
c
30       continue
40     continue
c
c-----------------------------------------------------------------------
c------------------------  K - B O U N D A R Y  ------------------------
c-----------------------------------------------------------------------
c
       do 60 j=js-2,je+3
         jm1 =  max ( js-2, j - 1 )
         do 50 i=is-2,ie+3
           im1 =  max ( is-2, i - 1 )
c
c      Inner k boundary
c
           if ( (nikb(i,j) .eq. 4) .or. (nikb(im1,j) .eq. 4) )
     1       nikb1(i,j) = 4
           if ( (nikb(i,j) .eq. 1) .or. (nikb(im1,j) .eq. 1) )
     1       nikb1(i,j) = 1
           if ( (nikb(i,j) .eq.-1) .or. (nikb(im1,j) .eq.-1) )
     1       nikb1(i,j) =-1
           if ( (nikb(i,j) .eq. 5) .or. (nikb(im1,j) .eq. 5) )
     1       nikb1(i,j) = 5
           if ( (nikb(i,j) .eq. 3) .or. (nikb(im1,j) .eq. 3) )
     1       nikb1(i,j) = 3
c
           if ( (nikb(i,j) .eq. 4) .or. (nikb(i,jm1) .eq. 4) )
     1       nikb2(i,j) = 4
           if ( (nikb(i,j) .eq. 1) .or. (nikb(i,jm1) .eq. 1) )
     1       nikb2(i,j) = 1
           if ( (nikb(i,j) .eq.-1) .or. (nikb(i,jm1) .eq.-1) )
     1       nikb2(i,j) =-1
           if ( (nikb(i,j) .eq. 5) .or. (nikb(i,jm1) .eq. 5) )
     1       nikb2(i,j) = 5
           if ( (nikb(i,j) .eq. 3) .or. (nikb(i,jm1) .eq. 3) )
     1       nikb2(i,j) = 3
c
           if ( (nikb1(i,j) .eq. 4) .or. (nikb1(i,jm1) .eq. 4) )
     1       nikb12(i,j) = 4
           if ( (nikb1(i,j) .eq. 1) .or. (nikb1(i,jm1) .eq. 1) )
     1       nikb12(i,j) = 1
           if ( (nikb1(i,j) .eq.-1) .or. (nikb1(i,jm1) .eq.-1) )
     1       nikb12(i,j) =-1
           if ( (nikb1(i,j) .eq. 5) .or. (nikb1(i,jm1) .eq. 5) )
     1       nikb12(i,j) = 5
           if ( (nikb1(i,j) .eq. 3) .or. (nikb1(i,jm1) .eq. 3) )
     1       nikb12(i,j) = 3
c
c      Outer k boundary
c
           if ( (nokb(i,j) .eq. 4) .or. (nokb(im1,j) .eq. 4) )
     1       nokb1(i,j) = 4
           if ( (nokb(i,j) .eq. 1) .or. (nokb(im1,j) .eq. 1) )
     1       nokb1(i,j) = 1
           if ( (nokb(i,j) .eq.-1) .or. (nokb(im1,j) .eq.-1) )
     1       nokb1(i,j) =-1
           if ( (nokb(i,j) .eq. 5) .or. (nokb(im1,j) .eq. 5) )
     1       nokb1(i,j) = 5
           if ( (nokb(i,j) .eq. 3) .or. (nokb(im1,j) .eq. 3) )
     1       nokb1(i,j) = 3
c
           if ( (nokb(i,j) .eq. 4) .or. (nokb(i,jm1) .eq. 4) )
     1       nokb2(i,j) = 4
           if ( (nokb(i,j) .eq. 1) .or. (nokb(i,jm1) .eq. 1) )
     1       nokb2(i,j) = 1
           if ( (nokb(i,j) .eq.-1) .or. (nokb(i,jm1) .eq.-1) )
     1       nokb2(i,j) =-1
           if ( (nokb(i,j) .eq. 5) .or. (nokb(i,jm1) .eq. 5) )
     1       nokb2(i,j) = 5
           if ( (nokb(i,j) .eq. 3) .or. (nokb(i,jm1) .eq. 3) )
     1       nokb2(i,j) = 3
c
           if ( (nokb1(i,j) .eq. 4) .or. (nokb1(i,jm1) .eq. 4) )
     1       nokb12(i,j) = 4
           if ( (nokb1(i,j) .eq. 1) .or. (nokb1(i,jm1) .eq. 1) )
     1       nokb12(i,j) = 1
           if ( (nokb1(i,j) .eq.-1) .or. (nokb1(i,jm1) .eq.-1) )
     1       nokb12(i,j) =-1
           if ( (nokb1(i,j) .eq. 5) .or. (nokb1(i,jm1) .eq. 5) )
     1       nokb12(i,j) = 5
           if ( (nokb1(i,j) .eq. 3) .or. (nokb1(i,jm1) .eq. 3) )
     1       nokb12(i,j) = 3
c
50       continue
60     continue
c
       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////              B N D Y F L G S              \\\\\\\\\\
c
c=======================================================================
c
c
