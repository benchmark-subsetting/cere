












































































c=======================================================================
c
c    \\\\\\\\\\      B E G I N   S U B R O U T I N E      //////////
c    //////////                 A D V X 3                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
       subroutine advx3 (dlo,den

     &                  ,eod,edn


     &                  ,mflx,s1,s2,s3)
c
c    RAF, 2/19/97
c
c  PURPOSE: 
c    Controls the update of density, energy, and momenta
c    from the advection terms in the 3-direction.
c
c  INPUT:
c    dlo         Mass            density prior to update
c    eod         Specific energy density prior to update
c    ero         Specific radiation energy density prior to update
c
c  OUTPUT:
c    den         Mass            density    after update
c    edn         Specific energy density    after update
c    ern         Specific radiation energy density    after update
c
c  I/O:
c    s1,s2,s3    Momentum density components (get updated)
c
c  LOCAL:
c    mflx        Mass flux in the 3-direction at zone faces 
c
c  EXTERNALS:
c    BVALV1  , BVALV2  , BVALV3
c    BVALD   , BVALE   , BVALER
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
       integer k1,k2,i
       integer kbeg, kend, krange, kblocks, kskip, ktlb, kblk, kpage

       real*8    p3
c
       real*8  dlo(in,jn,kn), den(in,jn,kn), mflx(in,jn,kn)
     &     , s1 (in,jn,kn), s2 (in,jn,kn), s3  (in,jn,kn)

     &     , eod(in,jn,kn), edn(in,jn,kn)


c
c      External statements
c
       external      tranx3  , momx3   , bvald
     &             , bvalv1  , bvalv2  , bvalv3

     &             , bvale


c
c      Tunable data
c
c Fraction of interior points to do 3rd stage
       data p3 / 0.9 /   
c
c Set blocking factor for 3rd stage k-loops.  The number of iterations
c in a block should be small enough so that all the data fits on the
c number of pages that the 128 can hold.  Thus,
c
c iterations = <128 entries * page size> / <2-D arrays * word size>
c
c In tranx3 and momx3, 11 different 2-D arrays of data are used.  
c Assume 16kB pages and 8B words.  The number of iterations per
c block is forced to be at least 5.
c
c Get 128 size in MB from cpp -DTLB=n
       data ktlb / 128 /  

c Default min block size is this many
       data kblk / 5 /        

c Assume this many bytes per page
       data kpage / 16384 /   

c
c-----------------------------------------------------------------------
c
c Divide up the work.  Since we must have
c
c   ks+4 < k1-1
c   k1   < k2-1
c   k2   < ke-3
c
c   ke - ks .ge. 12   --- this is the smallest allowable k range.
c
c   nseq indicates the sweep sequence.
       nseq = nseq + 1        
       k2   = ke - int( p3 * nx3z )
       k2   = min( k2, ke - 3 )
       k2   = max( k2, ks + 6 )
       k1   = ( k2 + ks ) / 2
       k1   = max( k1, ks + 5 )



c......................................................................
c
c i boundaries
c
c    1) Post sends and receives. 
c       By exchanging the i, j, and k boundary data in three separate 
c       steps, we ensure that the corner and edge data are correctly
c       updated.
c

       nreq = 0
       nsub = nsub + 1

       call bvalv3 (1,0,0,0,0,0,v3 )
c

       call bvald  (1,0,0,0,0,0,dlo)



c
c    2) Do first portion of the interior points.
c
       call tranx3 (is+1,ie,js+1,je,ks+3,k1,dlo,den

     &             ,eod,edn


     &             ,mflx)



       call momx3  (is+2,ie,js+2,je,ks+4,k1-1,s1,s2,s3,mflx)




c......................................................................
c
c j boundaries
c
c    1) Post sends and receives.
c
       nreq = 0
       nsub = nsub + 1
       call bvald  (0,0,1,0,0,0,dlo)

       call bvalv3 (0,0,1,0,0,0,v3 )



c
c    2) Do second portion of the interior points, plus some on borders.
c
       call tranx3 (is  ,is  ,js+1,je,ks+3,k1,dlo,den

     &             ,eod,edn


     &             ,mflx)



       call momx3  (is  ,is+1,js+2,je,ks+4,k1-1,s1,s2,s3,mflx)



c
       call tranx3 (is  ,ie  ,js+1,je,k1+1,k2,dlo,den

     &             ,eod,edn


     &             ,mflx)



       call momx3  (is  ,ie  ,js+2,je,k1  ,k2-1,s1,s2,s3,mflx)




c......................................................................
c
c k boundaries
c
c    1) Post sends and receives.
c
       nreq = 0
       nsub = nsub + 1
c
c We need all the density slabs.
c
       call bvald  (0,0,0,0,3,3,dlo)
c

c
c We need all slabs of eod.
c
       call bvale  (0,0,0,0,3,3,eod)
c


c
c TRANX3 and MOMX3 together need all 3 velocities at ks-2 through ke+2.
c
       call bvalv1 (0,0,0,0,3,3,v1 )
       call bvalv2 (0,0,0,0,3,3,v2 )
       call bvalv3 (0,0,0,0,3,3,v3 )



c
c    2) Do last portion of the interior points, plus some on borders.
c
       call tranx3 (is  ,ie  ,js  ,js  ,ks+3,k2,dlo,den

     &             ,eod,edn


     &             ,mflx)



       call momx3  (is  ,ie  ,js  ,js+1,ks+4,k2-1,s1,s2,s3,mflx)



c
c Block the k loop to reduce 128 misses; the k ranges for the earlier
c stages above should be small enough already.
c
       kskip   = max( ktlb * kpage / (11*8*in*jn), kblk)
       krange = ke-2 - (k2+1) + 1
       kblocks = krange / kskip
       if ( mod(krange,kskip) .eq. 0) kblocks = max(kblocks-1,0)
c
       do 35 kbeg = k2+1, k2+1 + kblocks*kskip, kskip 
         kend = min( kbeg + kskip - 1, ke-2 )
c       call tranx3 (is  ,ie  ,js  ,je  ,k2+1,ke-2,dlo,den
         call tranx3 (is  ,ie  ,js  ,je  ,kbeg,kend,dlo,den

     &               ,eod,edn


     &               ,mflx)



c       call momx3  (is  ,ie  ,js  ,je  ,k2  ,ke-3,s1,s2,s3,mflx)
       call momx3  (is  ,ie  ,js  ,je  ,kbeg-1,kend-1,s1,s2,s3,mflx)



35     continue
c
c Mark d and e/d (e) boundary values out of date.
c
       do 40 i=1,6
c  d
         bvstat(i,1) = 0  
c  e or e/d
         bvstat(i,2) = 0  


40     continue
c


c......................................................................
c
c Finally, do the remaining border zones.
c
       call tranx3 (is  ,ie  ,js  ,je  ,ks, ks+2, dlo,den

     &             ,eod,edn


     &             ,mflx)



       call momx3  (is  ,ie  ,js  ,je  ,ks, ks+3, s1,s2,s3,mflx)



c
       call tranx3 (is  ,ie  ,js  ,je  ,ke-1, ke, dlo,den

     &             ,eod,edn


     &             ,mflx)



       call momx3  (is  ,ie  ,js  ,je  ,ke-2, ke, s1,s2,s3,mflx)



c

       return
       end
c
c=======================================================================
c
c    \\\\\\\\\\        E N D   S U B R O U T I N E        //////////
c    //////////                 A D V X 3                 \\\\\\\\\\
c
c=======================================================================
c
c
