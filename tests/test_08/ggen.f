












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine ggen
c
c  PURPOSE:  Initializes the grid in a new run according to the control
c  parameters in the input deck namelists "ggen1" and ggen2".  All grid
c  variables are initialized.
c
c  LOCALS:
c
c  LAST MODIFIED: 8/10/99
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
      integer  nbl,igrid,imin,imax,jmin,jmax,kmin,kmax,iter,i,j,k
      real*8     x1min,x1max,x1rat,dx1min,dfndx1r,x1r,deltx1r,errx1r
     &        ,x2min,x2max,x2rat,dx2min,dfndx2r,x2r,deltx2r,errx2r
     &        ,x3min,x3max,x3rat,dx3min,dfndx3r,x3r,deltx3r,errx3r
     &        ,fn
      logical  lgrid
c
      integer iblmax, ibl, mbl, ifirst, ioffst, jfirst, joffst
     &       , kfirst, koffst
      integer nzones(3), nzpt(3)
      parameter(iblmax=10)
      real*8 gridd(5,iblmax)
c
      real*8 remainder
c
      namelist /ggen1/ nbl,x1min,x1max,igrid,x1rat,dx1min,lgrid
      namelist /ggen2/ nbl,x2min,x2max,igrid,x2rat,dx2min,lgrid
      namelist /ggen3/ nbl,x3min,x3max,igrid,x3rat,dx3min,lgrid
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////
c=======================================================================
c-----------  GENERATE X1 GRID  ----------------------------------------
c
c  Read in blocks of x1 grid zones.  Note we loop over read statement
c  until all blocks are read (signalled by reading in lgrid = .true.).
c  We can zone within each block completely independently of the others,
c  however we must ensure the starting position of one block (x1min) is
c  the same as the ending position (x1max) of the previous.
c   nbl    is number of active zones in block read in
c   x1min  is x1a(imin) ; bottom position of block
c   x1max  is x1a(imax) ; top    position of block
c   igrid  selects zoning type; we solve the zoning equation:
c            x1max = x1min + SUM OVER N[dx1min*x1rat**n]  ; so we must 
c          input either dx1min or x1rat (the other is calculated)
c      igrid   = 0  => block has already been set (irestart=1)
c              =+1  => (ratioed) use input "x1rat" to compute "dx1min",
c                      where x1a(imin+1) = x1a(imin) + dx1min.
c              =-1  => (ratioed) use input "x1rat" to compute "dx1min",
c                      where x1a(imax) = x1a(imax-1) + dx1min.
c              =+2  => (ratioed) use input "dx1min" to compute "x1rat",
c                      where x1a(imin+1) = x1a(imin) + dx1min.
c              =-2  => (ratioed) use input "dx1min" to compute "x1rat",
c                      where x1a(imax) = x1a(imax-1) + dx1min.
c   lgrid  logical flag for additional blocks ( =.true. reads another)
c   imax,imin  are indices of top and bottom of block
c
      is   = 3
      if (myid .eq. 0) then
        imax = is
        nbl   = 1
        x1min = 0.0
        x1max = 0.0
        igrid = 0
        x1rat = 0.0
        dx1min= 0.0
        lgrid = .false.
        ibl   = 0
        mbl   = 0
c
10      continue
        read (1,ggen1)
cpu2006        write(2,ggen1)
c
        ibl  = ibl + 1
        if (ibl .gt. iblmax) then
          write(6,"(/1x,'ERROR: number of blocks in 1-direction exceeds'
     &    ,' array bounds'/1x,'ibl = ',i4,'  iblmax = ',i4)") ibl,iblmax
          mbl  = 1
          go to 31
        endif
        imin = imax
        imax = imax + nbl
        if (imax .gt. is + (in-4)*ntiles(1)) then
          write(6,"(/1x,'ERROR: number of zones in 1-direction exceeds'
     &    ,' array bounds',/1x,'imax = ',i4,'  in = ',i4)") imax,in
          mbl  = 1
          go to 31
        endif
c
c  1)  Compute dx1min from given value of x1rat.
c
        if (abs(igrid) .eq. 1) then
          if (igrid .eq. -1) x1rat = 1.0/x1rat
          if (x1rat .eq. 1.0)  then
            dx1min = (x1max-x1min)/real(nbl)
          else
            dx1min = (x1max-x1min)*(x1rat-1.0)/(x1rat**nbl - 1.0)
          endif
        endif
c
c  2)  Compute x1rat from given value of dx1min.  Newton Raphson 
c      iteration is required to find the root (x1rat) of the function:
c        fn(x1r) = (x1max-x1min) - dx1min*[(x1r)**nbl - 1]/[x1r-1] = 0
c
        if (abs(igrid) .eq. 2) then
          x1r = 1.01
          do 20 iter=1,20
            fn = (x1max - x1min) - dx1min*(x1r**nbl - 1.0)/(x1r - 1.0)
            dfn dx1r =  -nbl*dx1min*x1r**(nbl - 1)/(x1r - 1.0)
     .                 + dx1min*(x1r**nbl - 1.0)/(x1r - 1.0)**2
            deltx1r  = -fn/dfndx1r
            err x1r  = abs(deltx1r/x1r)
            x1r = x1r + deltx1r
            if (errx1r .lt. 1.0e-6) goto 30
20        continue
          write(6,"(1x,'GRIDI: Newton-Raphson failed'
     &    ,' for x1rat',/1x,'imin = ',i3,' x1r = ',1pe12.5,' deltx1r = '
     &    ,1e12.5,' fn = ',1e12.5)") imin,x1r,deltx1r,fn
          mbl  = 1
          go to 31
c
30        continue
          x1rat = x1r
        endif
        if (igrid .eq. -2) then
          dx1min = dx1min * x1rat**(nbl-1)
          x1rat  = 1.0 / x1rat
        endif
c
c Copy the grid descriptors into a temporary array for broadcast later.
c
        gridd(1,ibl) = float(imin)
        gridd(2,ibl) = float(imax)
        gridd(3,ibl) =  x1min
        gridd(4,ibl) = dx1min
        gridd(5,ibl) =  x1rat
c
c  Go back and read another block of x1 grid zones, if needed.
c
        if (.not. lgrid) go to 10
      endif
31    continue

      if (mbl  .ne. 0) then

        stop
      endif

c
c The full grid has gridd(2,ibl) zones.  Find the best number
c of zones per tile.  Assume for now that the number of zones
c is evenly divisible by the number of tiles in that direction.
c Otherwise, load-balancing becomes an issue.
c
      nzones(1) = int(gridd(2,ibl)-gridd(1,1))
c Because of using multigrid Poisson Solver (mgmpi) for gravitation,
c number of zones is not longer restricted to be equal for
c all tiles and some of the following lines are commnted out.
c      nzpt(1)   = nzones(1)/ntiles(1)
c      fn        = real(nzones(1))/real(ntiles(1))
c      if (abs(real(nzpt(1))-fn) .gt. 0.001) then
c        if (myid .eq. 0) write(6,"(1x,'ERROR from GRIDI: nzones(1) = '
c     &  ,i4,' is not evenly divisible by ntiles(1) = ',i3)") nzones(1)
c     &  ,ntiles(1)
c#ifdef MPI_USED
c        call MPI_FINALIZE(ierr)
c#endif
c        stop
c      endif
c Relax the assumption of equal number of zones per processor, and
c go through some checks to determine the best zones distribution
c for the zone and tile parameters given.
c
      nzpt(1) = 0
      remainder = mod(nzones(1),ntiles(1))
      if(remainder .eq. 0) then
       nzpt(1) = nzones(1)/ntiles(1)
      else if((ntiles(1)-remainder) .eq. 1) then
       if(coords(1) .ne. ntiles(1)-1) nzpt(1) = nzones(1)/ntiles(1)+1
       if(coords(1) .eq. ntiles(1)-1) nzpt(1) = nzones(1)/ntiles(1)
      else if(((ntiles(1)-remainder) .eq. 3) .and.
     .         (ntiles(1) .eq. 4)                 ) then
       if(coords(1) .le. 1) nzpt(1) = nzones(1)/ntiles(1)+1
       if(coords(1) .eq. 2) nzpt(1) = nzones(1)/ntiles(1)
       if(coords(1) .eq. 3) nzpt(1) = nzones(1)/ntiles(1)-1
      else
       if(coords(1) .ne. ntiles(1)-1) nzpt(1) = nzones(1)/ntiles(1)+1
       if(coords(1) .eq. ntiles(1)-1) nzpt(1) = nzones(1) -
     .   (ntiles(1)-1)*(nzones(1)/ntiles(1) + 1)
      endif
      if(nzpt(1) .eq. 0) then
       print *,'GGEN1: could not set nzpt(1)'

       stop
      endif
c
c From my coordinates in the 3-D Cartesian virtual topology (coords),
c figure out which zones I own and compute the grid.  This tile's
c first real zone has index ifirst.   Locate the block containing it.
c
c      ioffst = coords(1) * nzpt(1)
      if(remainder .eq. 0) then
       ioffst = coords(1) * nzpt(1)
      else if((ntiles(1)-remainder) .eq. 1) then
       ioffst = coords(1) * (nzones(1)/ntiles(1)+1)
      else if(((ntiles(1)-remainder) .eq. 3) .and.
     .         (ntiles(1) .eq. 4)                 ) then
       if(coords(1) .le. 2)
     .    ioffst = coords(1)*(nzones(1)/ntiles(1)+1)
       if(coords(1) .eq. 3)
     .    ioffst = 2*(nzones(1)/ntiles(1)+1) + nzones(1)/ntiles(1)
      else
       ioffst = coords(1) * (nzones(1)/ntiles(1)+1)
      endif
      ifirst = is + ioffst
      do 32 nbl=1,ibl
        mbl  = nbl
        if (int(gridd(1,mbl)).gt.ifirst) then
          mbl = max(1, mbl-1)
          go to 34
        endif
32    continue
34    continue
        imin = int(gridd(1,mbl))
        imax = int(gridd(2,mbl))
       x1min = gridd(3,mbl)
      dx1min = gridd(4,mbl)
       x1rat = gridd(5,mbl)
c
c The first real zone on this tile is in block mbl.  Compute the
c grid points of zones whose indices are less than ifirst, if any.
c
      if (imin .lt. ifirst) then
        do 36 i=imin+1,ifirst
          dx1min = dx1min * x1rat
           x1min =  x1min + dx1min
36      continue
      endif
      imin = max(imin,ifirst)
      imax = min(imax,ifirst+nzpt(1))
c
c Now do the rest of the grid points in this block.
c Set up x1a grid lines from i=imin to imax, using known values of
c x1min, x1rat. 
c
       x1a(imin-ioffst) =  x1min
      dx1a(imin-ioffst) = dx1min
      do 40 i=imin+1,imax
        dx1a(i-ioffst) = dx1a(i-ioffst-1) * x1rat
         x1a(i-ioffst) =  x1a(i-ioffst-1) + dx1a(i-ioffst-1)
40    continue
      if (mbl.lt.ibl) then
        mbl = mbl + 1
        go to 34
      endif
c
c  Set up all grid zones and scale factors in x1 direction
c
      ie = imax - ioffst

c
c..................  GRIDI BOUNDARIES  .................................
c
c Get the coords of overlap ghost zones from the neighbors,
c unless I am on a physical boundary.
c
      nreq = 0
      if (coords(1).eq.0 .and. niis(1).ne.4) then
c
c I am on the physical inner boundary.  Compute ghost zones there.
c
c If the BCs are reflecting, the grid should be symmetric.
c
        dx1a(is-1) = dx1a(is  )
        if (niis(1).ne.2 .and. niis(1).ne.3) then
c
c Use this for Reflection-Symmetric grids: 
c
          dx1a(is-2) = dx1a(is+1)
          dx1min     = dx1a(is+2)
        else
c
c The following seems best for Flow In/Out:
c
          dx1a(is-2) = dx1a(is-1)
          dx1min     = dx1a(is-2)
        endif
      else

c
c Periodic grid, one tile.
c
        dx1a(is-1) = dx1a(ie-1)
        dx1a(is-2) = dx1a(ie-2)
        dx1min     = dx1a(ie-3)
c

      endif
c
      if (coords(1).eq.ntiles(1)-1 .and. nois(1).ne.4) then
c
c I am on the physical outer boundary.  Compute ghost zones there.
c
        dx1a(ie  ) = dx1a(ie-1)
        if (nois(1).ne.2 .and. nois(1).ne.3) then
c
c Use this for Reflection-Symmetric grids: 
c
          dx1a(ie+1) = dx1a(ie-2)
          dx1a(ie+2) = dx1a(ie-3)
        else
c
c Use this for Flow In/Out:
c
          dx1a(ie+1) = dx1a(ie  )
          dx1a(ie+2) = dx1a(ie+1)
        endif
      else

c
c Periodic grid.
c
        dx1a(ie  ) = dx1a(is  )
        dx1a(ie+1) = dx1a(is+1)
        dx1a(ie+2) = dx1a(is+2)
c

      endif

      x1a (is-1) = x1a (is  ) - dx1a(is-1)
      x1a (is-2) = x1a (is-1) - dx1a(is-2)
      x1min      = x1a (is-2) - dx1min
      x1a (ie+1) = x1a (ie  ) + dx1a(ie  )
      x1a (ie+2) = x1a (ie+1) + dx1a(ie+1)
      x1max      = x1a (ie+2) + dx1a(ie+2)
c
c................  GRIDI B-MESH AND GEOM FACTORS  ......................
c
       x1min     = x1min      + 0.5 * dx1min
       x1b(is-2) = x1a (is-2) + 0.5 * dx1a(is-2)
      dx1b(is-2) = x1b (is-2) - x1min
      do 50 i=is-1,ie+2
         x1b(i) = x1a(i) + 0.5*dx1a(i)
        dx1b(i) = x1b(i) - x1b(i-1)
50    continue
      do 60 i=is-2,ie+2

         g2 a (i) = 1.0
         g2 b (i) = 1.0
         g31a (i) = 1.0
         g31b (i) = 1.0
        dg2 ad1(i) = 0.0
        dg2 bd1(i) = 0.0
        dg31ad1(i) = 0.0
        dg31bd1(i) = 0.0
        vol1a  (i) = x1a(i)
        vol1b  (i) = x1b(i)


        x1ai  (i) = 1.0 / ( x1a  (i) + tiny )
        x1bi  (i) = 1.0 / ( x1b  (i) + tiny )
        dx1ai (i) = 1.0 / ( dx1a (i) + tiny )
        dx1bi (i) = 1.0 / ( dx1b (i) + tiny )
        g2ai  (i) = 1.0 / ( g2a  (i) + tiny )
        g2bi  (i) = 1.0 / ( g2b  (i) + tiny )
        g31ai (i) = 1.0 / ( g31a (i) + tiny )
        g31bi (i) = 1.0 / ( g31b (i) + tiny )
60    continue
c

      dvl1b (is-2) = vol1b(is-2) - x1min


      dvl1bi(is-2) = 1.0 / ( dvl1b(is-2) + tiny )
      do 70 i=is-2,ie+1
        dvl1a (i  ) = vol1a(i+1) - vol1a(i)
        dvl1b (i+1) = vol1b(i+1) - vol1b(i)
        dvl1ai(i  ) = 1.0 / ( dvl1a(i  ) + tiny )
        dvl1bi(i+1) = 1.0 / ( dvl1b(i+1) + tiny )
70    continue

      dvl1a (ie+2) = x1max - vol1a(ie+2)


      dvl1ai(ie+2) = 1.0 / ( dvl1a(ie+2) + tiny )

c
c-----------  X2 GRID GENERATOR  ---------------------------------------
c

c
c  Variable names and values are the same as used in x1 grid generator
c
c
      js   = 3
      if (myid .eq. 0) then
        jmax = js
        nbl   = 1
        x2min = 0.0
        x2max = 0.0
        igrid = 0
        x2rat = 0.0
        dx2min= 0.0
        lgrid = .false.
        ibl   = 0
        mbl   = 0
c
110     continue
        read (1,ggen2)
cpu2006        write(2,ggen2)
c
        ibl  = ibl + 1
        if (ibl .gt. iblmax) then
          write(6,"(/1x,'ERROR: number of blocks in 2-direction exceeds'
     &    ,' array bounds'/1x,'ibl = ',i4,'  iblmax = ',i4)") ibl,iblmax
          mbl  = 1
          go to 131
        endif
        jmin = jmax
        jmax = jmax + nbl
        if (jmax .gt. js + (jn-4)*ntiles(2)) then
          write(6,"(/1x,'ERROR: number of zones in 2-direction exceeds'
     &    ,' array bounds',/1x,'jmax = ',i4,'  jn = ',i4)") jmax,jn
          mbl  = 1
          go to 131
        endif
c
c  1)  Compute dx2min from given value of x2rat.
c
        if (abs(igrid) .eq. 1) then
          if (igrid .eq. -1) x2rat = 1.0/x2rat
          if (x2rat .eq. 1.0)  then
            dx2min = (x2max-x2min)/real(nbl)
          else
            dx2min = (x2max-x2min)*(x2rat-1.0)/(x2rat**nbl - 1.0)
          endif
        endif
c
c  2)  Compute x2rat from given value of dx2min.  Newton Raphson 
c      iteration is required to find the root (x2rat) of the function:
c        fn(x2r) = (x2max-x2min) - dx2min*[(x2r)**nbl - 1]/[x2r-1] = 0
c
        if (abs(igrid) .eq. 2) then
          x2r = 1.01
          do 120 iter=1,20
            fn = (x2max - x2min) - dx2min*(x2r**nbl - 1.0)/(x2r - 1.0)
            dfn dx2r =  -nbl*dx2min*x2r**(nbl - 1)/(x2r - 1.0)
     .                 + dx2min*(x2r**nbl - 1.0)/(x2r - 1.0)**2
            deltx2r  = -fn/dfndx2r
            err x2r  = abs(deltx2r/x2r)
            x2r = x2r + deltx2r
            if (errx2r .lt. 1.0e-6) goto 130
120       continue
          write(6,"(1x,'GRIDJ: Newton-Raphson failed'
     &    ,' for x2rat',/1x,'jmin = ',i3,' x2r = ',1pe12.5,' deltx2r = '
     &    ,1e12.5,' fn = ',1e12.5)") jmin,x2r,deltx2r,fn
          ierr = 1
          go to 131
c
130       continue
          x2rat = x2r
        endif
        if (igrid .eq. -2) then
          dx2min = dx2min * x2rat**(nbl-1)
          x2rat  = 1.0 / x2rat
        endif
c
c Copy the grid descriptors into a temporary array for broadcast later.
c
        gridd(1,ibl) = float(jmin)
        gridd(2,ibl) = float(jmax)
        gridd(3,ibl) =  x2min
        gridd(4,ibl) = dx2min
        gridd(5,ibl) =  x2rat
c
c  Go back and read another block of x2 grid zones, if needed.
c
        if (.not. lgrid) go to 110
      endif
131   continue

      if (mbl  .ne. 0) then

        stop
      endif

c
c The full grid has gridd(2,ibl) zones.  Find the best number
c of zones per tile.  Assume for now that the number of zones
c is evenly divisible by the number of tiles in that direction.
c Otherwise, load-balancing becomes an issue.
c
      nzones(2) = int(gridd(2,ibl)-gridd(1,1))
c      nzpt(2)   = nzones(2)/ntiles(2)
c      fn        = real(nzones(2))/real(ntiles(2))
c      if (abs(real(nzpt(2))-fn) .gt. 0.001) then
c        if (myid .eq. 0) write(6,"(1x,'ERROR from GRIDJ: nzones(2) = '
c     &  ,i4,' is not evenly divisible by ntiles(2) = ',i3)") nzones(2)
c     &  ,ntiles(2)
c#ifdef MPI_USED
c        call MPI_FINALIZE(ierr)
c#endif
c        stop
c      endif
c
c Relax the assumption of equal number of zones per processor, and
c go through some checks to determine the best zones distribution
c for the zone and tile parameters given.
c
      nzpt(2) = 0
      remainder = mod(nzones(2),ntiles(2))
      if(remainder .eq. 0) then
       nzpt(2) = nzones(2)/ntiles(2)
      else if((ntiles(2)-remainder) .eq. 1) then
       if(coords(2) .ne. ntiles(2)-1) nzpt(2) = nzones(2)/ntiles(2)+1
       if(coords(2) .eq. ntiles(2)-1) nzpt(2) = nzones(2)/ntiles(2)
      else if(((ntiles(2)-remainder) .eq. 3) .and.
     .         (ntiles(2) .eq. 4)                 ) then
       if(coords(2) .le. 1) nzpt(2) = nzones(2)/ntiles(2)+1
       if(coords(2) .eq. 2) nzpt(2) = nzones(2)/ntiles(2)
       if(coords(2) .eq. 3) nzpt(2) = nzones(2)/ntiles(2)-1
      else
       if(coords(2) .ne. ntiles(2)-1) nzpt(2) = nzones(2)/ntiles(2)+1
       if(coords(2) .eq. ntiles(2)-1) nzpt(2) = nzones(2) -
     .   (ntiles(2)-1)*(nzones(2)/ntiles(2) + 1)
      endif
      if(nzpt(2) .eq. 0) then
       print *,'GGEN2: could not set nzpt(2)'

       stop
      endif
c
c From my coordinates in the 3-D Cartesian virtual topology (coords),
c figure out which zones I own and compute the grid.  This tile's
c first real zone has index ifirst.   Locate the block containing it.
c
c      joffst = coords(2) * nzpt(2)
      if(remainder .eq. 0) then
       joffst = coords(2) * nzpt(2)
      else if((ntiles(2)-remainder) .eq. 1) then
       joffst = coords(2) * (nzones(2)/ntiles(2)+1)
      else if(((ntiles(2)-remainder) .eq. 3) .and.
     .         (ntiles(2) .eq. 4)                 ) then
       if(coords(2) .le. 2)
     .    joffst = coords(2)*(nzones(2)/ntiles(2)+1)
       if(coords(2) .eq. 3)
     .    joffst = 2*(nzones(2)/ntiles(2)+1) + nzones(2)/ntiles(2)
      else
       joffst = coords(2) * (nzones(2)/ntiles(2)+1)
      endif
      jfirst = js + joffst
      do 132 nbl=1,ibl
        mbl  = nbl
        if (int(gridd(1,mbl)).gt.jfirst) then
          mbl = max(1, mbl-1)
          go to 134
        endif
132   continue
134   continue
        jmin = int(gridd(1,mbl))
        jmax = int(gridd(2,mbl))
       x2min = gridd(3,mbl)
      dx2min = gridd(4,mbl)
       x2rat = gridd(5,mbl)
c
c The first real zone on this tile is in block mbl.  Compute the
c grid points of zones whose indeces are less than jfirst, if any.
c
      if (jmin .lt. jfirst) then
        do 136 j=jmin+1,jfirst
          dx2min = dx2min * x2rat
           x2min =  x2min + dx2min
136     continue
      endif
      jmin = max(jmin,jfirst)
      jmax = min(jmax,jfirst+nzpt(2))
c
c Now do the rest of the grid points in this block.
c Set up x2a grid lines from j=jmin to jmax, using known values of
c x2min, x2rat. 
c
       x2a(jmin-joffst) =  x2min
      dx2a(jmin-joffst) = dx2min
      do 140 j=jmin+1,jmax
        dx2a(j-joffst) = dx2a(j-joffst-1) * x2rat
         x2a(j-joffst) =  x2a(j-joffst-1) + dx2a(j-joffst-1)
140   continue
      if (mbl.lt.ibl) then
        mbl = mbl + 1
        go to 134
      endif
c
c  Set up all grid zones, scale factors in x2 direction
c
      je = jmax - joffst
c

c
c..................  GRIDJ BOUNDARIES  .................................
c
c Get the coords of overlap ghost zones from the neighbors,
c unless I am on a physical boundary.
c
      nreq = 0
      if (coords(2).eq.0 .and. nijs(1).ne.4) then
c
c I am on the physical inner boundary.  Compute ghost zones there.
c
c If the BCs are reflecting, the grid should be symmetric.
c
        dx2a(js-1) = dx2a(js  )
        if (nijs(1).ne.2 .and. nijs(1).ne.3) then
c
c Use this for Reflection-Symmetric grids: 
c
          dx2a(js-2) = dx2a(js+1)
          dx2min     = dx2a(js+1)
        else
c
c The following seems best for Flow In/Out:
c
          dx2a(js-2) = dx2a(js-1)
          dx2min     = dx2a(js-2)
        endif
c
      else

c
c Periodic grid, one tile.
c
        dx2a(js-1) = dx2a(je-1)
        dx2a(js-2) = dx2a(je-2)
        dx2min     = dx2a(je-3)
c

      endif
c
      if (coords(2).eq.ntiles(2)-1 .and. nojs(1).ne.4) then
c
c I am on the physical outer boundary.  Compute ghost zones there.
c
        dx2a(je  ) = dx2a(je-1)
        if (nojs(1).ne.2 .and. nojs(1).ne.3) then
c
c Use this for Reflection-Symmetric and Periodic grids: 
c
          dx2a(je+1) = dx2a(je-2)
          dx2min     = dx2a(je-3)
        else
c
c Use this for Flow In/Out:
c
          dx2a(je+1) = dx2a(je  )
          dx2a(je+2) = dx2a(je+1)
        endif
      else

c
c Periodic grid.
c
        dx2a(je  ) = dx2a(js  )
        dx2a(je+1) = dx2a(js+1)
        dx2a(je+2) = dx2a(js+2)
c

      endif

      x2a (js-1) = x2a (js  ) - dx2a(js-1)
      x2a (js-2) = x2a (js-1) - dx2a(js-2)
      x2min      = x2a (js-2) - dx2min
      x2a (je+1) = x2a (je  ) + dx2a(je  )
      x2a (je+2) = x2a (je+1) + dx2a(je+1)
      x2max      = x2a (je+2) + dx2a(je+2)
c
c................  GRIDJ B-MESH AND GEOM FACTORS  ......................
c
       x2min     = x2min      + 0.5 * dx2min 
       x2b(js-2) = x2a (js-2) + 0.5 * dx2a(js-2)
      dx2b(js-2) = x2b (js-2) - x2min
      do 150 j=js-1,je+2
         x2b(j) = x2a(j) + 0.5 * dx2a(j)
        dx2b(j) = x2b(j) - x2b(j-1)
150   continue
c
      do 160 j=js-2,je+2

        g32a (j)  = 1.0
        g32b (j)  = 1.0
       dg32ad2(j) = 0.0
       dg32bd2(j) = 0.0
       vol2a  (j) = x2a(j)
       vol2b  (j) = x2b(j)



        x2ai  (j) = 1.0 / ( x2a  (j) + tiny )
        x2bi  (j) = 1.0 / ( x2b  (j) + tiny )
        dx2ai (j) = 1.0 / ( dx2a (j) + tiny )
        dx2bi (j) = 1.0 / ( dx2b (j) + tiny )
        g32ai (j) = 1.0 / ( g32a (j) + tiny )
        g32bi (j) = 1.0 / ( g32b (j) + tiny )
160   continue
c

      dvl2b (js-2) = vol2b(js-2) - x2min


      dvl2bi(js-2) = 1.0 / ( dvl2b(js-2) + tiny )
      do 170 j=js-2,je+1
        dvl2a (j  ) = vol2a(j+1) - vol2a(j)
        dvl2b (j+1) = vol2b(j+1) - vol2b(j)
        dvl2ai(j  ) = 1.0 / ( dvl2a(j  ) + tiny )
        dvl2bi(j+1) = 1.0 / ( dvl2b(j+1) + tiny )
170   continue

      dvl2a (je+2) = x2max - vol2a(je+2)


      dvl2ai(je+2) = 1.0 / ( dvl2a(je+2) + tiny )

c
c-----------  X3 GRID GENERATOR  ---------------------------------------
c

c
c  Variable names and values are the same as used in x1 grid generator
c
      ks   = 3
      if (myid .eq. 0) then
        kmax = ks
        nbl   = 1
        x3min = 0.0
        x3max = 0.0
        igrid = 0
        x3rat = 0.0
        dx3min= 0.0
        lgrid = .false.
        ibl   = 0
        mbl   = 0
c
210     continue
        read (1,ggen3)
cpu2006        write(2,ggen3)
c
        ibl  = ibl + 1
        if (ibl .gt. iblmax) then
          write(6,"(/1x,'ERROR: number of blocks in 3-direction exceeds'
     &    ,' array bounds'/1x,'ibl = ',i4,'  iblmax = ',i4)") ibl,iblmax
          mbl  = 1
          go to 231
        endif
        kmin = kmax
        kmax = kmax + nbl
        if (kmax .gt. ks + (kn-4)*ntiles(3)) then
          write(6,"(/1x,'ERROR: number of zones in 3-direction exceeds'
     &    ,' array bounds',/1x,'kmax = ',i4,'  kn = ',i4)") kmax,kn
          mbl  = 1
          go to 231
        endif
c
c  1)  Compute dx3min from given value of x3rat.
c
        if (abs(igrid) .eq. 1) then
          if (igrid .eq. -1) x3rat = 1.0/x3rat
          if (x3rat .eq. 1.0)  then
            dx3min = (x3max-x3min)/real(nbl)
          else
            dx3min = (x3max-x3min)*(x3rat-1.0)/(x3rat**nbl - 1.0)
          endif
        endif
c
c  2)  Compute x3rat from given value of dx3min.  Newton Raphson 
c      iteration is required to find the root (x3rat) of the function:
c        fn(x3r) = (x3max-x3min) - dx3min*[(x3r)**nbl - 1]/[x3r-1] = 0
c
        if (abs(igrid) .eq. 2) then
          x3r = 1.01
          do 220 iter=1,20
            fn = (x3max - x3min) - dx3min*(x3r**nbl - 1.0)/(x3r - 1.0)
            dfn dx3r =  -nbl*dx3min*x3r**(nbl - 1)/(x3r - 1.0)
     .                 + dx3min*(x3r**nbl - 1.0)/(x3r - 1.0)**2
            deltx3r  = -fn/dfndx3r
            err x3r  = abs(deltx3r/x3r)
            x3r = x3r + deltx3r
            if (errx3r .lt. 1.0e-6) goto 230
220       continue
          write(6,"(1x,'GRIDK: Newton-Raphson failed'
     &    ,' for x3rat',/1x,'kmin = ',i3,' x3r = ',1pe12.5,' deltx3r = '
     &    ,1e12.5,' fn = ',1e12.5)") kmin,x3r,deltx3r,fn
          mbl  = 1
          go to 231
c
230       continue
          x3rat = x3r
        endif
        if (igrid .eq. -2) then
          dx3min = dx3min * x3rat**(nbl-1)
          x3rat  = 1.0 / x3rat
        endif
c
c Copy the grid descriptors into a temporary array for broadcast later.
c
        gridd(1,ibl) = float(kmin)
        gridd(2,ibl) = float(kmax)
        gridd(3,ibl) =  x3min
        gridd(4,ibl) = dx3min
        gridd(5,ibl) =  x3rat
c
c  Go back and read another block of x3 grid zones, if needed.
c
        if (.not. lgrid) go to 210
      endif
231   continue

      if (mbl  .ne. 0) then

        stop
      endif

c
c The full grid has gridd(2,ibl) zones.  Find the best number
c of zones per tile.  Assume for now that the number of zones
c is evenly divisible by the number of tiles in that direction.
c Otherwise, load-balancing becomes an issue.
c
      nzones(3) = int(gridd(2,ibl)-gridd(1,1))
c      nzpt(3)   = nzones(3)/ntiles(3)
c      fn        = real(nzones(3))/real(ntiles(3))
c      if (abs(real(nzpt(3))-fn) .gt. 0.001) then
c        if (myid .eq. 0) write(6,"(1x,'ERROR from GRIDK: nzones(3) = '
c     &  ,i4,' is not evenly divisible by ntiles(3) = ',i3)") nzones(3)
c     &  ,ntiles(3)
c#ifdef MPI_USED
c        call MPI_FINALIZE(ierr)
c#endif
c        stop
c      endif
c
c Relax the assumption of equal number of zones per processor, and
c go through some checks to determine the best zones distribution
c for the zone and tile parameters given.
c
      nzpt(3) = 0
      remainder = mod(nzones(3),ntiles(3))
      if(remainder .eq. 0) then
       nzpt(3) = nzones(3)/ntiles(3)
      else if((ntiles(3)-remainder) .eq. 1) then
       if(coords(3) .ne. ntiles(3)-1) nzpt(3) = nzones(3)/ntiles(3)+1
       if(coords(3) .eq. ntiles(3)-1) nzpt(3) = nzones(3)/ntiles(3)
      else if(((ntiles(3)-remainder) .eq. 3) .and.
     .         (ntiles(3) .eq. 4)                 ) then
       if(coords(3) .le. 1) nzpt(3) = nzones(3)/ntiles(3)+1
       if(coords(3) .eq. 2) nzpt(3) = nzones(3)/ntiles(3)
       if(coords(3) .eq. 3) nzpt(3) = nzones(3)/ntiles(3)-1
      else
       if(coords(3) .ne. ntiles(3)-1) nzpt(3) = nzones(3)/ntiles(3)+1
       if(coords(3) .eq. ntiles(3)-1) nzpt(3) = nzones(3) -
     .   (ntiles(3)-1)*(nzones(3)/ntiles(3) + 1)
      endif
      if(nzpt(3) .eq. 0) then
       print *,'GGEN3: could not set nzpt(3)'

       stop
      endif
c
c From my coordinates in the 3-D Cartesian virtual topology (coords),
c figure out which zones I own and compute the grid.  This tile's
c first real zone has index ifirst.   Locate the block containing it.
c
c      koffst = coords(3) * nzpt(3)
      if(remainder .eq. 0) then
       koffst = coords(3) * nzpt(3)
      else if((ntiles(3)-remainder) .eq. 1) then
       koffst = coords(3) * (nzones(3)/ntiles(3)+1)
      else if(((ntiles(3)-remainder) .eq. 3) .and.
     .         (ntiles(3) .eq. 4)                 ) then
       if(coords(3) .le. 2)
     .    koffst = coords(3)*(nzones(3)/ntiles(3)+1)
       if(coords(3) .eq. 3)
     .    koffst = 2*(nzones(3)/ntiles(3)+1) + nzones(3)/ntiles(3)
      else
       koffst = coords(3) * (nzones(3)/ntiles(3)+1)
      endif
      kfirst = ks + koffst
      do 232 nbl=1,ibl
        mbl  = nbl
        if (int(gridd(1,mbl)).gt.kfirst) then
          mbl = max(1, mbl-1)
          go to 234
        endif
232   continue
234   continue
        kmin = int(gridd(1,mbl))
        kmax = int(gridd(2,mbl))
       x3min = gridd(3,mbl)
      dx3min = gridd(4,mbl)
       x3rat = gridd(5,mbl)
c
c The first real zone on this tile is in block mbl.  Compute the
c grid points of zones whose indeces are less than kfirst, if any.
c
      if (kmin .lt. kfirst) then
        do 236 k=kmin+1,kfirst
          dx3min = dx3min * x3rat
           x3min =  x3min + dx3min
236     continue
      endif
      kmin = max(kmin,kfirst)
      kmax = min(kmax,kfirst+nzpt(3))
c
c Now do the rest of the grid points in this block.
c Set up x3a grid lines from k=kmin to kmax, using known values of
c x3min, x3rat. 
c
       x3a(kmin-koffst) =  x3min
      dx3a(kmin-koffst) = dx3min
      do 240 k=kmin+1,kmax
        dx3a(k-koffst) = dx3a(k-koffst-1) * x3rat
         x3a(k-koffst) =  x3a(k-koffst-1) + dx3a(k-koffst-1)
240   continue
      if (mbl.lt.ibl) then
        mbl = mbl + 1
        go to 234
      endif
c
c  Set up all grid zones, scale factors in x3 direction
c
      ke = kmax - koffst
c

c
c..................  GRIDK BOUNDARIES  .................................
c
c Get the coords of overlap ghost zones from the neighbors,
c unless I am on a physical boundary.
c
      nreq = 0
      if (coords(3).eq.0 .and. niks(1).ne.4) then
c
c I am on the physical inner boundary.  Compute ghost zones there.
c
c If the BCs are reflecting, the grid should be symmetric.
c
        dx3a(ks-1) = dx3a(ks  )
        if (niks(1).ne.2 .and. niks(1).ne.3) then
c
c Use this for Reflection-Symmetric grids: 
c
          dx3a(ks-2) = dx3a(ks+1)
          dx3min     = dx3a(ks+2)
        else
c
c The following seems best for Flow In/Out:
c
          dx3a(ks-2) = dx3a(ks-1)
          dx3min     = dx3a(ks-2)
        endif
      else

c
c Periodic grid, one tile.
c
        dx3a(ks-1) = dx3a(ke-1)
        dx3a(ks-2) = dx3a(ke-2)
        dx3min     = dx3a(ke-3)
c

      endif
c
      if (coords(3).eq.ntiles(3)-1 .and. noks(1).ne.4) then
c
c I am on the physical outer boundary.  Compute ghost zones there.
c
        dx3a(ke+1) = dx3a(ke  )
        if (noks(1).ne.2 .and. noks(1).ne.3) then
c
c Use this for Reflection-Symmetric and Periodic grids: 
c
          dx3a(ke+1) = dx3a(ke-2)
          dx3a(ke+2) = dx3a(ke-3)
        else
c
c Use this for Flow In/Out:
c
          dx3a(ke+1) = dx3a(ke  )
          dx3a(ke+2) = dx3a(ke+1)
        endif
      else

c
c Periodic grid.
c
        dx3a(ke  ) = dx3a(ks  )
        dx3a(ke+1) = dx3a(ks+1)
        dx3a(ke+2) = dx3a(ks+2)
c

      endif

      x3a (ks-1) = x3a (ks  ) - dx3a(ks-1)
      x3a (ks-2) = x3a (ks-1) - dx3a(ks-2)
      x3min      = x3a (ks-2) - dx3min
      x3a (ke+1) = x3a (ke  ) + dx3a(ke  )
      x3a (ke+2) = x3a (ke+1) + dx3a(ke+1)
      x3max      = x3a (ke+2) + dx3a(ke+2)
c
c................  GRIDK B-MESH AND GEOM FACTORS  ......................
c
       x3min     = x3min      + 0.5 * dx3min
       x3b(ks-2) = x3a (ks-2) + 0.5 * dx3a(ks-2)
      dx3b(ks-2) = x3b (ks-2) - x3min
      do 250 k=ks-1,ke+2
         x3b(k) = x3a(k) + 0.5 * dx3a(k)
        dx3b(k) = x3b(k) - x3b(k-1)
250   continue
      do 260 k=ks-2,ke+2
        dx3a i(k) = 1.0 / ( dx3a(k) + tiny )
        dx3b i(k) = 1.0 / ( dx3b(k) + tiny )
       vol3a  (k) = x3a(k)
       vol3b  (k) = x3b(k)
260   continue
c
      dvl3b (ks-2) = vol3b(ks-2) - x3min
      dvl3bi(ks-2) = 1.0 / ( dvl3b(ks-2) + tiny )
      do 270 k=ks-2,ke+1
        dvl3a (k  ) = vol3a(k+1) - vol3a(k)
        dvl3b (k+1) = vol3b(k+1) - vol3b(k)
        dvl3ai(k  ) = 1.0 / ( dvl3a(k  ) + tiny )
        dvl3bi(k+1) = 1.0 / ( dvl3b(k+1) + tiny )
270   continue
      dvl3a (ke+2) = x3max - vol3a(ke+2)
      dvl3ai(ke+2) = 1.0 / ( dvl3a(ke+2) + tiny )

c
c-----------------------------------------------------------------------
c
c  is,ie [js,je] are starting and ending indices of ACTIVE i [j] zones,
c  so modify accordingly
c
      ie = ie - 1
      je = je - 1
      ke = ke - 1
      nx1z = ie - is + 1
      nx2z = je - js + 1
      nx3z = ke - ks + 1
c
      return
      end
