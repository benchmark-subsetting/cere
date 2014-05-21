












































































c=======================================================================
c
c    \\\\\\\\\\        B E G I N   P R O G R A M          //////////
c    //////////               Z E U S M P                 \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
      program zeusmp
c
c PURPOSE
c   Main program for 3-D MPI version of ZEUS.
c
c AUTHOR
c   Robert A. Fiedler
c
c LAST MODIFIED by PSLi
c   12/30/99.
c.......................................................................
c
c DECLARATIONS
c
      implicit NONE















































































      integer in, jn, kn, ijkn, neqm
      parameter(in =           64+5
     &        , jn =           64+5
     &        , kn =           64+5)
      parameter(ijkn =   64+5)
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






































c Commented out for SPEC CPU2006 
c      real*4    tarray(2), etime, cputime0, cputime
c     &        , wclock0, wclock
c      external  etime



c Commented out for SPEC CPU2006 
c      integer   iarray(3), itime
c      external  itime




c Commented out for SPEC CPU2006 
c      common    /checkr/ tarray, cputime0, wclock0


c      common    /checki/ iarray





      real*8 totlsit, totnrit, nrpert, lspert
      common /impacct/ totlsit, totnrit


      real*8    g,tgrav,ptmass,x1ptm,x2ptm,x3ptm,
     &        rt_fn_grv,rt_co_grv,wgt_grv

      integer mi_fn_grv,mi_co_grv,m_grv,nu_pr_grv,nu_po_grv,
     &        vv_grv,sl_fn_grv,sl_co_grv,smth_grv
CPS


      common /gravcomr/ g,tgrav,ptmass,x1ptm,x2ptm,x3ptm,
     &        rt_fn_grv,rt_co_grv,wgt_grv

      common /gravcomi/ mi_fn_grv,mi_co_grv,m_grv,nu_pr_grv,nu_po_grv,
     &        vv_grv,sl_fn_grv,sl_co_grv,smth_grv
CPS




      real*8    c,epsme,demax,dermax,dtotmax,radth,epsrad,
     .        dtimrdi,ernom,ennom,epsmaxd,epsmaxc,dtimpmxi2
      integer ifld,nmeiter,maxrad,ks0rad,iorad,cnvcrit
      common /radr/ c,epsme,demax,dermax,dtotmax,radth,epsrad
     .             ,ernom,ennom,epsmaxd,epsmaxc
      common /radi/ ifld,nmeiter,maxrad,ks0rad,iorad,cnvcrit
      common /impdt/ dtimrdi,dtimpmxi2
c#if defined RAD || defined EXP_DIFF
      real*8 f11 (in,jn,kn), f22 (in,jn,kn), f12 (in,jn,kn), 
     .     dr1 (in,jn,kn), dr2 (in,jn,kn), dr3 (in,jn,kn)
      real*8 dvl11(in,jn,kn), dvl22(in,jn,kn), dvl12(in,jn,kn),
     .     dvl21(in,jn,kn), divvl(in,jn,kn), dvl33(in,jn,kn),
     .     en  (in,jn,kn), ern (in,jn,kn), de  (in,jn,kn), 
     .     der (in,jn,kn),
     .     pn  (in,jn,kn),
     .     dpde(in,jn,kn),
     .     fr1 (in,jn,kn), fr2 (in,jn,kn),
     .     fr3 (in,jn,kn), p   (in,jn,kn)
      common /radr/
     .     f11 , f22, f12, dr1, dr2, dvl11, dvl22, dvl12, 
     .     dvl21, divvl,
     .     dvl33, en , ern, de , der, pn  , 
     .     dpde, fr1, fr2, fr3, dr3, p
c#endif 
      integer mi_fn_rad,mi_co_rad,m_rad,nu_pr_rad,nu_po_rad,
     &        vv_rad,sl_fn_rad,sl_co_rad,smth_rad
      real*8 rt_fn_rad,rt_co_rad,wgt_rad
      common /radcomr/ rt_fn_rad,rt_co_rad,wgt_rad

      common /radcomi/ mi_fn_rad,mi_co_rad,m_rad,nu_pr_rad,nu_po_rad,
     &        vv_rad,sl_fn_rad,sl_co_rad,smth_rad
c
      real*8     zcs
      integer i,j,k


CJH
      real*8 t1, t2


      real*8     cpuall



      external mstart,dataio,srcstep,nudt,newgrid,intchk,empty
c
c DATA
c
#ifndef SPEC_CPU
      data nhy, time, nsub / 0, 0.0, 0 /
      data zcs / 0. /

      data ifsen / 0, 0, 1, 1, 1, 1/

      data myid_w, myid, nprocs_w, nprocs / 0, 0, 1, 1 /
      data coords / 0, 0, 0 /
      data reorder  / .true. /
#else
      nhy = 0
      time = 0.0
      nsub = 0
      zcs = 0.
      ifsen(1) = 0
      ifsen(2) = 0
      ifsen(3) = 1
      ifsen(4) = 1
      ifsen(5) = 1
      ifsen(6) = 1
      myid_w = 0
      myid = 0
      nprocs_w = 1
      nprocs = 1
      coords(1) = 0
      coords(2) = 0
      coords(3) = 0
      reorder = .true.
#endif


c Commented out for SPEC CPU2006 
c
c  Master writes greeting.
c
c      if (myid_w .eq. 0) then
c        write(6,"(///10x,'ZZZZZ EEEEE U   U  SSSS     M   M PPPP ')")
c        write(6,   "(10x,'   Z  E     U   U S         MM MM P   P')")
c        write(6,   "(10x,'  Z   EEEE  U   U  SSS  === M M M PPPP ')")
c        write(6,   "(10x,' Z    E     U   U     S     M   M P    ')")
c        write(6,   "(10x,'ZZZZZ EEEEE  UUU  SSSS      M   M P    ')")
c        write(6,"()")
c        write(6,"()")
c        write(6,"(10x,'       DEVELOPED BY ROBERT A. FIEDLER')")
c        write(6,"(10x,'         ZEUS-MP V1.0 - 09/30/99')")
c        write(6,"()")
c        write(6,"(10x,'RUNNING AS ',i4,' PROCESS(ES)')") nprocs_w
c        write(6,"(10x,'WITH THE FOLLOWING CONFIGURATION:')")
c        write(6,"()")
c        write(6, "(14x,'* geometry:        XYZ')"  )
c        write(6,    "(14x,'* moving grid      OFF')"  )
c        write(6, "(14x,'* point masses     OFF')"   )
c        write(6, "(14x,'* self-gravity     OFF')"   )
c
c        write(6, "(14x,'* magnetic fields  ON ')"       )
c        write(6, "(14x,'* implicit rad alg OFF')" )
c        write(6, "(14x,'* PF rad algorithm OFF')" )
c        write(6, "(14x,'* precision:       DBL')" )
c        write(6, "(14x,'* thread time:     CPU')" )
c        write(6, "(14x,'* profiling        OFF')" )
c        write(6, "(14x,'* message passing  OFF')" )
c        write(6, "(14x,'* MP Environment   OFF')" )
c        write(6, "(14x,'* debug messages   OFF')" )
c        write(6, "(14x,'* restart dumps    OFF')" )
c        write(6, "(14x,'* HDF dumps        OFF')" )
c        write(6, "(14x,'* TSL dumps        ON ')" )
c        write(6, "(14x,'* history dumps    OFF')" )
c        write(6, "(14x,'* text dumps       OFF')" )
c        write(6,"()")
c      endif
c
c Set up the problem: read input deck and possibly a restart file.
c
      call mstart
c
c Write out initial data dumps.
c
c      call dataio( iswres, iswhdf, iswhst, iswusr 

c    .                                             )
      call dataio( ifsen(2), ifsen(3), ifsen(4), ifsen(5), ifsen(6)

     .                                                   )
c
c  Initialize cpu and wall clocks.  The quantities "cputime" and
c  "wclock" are the CPU and wall clock times (in seconds) in the main 
c  loop.
c
c Commented out for SPEC CPU2006 
c        wclock0 = 0.0
c        cputime0 = 0.0
c        call clocks (cputime, wclock)

c        wclock0 = wclock
c        cputime0 = cputime


c Commented out for SPEC CPU2006 
c      if (myid .eq. 0)
c     &  write(6,"(/,' Set-up complete with ',i2,' warning(s):'
c     &             ,' entering main loop...')") nwarn

CPS

c
c--------------------------  start of main loop  -----------------------
c
c Execution ends when INTCHK returns a value of 1 for ifsen(1).
c

1000  continue

      nsub = 1

CPS

C
c
c Evaluate all non-advective terms in the evolution equations.
c
        call srcstep
c
c Compute the advection of all field variables.
c
        call transprt
c
c Optional call user-supplied module.
c
        call empty
c
c Update the step counter and current time value.
c
        nhy   = nhy   + 1
        time  = time  + dt
c
c Check the CPU time, number of steps, output times to determine if
c a stopping criterion has been met or output is desired.
c Also check for keyboard input, depending on the value of mbatch.
c
c        call intchk( iswres, iswhdf, iswhst, iswusr )
        call intchk( ifsen(2), ifsen(3), ifsen(4), ifsen(5), ifsen(6) )
c
c Compute new timestep
c
        call nudt

c
c Write out any desired output files now that everything has been
c updated.
c Skip dataio if the run is being terminated to avoid duplicate output.
c
        if (ifsen(1) .eq. 1) goto 2000
        call dataio( ifsen(2), ifsen(3), ifsen(4), ifsen(5), ifsen(6)

     .                                                     )
c
c  Loop back to begin the next time step.
      goto 1000  
c
c--------------------------  end of main loop  -------------------------
c
c Terminate the run by making final dumps, write goodbyes
c
2000  continue

c Commented out for SPEC CPU2006 
c      call clocks (cputime, wclock)


c      tused = real(cputime)

      ifsen(2) = 1
      ifsen(3) = 1
      ifsen(4) = 1
      ifsen(5) = 1
      ifsen(6) = 1

      call dataio( ifsen(2), ifsen(3), ifsen(4), ifsen(5) , ifsen(6)

     .                                                   )

c      cpuall = tused

      if (myid .eq. 0) then      
c
c Let's assume tused is user + system time on the master thread.
c One would also like to keep track of wall-clock time and the sum
c of CPU times used by each processor.
c
        zcs = real(nprocs_w*nhy*nx1z*nx2z*nx3z)/(tused+tiny)
c Commented out for SPEC CPU2006 
c        write(6,"(/' Execution terminated with ',i4,' warning(s)')") 
c     &     nwarn
c        write(6,"(/' Performance summary:')")
c        write(6,"('  zone-cycles per cpu second =',1pe12.5)") zcs
c        write(6,"('  Master CPU                 =',1pe12.5, ' sec')") 
c     &     tused
c        write(6,"('  Average CPU/node           =',1pe12.5, ' sec')") 
c     &     cpuall/real(nprocs_w)
c        write(6,"('  Wall Clock                 =',1pe12.5, ' sec')") 
c     &     wclock
c        write(6,"()")
        close(unit=2)
        close(unit=3)
        close(unit=30)

        close(unit=31)

      endif



c


c
c=======================================================================
c
c    \\\\\\\\\\          E N D  P R O G R A M             //////////
c    //////////               Z E U S M P                 \\\\\\\\\\
c
c=======================================================================
c
      end


