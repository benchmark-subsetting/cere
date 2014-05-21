












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine mstart
c
c  PURPOSE:  Starts a run.
c
c  EXTERNALS: SETUP, MGET, empty
c
c  LOCALS:
c
c  LAST MODIFIED: 7/20/01 by PSLi
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
      integer irestart
c
      real*8 t_out(nbuff-8)
      common / op_times / t_out
      integer incr,strtoi

      external setup, mget, restart, strtoi
      namelist /mpitop/ ntiles, periodic
      namelist /rescon/ irestart,tdump,dtdump,id,resfile
      namelist /iocon/ thdf,dthdf,thist,dthist,tusr,dtusr,t_out
     .                ,ttsl,dttsl

c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////
c=======================================================================
c

c
c Open input and log files -- master thread only.
c
      if (myid_w .eq. 0) then
        open(unit=1,file='zmp_inp',status='old')
cpu2006        open(unit=2,file='zmp_log',status='unknown')
      endif
CJH
c------------------------  MPI TOPOLOGY  -------------------------------c
c  ntiles:   elements equal the number of tiles in each direction.
c  periodic: elements are true if grid is periodic in that direction --
c            we check hydro BC flags for defaults, but can override.
c
      if (myid_w .eq. 0) then
        ntiles(1) = 1
        ntiles(2) = 1
        ntiles(3) = 1
        periodic(1) = .false.
        periodic(2) = .false.
        periodic(3) = .false.
        read (1,mpitop)
cpu2006        write(2,mpitop)
      endif
c

c
c------------------------  empty CONTROL  ----------------------------
c
c  irestart: set to one for calculation restarted from restart dump
c  tdump: time of last restart dump
c  dtdump: time between restart dumps
c  id: character*2 tag attended to filenames to identify run
c  resfile: name of restart file to restart from
c
      if (myid_w .eq. 0) then
        irestart = 0
        tdump = 0.0
        dtdump = 0.0
        id     = 'aa'
        resfile= 'resaa000000.000'
        read (1,rescon)
cpu2006        write(2,rescon)
      endif

c
c Read the remaining namelists (except for iocon) in setup/restart.
c
      if (irestart .eq. 0) then

        call setup
        time = 0.0D0
      else

        call mget(resfile)
        nwarn = 0
        ifsen(1) = 0
        call restart
      endif
c
c------------------------  I/O CONTROL ---------------------------------
c
c  thdf: time of last HDF dump
c  dthdf: time between HDF dumps
c  ttsl: time of last tslice dump
c  dttsl: time between tslice dumps
c  thist: time of last history dump
c  dthist: time between history dumps
c  tusr: time of last user dump
c  dtusr: time between user dumps

c
      if (myid_w .eq. 0) then
        if (irestart .eq. 0) then
           thdf  = 0.0
          dthdf  = 0.0
           ttsl  = 0.0
          dttsl  = 0.0
           thist = 0.0
          dthist = 0.0
           tusr  = 0.0
          dtusr  = 0.0

          do incr=1,nbuff-8
            t_out(incr) = 0.0
c incr
          enddo 
        endif
        read (1,iocon)
cpu2006        write(2,iocon)
      endif

c
c Output file names are of the form "hdfidccc.n" for ease of
c use with graphics packages that can process a series of files.
c
      if (irestart .eq. 0) then
        incr = 0
        ifsen(2) = 1
        ifsen(3) = 1
        ifsen(4) = 1
        ifsen(5) = 1
        ifsen(6) = 1

        write(tslfile,"(a3,i3.3,a2)") 'tsl',incr,id

        write(resfile,"(a3,a2,3i2.2,'.',i3.3)") 'res',id,coords(1)
     &                                      ,coords(2),coords(3),incr
        write(hdffile,"(a3,a2,3i2.2,'.',i3.3)") 'hdf',id,coords(1)
     &                                      ,coords(2),coords(3),incr
        write(hstfile,"(a3,a2,3i2.2,'.',i3.3)") 'hst',id,coords(1)
     &                                      ,coords(2),coords(3),incr
        write(usrfile,"(a3,a2,3i2.2,'.',i3.3)") 'usr',id,coords(1)
     &                                      ,coords(2),coords(3),incr


      else
        ifsen(2) = 0
        ifsen(3) = 0
        ifsen(4) = 0
        ifsen(5) = 0
        ifsen(6) = 0

        incr = strtoi(tslfile,4,6) + 1
        write(tslfile,"(a3,i3.3,a2)") 'tsl',incr,id 

        incr = strtoi(resfile,13,15) + 1
        write(resfile,"(a3,a2,3i2.2,'.',i3.3)") 'res',id,coords(1)
     &                                      ,coords(2),coords(3),incr
        incr = strtoi(hdffile,13,15) + 1
        write(hdffile,"(a3,a2,3i2.2,'.',i3.3)") 'hdf',id,coords(1)
     &                                      ,coords(2),coords(3),incr
        incr = strtoi(hstfile,13,15) + 1
        write(hstfile,"(a3,a2,3i2.2,'.',i3.3)") 'hst',id,coords(1)
     &                                      ,coords(2),coords(3),incr
        incr = strtoi(usrfile,13,15) + 1
        write(usrfile,"(a3,a2,3i2.2,'.',i3.3)") 'usr',id,coords(1)
     &                                      ,coords(2),coords(3),incr

      endif


       if (myid_w .eq. 0) then
         open (unit=31,file=tslfile,status='unknown')
       endif


      if (incr+1 .le. nbuff-10) then
c
c Adjust dump times to hit the next t_out, if defined non-zero.
c
        if (t_out(incr+1) .gt. 0.0) then
          tusr = t_out(incr+1) - dtusr
          thdf = t_out(incr+1) - dthdf
        endif
      endif
c
c  Close unit=1 (input deck).  Unit=2 (zeuslp) is kept open throughout
c  entire run to accept warning messages.  It is closed in zeusmp.
c
      close(unit=1)
c
c Write out initial data dumps.
c
      if(irestart .eq. 0) then
c      call dataio( iswres, iswhdf, iswhst, iswusr

c    .            )
       call dataio( ifsen(2), ifsen(3), ifsen(4), ifsen(5), ifsen(6)

     .            )
       nhy = 0
      else
       nhy = 0
      endif
c
      return
      end
