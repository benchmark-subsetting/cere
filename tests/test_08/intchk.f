












































































c=======================================================================
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine intchk(iswres,iswhdf,iswtsl,iswhst,iswusr)
c
c  PURPOSE:  Reads the buffer for valid interrupt messages and takes
c    appropriate action. Also checks stopping criteria, and sets ifsen=1
c    if stop condition is detected.
c
c  INPUT ARGUMENTS: none
c
c  OUTPUT ARGUMENTS: iswres,iswhdf,iswtsl,iswhst,iswusr=switches for restart,
c  hdf, history, and USER dumps; set to 1 if dump to be made in DATAIO
c
c  EXTERNALS:  BCDFLT, FINDNO, [ETIME,SECOND]

c
c  LOCALS:
c
c  LAST MODIFIED: RAF 9/09/96 for ZEUS-MP
c  LAST MODIFIED: efh 04/15/99 including tslice
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






































c Commented out for SPEC CPU2006 
c      real*4    tarray(2), etime, cputime0, cputime
c     &        , wclock0, wclock
c      external  etime



c      integer   iarray(3), itime
c      external  itime




c      common    /checkr/ tarray, cputime0, wclock0


c      common    /checki/ iarray




c-----------------------------------------------------------------------
      integer iswres,iswhdf,iswtsl,iswhst,iswusr
c
      integer i,nchar,istrt,iend
      real*8 valnew
      character*80 msg
      integer maxwarn
      integer icheckn
c Commented out for SPEC CPU2006 
c      external  bcdflt,findno

c  List of valid interrupt messages
      character*3 intmsg(17)
      data intmsg /  'sto','?  ','pau','abo','tli','cpu','nli','dum'
     &  ,'dtd','hdf','dtf','tsl','dtt','hst','dth','usr','dtu' /
c
cRAF: Number of warnings to issue before giving up
c
      data maxwarn /20/
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\////////////////////////////////
c=======================================================================
c  Check stopping criteria
c
c We check to see if the CPU time limit is approaching only in
c interactive mode (mbatch = 0), because this requires
c broadcasting the master's value of the CPU time used.
c
c Do we have to make the following system call every time step?
c

c Commented out for SPEC CPU2006 
c      if (myid_w .eq. 0) then
c        call clocks (cputime, wclock)

c        tused = real(cputime)

c      endif

      valnew = 0.0d0
c
      if (tlim .gt. 0.0 .and. time .ge. tlim) then
        if (myid_w .eq. 0) 
     &    write(6,"(/1x,'terminating on physical time limit',
     &    /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &    /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5)") 
     &    tlim,nlim,cpulim,time,nhy,tused
        ifsen(1) = 1
      endif
      if (nlim .gt. 0    .and. nhy .ge. nlim) then
        if (myid_w .eq. 0) 
     &    write(6,"(/1x,'terminating on cycle limit',
     &    /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &    /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5)") 
     &    tlim,nlim,cpulim,time,nhy,tused
        ifsen(1) = 1
      endif
c
cRAF: Stop if more than maxwarn warnings have been issued.
c
      if (nwarn .gt. maxwarn) then
        if (myid_w .eq. 0) 
     &    write(6,"(/1x,'terminating after ',i4,' warnings',
     &    /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &    /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5)") 
     &    maxwarn,tlim,nlim,cpulim,time,nhy,tused
        ifsen(1) = 1
      endif
c
c check to see if operator at RZG is asking the job to shut down
c 

      if (ifsen(1) .eq. 1) return
      if (mbatch .ne. 0) return
c
c.......................................................................
c
c FOR INTERACTIVE MODE ONLY -- REQUIRES COMMUNICATION
c
c Stop if the CPU time is approaching the limit.  See if there's 
c enough time remaining to take an average time step.
c

      if (cpulim - tsave - tused .lt. tused/real(nhy)) then
        if (myid_w .eq. 0) 
     &    write(6,"(/1x,'terminating on single-process CPU time limit',
     &    /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &    /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5)") 
     &    tlim,nlim,cpulim,time,nhy,tused
        ifsen(1) = 1
        return
      endif
c
c  Check for interrupt messages if not in batch mode.  If none or 
c  illegal message found then return
c
      icheckn = 1

c      if (nchar .eq. 0) return

      do i=1,15
        if (msg(1:3) .eq. intmsg(i)) goto 20
      enddo
      if (myid .eq. 0)
     &write(6,"(1x,a3,' is not an interrupt message.  Legal messages ',
     &'are:',/1x,'sto ? pau abo tli nli cpu dum dtd hdf dtf hst dth',
     &' usr dtu')") msg(1:3)
      return
20    continue
c
c  Legal interrupt message found, process it
c
c  stop command
c
      if (msg(1:3) .eq. 'sto') then
        if (myid .eq. 0)
     &  write(6,"(1x,a3,': execution stopped with',
     &  /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &  /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5)") 
     &  msg,tlim,nlim,cpulim,time,nhy,tused
        ifsen(1) = 1
        return
      endif
c
c  status command
c
      if (msg(1:3) .eq. '?  ') then
        if (myid .eq. 0) 
     &  write(6,"(1x,a3,': execution continuing with',
     &  /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &  /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5)") 
     &  msg,tlim,nlim,cpulim,time,nhy,tused
        return
      endif
c
c  pause command
c
      if (msg(1:3) .eq. 'pau') then
        if (myid .eq. 0)
     &  write(6,"(1x,a3,': execution halted with',
     &  /1x,'tlim=',1pe12.5,'   nlim=',i7,'  cpulim=',1pe12.5,
     &  /1x,'time=',1pe12.5,'  cycle=',i7,'   tused=',1pe12.5,
     &  /1x,'Hit any key to restart execution')") 
     &  msg,tlim,nlim,cpulim,time,nhy,tused
        icheckn = 0

        return
      endif
c
c  abort command
c
      if (msg(1:3) .eq. 'abo') then
        if (myid .eq. 0)
     &  write(6,"(a3,': ABORT! do you really want to abort execution?',
     &   ' (type yes or no)')") msg
        icheckn = 0

c        if (nchar .eq. 0) return

        if (msg(1:3) .ne. 'yes') then
          if (myid .eq. 0)
     &    write(6,"('Abort cancelled, continuing execution ...')")
          return
        else
          if (myid .eq. 0)
     &    write(6,"('ABORT.................')")

          stop
        endif
      endif
c
c  reset physical time limit (tlim) command
c
      if (msg(1:3) .eq. 'tli') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
2000      format(1x,a3,': could not read reset number; execution ',
     &    'continuing')
c          return
c        endif
        tlim = valnew
        if (myid .eq. 0)
     &  write(6,"(a3,': tlim reset to ',1pe12.5)") msg,tlim
        return
      endif
c
c  reset CPU time limit (cpulim) command
c
      if (msg(1:3) .eq. 'cpu') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return
c        endif
        cpulim = valnew
        if (myid .eq. 0)
     &  write(6,"(a3,': cpulim reset to ',1pe12.5)") msg,cpulim
        return
      endif
c
c  reset cycle limit (nlim) command
c
      if (msg(1:3) .eq. 'nli') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return
c        endif
        nlim = nint(valnew)
        if (myid .eq. 0) write(6,"(a3,': nlim reset to ',i12)") msg,nlim
      endif
c
c  turn restart dump switch on
c
      if (msg(1:3) .eq. 'dum') then
        if (myid .eq. 0) write(6,"(a3,': restart dump switch on')") msg
        iswres = 1
        return
      endif
c
c  reset dump frequency (dtdump) command
c
      if (msg(1:3) .eq. 'dtd') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return
c        endif
        dtdump = valnew
        if (myid .eq. 0)
     &  write(6,"(a3,': dtdump reset to ',1pe12.5)") msg,dtdump
      endif
c
c  turn hdf dumps on
c
      if (msg(1:3) .eq. 'hdf') then
        if (myid .eq. 0) write(6,"(a3,': hdf dump switch on')") msg
        iswhdf = 1
        return
      endif
c
c  reset hdf dump frequency (dthdf) command
c
      if (msg(1:3) .eq. 'dtf') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return
c        endif
        dthdf = valnew
        if (myid .eq. 0)
     &  write(6,"(a3,': dthdf reset to ',1pe12.5)") msg,dthdf
      endif
c
c  turn tsl dumps on
c
      if (msg(1:3) .eq. 'tsl') then
        if (myid .eq. 0) write(6,"(a3,': tsl dump switch on')") msg
        iswtsl = 1
        return
      endif
c
c  reset tsl dump frequency (dttsl) command
c
      if (msg(1:3) .eq. 'dtt') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return
c        endif
        dttsl = valnew
        if (myid .eq. 0)
     &  write(6,"(a3,': dttsl reset to ',1pe12.5)") msg,dttsl
      endif
c
c  turn history dumps on
c
      if (msg(1:3) .eq. 'hst') then
        if (myid .eq. 0) write(6,"(a3,': history dump switch on')") msg
        iswhst = 1
        return
      endif
c
c  reset history dump frequency (dth) command
c
      if (msg(1:3) .eq. 'dth') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then  
          if (myid .eq. 0) write(6,2000) msg  
          return 
        endif  
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return 
c        endif  
        dthist = valnew   
        if (myid .eq. 0)
     &  write(6,"(a3,': dthist reset to ',1pe12.5)") msg,dthist
      endif
c
c  turn USER dumps on
c
      if (msg(1:3) .eq. 'usr') then
        if (myid .eq. 0) write(6,"(a3,': USER dump switch on')") msg
        iswusr = 1
        return
      endif
c
c  reset USER dump frequency (dtu) command
c
      if (msg(1:3) .eq. 'dtu') then
        call findno(msg,istrt,iend)
        if (istrt .lt. 0) then
          if (myid .eq. 0) write(6,2000) msg
          return
        endif
c        if (valnew .lt. 0.0 .or. valnew .ge. huge) then
c          if (myid .eq. 0) write(6,2000) msg
c          return
c        endif
        dtusr = valnew   
        if (myid .eq. 0)
     &  write(6,"(a3,': dtusr reset to ',1pe12.5)") msg,dtusr
      endif
c
      return
      end
