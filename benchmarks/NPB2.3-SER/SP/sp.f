!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  2.3         !
!                                                                         !
!                     S E R I A L     V E R S I O N S                     !
!                                                                         !
!                                   S P                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is a serial version of the NPB SP code.               !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 2.3. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 2.3, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/NAS/NPB/                              !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!    Send bug reports to              npb-bugs@nas.nasa.gov               !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (415) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!


c---------------------------------------------------------------------
c
c Author: R. Van der Wijngaart
c         W. Saphir
c---------------------------------------------------------------------

c---------------------------------------------------------------------
       program SP
c---------------------------------------------------------------------

       include  'header.h'
      
       integer          i, niter, step, fstatus
       external         timer_read
       double precision mflops, t, tmax, timer_read
       logical          verified
       character        class

c---------------------------------------------------------------------
c      Read input file (if it exists), else take
c      defaults from parameters
c---------------------------------------------------------------------
          
       write(*, 1000)
       open (unit=2,file='inputsp.data',status='old', iostat=fstatus)

       if (fstatus .eq. 0) then
         write(*,233) 
 233     format(' Reading from input file inputsp.data')
         read (2,*) niter
         read (2,*) dt
         read (2,*) grid_points(1), grid_points(2), grid_points(3)
         close(2)
       else
         write(*,234) 
         niter = niter_default
         dt    = dt_default
         grid_points(1) = problem_size
         grid_points(2) = problem_size
         grid_points(3) = problem_size
       endif
 234   format(' No input file inputsp.data. Using compiled defaults')

       write(*, 1001) grid_points(1), grid_points(2), grid_points(3)
       write(*, 1002) niter, dt

 1000 format(//,' NAS Parallel Benchmarks 2.3-serial version',
     >          ' - SP Benchmark', /)
 1001     format(' Size: ', i3, 'x', i3, 'x', i3)
 1002     format(' Iterations: ', i3, '    dt: ', F10.6)

       if ( (grid_points(1) .gt. IMAX) .or.
     >      (grid_points(2) .gt. JMAX) .or.
     >      (grid_points(3) .gt. KMAX) ) then
             print *, (grid_points(i),i=1,3)
             print *,' Problem size too big for compiled array sizes'
             goto 999
       endif

       call set_constants

       call initialize

       call lhsinit

       call exact_rhs

c---------------------------------------------------------------------
c      do one time step to touch all code, and reinitialize
c---------------------------------------------------------------------
       call adi
       call initialize

       call timer_clear(1)
       call timer_start(1)

       do  step = 1, niter

          if (mod(step, 20) .eq. 0 .or. step .eq. 1) then
             write(*, 200) step
 200         format(' Time step ', i4)
          endif

          call adi

       end do

       call timer_stop(1)
       tmax = timer_read(1)
       
       call verify(niter, class, verified)

       if( tmax .ne. 0. ) then
          mflops = (881.174*float( problem_size )**3
     >             -4683.91*float( problem_size )**2
     >             +11484.5*float( problem_size )
     >             -19272.4) * float( niter ) / (tmax*1000000.0d0)
       else
          mflops = 0.0
       endif

      call print_results('SP', class, grid_points(1), 
     >     grid_points(2), grid_points(3), niter, 
     >     tmax, mflops, '          floating point', 
     >     verified, npbversion,compiletime, cs1, cs2, cs3, cs4, cs5, 
     >     cs6, '(none)')

 999   continue

       end
