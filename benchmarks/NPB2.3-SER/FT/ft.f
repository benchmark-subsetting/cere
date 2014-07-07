!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  2.3         !
!                                                                         !
!                     S E R I A L     V E R S I O N S                     !
!                                                                         !
!                                   F T                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is a serial version of the NPB FT code.               !
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
c Authors: D. Bailey
c          W. Saphir
c
c---------------------------------------------------------------------

c---------------------------------------------------------------------

c---------------------------------------------------------------------
c FT benchmark
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      program ft

c---------------------------------------------------------------------
c---------------------------------------------------------------------


      implicit none

      include 'global.h'
      integer i, ierr
      
c---------------------------------------------------------------------
c u0, u1, u2 are the main arrays in the problem. 
c Depending on the decomposition, these arrays will have different 
c dimensions. To accomodate all possibilities, we allocate them as 
c one-dimensional arrays and pass them to subroutines for different 
c views
c  - u0 contains the initial (transformed) initial condition
c  - u1 and u2 are working arrays
c  - indexmap maps i,j,k of u0 to the correct i^2+j^2+k^2 for the
c    time evolution operator. 
c---------------------------------------------------------------------

      double complex u0(ntotal), 
     >               u1(ntotal), 
     >               u2(ntotal)
      integer indexmap(ntotal)
c---------------------------------------------------------------------
c Large arrays are in common so that they are allocated on the
c heap rather than the stack. This common block is not
c referenced directly anywhere else. Padding is to avoid accidental 
c cache problems, since all array sizes are powers of two.
c---------------------------------------------------------------------

      double complex pad1(3), pad2(3), pad3(3)
      common /bigarrays/ u0, pad1, u1, pad2, u2, pad3, indexmap

      integer iter
      double precision total_time, mflops
      logical verified
      character class

c---------------------------------------------------------------------
c Run the entire problem once to make sure all data is touched. 
c This reduces variable startup costs, which is important for such a 
c short benchmark. The other NPB 2 implementations are similar. 
c---------------------------------------------------------------------
      do i = 1, t_max
         call timer_clear(i)
      end do
      call setup()
      call compute_indexmap(indexmap, dims(1,3))
      call compute_initial_conditions(u1, dims(1,1))
      call fft_init (dims(1,1))
      call fft(1, u1, u0)

c---------------------------------------------------------------------
c Start over from the beginning. Note that all operations must
c be timed, in contrast to other benchmarks. 
c---------------------------------------------------------------------
      do i = 1, t_max
         call timer_clear(i)
      end do

      call timer_start(T_total)
      if (timers_enabled) call timer_start(T_setup)

      call compute_indexmap(indexmap, dims(1,3))

      call compute_initial_conditions(u1, dims(1,1))

      call fft_init (dims(1,1))

      if (timers_enabled) call timer_stop(T_setup)
      if (timers_enabled) call timer_start(T_fft)
      call fft(1, u1, u0)
      if (timers_enabled) call timer_stop(T_fft)

      do iter = 1, niter
         if (timers_enabled) call timer_start(T_evolve)
         call evolve(u0, u1, iter, indexmap, dims(1, 1))
         if (timers_enabled) call timer_stop(T_evolve)
         if (timers_enabled) call timer_start(T_fft)
         call fft(-1, u1, u2)
         if (timers_enabled) call timer_stop(T_fft)
         if (timers_enabled) call timer_start(T_checksum)
         call checksum(iter, u2, dims(1,1))
         if (timers_enabled) call timer_stop(T_checksum)
      end do

      call verify(nx, ny, nz, niter, verified, class)

      call timer_stop(t_total)
      total_time = timer_read(t_total)

      if( total_time .ne. 0. ) then
         mflops = 1.0d-6*float(ntotal) *
     >             (14.8157+7.19641*log(float(ntotal))
     >          +  (5.23518+7.21113*log(float(ntotal)))*niter)
     >                 /total_time
      else
         mflops = 0.0
      endif
      call print_results('FT', class, nx, ny, nz, niter,
     >  total_time, mflops, '          floating point', verified, 
     >  npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6, cs7)
      if (timers_enabled) call print_timers()

      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine evolve(u0, u1, t, indexmap, d)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c evolve u0 -> u1 (t time steps) in fourier space
c---------------------------------------------------------------------

      implicit none
      include 'global.h'
      integer d(3)
      double complex u0(d(1),d(2),d(3))
      double complex u1(d(1),d(2),d(3))
      integer  indexmap(d(1),d(2),d(3))
      integer t
      integer i, j, k

      do k = 1, d(3)
         do j = 1, d(2)
            do i = 1, d(1)
               u1(i,j,k) = u0(i,j,k)*ex(t*indexmap(i,j,k))
            end do
         end do
      end do

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine compute_initial_conditions(u0, d)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c Fill in array u0 with initial conditions from 
c random number generator 
c---------------------------------------------------------------------
      implicit none
      include 'global.h'
      integer d(3)
      double complex u0(d(1), d(2), d(3))
      integer k
      double precision x0, start, an, dummy
      

      start = seed                                    
c---------------------------------------------------------------------
c Jump to the starting element for our first plane.
c---------------------------------------------------------------------
      call ipow46(a, (zstart(1)-1)*2*nx*ny + (ystart(1)-1)*2*nx, an)
      dummy = randlc(start, an)
      call ipow46(a, 2*nx*ny, an)
      
c---------------------------------------------------------------------
c Go through by z planes filling in one square at a time.
c---------------------------------------------------------------------
      do k = 1, dims(3, 1) 
         x0 = start
         call vranlc(2*nx*dims(2, 1), x0, a, u0(1, 1, k))
         if (k .ne. dims(3, 1)) dummy = randlc(start, an)
      end do

      return
      end

	            
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine ipow46(a, exponent, result)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c compute a^exponent mod 2^46
c---------------------------------------------------------------------

      implicit none
      double precision a, result, dummy, q, r
      integer exponent, n, n2
      external randlc
      double precision randlc
c---------------------------------------------------------------------
c Use
c   a^n = a^(n/2)*a^(n/2) if n even else
c   a^n = a*a^(n-1)       if n odd
c---------------------------------------------------------------------
      result = 1
      if (exponent .eq. 0) return
      q = a
      r = 1
      n = exponent


      do while (n .gt. 1)
         n2 = n/2
         if (n2 * 2 .eq. n) then
            dummy = randlc(q, q) 
            n = n2
         else
            dummy = randlc(r, q)
            n = n-1
         endif
      end do
      dummy = randlc(r, q)
      result = r
      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine setup

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none
      include 'global.h'

      integer ierr, i, j, fstatus
      debug = .FALSE.
      
      write(*, 1000)

      niter = niter_default

      write(*, 1001) nx, ny, nz
      write(*, 1002) niter


 1000 format(//,' NAS Parallel Benchmarks 2.3-serial version',
     >          ' - FT Benchmark', /)
 1001 format(' Size                : ', i3, 'x', i3, 'x', i3)
 1002 format(' Iterations          :     ', i7)
 1004 format(' Number of processes :     ', i7)
 1005 format(' Processor array     :     ', i3, 'x', i3)
 1006 format(' WARNING: compiled for ', i5, ' processes. ',
     >       ' Will not verify. ')

      do i = 1, 3
         dims(1, i) = nx
         dims(2, i) = ny
         dims(3, i) = nz
      end do


      do i = 1, 3
         xstart(i) = 1
         xend(i)   = nx
         ystart(i) = 1
         yend(i)   = ny
         zstart(i) = 1
         zend(i)   = nz
      end do

c---------------------------------------------------------------------
c Set up info for blocking of ffts and transposes.  This improves
c performance on cache-based systems. Blocking involves
c working on a chunk of the problem at a time, taking chunks
c along the first, second, or third dimension. 
c
c - In cffts1 blocking is on 2nd dimension (with fft on 1st dim)
c - In cffts2/3 blocking is on 1st dimension (with fft on 2nd and 3rd dims)

c Since 1st dim is always in processor, we'll assume it's long enough 
c (default blocking factor is 16 so min size for 1st dim is 16)
c The only case we have to worry about is cffts1 in a 2d decomposition. 
c so the blocking factor should not be larger than the 2nd dimension. 
c---------------------------------------------------------------------

      fftblock = fftblock_default
      fftblockpad = fftblockpad_default

      if (fftblock .ne. fftblock_default) fftblockpad = fftblock+3

      return
      end

      
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine compute_indexmap(indexmap, d)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c compute function from local (i,j,k) to ibar^2+jbar^2+kbar^2 
c for time evolution exponent. 
c---------------------------------------------------------------------

      implicit none
      include 'global.h'
      integer d(3)
      integer indexmap(d(1), d(2), d(3))
      integer i, j, k, ii, ii2, jj, ij2, kk
      double precision ap

c---------------------------------------------------------------------
c basically we want to convert the fortran indices 
c   1 2 3 4 5 6 7 8 
c to 
c   0 1 2 3 -4 -3 -2 -1
c The following magic formula does the trick:
c mod(i-1+n/2, n) - n/2
c---------------------------------------------------------------------

      do i = 1, dims(1,3)
         ii =  mod(i+xstart(3)-2+nx/2, nx) - nx/2
         ii2 = ii*ii
         do j = 1, dims(2,3)
            jj = mod(j+ystart(3)-2+ny/2, ny) - ny/2
            ij2 = jj*jj+ii2
            do k = 1, dims(3,3)
               kk = mod(k+zstart(3)-2+nz/2, nz) - nz/2
               indexmap(i, j, k) = kk*kk+ij2
            end do
         end do
      end do

c---------------------------------------------------------------------
c compute array of exponentials for time evolution. 
c---------------------------------------------------------------------
      ap = - 4.d0 * alpha * pi *pi

      ex(0) = 1.0d0
      ex(1) = exp(ap)
      do i = 2, expmax
         ex(i) = ex(i-1)*ex(1)
      end do

      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine print_timers()

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none
      integer i
      include 'global.h'
      character*25 tstrings(T_max)
      data tstrings / '          total ', 
     >                '          setup ', 
     >                '            fft ', 
     >                '         evolve ', 
     >                '       checksum ', 
     >                '         fftlow ', 
     >                '        fftcopy ' /

      do i = 1, t_max
         if (timer_read(i) .ne. 0.0d0) then
            write(*, 100) i, tstrings(i), timer_read(i)
         endif
      end do
 100  format(' timer ', i2, '(', A16,  ') :', F10.6)
      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine fft(dir, x1, x2)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none
      include 'global.h'
      integer dir
      double complex x1(ntotal), x2(ntotal)

      double complex scratch(fftblockpad_default*maxdim*2)

c---------------------------------------------------------------------
c note: args x1, x2 must be different arrays
c note: args for cfftsx are (direction, layout, xin, xout, scratch)
c       xin/xout may be the same and it can be somewhat faster
c       if they are
c---------------------------------------------------------------------

      if (dir .eq. 1) then
         call cffts1(1, dims(1,1), x1, x1, scratch)
         call cffts2(1, dims(1,2), x1, x1, scratch)
         call cffts3(1, dims(1,3), x1, x2, scratch)
      else
         call cffts3(-1, dims(1,3), x1, x1, scratch)
         call cffts2(-1, dims(1,2), x1, x1, scratch)
         call cffts1(-1, dims(1,1), x1, x2, scratch)
      endif
      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine cffts1(is, d, x, xout, y)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'global.h'
      integer is, d(3), logd(3)
      double complex x(d(1),d(2),d(3))
      double complex xout(d(1),d(2),d(3))
      double complex y(fftblockpad, d(1), 2) 
      integer i, j, k, jj

      do i = 1, 3
         logd(i) = ilog2(d(i))
      end do
      do k = 1, d(3)
         do jj = 0, d(2) - fftblock, fftblock
            if (timers_enabled) call timer_start(T_fftcopy)
            do j = 1, fftblock
               do i = 1, d(1)
                  y(j,i,1) = x(i,j+jj,k)
               enddo
            enddo
            if (timers_enabled) call timer_stop(T_fftcopy)
            
            if (timers_enabled) call timer_start(T_fftlow)
            call cfftz (is, logd(1), 
     >                  d(1), y, y(1,1,2))

            if (timers_enabled) call timer_stop(T_fftlow)

            if (timers_enabled) call timer_start(T_fftcopy)
            do j = 1, fftblock
               do i = 1, d(1)
                  xout(i,j+jj,k) = y(j,i,1)
               enddo
            enddo
            if (timers_enabled) call timer_stop(T_fftcopy)
         enddo
      enddo

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine cffts2(is, d, x, xout, y)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'global.h'
      integer is, d(3), logd(3)
      double complex x(d(1),d(2),d(3))
      double complex xout(d(1),d(2),d(3))
      double complex y(fftblockpad, d(2), 2) 
      integer i, j, k, ii

      do i = 1, 3
         logd(i) = ilog2(d(i))
      end do
      do k = 1, d(3)
        do ii = 0, d(1) - fftblock, fftblock
           if (timers_enabled) call timer_start(T_fftcopy)
           do j = 1, d(2)
              do i = 1, fftblock
                 y(i,j,1) = x(i+ii,j,k)
              enddo
           enddo
           if (timers_enabled) call timer_stop(T_fftcopy)

           if (timers_enabled) call timer_start(T_fftlow)
           call cfftz (is, logd(2), 
     >          d(2), y, y(1, 1, 2))
           
           if (timers_enabled) call timer_stop(T_fftlow)
           if (timers_enabled) call timer_start(T_fftcopy)
           do j = 1, d(2)
              do i = 1, fftblock
                 xout(i+ii,j,k) = y(i,j,1)
              enddo
           enddo
           if (timers_enabled) call timer_stop(T_fftcopy)
        enddo
      enddo

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine cffts3(is, d, x, xout, y)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'global.h'
      integer is, d(3), logd(3)
      double complex x(d(1),d(2),d(3))
      double complex xout(d(1),d(2),d(3))
      double complex y(fftblockpad, d(3), 2) 
      integer i, j, k, ii

      do i = 1, 3
         logd(i) = ilog2(d(i))
      end do
      do j = 1, d(2)
        do ii = 0, d(1) - fftblock, fftblock
           if (timers_enabled) call timer_start(T_fftcopy)
           do k = 1, d(3)
              do i = 1, fftblock
                 y(i,k,1) = x(i+ii,j,k)
              enddo
           enddo
           if (timers_enabled) call timer_stop(T_fftcopy)

           if (timers_enabled) call timer_start(T_fftlow)
           call cfftz (is, logd(3), 
     >          d(3), y, y(1, 1, 2))
           if (timers_enabled) call timer_stop(T_fftlow)
           if (timers_enabled) call timer_start(T_fftcopy)
           do k = 1, d(3)
              do i = 1, fftblock
                 xout(i+ii,j,k) = y(i,k,1)
              enddo
           enddo
           if (timers_enabled) call timer_stop(T_fftcopy)
        enddo
      enddo

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine fft_init (n)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c compute the roots-of-unity array that will be used for subsequent FFTs. 
c---------------------------------------------------------------------

      implicit none
      include 'global.h'

      integer m,n,nu,ku,i,j,ln
      double precision t, ti


c---------------------------------------------------------------------
c   Initialize the U array with sines and cosines in a manner that permits
c   stride one access at each FFT iteration.
c---------------------------------------------------------------------
      nu = n
      m = ilog2(n)
      u(1) = m
      ku = 2
      ln = 1

      do j = 1, m
         t = pi / ln
         
         do i = 0, ln - 1
            ti = i * t
            u(i+ku) = dcmplx (cos (ti), sin(ti))
         enddo
         
         ku = ku + ln
         ln = 2 * ln
      enddo
      
      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine cfftz (is, m, n, x, y)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   Computes NY N-point complex-to-complex FFTs of X using an algorithm due
c   to Swarztrauber.  X is both the input and the output array, while Y is a 
c   scratch array.  It is assumed that N = 2^M.  Before calling CFFTZ to 
c   perform FFTs, the array U must be initialized by calling CFFTZ with IS 
c   set to 0 and M set to MX, where MX is the maximum value of M for any 
c   subsequent call.
c---------------------------------------------------------------------

      implicit none
      include 'global.h'

      integer is,m,n,i,j,l,mx
      double complex x, y

      dimension x(fftblockpad,n), y(fftblockpad,n)

c---------------------------------------------------------------------
c   Check if input parameters are invalid.
c---------------------------------------------------------------------
      mx = u(1)
      if ((is .ne. 1 .and. is .ne. -1) .or. m .lt. 1 .or. m .gt. mx)    
     >  then
        write (*, 1)  is, m, mx
 1      format ('CFFTZ: Either U has not been initialized, or else'/    
     >    'one of the input parameters is invalid', 3I5)
        stop
      endif

c---------------------------------------------------------------------
c   Perform one variant of the Stockham FFT.
c---------------------------------------------------------------------
      do l = 1, m, 2
        call fftz2 (is, l, m, n, fftblock, fftblockpad, u, x, y)
        if (l .eq. m) goto 160
        call fftz2 (is, l + 1, m, n, fftblock, fftblockpad, u, y, x)
      enddo

      goto 180

c---------------------------------------------------------------------
c   Copy Y to X.
c---------------------------------------------------------------------
 160  do j = 1, n
        do i = 1, fftblock
          x(i,j) = y(i,j)
        enddo
      enddo

 180  continue

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine fftz2 (is, l, m, n, ny, ny1, u, x, y)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   Performs the L-th iteration of the second variant of the Stockham FFT.
c---------------------------------------------------------------------

      implicit none

      integer is,k,l,m,n,ny,ny1,n1,li,lj,lk,ku,i,j,i11,i12,i21,i22
      double complex u,x,y,u1,x11,x21
      dimension u(n), x(ny1,n), y(ny1,n)


c---------------------------------------------------------------------
c   Set initial parameters.
c---------------------------------------------------------------------

      n1 = n / 2
      lk = 2 ** (l - 1)
      li = 2 ** (m - l)
      lj = 2 * lk
      ku = li + 1

      do i = 0, li - 1
        i11 = i * lk + 1
        i12 = i11 + n1
        i21 = i * lj + 1
        i22 = i21 + lk
        if (is .ge. 1) then
          u1 = u(ku+i)
        else
          u1 = dconjg (u(ku+i))
        endif

c---------------------------------------------------------------------
c   This loop is vectorizable.
c---------------------------------------------------------------------
        do k = 0, lk - 1
          do j = 1, ny
            x11 = x(j,i11+k)
            x21 = x(j,i12+k)
            y(j,i21+k) = x11 + x21
            y(j,i22+k) = u1 * (x11 - x21)
          enddo
        enddo
      enddo

      return
      end

c---------------------------------------------------------------------


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      integer function ilog2(n)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none
      integer n, nn, lg
      if (n .eq. 1) then
         ilog2=0
         return
      endif
      lg = 1
      nn = 2
      do while (nn .lt. n)
         nn = nn*2
         lg = lg+1
      end do
      ilog2 = lg
      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine checksum(i, u1, d)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none
      include 'global.h'
      integer i, d(3)
      double complex u1(d(1),d(2),d(3))
      integer j, q,r,s, ierr
      double complex chk,allchk
      chk = (0.0,0.0)

      do j=1,1024
         q = mod(j, nx)+1
         if (q .ge. xstart(1) .and. q .le. xend(1)) then
            r = mod(3*j,ny)+1
            if (r .ge. ystart(1) .and. r .le. yend(1)) then
               s = mod(5*j,nz)+1
               if (s .ge. zstart(1) .and. s .le. zend(1)) then
                  chk=chk+u1(q-xstart(1)+1,r-ystart(1)+1,s-zstart(1)+1)
               end if
            end if
         end if
      end do

      chk = chk/dble(ntotal)
      
      write (*, 30) i, chk
 30   format (' T =',I5,5X,'Checksum =',1P2D22.12)
      sums(i) = chk
      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine verify (d1, d2, d3, nt, verified, class)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none
      include 'global.h'
      integer d1, d2, d3, nt
      character class
      logical verified
      integer ierr, size, i
      double precision err, epsilon

c---------------------------------------------------------------------
c   Sample size reference checksums
c---------------------------------------------------------------------
      double precision vdata_real_s(6)
      double precision vdata_imag_s(6)
      double precision vdata_real_w(6)
      double precision vdata_imag_w(6)
      double precision vdata_real_a(6)
      double precision vdata_imag_a(6)
      double precision vdata_real_b(20)
      double precision vdata_imag_b(20)
      double precision vdata_real_c(20)
      double precision vdata_imag_c(20)
      data vdata_real_s / 5.546087004964D+02,
     >                5.546385409189D+02,
     >                5.546148406171D+02,
     >                5.545423607415D+02,
     >                5.544255039624D+02,
     >                5.542683411902D+02 /

      data vdata_imag_s / 4.845363331978D+02,
     >                4.865304269511D+02,
     >                4.883910722336D+02,
     >                4.901273169046D+02,
     >                4.917475857993D+02,
     >                4.932597244941D+02 /
c---------------------------------------------------------------------
c   Class W size reference checksums
c---------------------------------------------------------------------
       data vdata_real_w /
     >                5.673612178944D+02,
     >                5.631436885271D+02,
     >                5.594024089970D+02,
     >                5.560698047020D+02,
     >                5.530898991250D+02,
     >                5.504159734538D+02/
       data vdata_imag_w /
     >               5.293246849175D+02,
     >               5.282149986629D+02,
     >               5.270996558037D+02, 
     >               5.260027904925D+02, 
     >               5.249400845633D+02,
     >               5.239212247086D+02/

c---------------------------------------------------------------------
c   Class A size reference checksums
c---------------------------------------------------------------------
      data vdata_real_a / 5.046735008193D+02,
     >                5.059412319734D+02,
     >                5.069376896287D+02,
     >                5.077892868474D+02,
     >                5.085233095391D+02,
     >                5.091487099959D+02 /
      
      data vdata_imag_a / 5.114047905510D+02,
     >                5.098809666433D+02,
     >                5.098144042213D+02,
     >                5.101336130759D+02,
     >                5.104914655194D+02,
     >                5.107917842803D+02 /
      
c---------------------------------------------------------------------
c   Class B size reference checksums
c---------------------------------------------------------------------
      data vdata_real_b / 5.177643571579D+02,
     >                5.154521291263D+02,
     >                5.146409228649D+02,
     >                5.142378756213D+02,
     >                5.139626667737D+02,
     >                5.137423460082D+02,
     >                5.135547056878D+02,
     >                5.133910925466D+02,
     >                5.132470705390D+02,
     >                5.131197729984D+02,
     >                5.130070319283D+02,
     >                5.129070537032D+02,
     >                5.128182883502D+02,
     >                5.127393733383D+02,
     >                5.126691062020D+02,
     >                5.126064276004D+02,
     >                5.125504076570D+02,
     >                5.125002331720D+02,
     >                5.124551951846D+02,
     >                5.124146770029D+02 /
   
      data vdata_imag_b / 5.077803458597D+02,
     >                5.088249431599D+02,                  
     >                5.096208912659D+02,                     
     >                5.101023387619D+02,                  
     >                5.103976610617D+02,                  
     >                5.105948019802D+02,                  
     >                5.107404165783D+02,                  
     >                5.108576573661D+02,                  
     >                5.109577278523D+02,                  
     >                5.110460304483D+02,                  
     >                5.111252433800D+02,                  
     >                5.111968077718D+02,                  
     >                5.112616233064D+02,                  
     >                5.113203605551D+02,                  
     >                5.113735928093D+02,                  
     >                5.114218460548D+02,
     >                5.114656139760D+02,
     >                5.115053595966D+02,
     >                5.115415130407D+02,
     >                5.115744692211D+02 /

c---------------------------------------------------------------------
c   Class C size reference checksums
c---------------------------------------------------------------------
      data vdata_real_c / 5.195078707457D+02,
     >                5.155422171134D+02,
     >                5.144678022222D+02,
     >                5.140150594328D+02,
     >                5.137550426810D+02,
     >                5.135811056728D+02,
     >                5.134569343165D+02,
     >                5.133651975661D+02,
     >                5.132955192805D+02,
     >                5.132410471738D+02,
     >                5.131971141679D+02,
     >                5.131605205716D+02,
     >                5.131290734194D+02,
     >                5.131012720314D+02,
     >                5.130760908195D+02,
     >                5.130528295923D+02,
     >                5.130310107773D+02,
     >                5.130103090133D+02,
     >                5.129905029333D+02,
     >                5.129714421109D+02 /

      data vdata_imag_c / 5.149019699238D+02,
     >                5.127578201997D+02,
     >                5.122251847514D+02,
     >                5.121090289018D+02,
     >                5.121143685824D+02,
     >                5.121496764568D+02,
     >                5.121870921893D+02,
     >                5.122193250322D+02,
     >                5.122454735794D+02,
     >                5.122663649603D+02,
     >                5.122830879827D+02,
     >                5.122965869718D+02,
     >                5.123075927445D+02,
     >                5.123166486553D+02,
     >                5.123241541685D+02,
     >                5.123304037599D+02,
     >                5.123356167976D+02,
     >                5.123399592211D+02,
     >                5.123435588985D+02,
     >                5.123465164008D+02 /



      epsilon = 1.0d-12

      verified = .FALSE.
      class = 'U'

      if (d1 .eq. 64 .and.
     >    d2 .eq. 64 .and.
     >    d3 .eq. 64 .and.
     >    nt .eq. 6) then
         class = 'S'
         do i = 1, nt
            err = (dble(sums(i)) - vdata_real_s(i)) / vdata_real_s(i)
            if (abs(err) .gt. epsilon) goto 100
            err = (dimag(sums(i)) - vdata_imag_s(i)) / vdata_imag_s(i)
c If you have a machine where the above does not compile, let 
c us know and use the following
c            err = (aimag(sums(i)) - vdata_imag_s(i)) / vdata_imag_s(i)
            if (abs(err) .gt. epsilon) goto 100
         end do
         verified = .TRUE.
 100     continue
      else if (d1 .eq. 128 .and.
     >    d2 .eq. 128 .and.
     >    d3 .eq. 32 .and.
     >    nt .eq. 6) then
         class = 'W'
         do i = 1, nt
            err = (dble(sums(i)) - vdata_real_w(i)) / vdata_real_w(i)
            if (abs(err) .gt. epsilon) goto 105
            err = (dimag(sums(i)) - vdata_imag_w(i)) / vdata_imag_w(i)
c If you have a machine where the above does not compile, let 
c us know and use the following
c            err = (aimag(sums(i)) - vdata_imag_w(i)) / vdata_imag_w(i)
            if (abs(err) .gt. epsilon) goto 105
         end do
         verified = .TRUE.
 105     continue
      else if (d1 .eq. 256 .and.
     >    d2 .eq. 256 .and.
     >    d3 .eq. 128 .and.
     >    nt .eq. 6) then
         class = 'A'
         do i = 1, nt
            err = (dble(sums(i)) - vdata_real_a(i)) / vdata_real_a(i)
            if (abs(err) .gt. epsilon) goto 110
            err = (dimag(sums(i)) - vdata_imag_a(i)) / vdata_imag_a(i)
c If you have a machine where the above does not compile, let 
c us know and use the following
c            err = (aimag(sums(i)) - vdata_imag_a(i)) / vdata_imag_a(i)
            if (abs(err) .gt. epsilon) goto 110
         end do
         verified = .TRUE.
 110     continue
      else if (d1 .eq. 512 .and.
     >    d2 .eq. 256 .and.
     >    d3 .eq. 256 .and.
     >    nt .eq. 20) then
         class = 'B'
         do i = 1, nt
            err = (dble(sums(i)) - vdata_real_b(i)) / vdata_real_b(i)
            if (abs(err) .gt. epsilon) goto 120
            err = (dimag(sums(i)) - vdata_imag_b(i)) / vdata_imag_b(i)
c If you have a machine where the above does not compile, let 
c us know and use the following
c            err = (aimag(sums(i)) - vdata_imag_b(i)) / vdata_imag_b(i)
            if (abs(err) .gt. epsilon) goto 120
         end do
         verified = .TRUE.
 120     continue
      else if (d1 .eq. 512 .and.
     >    d2 .eq. 512 .and.
     >    d3 .eq. 512 .and.
     >    nt .eq. 20) then
         class = 'C'
         do i = 1, nt 
            err = (dble(sums(i)) - vdata_real_c(i)) / vdata_real_c(i)
            if (abs(err) .gt. epsilon) goto 130
            err = (dimag(sums(i)) - vdata_imag_c(i)) / vdata_imag_c(i)
c            err = (aimag(sums(i)) - vdata_imag_c(i)) / vdata_imag_c(i)
            if (abs(err) .gt. epsilon) goto 130
         end do
         verified = .TRUE.
 130     continue
      endif
         
      if (class .ne. 'U') then
         if (verified) then
            write(*,2000)
 2000       format(' Result verification successful')
         else
            write(*,2001)
 2001       format(' Result verification failed')
         endif
      endif
      print *, 'class = ', class

      return
      end


