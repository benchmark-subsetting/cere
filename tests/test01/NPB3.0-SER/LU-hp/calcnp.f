c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine calcnp(lst, lend, np, indxp, jndxp)

c---------------------------------------------------------------------
c   Calculate available indices on each 'hyper-plane'
c   for the use with lower- and upper-triangular solvers.
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

      integer lst, lend, np(*)
      integer indxp(isiz1*isiz2*3/4,*),
     >        jndxp(isiz1*isiz2*3/4,*)
      integer i, j, k, l, n

      do l = lst, lend
      	 n = 0
      	 do j = max(2, l-nx-nz+2), min(ny-1, l-4)
      	    do i = max(2, l-j-nz+1), min(nx-1, l-j-2)
	       k = l - i - j
	       if (k.ge.2 .and. k.le.nz-1) then
	          n = n + 1
	          indxp(n,l) = i
	          jndxp(n,l) = j
	       endif
	    end do
      	 end do
      	 np(l) = n
      end do

      return
      end

