c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine calcnp(l, jstp, jendp)

c---------------------------------------------------------------------
c   Calculate j-index limit on a given 'hyper-plane' (l = j + k)
c   for the use with lower- and upper-triangular solvers.
c---------------------------------------------------------------------

      implicit none
      integer l, jstp, jendp

      include 'applu.incl'

      jstp = l - nz + 1
      if (jstp.lt.jst) jstp = jst
      jendp = l - 2
      if (jendp.gt.jend) jendp = jend

      return
      end

