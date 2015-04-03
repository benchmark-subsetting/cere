CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0100
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Lennard-Jones
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0100(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     rinvsix
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinvsq11         = 1.0/(rsq11)
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          fs11             = (twelve*vnb12-six*vnb6)*rinvsq11
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0110
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Lennard-Jones
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0110(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     rinvsix
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinvsq11         = 1.0/(rsq11)
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            fs11             = (twelve*vnb12-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ii               = ii + nscoul-nsvdwc
        ii3              = 3*ii-2
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinvsq11         = 1.0/(rsq11)
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            fs11             = (twelve*vnb12-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0200
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0200(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     rinvsix
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          fs11             = (br*vnbexp-six*vnb6)*rinvsq11
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0210
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0210(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     rinvsix
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            fs11             = (br*vnbexp-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ii               = ii + nscoul-nsvdwc
        ii3              = 3*ii-2
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            fs11             = (br*vnbexp-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0300
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Tabulated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0300(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     c6
      real*4     c12
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          fs11             = -((fijD+fijR)*tabscale)*rinv11
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0301
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Tabulated
C    Solvent opt: No
C    Free energy: Lambda (alpha=0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0301(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  lambda,
     &  dvdlambda,
     &  typeB)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     lambda
      real*4     dvdlambda
      integer*4  typeB(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     c6
      real*4     c12
      real*4     c6a
      real*4     c12a
      real*4     c6b
      real*4     c12b
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     dvdl
      real*4     L1
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     one
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  ntiB
      integer*4  tjA
      integer*4  tjB
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (one  =  1.0000000000000000)
      parameter (two  =  2.0000000000000000)

      dvdl             = nul
      L1               = one - lambda

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ntiA             = 2*ntype*type(ii)
        ntiB             = 2*ntype*typeB(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+2*type(jnr)+1
          tjB              = ntiB+2*typeB(jnr)+1
          c6a              = nbfp(tjA)
          c6b              = nbfp(tjB)
          c6               = L1*c6a + lambda*c6b
          c12a             = nbfp(tjA+1)
          c12b             = nbfp(tjB+1)
          c12              = L1*c12a + lambda*c12b
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          dvdl             = dvdl + (c6b - c6a)*VV
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          dvdl             = dvdl + (c12b - c12a)*VV
          fs11             = -((fijD+fijR)*tabscale)*rinv11
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      dvdlambda        = dvdlambda + dvdl
      return
      end








CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0310
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Tabulated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0310(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     c6
      real*4     c12
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            fs11             = -((fijD+fijR)*tabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ii               = ii + nscoul-nsvdwc
        ii3              = 3*ii-2
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            fs11             = -((fijD+fijR)*tabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0400
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0400(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          fs11             = 
     &       -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0401
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: No
C    Free energy: Lambda (alpha=0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0401(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale,
     &  lambda,
     &  dvdlambda,
     &  typeB)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      real*4     lambda
      real*4     dvdlambda
      integer*4  typeB(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     cexp1a
      real*4     cexp2a
      real*4     cexp1b
      real*4     cexp2b
      real*4     c6
      real*4     c6a
      real*4     c6b
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     dvdl
      real*4     L1
      real*4     vnbexpa
      real*4     vnbexpb
      real*4     fijRa
      real*4     fijRb
      real*4     vnb6
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     one
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  ntiB
      integer*4  tjA
      integer*4  tjB
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (one  =  1.0000000000000000)
      parameter (two  =  2.0000000000000000)

      dvdl             = nul
      L1               = one - lambda

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ntiA             = 3*ntype*type(ii)
        ntiB             = 3*ntype*typeB(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+3*type(jnr)+1
          tjB              = ntiB+3*typeB(jnr)+1
          c6a              = nbfp(tjA)
          c6b              = nbfp(tjB)
          c6               = L1*c6a + lambda*c6b
          cexp1a           = nbfp(tjA+1)
          cexp1b           = nbfp(tjB+1)
          cexp2a           = nbfp(tjA+2)
          cexp2b           = nbfp(tjB+2)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          dvdl             = dvdl + (c6b - c6a)*VV
          rt               = cexp2a*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexpa          = cexp1a*VV
          fijRa            = cexp1a*cexp2a*FF
          rt               = cexp2b*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexpb          = cexp1b*VV
          fijRb            = cexp1b*cexp2b*FF
          fijR             = L1*fijRa + lambda*fijRb
          vnbtot           = vnbtot + vnb6 + L1*vnbexpa + lambda*vnbexpb
          dvdl             = dvdl + vnbexpb - vnbexpa
          fs11             = 
     &       -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      dvdlambda        = dvdlambda + dvdl
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl0410
C    Forces:      Calculated
C    Coulomb:     Not calculated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl0410(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            fs11             = 
     &         -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ii               = ii + nscoul-nsvdwc
        ii3              = 3*ii-2
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            fs11             = 
     &         -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1000
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Not calculated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1000(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qq
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          rinvsq11         = rinv11*rinv11
          qq               = iqA*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = (vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1010
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Not calculated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1010(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     qq
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1020
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Not calculated
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1020(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinvsq11         = rinv11*rinv11
          qq               = qO*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = (vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1030
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Not calculated
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1030(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          rinvsq11         = rinv11*rinv11
          vcoul            = qqOO*rinv11
          fs11             = (vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          vcoul            = qqOH*rinv12
          fs12             = (vcoul)*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          vcoul            = qqOH*rinv13
          fs13             = (vcoul)*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          vcoul            = qqOH*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          vcoul            = qqHH*rinv22
          fs22             = (vcoul)*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          vcoul            = qqHH*rinv23
          fs23             = (vcoul)*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          vcoul            = qqOH*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          vcoul            = qqHH*rinv32
          fs32             = (vcoul)*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          vcoul            = qqHH*rinv33
          fs33             = (vcoul)*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1100
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Lennard-Jones
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1100(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          qq               = iqA*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = (twelve*vnb12-six*vnb6+vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1110
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Lennard-Jones
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1110(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (twelve*vnb12-six*vnb6+vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinvsq11         = 1.0/(rsq11)
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            fs11             = (twelve*vnb12-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1120
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Lennard-Jones
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1120(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 2*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          qq               = qO*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = (twelve*vnb12-six*vnb6+vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1130
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Lennard-Jones
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1130(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     rinvsix
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 2*ntype*type(ii)
      tjA              = ntiA+2*type(ii)+1
      c6               = nbfp(tjA)
      c12              = nbfp(tjA+1)


      do n=1,nri
        write (*,*), "n", n, "nri", nri, "shift(n)", shift(n)
        is3              = 3*shift(n)+1

        write (*,*), "is3", is3
        write (*,*), "shiftvec(is3)", shiftvec(is3)
        write (*,*), "shX", shX
        shX              = shiftvec(is3)
        write (*,*), "shX", shX
        write (*,*), "shiftvec(is3+1)", shiftvec(is3+1)
        write (*,*), "shY", shY
        shY              = shiftvec(is3+1)
        write (*,*), "shY", shY
        write (*,*), "shiftvec(is3+2)", shiftvec(is3+2)
        write (*,*), "shZ", shZ
        shZ              = shiftvec(is3+2)
        write (*,*), "shZ", shZ
        write (*,*), "iinr(n)", iinr(n)
        write (*,*), "ii", ii
        ii               = iinr(n)+1
        write (*,*), "ii", ii
        ii3              = 3*ii-2
        write (*,*), "ii3", ii3
        vctot            = nul
        vnbtot           = nul
        write (*,*), "jindex(n)", jindex(n)
        nj0              = jindex(n)+1
        write (*,*), "jindex(n+1)", jindex(n+1)
        nj1              = jindex(n+1)
        write (*,*), "ii3", ii3
        write (*,*), "pos(ii3)", pos(ii3)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        write (*,*), "pos(ii3)", pos(ii3+8)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        write (*,*), "start inner looop, nj0", nj0, "nj1", nj1 
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          vnb6             = c6*rinvsix
          vnb12            = c12*rinvsix*rinvsix
          vnbtot           = vnbtot + vnb12-vnb6
          vcoul            = qqOO*rinv11
          fs11             = (twelve*vnb12-six*vnb6+vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          vcoul            = qqOH*rinv12
          fs12             = (vcoul)*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          vcoul            = qqOH*rinv13
          fs13             = (vcoul)*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          vcoul            = qqOH*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          vcoul            = qqHH*rinv22
          fs22             = (vcoul)*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          vcoul            = qqHH*rinv23
          fs23             = (vcoul)*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          vcoul            = qqOH*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          vcoul            = qqHH*rinv32
          fs32             = (vcoul)*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          vcoul            = qqHH*rinv33
          fs33             = (vcoul)*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1200
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1200(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          qq               = iqA*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = (br*vnbexp-six*vnb6+vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1210
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1210(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (br*vnbexp-six*vnb6+vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            fs11             = (br*vnbexp-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1220
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Buckingham
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1220(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     r11
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 3*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          qq               = qO*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = (br*vnbexp-six*vnb6+vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1230
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Buckingham
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1230(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     rinvsix
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     r11
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (six  =  6.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 3*ntype*type(ii)
      tjA              = ntiA+3*type(ii)+1
      c6               = nbfp(tjA)
      cexp1            = nbfp(tjA+1)
      cexp2            = nbfp(tjA+2)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          vnb6             = c6*rinvsix
          br               = cexp2*r11
          vnbexp           = cexp1*exp(-br)
          vnbtot           = vnbtot + vnbexp-vnb6
          vcoul            = qqOO*rinv11
          fs11             = (br*vnbexp-six*vnb6+vcoul)*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          vcoul            = qqOH*rinv12
          fs12             = (vcoul)*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          vcoul            = qqOH*rinv13
          fs13             = (vcoul)*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          vcoul            = qqOH*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          vcoul            = qqHH*rinv22
          fs22             = (vcoul)*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          vcoul            = qqHH*rinv23
          fs23             = (vcoul)*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          vcoul            = qqOH*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          vcoul            = qqHH*rinv32
          fs32             = (vcoul)*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          vcoul            = qqHH*rinv33
          fs33             = (vcoul)*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1300
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1300(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          qq               = iqA*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = 
     &       ((vcoul)*rinv11-((fijD+fijR)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1310
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1310(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = 
     &         ((vcoul)*rinv11-((fijD+fijR)*tabscale))*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            fs11             = -((fijD+fijR)*tabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1320
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1320(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     r11
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 2*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          qq               = qO*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = 
     &       ((vcoul)*rinv11-((fijD+fijR)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1330
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1330(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     r11
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 2*ntype*type(ii)
      tjA              = ntiA+2*type(ii)+1
      c6               = nbfp(tjA)
      c12              = nbfp(tjA+1)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          vcoul            = qqOO*rinv11
          fs11             = 
     &       ((vcoul)*rinv11-((fijD+fijR)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          vcoul            = qqOH*rinv12
          fs12             = (vcoul)*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          vcoul            = qqOH*rinv13
          fs13             = (vcoul)*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          vcoul            = qqOH*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          vcoul            = qqHH*rinv22
          fs22             = (vcoul)*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          vcoul            = qqHH*rinv23
          fs23             = (vcoul)*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          vcoul            = qqOH*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          vcoul            = qqHH*rinv32
          fs32             = (vcoul)*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          vcoul            = qqHH*rinv33
          fs33             = (vcoul)*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1400
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1400(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          qq               = iqA*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = 
     &       ((vcoul)*rinv11-((fijD)*tabscale+(fijR)*exptabscale))
     &  *rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1410
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1410(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = 
     &         ((vcoul)*rinv11-((fijD)*tabscale+(fijR)*exptabscale))
     &  *rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            vcoul            = qq*rinv11
            fs11             = (vcoul)*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            fs11             = 
     &         -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1420
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1420(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     r11
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 3*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          qq               = qO*charge(jnr)
          vcoul            = qq*rinv11
          fs11             = 
     &       ((vcoul)*rinv11-((fijD)*tabscale+(fijR)*exptabscale))
     &  *rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          vcoul            = qq*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl1430
C    Forces:      Calculated
C    Coulomb:     Normal
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl1430(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     r11
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 3*ntype*type(ii)
      tjA              = ntiA+3*type(ii)+1
      c6               = nbfp(tjA)
      cexp1            = nbfp(tjA+1)
      cexp2            = nbfp(tjA+2)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          vcoul            = qqOO*rinv11
          fs11             = 
     &       ((vcoul)*rinv11-((fijD)*tabscale+(fijR)*exptabscale))
     &  *rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          vcoul            = qqOH*rinv12
          fs12             = (vcoul)*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          vcoul            = qqOH*rinv13
          fs13             = (vcoul)*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          vcoul            = qqOH*rinv21
          fs21             = (vcoul)*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          vcoul            = qqHH*rinv22
          fs22             = (vcoul)*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          vcoul            = qqHH*rinv23
          fs23             = (vcoul)*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          vcoul            = qqOH*rinv31
          fs31             = (vcoul)*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          vcoul            = qqHH*rinv32
          fs32             = (vcoul)*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          vcoul            = qqHH*rinv33
          fs33             = (vcoul)*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2000
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Not calculated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2000(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qq
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          rinvsq11         = rinv11*rinv11
          qq               = iqA*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = (qq*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2010
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Not calculated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2010(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     krsq
      real*4     qq
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = (qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2020
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Not calculated
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2020(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinvsq11         = rinv11*rinv11
          qq               = qO*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = (qq*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          krsq             = krf*rsq21
          vcoul            = qq*(rinv21+krsq-crf)
          fs21             = (qq*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          krsq             = krf*rsq31
          vcoul            = qq*(rinv31+krsq-crf)
          fs31             = (qq*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2030
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Not calculated
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2030(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     vctot
      real*4     vcoul
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          rinvsq11         = rinv11*rinv11
          krsq             = krf*rsq11
          vcoul            = qqOO*(rinv11+krsq-crf)
          fs11             = (qqOO*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          krsq             = krf*rsq12
          vcoul            = qqOH*(rinv12+krsq-crf)
          fs12             = (qqOH*(rinv12-two*krsq))*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          krsq             = krf*rsq13
          vcoul            = qqOH*(rinv13+krsq-crf)
          fs13             = (qqOH*(rinv13-two*krsq))*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          krsq             = krf*rsq21
          vcoul            = qqOH*(rinv21+krsq-crf)
          fs21             = (qqOH*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          krsq             = krf*rsq22
          vcoul            = qqHH*(rinv22+krsq-crf)
          fs22             = (qqHH*(rinv22-two*krsq))*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          krsq             = krf*rsq23
          vcoul            = qqHH*(rinv23+krsq-crf)
          fs23             = (qqHH*(rinv23-two*krsq))*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          krsq             = krf*rsq31
          vcoul            = qqOH*(rinv31+krsq-crf)
          fs31             = (qqOH*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          krsq             = krf*rsq32
          vcoul            = qqHH*(rinv32+krsq-crf)
          fs32             = (qqHH*(rinv32-two*krsq))*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          krsq             = krf*rsq33
          vcoul            = qqHH*(rinv33+krsq-crf)
          fs33             = (qqHH*(rinv33-two*krsq))*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2100
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Lennard-Jones
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2100(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          qq               = iqA*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       (twelve*vnb12-six*vnb6+qq*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2110
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Lennard-Jones
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2110(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     krsq
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = 
     &         (twelve*vnb12-six*vnb6+qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = (qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinvsq11         = 1.0/(rsq11)
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            fs11             = (twelve*vnb12-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2120
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Lennard-Jones
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2120(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 2*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          qq               = qO*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       (twelve*vnb12-six*vnb6+qq*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          krsq             = krf*rsq21
          vcoul            = qq*(rinv21+krsq-crf)
          fs21             = (qq*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          krsq             = krf*rsq31
          vcoul            = qq*(rinv31+krsq-crf)
          fs31             = (qq*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2130
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Lennard-Jones
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2130(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     rinvsix
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 2*ntype*type(ii)
      tjA              = ntiA+2*type(ii)+1
      c6               = nbfp(tjA)
      c12              = nbfp(tjA+1)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          vnb6             = c6*rinvsix
          vnb12            = c12*rinvsix*rinvsix
          vnbtot           = vnbtot + vnb12-vnb6
          krsq             = krf*rsq11
          vcoul            = qqOO*(rinv11+krsq-crf)
          fs11             = 
     &       (twelve*vnb12-six*vnb6+qqOO*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          krsq             = krf*rsq12
          vcoul            = qqOH*(rinv12+krsq-crf)
          fs12             = (qqOH*(rinv12-two*krsq))*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          krsq             = krf*rsq13
          vcoul            = qqOH*(rinv13+krsq-crf)
          fs13             = (qqOH*(rinv13-two*krsq))*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          krsq             = krf*rsq21
          vcoul            = qqOH*(rinv21+krsq-crf)
          fs21             = (qqOH*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          krsq             = krf*rsq22
          vcoul            = qqHH*(rinv22+krsq-crf)
          fs22             = (qqHH*(rinv22-two*krsq))*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          krsq             = krf*rsq23
          vcoul            = qqHH*(rinv23+krsq-crf)
          fs23             = (qqHH*(rinv23-two*krsq))*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          krsq             = krf*rsq31
          vcoul            = qqOH*(rinv31+krsq-crf)
          fs31             = (qqOH*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          krsq             = krf*rsq32
          vcoul            = qqHH*(rinv32+krsq-crf)
          fs32             = (qqHH*(rinv32-two*krsq))*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          krsq             = krf*rsq33
          vcoul            = qqHH*(rinv33+krsq-crf)
          fs33             = (qqHH*(rinv33-two*krsq))*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2200
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2200(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          qq               = iqA*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       (br*vnbexp-six*vnb6+qq*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2210
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2210(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      real*4     krsq
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = 
     &         (br*vnbexp-six*vnb6+qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = (qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            fs11             = (br*vnbexp-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2220
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Buckingham
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2220(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     r11
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 3*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          qq               = qO*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       (br*vnbexp-six*vnb6+qq*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          krsq             = krf*rsq21
          vcoul            = qq*(rinv21+krsq-crf)
          fs21             = (qq*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          krsq             = krf*rsq31
          vcoul            = qq*(rinv31+krsq-crf)
          fs31             = (qq*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2230
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Buckingham
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2230(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     rinvsix
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     r11
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 3*ntype*type(ii)
      tjA              = ntiA+3*type(ii)+1
      c6               = nbfp(tjA)
      cexp1            = nbfp(tjA+1)
      cexp2            = nbfp(tjA+2)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          vnb6             = c6*rinvsix
          br               = cexp2*r11
          vnbexp           = cexp1*exp(-br)
          vnbtot           = vnbtot + vnbexp-vnb6
          krsq             = krf*rsq11
          vcoul            = qqOO*(rinv11+krsq-crf)
          fs11             = 
     &       (br*vnbexp-six*vnb6+qqOO*(rinv11-two*krsq))*rinvsq11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          krsq             = krf*rsq12
          vcoul            = qqOH*(rinv12+krsq-crf)
          fs12             = (qqOH*(rinv12-two*krsq))*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          krsq             = krf*rsq13
          vcoul            = qqOH*(rinv13+krsq-crf)
          fs13             = (qqOH*(rinv13-two*krsq))*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          krsq             = krf*rsq21
          vcoul            = qqOH*(rinv21+krsq-crf)
          fs21             = (qqOH*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          krsq             = krf*rsq22
          vcoul            = qqHH*(rinv22+krsq-crf)
          fs22             = (qqHH*(rinv22-two*krsq))*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          krsq             = krf*rsq23
          vcoul            = qqHH*(rinv23+krsq-crf)
          fs23             = (qqHH*(rinv23-two*krsq))*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          krsq             = krf*rsq31
          vcoul            = qqOH*(rinv31+krsq-crf)
          fs31             = (qqOH*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          krsq             = krf*rsq32
          vcoul            = qqHH*(rinv32+krsq-crf)
          fs32             = (qqHH*(rinv32-two*krsq))*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          krsq             = krf*rsq33
          vcoul            = qqHH*(rinv33+krsq-crf)
          fs33             = (qqHH*(rinv33-two*krsq))*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2300
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2300(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          qq               = iqA*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       ((qq*(rinv11-two*krsq))*rinv11-((fijD+fijR)*tabscale))
     &  *rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2310
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2310(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = 
     &         ((qq*(rinv11-two*krsq))*rinv11-((fijD+fijR)
     &  *tabscale))*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = (qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            fs11             = -((fijD+fijR)*tabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2320
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2320(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     r11
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 2*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          qq               = qO*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       ((qq*(rinv11-two*krsq))*rinv11-((fijD+fijR)*tabscale))
     &  *rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          krsq             = krf*rsq21
          vcoul            = qq*(rinv21+krsq-crf)
          fs21             = (qq*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          krsq             = krf*rsq31
          vcoul            = qq*(rinv31+krsq-crf)
          fs31             = (qq*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2330
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2330(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     r11
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 2*ntype*type(ii)
      tjA              = ntiA+2*type(ii)+1
      c6               = nbfp(tjA)
      c12              = nbfp(tjA+1)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          krsq             = krf*rsq11
          vcoul            = qqOO*(rinv11+krsq-crf)
          fs11             = 
     &       ((qqOO*(rinv11-two*krsq))*rinv11-((fijD+fijR)*tabscale))
     &  *rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          krsq             = krf*rsq12
          vcoul            = qqOH*(rinv12+krsq-crf)
          fs12             = (qqOH*(rinv12-two*krsq))*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          krsq             = krf*rsq13
          vcoul            = qqOH*(rinv13+krsq-crf)
          fs13             = (qqOH*(rinv13-two*krsq))*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          krsq             = krf*rsq21
          vcoul            = qqOH*(rinv21+krsq-crf)
          fs21             = (qqOH*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          krsq             = krf*rsq22
          vcoul            = qqHH*(rinv22+krsq-crf)
          fs22             = (qqHH*(rinv22-two*krsq))*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          krsq             = krf*rsq23
          vcoul            = qqHH*(rinv23+krsq-crf)
          fs23             = (qqHH*(rinv23-two*krsq))*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          krsq             = krf*rsq31
          vcoul            = qqOH*(rinv31+krsq-crf)
          fs31             = (qqOH*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          krsq             = krf*rsq32
          vcoul            = qqHH*(rinv32+krsq-crf)
          fs32             = (qqHH*(rinv32-two*krsq))*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          krsq             = krf*rsq33
          vcoul            = qqHH*(rinv33+krsq-crf)
          fs33             = (qqHH*(rinv33-two*krsq))*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2400
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2400(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          qq               = iqA*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       ((qq*(rinv11-two*krsq))*rinv11-((fijD)*tabscale+(fijR)
     &  *exptabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2410
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2410(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = 
     &         ((qq*(rinv11-two*krsq))*rinv11-((fijD)*tabscale
     &  +(fijR)*exptabscale))*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            rinvsq11         = rinv11*rinv11
            qq               = iqA*charge(jnr)
            krsq             = krf*rsq11
            vcoul            = qq*(rinv11+krsq-crf)
            fs11             = (qq*(rinv11-two*krsq))*rinvsq11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 8*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            fs11             = 
     &         -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2420
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2420(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     rinvsq21
      real*4     rinvsq31
      real*4     r11
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 3*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          qq               = qO*charge(jnr)
          krsq             = krf*rsq11
          vcoul            = qq*(rinv11+krsq-crf)
          fs11             = 
     &       ((qq*(rinv11-two*krsq))*rinv11-((fijD)*tabscale+(fijR)
     &  *exptabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq21         = rinv21*rinv21
          qq               = qH*charge(jnr)
          krsq             = krf*rsq21
          vcoul            = qq*(rinv21+krsq-crf)
          fs21             = (qq*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq31         = rinv31*rinv31
          qq               = qH*charge(jnr)
          krsq             = krf*rsq31
          vcoul            = qq*(rinv31+krsq-crf)
          fs31             = (qq*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl2430
C    Forces:      Calculated
C    Coulomb:     Reaction field
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl2430(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  krf,
     &  crf,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     krf
      real*4     crf
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     krsq
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     rinvsq12
      real*4     rinvsq13
      real*4     rinvsq21
      real*4     rinvsq22
      real*4     rinvsq23
      real*4     rinvsq31
      real*4     rinvsq32
      real*4     rinvsq33
      real*4     r11
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 3*ntype*type(ii)
      tjA              = ntiA+3*type(ii)+1
      c6               = nbfp(tjA)
      cexp1            = nbfp(tjA+1)
      cexp2            = nbfp(tjA+2)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 8*n0+1
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          krsq             = krf*rsq11
          vcoul            = qqOO*(rinv11+krsq-crf)
          fs11             = 
     &       ((qqOO*(rinv11-two*krsq))*rinv11-((fijD)*tabscale+(fijR)
     &  *exptabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          rinvsq12         = rinv12*rinv12
          krsq             = krf*rsq12
          vcoul            = qqOH*(rinv12+krsq-crf)
          fs12             = (qqOH*(rinv12-two*krsq))*rinvsq12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          rinvsq13         = rinv13*rinv13
          krsq             = krf*rsq13
          vcoul            = qqOH*(rinv13+krsq-crf)
          fs13             = (qqOH*(rinv13-two*krsq))*rinvsq13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          rinvsq21         = rinv21*rinv21
          krsq             = krf*rsq21
          vcoul            = qqOH*(rinv21+krsq-crf)
          fs21             = (qqOH*(rinv21-two*krsq))*rinvsq21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          rinvsq22         = rinv22*rinv22
          krsq             = krf*rsq22
          vcoul            = qqHH*(rinv22+krsq-crf)
          fs22             = (qqHH*(rinv22-two*krsq))*rinvsq22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          rinvsq23         = rinv23*rinv23
          krsq             = krf*rsq23
          vcoul            = qqHH*(rinv23+krsq-crf)
          fs23             = (qqHH*(rinv23-two*krsq))*rinvsq23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          rinvsq31         = rinv31*rinv31
          krsq             = krf*rsq31
          vcoul            = qqOH*(rinv31+krsq-crf)
          fs31             = (qqOH*(rinv31-two*krsq))*rinvsq31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          rinvsq32         = rinv32*rinv32
          krsq             = krf*rsq32
          vcoul            = qqHH*(rinv32+krsq-crf)
          fs32             = (qqHH*(rinv32-two*krsq))*rinvsq32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          rinvsq33         = rinv33*rinv33
          krsq             = krf*rsq33
          vcoul            = qqHH*(rinv33+krsq-crf)
          fs33             = (qqHH*(rinv33-two*krsq))*rinvsq33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3000
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Not calculated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3000(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = iqA*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs11             = -((fijC)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3001
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Not calculated
C    Solvent opt: No
C    Free energy: Lambda (alpha=0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3001(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  tabscale,
     &  VFtab,
     &  lambda,
     &  dvdlambda,
     &  chargeB)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     lambda
      real*4     dvdlambda
      real*4     chargeB(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qqA
      real*4     qqB
      real*4     qqq
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     dvdl
      real*4     L1
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     one
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     iqB
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (one  =  1.0000000000000000)
      parameter (two  =  2.0000000000000000)

      dvdl             = nul
      L1               = one - lambda

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        iqB              = facel*chargeB(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qqA              = iqA*charge(jnr)
          qqB              = iqB*chargeB(jnr)
          qqq              = L1*qqA + lambda*qqB
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqq*VV
          fijC             = qqq*FF
          dvdl             = dvdl + (qqB - qqA)*VV
          fs11             = -((fijC)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      dvdlambda        = dvdlambda + dvdl
      return
      end






CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3010
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Not calculated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3010(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 4*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = -((fijC)*tabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3020
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Not calculated
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3020(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     r11
      real*4     r21
      real*4     r31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qO*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs11             = -((fijC)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3030
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Not calculated
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3030(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     r11
      real*4     r12
      real*4     r13
      real*4     r21
      real*4     r22
      real*4     r23
      real*4     r31
      real*4     r32
      real*4     r33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOO*VV
          fijC             = qqOO*FF
          fs11             = -((fijC)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r12              = rsq12*rinv12
          rt               = r12*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs12             = -((fijC)*tabscale)*rinv12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          r13              = rsq13*rinv13
          rt               = r13*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs13             = -((fijC)*tabscale)*rinv13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r22              = rsq22*rinv22
          rt               = r22*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs22             = -((fijC)*tabscale)*rinv22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          r23              = rsq23*rinv23
          rt               = r23*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs23             = -((fijC)*tabscale)*rinv23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          r32              = rsq32*rinv32
          rt               = r32*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs32             = -((fijC)*tabscale)*rinv32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          r33              = rsq33*rinv33
          rt               = r33*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs33             = -((fijC)*tabscale)*rinv33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3100
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Lennard-Jones
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3100(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = iqA*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs11             = 
     &       ((twelve*vnb12-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3110
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Lennard-Jones
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3110(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 4*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = 
     &         ((twelve*vnb12-six*vnb6)*rinv11-((fijC)*tabscale))
     &  *rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 4*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = -((fijC)*tabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinvsq11         = 1.0/(rsq11)
            r11              = rsq11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+2*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
            vnbtot           = vnbtot + vnb12-vnb6
            fs11             = (twelve*vnb12-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3120
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Lennard-Jones
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3120(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     r11
      real*4     r21
      real*4     r31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 2*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+2*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          vnb12            = rinvsix*rinvsix*nbfp(tjA+1)
          vnbtot           = vnbtot + vnb12-vnb6
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qO*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs11             = 
     &       ((twelve*vnb12-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3130
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Lennard-Jones
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3130(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     rinvsix
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     twelve
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     r11
      real*4     r12
      real*4     r13
      real*4     r21
      real*4     r22
      real*4     r23
      real*4     r31
      real*4     r32
      real*4     r33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)
      parameter (twelve  =  12.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 2*ntype*type(ii)
      tjA              = ntiA+2*type(ii)+1
      c6               = nbfp(tjA)
      c12              = nbfp(tjA+1)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          vnb6             = c6*rinvsix
          vnb12            = c12*rinvsix*rinvsix
          vnbtot           = vnbtot + vnb12-vnb6
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOO*VV
          fijC             = qqOO*FF
          fs11             = 
     &       ((twelve*vnb12-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r12              = rsq12*rinv12
          rt               = r12*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs12             = -((fijC)*tabscale)*rinv12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          r13              = rsq13*rinv13
          rt               = r13*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs13             = -((fijC)*tabscale)*rinv13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r22              = rsq22*rinv22
          rt               = r22*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs22             = -((fijC)*tabscale)*rinv22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          r23              = rsq23*rinv23
          rt               = r23*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs23             = -((fijC)*tabscale)*rinv23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          r32              = rsq32*rinv32
          rt               = r32*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs32             = -((fijC)*tabscale)*rinv32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          r33              = rsq33*rinv33
          rt               = r33*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs33             = -((fijC)*tabscale)*rinv33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3200
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3200(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = iqA*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs11             = 
     &       ((br*vnbexp-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3210
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3210(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     rinvsq11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 4*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = 
     &         ((br*vnbexp-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 4*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = -((fijC)*tabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rinvsq11         = rinv11*rinv11
            rinvsix          = rinvsq11*rinvsq11*rinvsq11
            tjA              = ntiA+3*type(jnr)+1
            vnb6             = rinvsix*nbfp(tjA)
            br               = r11*nbfp(tjA+2)
            vnbexp           = exp(-br)*nbfp(tjA+1)
            vnbtot           = vnbtot + vnbexp-vnb6
            fs11             = (br*vnbexp-six*vnb6)*rinvsq11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3220
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Buckingham
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3220(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     rinvsix
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     rinvsq11
      real*4     r11
      real*4     r21
      real*4     r31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 3*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          tjA              = ntiA+3*type(jnr)+1
          vnb6             = rinvsix*nbfp(tjA)
          br               = r11*nbfp(tjA+2)
          vnbexp           = exp(-br)*nbfp(tjA+1)
          vnbtot           = vnbtot + vnbexp-vnb6
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qO*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs11             = 
     &       ((br*vnbexp-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3230
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Buckingham
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3230(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     rinvsix
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      real*4     br
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     six
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     rinvsq11
      real*4     r11
      real*4     r12
      real*4     r13
      real*4     r21
      real*4     r22
      real*4     r23
      real*4     r31
      real*4     r32
      real*4     r33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)
      parameter (six  =  6.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 3*ntype*type(ii)
      tjA              = ntiA+3*type(ii)+1
      c6               = nbfp(tjA)
      cexp1            = nbfp(tjA+1)
      cexp2            = nbfp(tjA+2)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rinvsq11         = rinv11*rinv11
          rinvsix          = rinvsq11*rinvsq11*rinvsq11
          vnb6             = c6*rinvsix
          br               = cexp2*r11
          vnbexp           = cexp1*exp(-br)
          vnbtot           = vnbtot + vnbexp-vnb6
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOO*VV
          fijC             = qqOO*FF
          fs11             = 
     &       ((br*vnbexp-six*vnb6)*rinv11-((fijC)*tabscale))*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r12              = rsq12*rinv12
          rt               = r12*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs12             = -((fijC)*tabscale)*rinv12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          r13              = rsq13*rinv13
          rt               = r13*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs13             = -((fijC)*tabscale)*rinv13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r22              = rsq22*rinv22
          rt               = r22*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs22             = -((fijC)*tabscale)*rinv22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          r23              = rsq23*rinv23
          rt               = r23*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs23             = -((fijC)*tabscale)*rinv23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          r32              = rsq32*rinv32
          rt               = r32*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs32             = -((fijC)*tabscale)*rinv32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          r33              = rsq33*rinv33
          rt               = r33*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 4*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs33             = -((fijC)*tabscale)*rinv33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3300
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3300(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 2*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = iqA*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          fs11             = -((fijC+fijD+fijR)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3301
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated
C    Solvent opt: No
C    Free energy: Lambda (alpha=0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3301(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  lambda,
     &  dvdlambda,
     &  chargeB,
     &  typeB)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     lambda
      real*4     dvdlambda
      real*4     chargeB(*)
      integer*4  typeB(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qqA
      real*4     qqB
      real*4     qqq
      real*4     c6
      real*4     c12
      real*4     c6a
      real*4     c12a
      real*4     c6b
      real*4     c12b
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     dvdl
      real*4     L1
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     one
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     iqB
      integer*4  ntiA
      integer*4  ntiB
      integer*4  tjA
      integer*4  tjB
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (one  =  1.0000000000000000)
      parameter (two  =  2.0000000000000000)

      dvdl             = nul
      L1               = one - lambda

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        iqB              = facel*chargeB(ii)
        ntiA             = 2*ntype*type(ii)
        ntiB             = 2*ntype*typeB(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qqA              = iqA*charge(jnr)
          qqB              = iqB*chargeB(jnr)
          qqq              = L1*qqA + lambda*qqB
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqq*VV
          fijC             = qqq*FF
          dvdl             = dvdl + (qqB - qqA)*VV
          tjA              = ntiA+2*type(jnr)+1
          tjB              = ntiB+2*typeB(jnr)+1
          c6a              = nbfp(tjA)
          c6b              = nbfp(tjB)
          c6               = L1*c6a + lambda*c6b
          c12a             = nbfp(tjA+1)
          c12b             = nbfp(tjB+1)
          c12              = L1*c12a + lambda*c12b
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          dvdl             = dvdl + (c6b - c6a)*VV
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          dvdl             = dvdl + (c12b - c12a)*VV
          fs11             = -((fijC+fijD+fijR)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      dvdlambda        = dvdlambda + dvdl
      return
      end






CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3310
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3310(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+8
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            fs11             = -((fijC+fijD+fijR)*tabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = -((fijC)*tabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 2*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            tjA              = ntiA+2*type(jnr)+1
            c6               = nbfp(tjA)
            c12              = nbfp(tjA+1)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb12            = c12*VV
            fijR             = c12*FF
            vnbtot           = vnbtot + vnb6 + vnb12
            fs11             = -((fijD+fijR)*tabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3320
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3320(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     r11
      real*4     r21
      real*4     r31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 2*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = qO*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          tjA              = ntiA+2*type(jnr)+1
          c6               = nbfp(tjA)
          c12              = nbfp(tjA+1)
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          fs11             = -((fijC+fijD+fijR)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3330
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3330(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     c6
      real*4     c12
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnb12
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     r11
      real*4     r12
      real*4     r13
      real*4     r21
      real*4     r22
      real*4     r23
      real*4     r31
      real*4     r32
      real*4     r33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 2*ntype*type(ii)
      tjA              = ntiA+2*type(ii)+1
      c6               = nbfp(tjA)
      c12              = nbfp(tjA+1)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOO*VV
          fijC             = qqOO*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb12            = c12*VV
          fijR             = c12*FF
          vnbtot           = vnbtot + vnb6 + vnb12
          fs11             = -((fijC+fijD+fijR)*tabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r12              = rsq12*rinv12
          rt               = r12*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs12             = -((fijC)*tabscale)*rinv12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          r13              = rsq13*rinv13
          rt               = r13*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs13             = -((fijC)*tabscale)*rinv13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r22              = rsq22*rinv22
          rt               = r22*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs22             = -((fijC)*tabscale)*rinv22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          r23              = rsq23*rinv23
          rt               = r23*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs23             = -((fijC)*tabscale)*rinv23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          r32              = rsq32*rinv32
          rt               = r32*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs32             = -((fijC)*tabscale)*rinv32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          r33              = rsq33*rinv33
          rt               = r33*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs33             = -((fijC)*tabscale)*rinv33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3400
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: No
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3400(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        ntiA             = 3*ntype*type(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = iqA*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          fs11             = 
     &       -((fijC+fijD)*tabscale+(fijR)*exptabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3401
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: No
C    Free energy: Lambda (alpha=0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3401(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale,
     &  lambda,
     &  dvdlambda,
     &  chargeB,
     &  typeB)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      real*4     lambda
      real*4     dvdlambda
      real*4     chargeB(*)
      integer*4  typeB(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qqA
      real*4     qqB
      real*4     qqq
      real*4     cexp1a
      real*4     cexp2a
      real*4     cexp1b
      real*4     cexp2b
      real*4     c6
      real*4     c6a
      real*4     c6b
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     dvdl
      real*4     L1
      real*4     vnbexpa
      real*4     vnbexpb
      real*4     fijRa
      real*4     fijRb
      real*4     vnb6
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     one
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      real*4     iqB
      integer*4  ntiA
      integer*4  ntiB
      integer*4  tjA
      integer*4  tjB
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      parameter (nul  =  0.0000000000000000)
      parameter (one  =  1.0000000000000000)
      parameter (two  =  2.0000000000000000)

      dvdl             = nul
      L1               = one - lambda

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        iqA              = facel*charge(ii)
        iqB              = facel*chargeB(ii)
        ntiA             = 3*ntype*type(ii)
        ntiB             = 3*ntype*typeB(ii)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          rinv11           = 1.0/sqrt(rsq11)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qqA              = iqA*charge(jnr)
          qqB              = iqB*chargeB(jnr)
          qqq              = L1*qqA + lambda*qqB
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqq*VV
          fijC             = qqq*FF
          dvdl             = dvdl + (qqB - qqA)*VV
          tjA              = ntiA+3*type(jnr)+1
          tjB              = ntiB+3*typeB(jnr)+1
          c6a              = nbfp(tjA)
          c6b              = nbfp(tjB)
          c6               = L1*c6a + lambda*c6b
          cexp1a           = nbfp(tjA+1)
          cexp1b           = nbfp(tjB+1)
          cexp2a           = nbfp(tjA+2)
          cexp2b           = nbfp(tjB+2)
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          dvdl             = dvdl + (c6b - c6a)*VV
          rt               = cexp2a*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexpa          = cexp1a*VV
          fijRa            = cexp1a*cexp2a*FF
          rt               = cexp2b*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexpb          = cexp1b*VV
          fijRb            = cexp1b*cexp2b*FF
          fijR             = L1*fijRa + lambda*fijRb
          vnbtot           = vnbtot + vnb6 + L1*vnbexpa + lambda*vnbexpb
          dvdl             = dvdl + vnbexpb - vnbexpa
          fs11             = 
     &       -((fijC+fijD)*tabscale+(fijR)*exptabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          faction(j3)      = faction(j3)-tx11
          faction(j3+1)    = faction(j3+1)-ty11
          faction(j3+2)    = faction(j3+2)-tz11
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        fshift(is3)      = fshift(is3) + fix1
        fshift(is3+1)    = fshift(is3+1) + fiy1
        fshift(is3+2)    = fshift(is3+2) + fiz1
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      dvdlambda        = dvdlambda + dvdl
      return
      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3410
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: general M:N:O solvent - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3410(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale,
     &  nsatoms)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  nsatoms(*)
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  s
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     iqA
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     rsq11
      real*4     rinv11
      real*4     r11
      real*4     fs11
      real*4     tx11
      real*4     ty11
      real*4     tz11
      integer*4  nstot
      integer*4  nsvdwc
      integer*4  nscoul
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)


      do n=1,nri
        nstot            = nsatoms(3*n-2)
        nsvdwc           = nsatoms(3*n-1)
        nscoul           = nsatoms(3*n)
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        do s=1,nsvdwc
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            nnn              = n1+8
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            fs11             = 
     &         -((fijC+fijD)*tabscale+(fijR)*exptabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nsvdwc+1,nscoul
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          iqA              = facel*charge(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            qq               = iqA*charge(jnr)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vcoul            = qq*VV
            fijC             = qq*FF
            fs11             = -((fijC)*tabscale)*rinv11
            vctot            = vctot + vcoul
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        do s=nscoul+1,nstot
          ix1              = shX + pos(ii3)
          iy1              = shY + pos(ii3+1)
          iz1              = shZ + pos(ii3+2)
          ntiA             = 3*ntype*type(ii)
          fix1             = nul
          fiy1             = nul
          fiz1             = nul
          do k=nj0,nj1
            jnr              = jjnr(k)+1
            j3               = 3*jnr-2
            jx1              = pos(j3)
            jy1              = pos(j3+1)
            jz1              = pos(j3+2)
            dx11             = ix1 - jx1
            dy11             = iy1 - jy1
            dz11             = iz1 - jz1
            rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
            rinv11           = 1.0/sqrt(rsq11)
            r11              = rsq11*rinv11
            rt               = r11*tabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            tjA              = ntiA+3*type(jnr)+1
            c6               = nbfp(tjA)
            cexp1            = nbfp(tjA+1)
            cexp2            = nbfp(tjA+2)
            nnn              = n1
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnb6             = c6*VV
            fijD             = c6*FF
            rt               = cexp2*r11*exptabscale
            n0               = rt
            eps              = rt-n0
            eps2             = eps*eps
            n1               = 12*n0+1
            nnn              = n1+4
            Y                = VFtab(nnn)
            F                = VFtab(nnn+1)
            Geps             = eps*VFtab(nnn+2)
            Heps2            = eps2*VFtab(nnn+3)
            Fp               = F+Geps+Heps2
            VV               = Y+eps*Fp
            FF               = Fp+Geps+two*Heps2
            vnbexp           = cexp1*VV
            fijR             = cexp1*cexp2*FF
            vnbtot           = vnbtot + vnb6 + vnbexp
            fs11             = 
     &         -((fijD)*tabscale+(fijR)*exptabscale)*rinv11
            tx11             = dx11*fs11
            ty11             = dy11*fs11
            tz11             = dz11*fs11
            fix1             = fix1 + tx11
            fiy1             = fiy1 + ty11
            fiz1             = fiz1 + tz11
            faction(j3)      = faction(j3)-tx11
            faction(j3+1)    = faction(j3+1)-ty11
            faction(j3+2)    = faction(j3+2)-tz11
          end do
          faction(ii3)     = faction(ii3) + fix1
          faction(ii3+1)   = faction(ii3+1) + fiy1
          faction(ii3+2)   = faction(ii3+2) + fiz1
          fshift(is3)      = fshift(is3) + fix1
          fshift(is3+1)    = fshift(is3+1) + fiy1
          fshift(is3+2)    = fshift(is3+2) + fiz1
          ii               = ii + 1
          ii3              = ii3 + 3
        end do
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3420
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: Water (3 atoms) - other atom
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3420(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qq
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     rsq11
      real*4     rsq21
      real*4     rsq31
      real*4     rinv11
      real*4     rinv21
      real*4     rinv31
      real*4     r11
      real*4     r21
      real*4     r31
      real*4     fs11
      real*4     fs21
      real*4     fs31
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qO               = qO*facel
      qH               = qH*facel
      ntiA             = 3*ntype*type(ii)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = qO*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          tjA              = ntiA+3*type(jnr)+1
          c6               = nbfp(tjA)
          cexp1            = nbfp(tjA+1)
          cexp2            = nbfp(tjA+2)
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          fs11             = 
     &       -((fijC+fijD)*tabscale+(fijR)*exptabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          qq               = qH*charge(jnr)
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qq*VV
          fijC             = qq*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is gromacs innerloop inl3430
C    Forces:      Calculated
C    Coulomb:     Tabulated
C    Nonbonded:   Tabulated Buckingham
C    Solvent opt: Water (3 atoms) - water (3 atoms)
C    Free energy: No
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine inl3430(
     &  nri,
     &  iinr,
     &  jindex,
     &  jjnr,
     &  shift,
     &  shiftvec,
     &  fshift,
     &  gid,
     &  pos,
     &  faction,
     &  charge,
     &  facel,
     &  Vc,
     &  type,
     &  ntype,
     &  nbfp,
     &  Vnb,
     &  tabscale,
     &  VFtab,
     &  exptabscale)
      implicit none
      integer*4  nri
      integer*4  iinr(*)
      integer*4  jindex(*)
      integer*4  jjnr(*)
      integer*4  shift(*)
      real*4     shiftvec(*)
      real*4     fshift(*)
      integer*4  gid(*)
      real*4     pos(*)
      real*4     faction(*)
      real*4     charge(*)
      real*4     facel
      real*4     Vc(*)
      integer*4  type(*)
      integer*4  ntype
      real*4     nbfp(*)
      real*4     Vnb(*)
      real*4     tabscale
      real*4     VFtab(*)
      real*4     exptabscale
      integer*4  ii
      integer*4  k
      integer*4  n
      integer*4  nj0
      integer*4  nj1
      integer*4  is3
      integer*4  ggid
      integer*4  ii3
      integer*4  n0
      integer*4  n1
      integer*4  nnn
      real*4     qO
      real*4     qH
      real*4     qqOO
      real*4     qqOH
      real*4     qqHH
      real*4     cexp1
      real*4     cexp2
      real*4     c6
      real*4     vctot
      real*4     vcoul
      real*4     rt
      real*4     eps
      real*4     eps2
      real*4     Y
      real*4     F
      real*4     Geps
      real*4     Heps2
      real*4     Fp
      real*4     VV
      real*4     FF
      real*4     fijC
      real*4     fijD
      real*4     fijR
      real*4     vnb6
      real*4     vnbexp
      real*4     vnbtot
      integer*4  j3
      integer*4  jnr
      real*4     shX
      real*4     shY
      real*4     shZ
      real*4     nul
      real*4     two
      real*4     ix1
      real*4     iy1
      real*4     iz1
      real*4     ix2
      real*4     iy2
      real*4     iz2
      real*4     ix3
      real*4     iy3
      real*4     iz3
      integer*4  ntiA
      integer*4  tjA
      real*4     fix1
      real*4     fiy1
      real*4     fiz1
      real*4     fix2
      real*4     fiy2
      real*4     fiz2
      real*4     fix3
      real*4     fiy3
      real*4     fiz3
      real*4     jx1
      real*4     jy1
      real*4     jz1
      real*4     jx2
      real*4     jy2
      real*4     jz2
      real*4     jx3
      real*4     jy3
      real*4     jz3
      real*4     dx11
      real*4     dy11
      real*4     dz11
      real*4     dx12
      real*4     dy12
      real*4     dz12
      real*4     dx13
      real*4     dy13
      real*4     dz13
      real*4     dx21
      real*4     dy21
      real*4     dz21
      real*4     dx22
      real*4     dy22
      real*4     dz22
      real*4     dx23
      real*4     dy23
      real*4     dz23
      real*4     dx31
      real*4     dy31
      real*4     dz31
      real*4     dx32
      real*4     dy32
      real*4     dz32
      real*4     dx33
      real*4     dy33
      real*4     dz33
      real*4     rsq11
      real*4     rsq12
      real*4     rsq13
      real*4     rsq21
      real*4     rsq22
      real*4     rsq23
      real*4     rsq31
      real*4     rsq32
      real*4     rsq33
      real*4     rinv11
      real*4     rinv12
      real*4     rinv13
      real*4     rinv21
      real*4     rinv22
      real*4     rinv23
      real*4     rinv31
      real*4     rinv32
      real*4     rinv33
      real*4     r11
      real*4     r12
      real*4     r13
      real*4     r21
      real*4     r22
      real*4     r23
      real*4     r31
      real*4     r32
      real*4     r33
      real*4     fs11
      real*4     fs12
      real*4     fs13
      real*4     fs21
      real*4     fs22
      real*4     fs23
      real*4     fs31
      real*4     fs32
      real*4     fs33
      real*4     tx11
      real*4     ty11
      real*4     tz11
      real*4     tx12
      real*4     ty12
      real*4     tz12
      real*4     tx13
      real*4     ty13
      real*4     tz13
      real*4     tx21
      real*4     ty21
      real*4     tz21
      real*4     tx22
      real*4     ty22
      real*4     tz22
      real*4     tx23
      real*4     ty23
      real*4     tz23
      real*4     tx31
      real*4     ty31
      real*4     tz31
      real*4     tx32
      real*4     ty32
      real*4     tz32
      real*4     tx33
      real*4     ty33
      real*4     tz33
      real*4     fjx1
      real*4     fjy1
      real*4     fjz1
      real*4     fjx2
      real*4     fjy2
      real*4     fjz2
      real*4     fjx3
      real*4     fjy3
      real*4     fjz3
      parameter (nul  =  0.0000000000000000)
      parameter (two  =  2.0000000000000000)

      ii               = iinr(1)+1
      qO               = charge(ii)
      qH               = charge(ii+1)
      qqOO             = facel*qO*qO
      qqOH             = facel*qO*qH
      qqHH             = facel*qH*qH
      ntiA             = 3*ntype*type(ii)
      tjA              = ntiA+3*type(ii)+1
      c6               = nbfp(tjA)
      cexp1            = nbfp(tjA+1)
      cexp2            = nbfp(tjA+2)

      do n=1,nri
        is3              = 3*shift(n)+1
        shX              = shiftvec(is3)
        shY              = shiftvec(is3+1)
        shZ              = shiftvec(is3+2)
        ii               = iinr(n)+1
        ii3              = 3*ii-2
        vctot            = nul
        vnbtot           = nul
        nj0              = jindex(n)+1
        nj1              = jindex(n+1)
        ix1              = shX + pos(ii3)
        iy1              = shY + pos(ii3+1)
        iz1              = shZ + pos(ii3+2)
        ix2              = shX + pos(ii3+3)
        iy2              = shY + pos(ii3+4)
        iz2              = shZ + pos(ii3+5)
        ix3              = shX + pos(ii3+6)
        iy3              = shY + pos(ii3+7)
        iz3              = shZ + pos(ii3+8)
        fix1             = nul
        fiy1             = nul
        fiz1             = nul
        fix2             = nul
        fiy2             = nul
        fiz2             = nul
        fix3             = nul
        fiy3             = nul
        fiz3             = nul
        do k=nj0,nj1
          jnr              = jjnr(k)+1
          j3               = 3*jnr-2
          jx1              = pos(j3)
          jy1              = pos(j3+1)
          jz1              = pos(j3+2)
          jx2              = pos(j3+3)
          jy2              = pos(j3+4)
          jz2              = pos(j3+5)
          jx3              = pos(j3+6)
          jy3              = pos(j3+7)
          jz3              = pos(j3+8)
          dx11             = ix1 - jx1
          dy11             = iy1 - jy1
          dz11             = iz1 - jz1
          rsq11            = dx11*dx11+dy11*dy11+dz11*dz11
          dx12             = ix1 - jx2
          dy12             = iy1 - jy2
          dz12             = iz1 - jz2
          rsq12            = dx12*dx12+dy12*dy12+dz12*dz12
          dx13             = ix1 - jx3
          dy13             = iy1 - jy3
          dz13             = iz1 - jz3
          rsq13            = dx13*dx13+dy13*dy13+dz13*dz13
          dx21             = ix2 - jx1
          dy21             = iy2 - jy1
          dz21             = iz2 - jz1
          rsq21            = dx21*dx21+dy21*dy21+dz21*dz21
          dx22             = ix2 - jx2
          dy22             = iy2 - jy2
          dz22             = iz2 - jz2
          rsq22            = dx22*dx22+dy22*dy22+dz22*dz22
          dx23             = ix2 - jx3
          dy23             = iy2 - jy3
          dz23             = iz2 - jz3
          rsq23            = dx23*dx23+dy23*dy23+dz23*dz23
          dx31             = ix3 - jx1
          dy31             = iy3 - jy1
          dz31             = iz3 - jz1
          rsq31            = dx31*dx31+dy31*dy31+dz31*dz31
          dx32             = ix3 - jx2
          dy32             = iy3 - jy2
          dz32             = iz3 - jz2
          rsq32            = dx32*dx32+dy32*dy32+dz32*dz32
          dx33             = ix3 - jx3
          dy33             = iy3 - jy3
          dz33             = iz3 - jz3
          rsq33            = dx33*dx33+dy33*dy33+dz33*dz33
          rinv11           = 1.0/sqrt(rsq11)
          rinv21           = 1.0/sqrt(rsq21)
          rinv31           = 1.0/sqrt(rsq31)
          rinv12           = 1.0/sqrt(rsq12)
          rinv22           = 1.0/sqrt(rsq22)
          rinv32           = 1.0/sqrt(rsq32)
          rinv13           = 1.0/sqrt(rsq13)
          rinv23           = 1.0/sqrt(rsq23)
          rinv33           = 1.0/sqrt(rsq33)
          r11              = rsq11*rinv11
          rt               = r11*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOO*VV
          fijC             = qqOO*FF
          nnn              = n1+4
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnb6             = c6*VV
          fijD             = c6*FF
          rt               = cexp2*r11*exptabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1+8
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vnbexp           = cexp1*VV
          fijR             = cexp1*cexp2*FF
          vnbtot           = vnbtot + vnb6 + vnbexp
          fs11             = 
     &       -((fijC+fijD)*tabscale+(fijR)*exptabscale)*rinv11
          vctot            = vctot + vcoul
          tx11             = dx11*fs11
          ty11             = dy11*fs11
          tz11             = dz11*fs11
          fix1             = fix1 + tx11
          fiy1             = fiy1 + ty11
          fiz1             = fiz1 + tz11
          fjx1             = faction(j3)-tx11
          fjy1             = faction(j3+1)-ty11
          fjz1             = faction(j3+2)-tz11
          r12              = rsq12*rinv12
          rt               = r12*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs12             = -((fijC)*tabscale)*rinv12
          vctot            = vctot + vcoul
          tx12             = dx12*fs12
          ty12             = dy12*fs12
          tz12             = dz12*fs12
          fix1             = fix1 + tx12
          fiy1             = fiy1 + ty12
          fiz1             = fiz1 + tz12
          fjx2             = faction(j3+3)-tx12
          fjy2             = faction(j3+4)-ty12
          fjz2             = faction(j3+5)-tz12
          r13              = rsq13*rinv13
          rt               = r13*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs13             = -((fijC)*tabscale)*rinv13
          vctot            = vctot + vcoul
          tx13             = dx13*fs13
          ty13             = dy13*fs13
          tz13             = dz13*fs13
          fix1             = fix1 + tx13
          fiy1             = fiy1 + ty13
          fiz1             = fiz1 + tz13
          fjx3             = faction(j3+6)-tx13
          fjy3             = faction(j3+7)-ty13
          fjz3             = faction(j3+8)-tz13
          r21              = rsq21*rinv21
          rt               = r21*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs21             = -((fijC)*tabscale)*rinv21
          vctot            = vctot + vcoul
          tx21             = dx21*fs21
          ty21             = dy21*fs21
          tz21             = dz21*fs21
          fix2             = fix2 + tx21
          fiy2             = fiy2 + ty21
          fiz2             = fiz2 + tz21
          fjx1             = fjx1-tx21
          fjy1             = fjy1-ty21
          fjz1             = fjz1-tz21
          r22              = rsq22*rinv22
          rt               = r22*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs22             = -((fijC)*tabscale)*rinv22
          vctot            = vctot + vcoul
          tx22             = dx22*fs22
          ty22             = dy22*fs22
          tz22             = dz22*fs22
          fix2             = fix2 + tx22
          fiy2             = fiy2 + ty22
          fiz2             = fiz2 + tz22
          fjx2             = fjx2-tx22
          fjy2             = fjy2-ty22
          fjz2             = fjz2-tz22
          r23              = rsq23*rinv23
          rt               = r23*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs23             = -((fijC)*tabscale)*rinv23
          vctot            = vctot + vcoul
          tx23             = dx23*fs23
          ty23             = dy23*fs23
          tz23             = dz23*fs23
          fix2             = fix2 + tx23
          fiy2             = fiy2 + ty23
          fiz2             = fiz2 + tz23
          fjx3             = fjx3-tx23
          fjy3             = fjy3-ty23
          fjz3             = fjz3-tz23
          r31              = rsq31*rinv31
          rt               = r31*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqOH*VV
          fijC             = qqOH*FF
          fs31             = -((fijC)*tabscale)*rinv31
          vctot            = vctot + vcoul
          tx31             = dx31*fs31
          ty31             = dy31*fs31
          tz31             = dz31*fs31
          fix3             = fix3 + tx31
          fiy3             = fiy3 + ty31
          fiz3             = fiz3 + tz31
          faction(j3)      = fjx1-tx31
          faction(j3+1)    = fjy1-ty31
          faction(j3+2)    = fjz1-tz31
          r32              = rsq32*rinv32
          rt               = r32*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs32             = -((fijC)*tabscale)*rinv32
          vctot            = vctot + vcoul
          tx32             = dx32*fs32
          ty32             = dy32*fs32
          tz32             = dz32*fs32
          fix3             = fix3 + tx32
          fiy3             = fiy3 + ty32
          fiz3             = fiz3 + tz32
          faction(j3+3)    = fjx2-tx32
          faction(j3+4)    = fjy2-ty32
          faction(j3+5)    = fjz2-tz32
          r33              = rsq33*rinv33
          rt               = r33*tabscale
          n0               = rt
          eps              = rt-n0
          eps2             = eps*eps
          n1               = 12*n0+1
          nnn              = n1
          Y                = VFtab(nnn)
          F                = VFtab(nnn+1)
          Geps             = eps*VFtab(nnn+2)
          Heps2            = eps2*VFtab(nnn+3)
          Fp               = F+Geps+Heps2
          VV               = Y+eps*Fp
          FF               = Fp+Geps+two*Heps2
          vcoul            = qqHH*VV
          fijC             = qqHH*FF
          fs33             = -((fijC)*tabscale)*rinv33
          vctot            = vctot + vcoul
          tx33             = dx33*fs33
          ty33             = dy33*fs33
          tz33             = dz33*fs33
          fix3             = fix3 + tx33
          fiy3             = fiy3 + ty33
          fiz3             = fiz3 + tz33
          faction(j3+6)    = fjx3-tx33
          faction(j3+7)    = fjy3-ty33
          faction(j3+8)    = fjz3-tz33
        end do
        faction(ii3)     = faction(ii3) + fix1
        faction(ii3+1)   = faction(ii3+1) + fiy1
        faction(ii3+2)   = faction(ii3+2) + fiz1
        faction(ii3+3)   = faction(ii3+3) + fix2
        faction(ii3+4)   = faction(ii3+4) + fiy2
        faction(ii3+5)   = faction(ii3+5) + fiz2
        faction(ii3+6)   = faction(ii3+6) + fix3
        faction(ii3+7)   = faction(ii3+7) + fiy3
        faction(ii3+8)   = faction(ii3+8) + fiz3
        fshift(is3)      = fshift(is3) + fix1+fix2+fix3
        fshift(is3+1)    = fshift(is3+1) + fiy1+fiy2+fiy3
        fshift(is3+2)    = fshift(is3+2) + fiz1+fiz2+fiz3
        ggid             = gid(n)+1
        Vc(ggid)         = Vc(ggid) + vctot
        Vnb(ggid)        = Vnb(ggid) + vnbtot
      end do
      return
      end




