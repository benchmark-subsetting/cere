












































































c=======================================================================
c/////////////////////////  EXAMPLE textdmp  \\\\\\\\\\\\\\\\\\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
      subroutine textdmp
c
c  written by:   Robert Fiedler
c  Last modified: Feb. 18, 1997
c
c  PURPOSE:  Example textdmp routine to generate ASCII output
c            files during a ZEUS-MP run.
c
c  USAGE:  To use this routine, include "-u textdmp" on the
c          Make_zeusmp command line.
c
c  DESCRIPTION:  This example textdmp routine prints out the
c                coordinates and the density, energy density,
c                and velocity components for a 3-D
c                hydrodynamics simulation.  It must be modified
c                to list magnetic field components, 
c                gravitational potential, radiation energy density, 
c                etc.  
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

      real*8 clight,me,mp,mh,boltz,h,guniv
      real*8 msol,lsol
      real*8 cmau,cmpc,cmkpc,cmkm,everg
      real*8 rad_con,mmw,gasc,sbc
c   physical constants (cgs)
      data clight  /2.99792e10     /,
     &     me      /9.10953e-28    /,
     &     mp      /1.67265e-24    /,
     &     mh      /1.66053e-24    /,
     &     boltz   /1.38066e-16    /,
     &     rad_con /7.56000e-15    /,
     &     h       /6.62618e-27    /,
     &     guniv   /6.6720e-8      /,
     &     mmw     /1.0            /,
     &     gasc    /8.625e7        /,
     &     sbc     /1.8044e-5      /
c   astronomical constants (cgs)
      data msol    /1.989e33       /,
     &     lsol    /3.826e33       /
c   conversion factors
      data cmau    /1.49597e13     /,
     &     cmpc    /3.084e18       /,
     &     cmkpc   /3.084e21       /,
     &     cmkm    /1.0e5          /,
     &     everg   /1.60219e-12    /


       integer  i, j, k


c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////////
c=======================================================================
c
c Write out the coordinates and the density for each zone in the
c computational domain to unit 12 (usrfile).
c
      open(12,file=usrfile)
      write(12,"('ZEUS-MP field variables at time = ',1pe15.8)") time
      write(12,"(a)")
12    format(1p,5e16.8)
13    format(3i4,1p,6e16.8)
c12    format(1p6e12.4)




      write(12,"('         x1b             x2b             x3b',
     1           '         density       ener dens')")
      write(12,12) ( ( ( x1b(i), x2b(j), x3b(k), d (i,j,k), e (i,j,k)
     1           , i=is,ie ), j=js,je ), k=ks,ke)

      write(12,"(a)")
      write(12,"('         x1b             x2b             x3b',
     1           '         density    ')")
      write(12,14) ( ( ( x1b(i), x2b(j), x3b(k), d(i,j,k)
     1             , i=is,ie ), j=js,je ), k=ks,ke)
      write(12,"(a)")
 14   format(1p,4e16.8)
c      write(12,"('         x1a             x2b             x3b',
c     1 '          v1')")
c      write(12,22) ( ( ( x1a(i), x2b(j), x3b(k), v1(i,j,k), 
c     1                 i=is,ie ), j=js,je ), k=ks,ke )
c22    format(1p4e16.8)
c      write(12,"(a)")
c      write(12,"('         x1b             x2a             x3b',
c     1 '          v2')")
c      write(12,32) ( ( ( x1b(i), x2a(j), x3b(k), v2(i,j,k),
c     1                 i=is,ie ), j=js,je ), k=ks,ke )
c32    format(1p4e16.8)
c      write(12,"(a)")
c      write(12,"('         x1b             x2b             x3a',
c     1 '          v3')")
c      write(12,42) ( ( ( x1b(i), x2b(j), x3a(k), v3(i,j,k),
c     1                 i=is,ie ), j=js,je ), k=ks,ke )
cps42    format(1p4e16.8)


c
      close(12)
c
c Make a note of this dump in the log file.
c
c      write(2,"(/1x,'user dump written at time=',1pe12.5,' cycle='
c     & ,i6)") time,nhy
c
      end













































































c-----------------------------------------------------------------------
      subroutine marshak

      return
      end













































































c-----------------------------------------------------------------------
      subroutine xact

      return
      end
