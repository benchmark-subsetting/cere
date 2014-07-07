c---------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  header.h
c
c---------------------------------------------------------------------
c---------------------------------------------------------------------
 
      implicit none

c---------------------------------------------------------------------
c The following include file is generated automatically by the
c "setparams" utility. It defines 
c      maxcells:      the square root of the maximum number of processors
c      problem_size:  12, 64, 102, 162 (for class T, A, B, C)
c      dt_default:    default time step for this problem size if no
c                     config file
c      niter_default: default number of iterations for this problem size
c---------------------------------------------------------------------

      include 'npbparams.h'

      integer           aa, bb, cc, BLOCK_SIZE
      parameter        (aa=1, bb=2, cc=3, BLOCK_SIZE=5)

      integer           grid_points(3)
      double precision  elapsed_time
      common /global/   elapsed_time, grid_points

      double precision  tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3, 
     >                  dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4, 
     >                  dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt, 
     >                  ce(5,13), dxmax, dymax, dzmax, xxcon1, xxcon2, 
     >                  xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1,
     >                  dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4,
     >                  yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1,
     >                  zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1, 
     >                  dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1, 
     >                  dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2, 
     >                  c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1,
     >                  dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1, 
     >                  c2dtty1, c2dttz1, comz1, comz4, comz5, comz6, 
     >                  c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16

      common /constants/ tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3,
     >                  dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4, 
     >                  dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt, 
     >                  ce, dxmax, dymax, dzmax, xxcon1, xxcon2, 
     >                  xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1,
     >                  dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4,
     >                  yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1,
     >                  zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1, 
     >                  dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1, 
     >                  dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2, 
     >                  c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1,
     >                  dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1, 
     >                  c2dtty1, c2dttz1, comz1, comz4, comz5, comz6, 
     >                  c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16

      integer IMAX, JMAX, KMAX

      parameter (IMAX=problem_size,JMAX=problem_size,KMAX=problem_size)

c
c   to improve cache performance, grid dimensions padded by 1 
c   for even number sizes only.
c
      double precision 
     >   us      (0:IMAX/2*2, 0:JMAX/2*2,  0:KMAX/2*2),
     >   vs      (0:IMAX/2*2, 0:JMAX/2*2,  0:KMAX/2*2),
     >   ws      (0:IMAX/2*2, 0:JMAX/2*2,  0:KMAX/2*2),
     >   qs      (0:IMAX/2*2, 0:JMAX/2*2,  0:KMAX/2*2),
     >   rho_i   (0:IMAX/2*2, 0:JMAX/2*2,  0:KMAX/2*2),
     >   square  (0:IMAX/2*2, 0:JMAX/2*2,  0:KMAX/2*2),
     >   forcing (       0:IMAX/2*2,   0:JMAX/2*2,   0:KMAX/2*2,  5),
     >   u       (5, 0:(IMAX+1)/2*2, 0:(JMAX+1)/2*2, 0:(KMAX+1)/2*2 ),
     >   rhs     (5,     0:IMAX/2*2,   0:JMAX/2*2,   0:KMAX/2*2),
     >   lhs     (5,5,3, 0:IMAX/2*2,   0:JMAX/2*2,   0:KMAX/2*2)
      common /fields/  u, us, vs, ws, qs, rho_i, square, 
     >                 rhs, forcing, lhs

      double precision cv(-2:problem_size+1),   
     >                 cuf(-2:problem_size+1),  q(-2:problem_size+1),
     >                 ue(-2:problem_size+1,5), buf(-2:problem_size+1,5)
      common /work_1d/ cv, cuf, q, ue, buf
c
c   to improve cache performance, grid dimensions (first two for these
c   to arrays) padded by 1 for even number sizes only.
c
      double precision fjac(5, 5, 0:IMAX/2*2, 0:JMAX/2*2, 0:KMAX-1),
     >                 njac(5, 5, 0:IMAX/2*2, 0:JMAX/2*2, 0:KMAX-1),
     >                 tmp1, tmp2, tmp3
      common /work_lhs/ fjac, njac, tmp1, tmp2, tmp3

      double precision  tmp_block(5,5), b_inverse(5,5), tmp_vec(5)
      common /work_solve/ tmp_block, b_inverse, tmp_vec
      


