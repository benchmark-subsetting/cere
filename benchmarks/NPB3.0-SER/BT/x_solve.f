
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine x_solve

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     
c     Performs line solves in X direction by first factoring
c     the block-tridiagonal matrix into an upper triangular matrix, 
c     and then performing back substitution to solve for the unknow
c     vectors of each line.  
c     
c     Make sure we treat elements zero to cell_size in the direction
c     of the sweep.
c     
c---------------------------------------------------------------------

      include 'header.h'

      integer i,j,k,m,n,isize

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      if (timeron) call timer_start(t_xsolve)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     This function computes the left hand side in the xi-direction
c---------------------------------------------------------------------

      isize = grid_points(1)-1

c---------------------------------------------------------------------
c     determine a (labeled f) and n jacobians
c---------------------------------------------------------------------
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 0, isize

               tmp1 = rho_i(i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2
c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------
               fjac(1,1,i) = 0.0d+00
               fjac(1,2,i) = 1.0d+00
               fjac(1,3,i) = 0.0d+00
               fjac(1,4,i) = 0.0d+00
               fjac(1,5,i) = 0.0d+00

               fjac(2,1,i) = -(u(2,i,j,k) * tmp2 * 
     >              u(2,i,j,k))
     >              + c2 * qs(i,j,k)
               fjac(2,2,i) = ( 2.0d+00 - c2 )
     >              * ( u(2,i,j,k) / u(1,i,j,k) )
               fjac(2,3,i) = - c2 * ( u(3,i,j,k) * tmp1 )
               fjac(2,4,i) = - c2 * ( u(4,i,j,k) * tmp1 )
               fjac(2,5,i) = c2

               fjac(3,1,i) = - ( u(2,i,j,k)*u(3,i,j,k) ) * tmp2
               fjac(3,2,i) = u(3,i,j,k) * tmp1
               fjac(3,3,i) = u(2,i,j,k) * tmp1
               fjac(3,4,i) = 0.0d+00
               fjac(3,5,i) = 0.0d+00

               fjac(4,1,i) = - ( u(2,i,j,k)*u(4,i,j,k) ) * tmp2
               fjac(4,2,i) = u(4,i,j,k) * tmp1
               fjac(4,3,i) = 0.0d+00
               fjac(4,4,i) = u(2,i,j,k) * tmp1
               fjac(4,5,i) = 0.0d+00

               fjac(5,1,i) = ( c2 * 2.0d0 * square(i,j,k)
     >              - c1 * u(5,i,j,k) )
     >              * ( u(2,i,j,k) * tmp2 )
               fjac(5,2,i) = c1 *  u(5,i,j,k) * tmp1 
     >              - c2
     >              * ( u(2,i,j,k)*u(2,i,j,k) * tmp2
     >              + qs(i,j,k) )
               fjac(5,3,i) = - c2 * ( u(3,i,j,k)*u(2,i,j,k) )
     >              * tmp2
               fjac(5,4,i) = - c2 * ( u(4,i,j,k)*u(2,i,j,k) )
     >              * tmp2
               fjac(5,5,i) = c1 * ( u(2,i,j,k) * tmp1 )

               njac(1,1,i) = 0.0d+00
               njac(1,2,i) = 0.0d+00
               njac(1,3,i) = 0.0d+00
               njac(1,4,i) = 0.0d+00
               njac(1,5,i) = 0.0d+00

               njac(2,1,i) = - con43 * c3c4 * tmp2 * u(2,i,j,k)
               njac(2,2,i) =   con43 * c3c4 * tmp1
               njac(2,3,i) =   0.0d+00
               njac(2,4,i) =   0.0d+00
               njac(2,5,i) =   0.0d+00

               njac(3,1,i) = - c3c4 * tmp2 * u(3,i,j,k)
               njac(3,2,i) =   0.0d+00
               njac(3,3,i) =   c3c4 * tmp1
               njac(3,4,i) =   0.0d+00
               njac(3,5,i) =   0.0d+00

               njac(4,1,i) = - c3c4 * tmp2 * u(4,i,j,k)
               njac(4,2,i) =   0.0d+00 
               njac(4,3,i) =   0.0d+00
               njac(4,4,i) =   c3c4 * tmp1
               njac(4,5,i) =   0.0d+00

               njac(5,1,i) = - ( con43 * c3c4
     >              - c1345 ) * tmp3 * (u(2,i,j,k)**2)
     >              - ( c3c4 - c1345 ) * tmp3 * (u(3,i,j,k)**2)
     >              - ( c3c4 - c1345 ) * tmp3 * (u(4,i,j,k)**2)
     >              - c1345 * tmp2 * u(5,i,j,k)

               njac(5,2,i) = ( con43 * c3c4
     >              - c1345 ) * tmp2 * u(2,i,j,k)
               njac(5,3,i) = ( c3c4 - c1345 ) * tmp2 * u(3,i,j,k)
               njac(5,4,i) = ( c3c4 - c1345 ) * tmp2 * u(4,i,j,k)
               njac(5,5,i) = ( c1345 ) * tmp1

            enddo
c---------------------------------------------------------------------
c     now jacobians set, so form left hand side in x direction
c---------------------------------------------------------------------
            call lhsinit(lhs, isize)
            do i = 1, isize-1

               tmp1 = dt * tx1
               tmp2 = dt * tx2

               lhs(1,1,aa,i) = - tmp2 * fjac(1,1,i-1)
     >              - tmp1 * njac(1,1,i-1)
     >              - tmp1 * dx1 
               lhs(1,2,aa,i) = - tmp2 * fjac(1,2,i-1)
     >              - tmp1 * njac(1,2,i-1)
               lhs(1,3,aa,i) = - tmp2 * fjac(1,3,i-1)
     >              - tmp1 * njac(1,3,i-1)
               lhs(1,4,aa,i) = - tmp2 * fjac(1,4,i-1)
     >              - tmp1 * njac(1,4,i-1)
               lhs(1,5,aa,i) = - tmp2 * fjac(1,5,i-1)
     >              - tmp1 * njac(1,5,i-1)

               lhs(2,1,aa,i) = - tmp2 * fjac(2,1,i-1)
     >              - tmp1 * njac(2,1,i-1)
               lhs(2,2,aa,i) = - tmp2 * fjac(2,2,i-1)
     >              - tmp1 * njac(2,2,i-1)
     >              - tmp1 * dx2
               lhs(2,3,aa,i) = - tmp2 * fjac(2,3,i-1)
     >              - tmp1 * njac(2,3,i-1)
               lhs(2,4,aa,i) = - tmp2 * fjac(2,4,i-1)
     >              - tmp1 * njac(2,4,i-1)
               lhs(2,5,aa,i) = - tmp2 * fjac(2,5,i-1)
     >              - tmp1 * njac(2,5,i-1)

               lhs(3,1,aa,i) = - tmp2 * fjac(3,1,i-1)
     >              - tmp1 * njac(3,1,i-1)
               lhs(3,2,aa,i) = - tmp2 * fjac(3,2,i-1)
     >              - tmp1 * njac(3,2,i-1)
               lhs(3,3,aa,i) = - tmp2 * fjac(3,3,i-1)
     >              - tmp1 * njac(3,3,i-1)
     >              - tmp1 * dx3 
               lhs(3,4,aa,i) = - tmp2 * fjac(3,4,i-1)
     >              - tmp1 * njac(3,4,i-1)
               lhs(3,5,aa,i) = - tmp2 * fjac(3,5,i-1)
     >              - tmp1 * njac(3,5,i-1)

               lhs(4,1,aa,i) = - tmp2 * fjac(4,1,i-1)
     >              - tmp1 * njac(4,1,i-1)
               lhs(4,2,aa,i) = - tmp2 * fjac(4,2,i-1)
     >              - tmp1 * njac(4,2,i-1)
               lhs(4,3,aa,i) = - tmp2 * fjac(4,3,i-1)
     >              - tmp1 * njac(4,3,i-1)
               lhs(4,4,aa,i) = - tmp2 * fjac(4,4,i-1)
     >              - tmp1 * njac(4,4,i-1)
     >              - tmp1 * dx4
               lhs(4,5,aa,i) = - tmp2 * fjac(4,5,i-1)
     >              - tmp1 * njac(4,5,i-1)

               lhs(5,1,aa,i) = - tmp2 * fjac(5,1,i-1)
     >              - tmp1 * njac(5,1,i-1)
               lhs(5,2,aa,i) = - tmp2 * fjac(5,2,i-1)
     >              - tmp1 * njac(5,2,i-1)
               lhs(5,3,aa,i) = - tmp2 * fjac(5,3,i-1)
     >              - tmp1 * njac(5,3,i-1)
               lhs(5,4,aa,i) = - tmp2 * fjac(5,4,i-1)
     >              - tmp1 * njac(5,4,i-1)
               lhs(5,5,aa,i) = - tmp2 * fjac(5,5,i-1)
     >              - tmp1 * njac(5,5,i-1)
     >              - tmp1 * dx5

               lhs(1,1,bb,i) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(1,1,i)
     >              + tmp1 * 2.0d+00 * dx1
               lhs(1,2,bb,i) = tmp1 * 2.0d+00 * njac(1,2,i)
               lhs(1,3,bb,i) = tmp1 * 2.0d+00 * njac(1,3,i)
               lhs(1,4,bb,i) = tmp1 * 2.0d+00 * njac(1,4,i)
               lhs(1,5,bb,i) = tmp1 * 2.0d+00 * njac(1,5,i)

               lhs(2,1,bb,i) = tmp1 * 2.0d+00 * njac(2,1,i)
               lhs(2,2,bb,i) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(2,2,i)
     >              + tmp1 * 2.0d+00 * dx2
               lhs(2,3,bb,i) = tmp1 * 2.0d+00 * njac(2,3,i)
               lhs(2,4,bb,i) = tmp1 * 2.0d+00 * njac(2,4,i)
               lhs(2,5,bb,i) = tmp1 * 2.0d+00 * njac(2,5,i)

               lhs(3,1,bb,i) = tmp1 * 2.0d+00 * njac(3,1,i)
               lhs(3,2,bb,i) = tmp1 * 2.0d+00 * njac(3,2,i)
               lhs(3,3,bb,i) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(3,3,i)
     >              + tmp1 * 2.0d+00 * dx3
               lhs(3,4,bb,i) = tmp1 * 2.0d+00 * njac(3,4,i)
               lhs(3,5,bb,i) = tmp1 * 2.0d+00 * njac(3,5,i)

               lhs(4,1,bb,i) = tmp1 * 2.0d+00 * njac(4,1,i)
               lhs(4,2,bb,i) = tmp1 * 2.0d+00 * njac(4,2,i)
               lhs(4,3,bb,i) = tmp1 * 2.0d+00 * njac(4,3,i)
               lhs(4,4,bb,i) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(4,4,i)
     >              + tmp1 * 2.0d+00 * dx4
               lhs(4,5,bb,i) = tmp1 * 2.0d+00 * njac(4,5,i)

               lhs(5,1,bb,i) = tmp1 * 2.0d+00 * njac(5,1,i)
               lhs(5,2,bb,i) = tmp1 * 2.0d+00 * njac(5,2,i)
               lhs(5,3,bb,i) = tmp1 * 2.0d+00 * njac(5,3,i)
               lhs(5,4,bb,i) = tmp1 * 2.0d+00 * njac(5,4,i)
               lhs(5,5,bb,i) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(5,5,i)
     >              + tmp1 * 2.0d+00 * dx5

               lhs(1,1,cc,i) =  tmp2 * fjac(1,1,i+1)
     >              - tmp1 * njac(1,1,i+1)
     >              - tmp1 * dx1
               lhs(1,2,cc,i) =  tmp2 * fjac(1,2,i+1)
     >              - tmp1 * njac(1,2,i+1)
               lhs(1,3,cc,i) =  tmp2 * fjac(1,3,i+1)
     >              - tmp1 * njac(1,3,i+1)
               lhs(1,4,cc,i) =  tmp2 * fjac(1,4,i+1)
     >              - tmp1 * njac(1,4,i+1)
               lhs(1,5,cc,i) =  tmp2 * fjac(1,5,i+1)
     >              - tmp1 * njac(1,5,i+1)

               lhs(2,1,cc,i) =  tmp2 * fjac(2,1,i+1)
     >              - tmp1 * njac(2,1,i+1)
               lhs(2,2,cc,i) =  tmp2 * fjac(2,2,i+1)
     >              - tmp1 * njac(2,2,i+1)
     >              - tmp1 * dx2
               lhs(2,3,cc,i) =  tmp2 * fjac(2,3,i+1)
     >              - tmp1 * njac(2,3,i+1)
               lhs(2,4,cc,i) =  tmp2 * fjac(2,4,i+1)
     >              - tmp1 * njac(2,4,i+1)
               lhs(2,5,cc,i) =  tmp2 * fjac(2,5,i+1)
     >              - tmp1 * njac(2,5,i+1)

               lhs(3,1,cc,i) =  tmp2 * fjac(3,1,i+1)
     >              - tmp1 * njac(3,1,i+1)
               lhs(3,2,cc,i) =  tmp2 * fjac(3,2,i+1)
     >              - tmp1 * njac(3,2,i+1)
               lhs(3,3,cc,i) =  tmp2 * fjac(3,3,i+1)
     >              - tmp1 * njac(3,3,i+1)
     >              - tmp1 * dx3
               lhs(3,4,cc,i) =  tmp2 * fjac(3,4,i+1)
     >              - tmp1 * njac(3,4,i+1)
               lhs(3,5,cc,i) =  tmp2 * fjac(3,5,i+1)
     >              - tmp1 * njac(3,5,i+1)

               lhs(4,1,cc,i) =  tmp2 * fjac(4,1,i+1)
     >              - tmp1 * njac(4,1,i+1)
               lhs(4,2,cc,i) =  tmp2 * fjac(4,2,i+1)
     >              - tmp1 * njac(4,2,i+1)
               lhs(4,3,cc,i) =  tmp2 * fjac(4,3,i+1)
     >              - tmp1 * njac(4,3,i+1)
               lhs(4,4,cc,i) =  tmp2 * fjac(4,4,i+1)
     >              - tmp1 * njac(4,4,i+1)
     >              - tmp1 * dx4
               lhs(4,5,cc,i) =  tmp2 * fjac(4,5,i+1)
     >              - tmp1 * njac(4,5,i+1)

               lhs(5,1,cc,i) =  tmp2 * fjac(5,1,i+1)
     >              - tmp1 * njac(5,1,i+1)
               lhs(5,2,cc,i) =  tmp2 * fjac(5,2,i+1)
     >              - tmp1 * njac(5,2,i+1)
               lhs(5,3,cc,i) =  tmp2 * fjac(5,3,i+1)
     >              - tmp1 * njac(5,3,i+1)
               lhs(5,4,cc,i) =  tmp2 * fjac(5,4,i+1)
     >              - tmp1 * njac(5,4,i+1)
               lhs(5,5,cc,i) =  tmp2 * fjac(5,5,i+1)
     >              - tmp1 * njac(5,5,i+1)
     >              - tmp1 * dx5

            enddo

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     performs guaussian elimination on this cell.
c     
c     assumes that unpacking routines for non-first cells 
c     preload C' and rhs' from previous cell.
c     
c     assumed send happens outside this routine, but that
c     c'(IMAX) and rhs'(IMAX) will be sent to next cell
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     outer most do loops - sweeping in i direction
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     multiply c(0,j,k) by b_inverse and copy back to c
c     multiply rhs(0) by b_inverse(0) and copy to rhs
c---------------------------------------------------------------------
            call binvcrhs( lhs(1,1,bb,0),
     >                        lhs(1,1,cc,0),
     >                        rhs(1,0,j,k) )

c---------------------------------------------------------------------
c     begin inner most do loop
c     do all the elements of the cell unless last 
c---------------------------------------------------------------------
            do i=1,isize-1

c---------------------------------------------------------------------
c     rhs(i) = rhs(i) - A*rhs(i-1)
c---------------------------------------------------------------------
               call matvec_sub(lhs(1,1,aa,i),
     >                         rhs(1,i-1,j,k),rhs(1,i,j,k))

c---------------------------------------------------------------------
c     B(i) = B(i) - C(i-1)*A(i)
c---------------------------------------------------------------------
               call matmul_sub(lhs(1,1,aa,i),
     >                         lhs(1,1,cc,i-1),
     >                         lhs(1,1,bb,i))


c---------------------------------------------------------------------
c     multiply c(i,j,k) by b_inverse and copy back to c
c     multiply rhs(1,j,k) by b_inverse(1,j,k) and copy to rhs
c---------------------------------------------------------------------
               call binvcrhs( lhs(1,1,bb,i),
     >                        lhs(1,1,cc,i),
     >                        rhs(1,i,j,k) )

            enddo

c---------------------------------------------------------------------
c     rhs(isize) = rhs(isize) - A*rhs(isize-1)
c---------------------------------------------------------------------
            call matvec_sub(lhs(1,1,aa,isize),
     >                         rhs(1,isize-1,j,k),rhs(1,isize,j,k))

c---------------------------------------------------------------------
c     B(isize) = B(isize) - C(isize-1)*A(isize)
c---------------------------------------------------------------------
            call matmul_sub(lhs(1,1,aa,isize),
     >                         lhs(1,1,cc,isize-1),
     >                         lhs(1,1,bb,isize))

c---------------------------------------------------------------------
c     multiply rhs() by b_inverse() and copy to rhs
c---------------------------------------------------------------------
            call binvrhs( lhs(1,1,bb,isize),
     >                       rhs(1,isize,j,k) )


c---------------------------------------------------------------------
c     back solve: if last cell, then generate U(isize)=rhs(isize)
c     else assume U(isize) is loaded in un pack backsub_info
c     so just use it
c     after call u(istart) will be sent to next cell
c---------------------------------------------------------------------

            do i=isize-1,0,-1
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,k) = rhs(m,i,j,k) 
     >                    - lhs(m,n,cc,i)*rhs(n,i+1,j,k)
                  enddo
               enddo
            enddo

         enddo
      enddo
      if (timeron) call timer_stop(t_xsolve)

      return
      end
      

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine matvec_sub(ablock,avec,bvec)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     subtracts bvec=bvec - ablock*avec
c---------------------------------------------------------------------

      implicit none

      double precision ablock,avec,bvec
      dimension ablock(5,5),avec(5),bvec(5)
      integer i

      do i=1,5
c---------------------------------------------------------------------
c            rhs(i,ic,jc,kc) = rhs(i,ic,jc,kc) 
c     $           - lhs(i,1,ablock,ia)*
c---------------------------------------------------------------------
         bvec(i) = bvec(i) - ablock(i,1)*avec(1)
     >                     - ablock(i,2)*avec(2)
     >                     - ablock(i,3)*avec(3)
     >                     - ablock(i,4)*avec(4)
     >                     - ablock(i,5)*avec(5)
      enddo


      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine matmul_sub(ablock, bblock, cblock)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     subtracts a(i,j,k) X b(i,j,k) from c(i,j,k)
c---------------------------------------------------------------------

      implicit none

      double precision ablock, bblock, cblock
      dimension ablock(5,5), bblock(5,5), cblock(5,5)
      integer j


      do j=1,5
         cblock(1,j) = cblock(1,j) - ablock(1,1)*bblock(1,j)
     >                             - ablock(1,2)*bblock(2,j)
     >                             - ablock(1,3)*bblock(3,j)
     >                             - ablock(1,4)*bblock(4,j)
     >                             - ablock(1,5)*bblock(5,j)
         cblock(2,j) = cblock(2,j) - ablock(2,1)*bblock(1,j)
     >                             - ablock(2,2)*bblock(2,j)
     >                             - ablock(2,3)*bblock(3,j)
     >                             - ablock(2,4)*bblock(4,j)
     >                             - ablock(2,5)*bblock(5,j)
         cblock(3,j) = cblock(3,j) - ablock(3,1)*bblock(1,j)
     >                             - ablock(3,2)*bblock(2,j)
     >                             - ablock(3,3)*bblock(3,j)
     >                             - ablock(3,4)*bblock(4,j)
     >                             - ablock(3,5)*bblock(5,j)
         cblock(4,j) = cblock(4,j) - ablock(4,1)*bblock(1,j)
     >                             - ablock(4,2)*bblock(2,j)
     >                             - ablock(4,3)*bblock(3,j)
     >                             - ablock(4,4)*bblock(4,j)
     >                             - ablock(4,5)*bblock(5,j)
         cblock(5,j) = cblock(5,j) - ablock(5,1)*bblock(1,j)
     >                             - ablock(5,2)*bblock(2,j)
     >                             - ablock(5,3)*bblock(3,j)
     >                             - ablock(5,4)*bblock(4,j)
     >                             - ablock(5,5)*bblock(5,j)
      enddo

              
      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine binvcrhs( lhs,c,r )

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------

      implicit none

      double precision pivot, coeff, lhs
      dimension lhs(5,5)
      double precision c(5,5), r(5)

c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------

      pivot = 1.00d0/lhs(1,1)
      lhs(1,2) = lhs(1,2)*pivot
      lhs(1,3) = lhs(1,3)*pivot
      lhs(1,4) = lhs(1,4)*pivot
      lhs(1,5) = lhs(1,5)*pivot
      c(1,1) = c(1,1)*pivot
      c(1,2) = c(1,2)*pivot
      c(1,3) = c(1,3)*pivot
      c(1,4) = c(1,4)*pivot
      c(1,5) = c(1,5)*pivot
      r(1)   = r(1)  *pivot

      coeff = lhs(2,1)
      lhs(2,2)= lhs(2,2) - coeff*lhs(1,2)
      lhs(2,3)= lhs(2,3) - coeff*lhs(1,3)
      lhs(2,4)= lhs(2,4) - coeff*lhs(1,4)
      lhs(2,5)= lhs(2,5) - coeff*lhs(1,5)
      c(2,1) = c(2,1) - coeff*c(1,1)
      c(2,2) = c(2,2) - coeff*c(1,2)
      c(2,3) = c(2,3) - coeff*c(1,3)
      c(2,4) = c(2,4) - coeff*c(1,4)
      c(2,5) = c(2,5) - coeff*c(1,5)
      r(2)   = r(2)   - coeff*r(1)

      coeff = lhs(3,1)
      lhs(3,2)= lhs(3,2) - coeff*lhs(1,2)
      lhs(3,3)= lhs(3,3) - coeff*lhs(1,3)
      lhs(3,4)= lhs(3,4) - coeff*lhs(1,4)
      lhs(3,5)= lhs(3,5) - coeff*lhs(1,5)
      c(3,1) = c(3,1) - coeff*c(1,1)
      c(3,2) = c(3,2) - coeff*c(1,2)
      c(3,3) = c(3,3) - coeff*c(1,3)
      c(3,4) = c(3,4) - coeff*c(1,4)
      c(3,5) = c(3,5) - coeff*c(1,5)
      r(3)   = r(3)   - coeff*r(1)

      coeff = lhs(4,1)
      lhs(4,2)= lhs(4,2) - coeff*lhs(1,2)
      lhs(4,3)= lhs(4,3) - coeff*lhs(1,3)
      lhs(4,4)= lhs(4,4) - coeff*lhs(1,4)
      lhs(4,5)= lhs(4,5) - coeff*lhs(1,5)
      c(4,1) = c(4,1) - coeff*c(1,1)
      c(4,2) = c(4,2) - coeff*c(1,2)
      c(4,3) = c(4,3) - coeff*c(1,3)
      c(4,4) = c(4,4) - coeff*c(1,4)
      c(4,5) = c(4,5) - coeff*c(1,5)
      r(4)   = r(4)   - coeff*r(1)

      coeff = lhs(5,1)
      lhs(5,2)= lhs(5,2) - coeff*lhs(1,2)
      lhs(5,3)= lhs(5,3) - coeff*lhs(1,3)
      lhs(5,4)= lhs(5,4) - coeff*lhs(1,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(1,5)
      c(5,1) = c(5,1) - coeff*c(1,1)
      c(5,2) = c(5,2) - coeff*c(1,2)
      c(5,3) = c(5,3) - coeff*c(1,3)
      c(5,4) = c(5,4) - coeff*c(1,4)
      c(5,5) = c(5,5) - coeff*c(1,5)
      r(5)   = r(5)   - coeff*r(1)


      pivot = 1.00d0/lhs(2,2)
      lhs(2,3) = lhs(2,3)*pivot
      lhs(2,4) = lhs(2,4)*pivot
      lhs(2,5) = lhs(2,5)*pivot
      c(2,1) = c(2,1)*pivot
      c(2,2) = c(2,2)*pivot
      c(2,3) = c(2,3)*pivot
      c(2,4) = c(2,4)*pivot
      c(2,5) = c(2,5)*pivot
      r(2)   = r(2)  *pivot

      coeff = lhs(1,2)
      lhs(1,3)= lhs(1,3) - coeff*lhs(2,3)
      lhs(1,4)= lhs(1,4) - coeff*lhs(2,4)
      lhs(1,5)= lhs(1,5) - coeff*lhs(2,5)
      c(1,1) = c(1,1) - coeff*c(2,1)
      c(1,2) = c(1,2) - coeff*c(2,2)
      c(1,3) = c(1,3) - coeff*c(2,3)
      c(1,4) = c(1,4) - coeff*c(2,4)
      c(1,5) = c(1,5) - coeff*c(2,5)
      r(1)   = r(1)   - coeff*r(2)

      coeff = lhs(3,2)
      lhs(3,3)= lhs(3,3) - coeff*lhs(2,3)
      lhs(3,4)= lhs(3,4) - coeff*lhs(2,4)
      lhs(3,5)= lhs(3,5) - coeff*lhs(2,5)
      c(3,1) = c(3,1) - coeff*c(2,1)
      c(3,2) = c(3,2) - coeff*c(2,2)
      c(3,3) = c(3,3) - coeff*c(2,3)
      c(3,4) = c(3,4) - coeff*c(2,4)
      c(3,5) = c(3,5) - coeff*c(2,5)
      r(3)   = r(3)   - coeff*r(2)

      coeff = lhs(4,2)
      lhs(4,3)= lhs(4,3) - coeff*lhs(2,3)
      lhs(4,4)= lhs(4,4) - coeff*lhs(2,4)
      lhs(4,5)= lhs(4,5) - coeff*lhs(2,5)
      c(4,1) = c(4,1) - coeff*c(2,1)
      c(4,2) = c(4,2) - coeff*c(2,2)
      c(4,3) = c(4,3) - coeff*c(2,3)
      c(4,4) = c(4,4) - coeff*c(2,4)
      c(4,5) = c(4,5) - coeff*c(2,5)
      r(4)   = r(4)   - coeff*r(2)

      coeff = lhs(5,2)
      lhs(5,3)= lhs(5,3) - coeff*lhs(2,3)
      lhs(5,4)= lhs(5,4) - coeff*lhs(2,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(2,5)
      c(5,1) = c(5,1) - coeff*c(2,1)
      c(5,2) = c(5,2) - coeff*c(2,2)
      c(5,3) = c(5,3) - coeff*c(2,3)
      c(5,4) = c(5,4) - coeff*c(2,4)
      c(5,5) = c(5,5) - coeff*c(2,5)
      r(5)   = r(5)   - coeff*r(2)


      pivot = 1.00d0/lhs(3,3)
      lhs(3,4) = lhs(3,4)*pivot
      lhs(3,5) = lhs(3,5)*pivot
      c(3,1) = c(3,1)*pivot
      c(3,2) = c(3,2)*pivot
      c(3,3) = c(3,3)*pivot
      c(3,4) = c(3,4)*pivot
      c(3,5) = c(3,5)*pivot
      r(3)   = r(3)  *pivot

      coeff = lhs(1,3)
      lhs(1,4)= lhs(1,4) - coeff*lhs(3,4)
      lhs(1,5)= lhs(1,5) - coeff*lhs(3,5)
      c(1,1) = c(1,1) - coeff*c(3,1)
      c(1,2) = c(1,2) - coeff*c(3,2)
      c(1,3) = c(1,3) - coeff*c(3,3)
      c(1,4) = c(1,4) - coeff*c(3,4)
      c(1,5) = c(1,5) - coeff*c(3,5)
      r(1)   = r(1)   - coeff*r(3)

      coeff = lhs(2,3)
      lhs(2,4)= lhs(2,4) - coeff*lhs(3,4)
      lhs(2,5)= lhs(2,5) - coeff*lhs(3,5)
      c(2,1) = c(2,1) - coeff*c(3,1)
      c(2,2) = c(2,2) - coeff*c(3,2)
      c(2,3) = c(2,3) - coeff*c(3,3)
      c(2,4) = c(2,4) - coeff*c(3,4)
      c(2,5) = c(2,5) - coeff*c(3,5)
      r(2)   = r(2)   - coeff*r(3)

      coeff = lhs(4,3)
      lhs(4,4)= lhs(4,4) - coeff*lhs(3,4)
      lhs(4,5)= lhs(4,5) - coeff*lhs(3,5)
      c(4,1) = c(4,1) - coeff*c(3,1)
      c(4,2) = c(4,2) - coeff*c(3,2)
      c(4,3) = c(4,3) - coeff*c(3,3)
      c(4,4) = c(4,4) - coeff*c(3,4)
      c(4,5) = c(4,5) - coeff*c(3,5)
      r(4)   = r(4)   - coeff*r(3)

      coeff = lhs(5,3)
      lhs(5,4)= lhs(5,4) - coeff*lhs(3,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(3,5)
      c(5,1) = c(5,1) - coeff*c(3,1)
      c(5,2) = c(5,2) - coeff*c(3,2)
      c(5,3) = c(5,3) - coeff*c(3,3)
      c(5,4) = c(5,4) - coeff*c(3,4)
      c(5,5) = c(5,5) - coeff*c(3,5)
      r(5)   = r(5)   - coeff*r(3)


      pivot = 1.00d0/lhs(4,4)
      lhs(4,5) = lhs(4,5)*pivot
      c(4,1) = c(4,1)*pivot
      c(4,2) = c(4,2)*pivot
      c(4,3) = c(4,3)*pivot
      c(4,4) = c(4,4)*pivot
      c(4,5) = c(4,5)*pivot
      r(4)   = r(4)  *pivot

      coeff = lhs(1,4)
      lhs(1,5)= lhs(1,5) - coeff*lhs(4,5)
      c(1,1) = c(1,1) - coeff*c(4,1)
      c(1,2) = c(1,2) - coeff*c(4,2)
      c(1,3) = c(1,3) - coeff*c(4,3)
      c(1,4) = c(1,4) - coeff*c(4,4)
      c(1,5) = c(1,5) - coeff*c(4,5)
      r(1)   = r(1)   - coeff*r(4)

      coeff = lhs(2,4)
      lhs(2,5)= lhs(2,5) - coeff*lhs(4,5)
      c(2,1) = c(2,1) - coeff*c(4,1)
      c(2,2) = c(2,2) - coeff*c(4,2)
      c(2,3) = c(2,3) - coeff*c(4,3)
      c(2,4) = c(2,4) - coeff*c(4,4)
      c(2,5) = c(2,5) - coeff*c(4,5)
      r(2)   = r(2)   - coeff*r(4)

      coeff = lhs(3,4)
      lhs(3,5)= lhs(3,5) - coeff*lhs(4,5)
      c(3,1) = c(3,1) - coeff*c(4,1)
      c(3,2) = c(3,2) - coeff*c(4,2)
      c(3,3) = c(3,3) - coeff*c(4,3)
      c(3,4) = c(3,4) - coeff*c(4,4)
      c(3,5) = c(3,5) - coeff*c(4,5)
      r(3)   = r(3)   - coeff*r(4)

      coeff = lhs(5,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(4,5)
      c(5,1) = c(5,1) - coeff*c(4,1)
      c(5,2) = c(5,2) - coeff*c(4,2)
      c(5,3) = c(5,3) - coeff*c(4,3)
      c(5,4) = c(5,4) - coeff*c(4,4)
      c(5,5) = c(5,5) - coeff*c(4,5)
      r(5)   = r(5)   - coeff*r(4)


      pivot = 1.00d0/lhs(5,5)
      c(5,1) = c(5,1)*pivot
      c(5,2) = c(5,2)*pivot
      c(5,3) = c(5,3)*pivot
      c(5,4) = c(5,4)*pivot
      c(5,5) = c(5,5)*pivot
      r(5)   = r(5)  *pivot

      coeff = lhs(1,5)
      c(1,1) = c(1,1) - coeff*c(5,1)
      c(1,2) = c(1,2) - coeff*c(5,2)
      c(1,3) = c(1,3) - coeff*c(5,3)
      c(1,4) = c(1,4) - coeff*c(5,4)
      c(1,5) = c(1,5) - coeff*c(5,5)
      r(1)   = r(1)   - coeff*r(5)

      coeff = lhs(2,5)
      c(2,1) = c(2,1) - coeff*c(5,1)
      c(2,2) = c(2,2) - coeff*c(5,2)
      c(2,3) = c(2,3) - coeff*c(5,3)
      c(2,4) = c(2,4) - coeff*c(5,4)
      c(2,5) = c(2,5) - coeff*c(5,5)
      r(2)   = r(2)   - coeff*r(5)

      coeff = lhs(3,5)
      c(3,1) = c(3,1) - coeff*c(5,1)
      c(3,2) = c(3,2) - coeff*c(5,2)
      c(3,3) = c(3,3) - coeff*c(5,3)
      c(3,4) = c(3,4) - coeff*c(5,4)
      c(3,5) = c(3,5) - coeff*c(5,5)
      r(3)   = r(3)   - coeff*r(5)

      coeff = lhs(4,5)
      c(4,1) = c(4,1) - coeff*c(5,1)
      c(4,2) = c(4,2) - coeff*c(5,2)
      c(4,3) = c(4,3) - coeff*c(5,3)
      c(4,4) = c(4,4) - coeff*c(5,4)
      c(4,5) = c(4,5) - coeff*c(5,5)
      r(4)   = r(4)   - coeff*r(5)


      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine binvrhs( lhs,r )

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------

      implicit none

      double precision pivot, coeff, lhs
      dimension lhs(5,5)
      double precision r(5)

c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------


      pivot = 1.00d0/lhs(1,1)
      lhs(1,2) = lhs(1,2)*pivot
      lhs(1,3) = lhs(1,3)*pivot
      lhs(1,4) = lhs(1,4)*pivot
      lhs(1,5) = lhs(1,5)*pivot
      r(1)   = r(1)  *pivot

      coeff = lhs(2,1)
      lhs(2,2)= lhs(2,2) - coeff*lhs(1,2)
      lhs(2,3)= lhs(2,3) - coeff*lhs(1,3)
      lhs(2,4)= lhs(2,4) - coeff*lhs(1,4)
      lhs(2,5)= lhs(2,5) - coeff*lhs(1,5)
      r(2)   = r(2)   - coeff*r(1)

      coeff = lhs(3,1)
      lhs(3,2)= lhs(3,2) - coeff*lhs(1,2)
      lhs(3,3)= lhs(3,3) - coeff*lhs(1,3)
      lhs(3,4)= lhs(3,4) - coeff*lhs(1,4)
      lhs(3,5)= lhs(3,5) - coeff*lhs(1,5)
      r(3)   = r(3)   - coeff*r(1)

      coeff = lhs(4,1)
      lhs(4,2)= lhs(4,2) - coeff*lhs(1,2)
      lhs(4,3)= lhs(4,3) - coeff*lhs(1,3)
      lhs(4,4)= lhs(4,4) - coeff*lhs(1,4)
      lhs(4,5)= lhs(4,5) - coeff*lhs(1,5)
      r(4)   = r(4)   - coeff*r(1)

      coeff = lhs(5,1)
      lhs(5,2)= lhs(5,2) - coeff*lhs(1,2)
      lhs(5,3)= lhs(5,3) - coeff*lhs(1,3)
      lhs(5,4)= lhs(5,4) - coeff*lhs(1,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(1,5)
      r(5)   = r(5)   - coeff*r(1)


      pivot = 1.00d0/lhs(2,2)
      lhs(2,3) = lhs(2,3)*pivot
      lhs(2,4) = lhs(2,4)*pivot
      lhs(2,5) = lhs(2,5)*pivot
      r(2)   = r(2)  *pivot

      coeff = lhs(1,2)
      lhs(1,3)= lhs(1,3) - coeff*lhs(2,3)
      lhs(1,4)= lhs(1,4) - coeff*lhs(2,4)
      lhs(1,5)= lhs(1,5) - coeff*lhs(2,5)
      r(1)   = r(1)   - coeff*r(2)

      coeff = lhs(3,2)
      lhs(3,3)= lhs(3,3) - coeff*lhs(2,3)
      lhs(3,4)= lhs(3,4) - coeff*lhs(2,4)
      lhs(3,5)= lhs(3,5) - coeff*lhs(2,5)
      r(3)   = r(3)   - coeff*r(2)

      coeff = lhs(4,2)
      lhs(4,3)= lhs(4,3) - coeff*lhs(2,3)
      lhs(4,4)= lhs(4,4) - coeff*lhs(2,4)
      lhs(4,5)= lhs(4,5) - coeff*lhs(2,5)
      r(4)   = r(4)   - coeff*r(2)

      coeff = lhs(5,2)
      lhs(5,3)= lhs(5,3) - coeff*lhs(2,3)
      lhs(5,4)= lhs(5,4) - coeff*lhs(2,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(2,5)
      r(5)   = r(5)   - coeff*r(2)


      pivot = 1.00d0/lhs(3,3)
      lhs(3,4) = lhs(3,4)*pivot
      lhs(3,5) = lhs(3,5)*pivot
      r(3)   = r(3)  *pivot

      coeff = lhs(1,3)
      lhs(1,4)= lhs(1,4) - coeff*lhs(3,4)
      lhs(1,5)= lhs(1,5) - coeff*lhs(3,5)
      r(1)   = r(1)   - coeff*r(3)

      coeff = lhs(2,3)
      lhs(2,4)= lhs(2,4) - coeff*lhs(3,4)
      lhs(2,5)= lhs(2,5) - coeff*lhs(3,5)
      r(2)   = r(2)   - coeff*r(3)

      coeff = lhs(4,3)
      lhs(4,4)= lhs(4,4) - coeff*lhs(3,4)
      lhs(4,5)= lhs(4,5) - coeff*lhs(3,5)
      r(4)   = r(4)   - coeff*r(3)

      coeff = lhs(5,3)
      lhs(5,4)= lhs(5,4) - coeff*lhs(3,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(3,5)
      r(5)   = r(5)   - coeff*r(3)


      pivot = 1.00d0/lhs(4,4)
      lhs(4,5) = lhs(4,5)*pivot
      r(4)   = r(4)  *pivot

      coeff = lhs(1,4)
      lhs(1,5)= lhs(1,5) - coeff*lhs(4,5)
      r(1)   = r(1)   - coeff*r(4)

      coeff = lhs(2,4)
      lhs(2,5)= lhs(2,5) - coeff*lhs(4,5)
      r(2)   = r(2)   - coeff*r(4)

      coeff = lhs(3,4)
      lhs(3,5)= lhs(3,5) - coeff*lhs(4,5)
      r(3)   = r(3)   - coeff*r(4)

      coeff = lhs(5,4)
      lhs(5,5)= lhs(5,5) - coeff*lhs(4,5)
      r(5)   = r(5)   - coeff*r(4)


      pivot = 1.00d0/lhs(5,5)
      r(5)   = r(5)  *pivot

      coeff = lhs(1,5)
      r(1)   = r(1)   - coeff*r(5)

      coeff = lhs(2,5)
      r(2)   = r(2)   - coeff*r(5)

      coeff = lhs(3,5)
      r(3)   = r(3)   - coeff*r(5)

      coeff = lhs(4,5)
      r(4)   = r(4)   - coeff*r(5)


      return
      end




