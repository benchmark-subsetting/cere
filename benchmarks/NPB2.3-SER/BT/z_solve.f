c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine z_solve

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     Performs line solves in Z direction by first factoring
c     the block-tridiagonal matrix into an upper triangular matrix, 
c     and then performing back substitution to solve for the unknow
c     vectors of each line.  
c     
c     Make sure we treat elements zero to cell_size in the direction
c     of the sweep.
c---------------------------------------------------------------------

      include 'header.h'

      call lhsz
      call z_solve_cell
      call z_backsubstitute

      return
      end
      
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine z_backsubstitute

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     back solve: if last cell, then generate U(ksize)=rhs(ksize)
c     else assume U(ksize) is loaded in un pack backsub_info
c     so just use it
c     after call u(kstart) will be sent to next cell
c---------------------------------------------------------------------

      include 'header.h'

      integer i, j, k, m, n
      
      do k=grid_points(3)-2,0,-1
         do j=1,grid_points(2)-2
            do i=1,grid_points(1)-2
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,k) = rhs(m,i,j,k) 
     >                    - lhs(m,n,cc,i,j,k)*rhs(n,i,j,k+1)
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine z_solve_cell

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     performs guaussian elimination on this cell.
c     
c     assumes that unpacking routines for non-first cells 
c     preload C' and rhs' from previous cell.
c     
c     assumed send happens outside this routine, but that
c     c'(KMAX) and rhs'(KMAX) will be sent to next cell.
c---------------------------------------------------------------------

      include 'header.h'

      integer i,j,k,ksize

      ksize = grid_points(3)-1

c---------------------------------------------------------------------
c     outer most do loops - sweeping in i direction
c---------------------------------------------------------------------
      do j=1,grid_points(2)-2 
         do i=1,grid_points(1)-2

c---------------------------------------------------------------------
c     multiply c(i,j,0) by b_inverse and copy back to c
c     multiply rhs(0) by b_inverse(0) and copy to rhs
c---------------------------------------------------------------------
            call binvcrhs( lhs(1,1,bb,i,j,0),
     >                        lhs(1,1,cc,i,j,0),
     >                        rhs(1,i,j,0) )

         enddo
      enddo

c---------------------------------------------------------------------
c     begin inner most do loop
c     do all the elements of the cell unless last 
c---------------------------------------------------------------------
      do k=1,ksize-1
         do j=1,grid_points(2)-2
            do i=1,grid_points(1)-2

c---------------------------------------------------------------------
c     subtract A*lhs_vector(k-1) from lhs_vector(k)
c     
c     rhs(k) = rhs(k) - A*rhs(k-1)
c---------------------------------------------------------------------
               call matvec_sub(lhs(1,1,aa,i,j,k),
     >                         rhs(1,i,j,k-1),rhs(1,i,j,k))

c---------------------------------------------------------------------
c     B(k) = B(k) - C(k-1)*A(k)
c     call matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k)
c---------------------------------------------------------------------
               call matmul_sub(lhs(1,1,aa,i,j,k),
     >                         lhs(1,1,cc,i,j,k-1),
     >                         lhs(1,1,bb,i,j,k))

c---------------------------------------------------------------------
c     multiply c(i,j,k) by b_inverse and copy back to c
c     multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
c---------------------------------------------------------------------
               call binvcrhs( lhs(1,1,bb,i,j,k),
     >                        lhs(1,1,cc,i,j,k),
     >                        rhs(1,i,j,k) )

            enddo
         enddo
      enddo

c---------------------------------------------------------------------
c     Now finish up special cases for last cell
c---------------------------------------------------------------------
      do j=1,grid_points(2)-2
         do i=1,grid_points(1)-2

c---------------------------------------------------------------------
c     rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
c---------------------------------------------------------------------
            call matvec_sub(lhs(1,1,aa,i,j,ksize),
     >                         rhs(1,i,j,ksize-1),rhs(1,i,j,ksize))

c---------------------------------------------------------------------
c     B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
c     call matmul_sub(aa,i,j,ksize,c,
c     $              cc,i,j,ksize-1,c,bb,i,j,ksize)
c---------------------------------------------------------------------
            call matmul_sub(lhs(1,1,aa,i,j,ksize),
     >                         lhs(1,1,cc,i,j,ksize-1),
     >                         lhs(1,1,bb,i,j,ksize))

c---------------------------------------------------------------------
c     multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
c---------------------------------------------------------------------
            call binvrhs( lhs(1,1,bb,i,j,ksize),
     >                       rhs(1,i,j,ksize) )

         enddo
      enddo

      return
      end
      





