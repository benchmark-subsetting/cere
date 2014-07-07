c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine y_solve

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     Performs line solves in Y direction by first factoring
c     the block-tridiagonal matrix into an upper triangular matrix, 
c     and then performing back substitution to solve for the unknow
c     vectors of each line.  
c     
c     Make sure we treat elements zero to cell_size in the direction
c     of the sweep.
c---------------------------------------------------------------------

      include 'header.h'

      call lhsy
      call y_solve_cell
      call y_backsubstitute

      return
      end
      

      subroutine y_backsubstitute

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     back solve: if last cell, then generate U(jsize)=rhs(jsize)
c     else assume U(jsize) is loaded in un pack backsub_info
c     so just use it
c     after call u(jstart) will be sent to next cell
c---------------------------------------------------------------------

      include 'header.h'

      integer i, j, k, m,n
      
      do k=1,grid_points(3)-2
         do j=grid_points(2)-2,0,-1
            do i=1,grid_points(1)-2
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,k) = rhs(m,i,j,k) 
     >                    - lhs(m,n,cc,i,j,k)*rhs(n,i,j+1,k)
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine y_solve_cell

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     performs guaussian elimination on this cell.
c     
c     assumes that unpacking routines for non-first cells 
c     preload C' and rhs' from previous cell.
c     
c     assumed send happens outside this routine, but that
c     c'(JMAX) and rhs'(JMAX) will be sent to next cell
c---------------------------------------------------------------------

      include 'header.h'

      integer i,j,k,jsize,jstart

      jstart = 0
      jsize = grid_points(2)-1

      do k=1,grid_points(3)-2 
         do i=1,grid_points(1)-2

c---------------------------------------------------------------------
c     multiply c(i,0,k) by b_inverse and copy back to c
c     multiply rhs(0) by b_inverse(0) and copy to rhs
c---------------------------------------------------------------------
            call binvcrhs( lhs(1,1,bb,i,0,k),
     >                        lhs(1,1,cc,i,0,k),
     >                        rhs(1,i,0,k) )

         enddo
      enddo

c---------------------------------------------------------------------
c     begin inner most do loop
c     do all the elements of the cell unless last 
c---------------------------------------------------------------------
      do k=1,grid_points(3)-2 
         do j=1,jsize-1
            do i=1,grid_points(1)-2

c---------------------------------------------------------------------
c     subtract A*lhs_vector(j-1) from lhs_vector(j)
c     
c     rhs(j) = rhs(j) - A*rhs(j-1)
c---------------------------------------------------------------------
               call matvec_sub(lhs(1,1,aa,i,j,k),
     >                         rhs(1,i,j-1,k),rhs(1,i,j,k))

c---------------------------------------------------------------------
c     B(j) = B(j) - C(j-1)*A(j)
c---------------------------------------------------------------------
               call matmul_sub(lhs(1,1,aa,i,j,k),
     >                         lhs(1,1,cc,i,j-1,k),
     >                         lhs(1,1,bb,i,j,k))

c---------------------------------------------------------------------
c     multiply c(i,j,k) by b_inverse and copy back to c
c     multiply rhs(i,1,k) by b_inverse(i,1,k) and copy to rhs
c---------------------------------------------------------------------
               call binvcrhs( lhs(1,1,bb,i,j,k),
     >                        lhs(1,1,cc,i,j,k),
     >                        rhs(1,i,j,k) )

            enddo
         enddo
      enddo

      do k=1,grid_points(3)-2 
         do i=1,grid_points(1)-2

c---------------------------------------------------------------------
c     rhs(jsize) = rhs(jsize) - A*rhs(jsize-1)
c---------------------------------------------------------------------
            call matvec_sub(lhs(1,1,aa,i,jsize,k),
     >                         rhs(1,i,jsize-1,k),rhs(1,i,jsize,k))

c---------------------------------------------------------------------
c     B(jsize) = B(jsize) - C(jsize-1)*A(jsize)
c     call matmul_sub(aa,i,jsize,k,c,
c     $              cc,i,jsize-1,k,c,bb,i,jsize,k)
c---------------------------------------------------------------------
            call matmul_sub(lhs(1,1,aa,i,jsize,k),
     >                         lhs(1,1,cc,i,jsize-1,k),
     >                         lhs(1,1,bb,i,jsize,k))

c---------------------------------------------------------------------
c     multiply rhs(jsize) by b_inverse(jsize) and copy to rhs
c---------------------------------------------------------------------
            call binvrhs( lhs(1,1,bb,i,jsize,k),
     >                       rhs(1,i,jsize,k) )

         enddo
      enddo

      return
      end
      


