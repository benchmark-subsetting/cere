
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine z_solve

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c this function performs the solution of the approximate factorization
c step in the z-direction for all five matrix components
c simultaneously. The Thomas algorithm is employed to solve the
c systems for the z-lines. Boundary conditions are non-periodic
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k, n, k1, k2, m
       double precision  fac1, fac2

c---------------------------------------------------------------------
c                          FORWARD ELIMINATION  
c---------------------------------------------------------------------

       call lhsz

       n = 0

       do    k = 0, grid_points(3)-3
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2
                k1 = k  + 1
                k2 = k  + 2
                fac1               = 1.d0/lhs(i,j,k,n+3)
                lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
                lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
                do    m = 1, 3
                   rhs(i,j,k,m) = fac1*rhs(i,j,k,m)
                end do
                lhs(i,j,k1,n+3) = lhs(i,j,k1,n+3) -
     >                         lhs(i,j,k1,n+2)*lhs(i,j,k,n+4)
                lhs(i,j,k1,n+4) = lhs(i,j,k1,n+4) -
     >                         lhs(i,j,k1,n+2)*lhs(i,j,k,n+5)
                do    m = 1, 3
                   rhs(i,j,k1,m) = rhs(i,j,k1,m) -
     >                         lhs(i,j,k1,n+2)*rhs(i,j,k,m)
                end do
                lhs(i,j,k2,n+2) = lhs(i,j,k2,n+2) -
     >                         lhs(i,j,k2,n+1)*lhs(i,j,k,n+4)
                lhs(i,j,k2,n+3) = lhs(i,j,k2,n+3) -
     >                         lhs(i,j,k2,n+1)*lhs(i,j,k,n+5)
                do    m = 1, 3
                   rhs(i,j,k2,m) = rhs(i,j,k2,m) -
     >                         lhs(i,j,k2,n+1)*rhs(i,j,k,m)
                end do
             end do
          end do
       end do

c---------------------------------------------------------------------
c      The last two rows in this grid block are a bit different, 
c      since they do not have two more rows available for the
c      elimination of off-diagonal entries
c---------------------------------------------------------------------
       k  = grid_points(3)-2
       k1 = grid_points(3)-1
       do    j = 1, grid_points(2)-2
          do    i = 1, grid_points(1)-2
             fac1               = 1.d0/lhs(i,j,k,n+3)
             lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
             lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
             do    m = 1, 3
                rhs(i,j,k,m) = fac1*rhs(i,j,k,m)
             end do
             lhs(i,j,k1,n+3) = lhs(i,j,k1,n+3) -
     >                      lhs(i,j,k1,n+2)*lhs(i,j,k,n+4)
             lhs(i,j,k1,n+4) = lhs(i,j,k1,n+4) -
     >                      lhs(i,j,k1,n+2)*lhs(i,j,k,n+5)
             do    m = 1, 3
                rhs(i,j,k1,m) = rhs(i,j,k1,m) -
     >                      lhs(i,j,k1,n+2)*rhs(i,j,k,m)
             end do
c---------------------------------------------------------------------
c               scale the last row immediately
c---------------------------------------------------------------------
             fac2               = 1.d0/lhs(i,j,k1,n+3)
             do    m = 1, 3
                rhs(i,j,k1,m) = fac2*rhs(i,j,k1,m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c      do the u+c and the u-c factors               
c---------------------------------------------------------------------
       do   m = 4, 5

          n = (m-3)*5
          do    k = 0, grid_points(3)-3
             do    j = 1, grid_points(2)-2
                do    i = 1, grid_points(1)-2
                   k1 = k  + 1
                   k2 = k  + 2
                   fac1               = 1.d0/lhs(i,j,k,n+3)
                   lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
                   lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
                   rhs(i,j,k,m) = fac1*rhs(i,j,k,m)
                   lhs(i,j,k1,n+3) = lhs(i,j,k1,n+3) -
     >                         lhs(i,j,k1,n+2)*lhs(i,j,k,n+4)
                   lhs(i,j,k1,n+4) = lhs(i,j,k1,n+4) -
     >                         lhs(i,j,k1,n+2)*lhs(i,j,k,n+5)
                   rhs(i,j,k1,m) = rhs(i,j,k1,m) -
     >                         lhs(i,j,k1,n+2)*rhs(i,j,k,m)
                   lhs(i,j,k2,n+2) = lhs(i,j,k2,n+2) -
     >                         lhs(i,j,k2,n+1)*lhs(i,j,k,n+4)
                   lhs(i,j,k2,n+3) = lhs(i,j,k2,n+3) -
     >                         lhs(i,j,k2,n+1)*lhs(i,j,k,n+5)
                   rhs(i,j,k2,m) = rhs(i,j,k2,m) -
     >                         lhs(i,j,k2,n+1)*rhs(i,j,k,m)
                end do
             end do
          end do

c---------------------------------------------------------------------
c         And again the last two rows separately
c---------------------------------------------------------------------
          k  = grid_points(3)-2
          k1 = grid_points(3)-1
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2
                fac1               = 1.d0/lhs(i,j,k,n+3)
                lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
                lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
                rhs(i,j,k,m)     = fac1*rhs(i,j,k,m)
                lhs(i,j,k1,n+3) = lhs(i,j,k1,n+3) -
     >                      lhs(i,j,k1,n+2)*lhs(i,j,k,n+4)
                lhs(i,j,k1,n+4) = lhs(i,j,k1,n+4) -
     >                      lhs(i,j,k1,n+2)*lhs(i,j,k,n+5)
                rhs(i,j,k1,m)   = rhs(i,j,k1,m) -
     >                      lhs(i,j,k1,n+2)*rhs(i,j,k,m)
c---------------------------------------------------------------------
c               Scale the last row immediately (some of this is overkill
c               if this is the last cell)
c---------------------------------------------------------------------
                fac2               = 1.d0/lhs(i,j,k1,n+3)
                rhs(i,j,k1,m)   = fac2*rhs(i,j,k1,m)

             end do
          end do
       end do


c---------------------------------------------------------------------
c                         BACKSUBSTITUTION 
c---------------------------------------------------------------------

       k  = grid_points(3)-2
       k1 = grid_points(3)-1
       n = 0
       do   m = 1, 3
          do   j = 1, grid_points(2)-2
             do   i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) -
     >                             lhs(i,j,k,n+4)*rhs(i,j,k1,m)
             end do
          end do
       end do

       do    m = 4, 5
          n = (m-3)*5
          do   j = 1, grid_points(2)-2
             do   i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) -
     >                             lhs(i,j,k,n+4)*rhs(i,j,k1,m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c      Whether or not this is the last processor, we always have
c      to complete the back-substitution 
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c      The first three factors
c---------------------------------------------------------------------
       n = 0
       do   m = 1, 3
          do   k = grid_points(3)-3, 0, -1
             do   j = 1, grid_points(2)-2
                do    i = 1, grid_points(1)-2
                   k1 = k  + 1
                   k2 = k  + 2
                   rhs(i,j,k,m) = rhs(i,j,k,m) - 
     >                          lhs(i,j,k,n+4)*rhs(i,j,k1,m) -
     >                          lhs(i,j,k,n+5)*rhs(i,j,k2,m)
                end do
             end do
          end do
       end do

c---------------------------------------------------------------------
c      And the remaining two
c---------------------------------------------------------------------
       do    m = 4, 5
          n = (m-3)*5
          do   k = grid_points(3)-3, 0, -1
             do   j = 1, grid_points(2)-2
                do    i = 1, grid_points(1)-2
                   k1 = k  + 1
                   k2 = k  + 2
                   rhs(i,j,k,m) = rhs(i,j,k,m) - 
     >                          lhs(i,j,k,n+4)*rhs(i,j,k1,m) -
     >                          lhs(i,j,k,n+5)*rhs(i,j,k2,m)
                end do
             end do
          end do
       end do

       call tzetar

       return
       end
    






