
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine y_solve

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c this function performs the solution of the approximate factorization
c step in the y-direction for all five matrix components
c simultaneously. The Thomas algorithm is employed to solve the
c systems for the y-lines. Boundary conditions are non-periodic
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k, n, j1, j2, m
       double precision  fac1, fac2

c---------------------------------------------------------------------
c                          FORWARD ELIMINATION  
c---------------------------------------------------------------------

       call lhsy

       n = 0

       do    k = 1, grid_points(3)-2
          do    j = 0, grid_points(2)-3
             do    i = 1, grid_points(1)-2
                j1 = j  + 1
                j2 = j  + 2
                fac1               = 1.d0/lhs(i,j,k,n+3)
                lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
                lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
                do    m = 1, 3
                   rhs(i,j,k,m) = fac1*rhs(i,j,k,m)
                end do
                lhs(i,j1,k,n+3) = lhs(i,j1,k,n+3) -
     >                         lhs(i,j1,k,n+2)*lhs(i,j,k,n+4)
                lhs(i,j1,k,n+4) = lhs(i,j1,k,n+4) -
     >                         lhs(i,j1,k,n+2)*lhs(i,j,k,n+5)
                do    m = 1, 3
                   rhs(i,j1,k,m) = rhs(i,j1,k,m) -
     >                         lhs(i,j1,k,n+2)*rhs(i,j,k,m)
                end do
                lhs(i,j2,k,n+2) = lhs(i,j2,k,n+2) -
     >                         lhs(i,j2,k,n+1)*lhs(i,j,k,n+4)
                lhs(i,j2,k,n+3) = lhs(i,j2,k,n+3) -
     >                         lhs(i,j2,k,n+1)*lhs(i,j,k,n+5)
                do    m = 1, 3
                   rhs(i,j2,k,m) = rhs(i,j2,k,m) -
     >                         lhs(i,j2,k,n+1)*rhs(i,j,k,m)
                end do
             end do
          end do
       end do

c---------------------------------------------------------------------
c      The last two rows in this grid block are a bit different, 
c      since they do not have two more rows available for the
c      elimination of off-diagonal entries
c---------------------------------------------------------------------

       j  = grid_points(2)-2
       j1 = grid_points(2)-1
       do    k = 1, grid_points(3)-2
          do    i = 1, grid_points(1)-2
             fac1               = 1.d0/lhs(i,j,k,n+3)
             lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
             lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
             do    m = 1, 3
                rhs(i,j,k,m) = fac1*rhs(i,j,k,m)
             end do
             lhs(i,j1,k,n+3) = lhs(i,j1,k,n+3) -
     >                      lhs(i,j1,k,n+2)*lhs(i,j,k,n+4)
             lhs(i,j1,k,n+4) = lhs(i,j1,k,n+4) -
     >                      lhs(i,j1,k,n+2)*lhs(i,j,k,n+5)
             do    m = 1, 3
                rhs(i,j1,k,m) = rhs(i,j1,k,m) -
     >                      lhs(i,j1,k,n+2)*rhs(i,j,k,m)
             end do
c---------------------------------------------------------------------
c            scale the last row immediately 
c---------------------------------------------------------------------
             fac2               = 1.d0/lhs(i,j1,k,n+3)
             do    m = 1, 3
                rhs(i,j1,k,m) = fac2*rhs(i,j1,k,m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c      do the u+c and the u-c factors                 
c---------------------------------------------------------------------
       do    m = 4, 5

          n = (m-3)*5
          do    k = 1, grid_points(3)-2
             do    j = 0, grid_points(2)-3
                do    i = 1, grid_points(1)-2
                   j1 = j  + 1
                   j2 = j  + 2
                   fac1               = 1.d0/lhs(i,j,k,n+3)
                   lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
                   lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
                   rhs(i,j,k,m) = fac1*rhs(i,j,k,m)
                   lhs(i,j1,k,n+3) = lhs(i,j1,k,n+3) -
     >                         lhs(i,j1,k,n+2)*lhs(i,j,k,n+4)
                   lhs(i,j1,k,n+4) = lhs(i,j1,k,n+4) -
     >                         lhs(i,j1,k,n+2)*lhs(i,j,k,n+5)
                   rhs(i,j1,k,m) = rhs(i,j1,k,m) -
     >                         lhs(i,j1,k,n+2)*rhs(i,j,k,m)
                   lhs(i,j2,k,n+2) = lhs(i,j2,k,n+2) -
     >                         lhs(i,j2,k,n+1)*lhs(i,j,k,n+4)
                   lhs(i,j2,k,n+3) = lhs(i,j2,k,n+3) -
     >                         lhs(i,j2,k,n+1)*lhs(i,j,k,n+5)
                   rhs(i,j2,k,m) = rhs(i,j2,k,m) -
     >                         lhs(i,j2,k,n+1)*rhs(i,j,k,m)
                end do
             end do
          end do

c---------------------------------------------------------------------
c         And again the last two rows separately
c---------------------------------------------------------------------
          j  = grid_points(2)-2
          j1 = grid_points(2)-1
          do    k = 1, grid_points(3)-2
             do    i = 1, grid_points(1)-2
                fac1               = 1.d0/lhs(i,j,k,n+3)
                lhs(i,j,k,n+4)   = fac1*lhs(i,j,k,n+4)
                lhs(i,j,k,n+5)   = fac1*lhs(i,j,k,n+5)
                rhs(i,j,k,m)     = fac1*rhs(i,j,k,m)
                lhs(i,j1,k,n+3) = lhs(i,j1,k,n+3) -
     >                      lhs(i,j1,k,n+2)*lhs(i,j,k,n+4)
                lhs(i,j1,k,n+4) = lhs(i,j1,k,n+4) -
     >                      lhs(i,j1,k,n+2)*lhs(i,j,k,n+5)
                rhs(i,j1,k,m)   = rhs(i,j1,k,m) -
     >                      lhs(i,j1,k,n+2)*rhs(i,j,k,m)
c---------------------------------------------------------------------
c               Scale the last row immediately 
c---------------------------------------------------------------------
                fac2               = 1.d0/lhs(i,j1,k,n+3)
                rhs(i,j1,k,m)   = fac2*rhs(i,j1,k,m)

             end do
          end do

       end do



c---------------------------------------------------------------------
c                         BACKSUBSTITUTION 
c---------------------------------------------------------------------

       j  = grid_points(2)-2
       j1 = grid_points(2)-1
       n = 0
       do   m = 1, 3
          do   k = 1, grid_points(3)-2
             do   i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) -
     >                           lhs(i,j,k,n+4)*rhs(i,j1,k,m)
             end do
          end do
       end do

       do    m = 4, 5
          n = (m-3)*5
          do   k = 1, grid_points(3)-2
             do   i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) -
     >                             lhs(i,j,k,n+4)*rhs(i,j1,k,m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c      The first three factors
c---------------------------------------------------------------------
       n = 0
       do   m = 1, 3
          do   k = 1, grid_points(3)-2
             do   j = grid_points(2)-3, 0, -1
                do    i = 1, grid_points(1)-2
                   j1 = j  + 1
                   j2 = j  + 2
                   rhs(i,j,k,m) = rhs(i,j,k,m) - 
     >                          lhs(i,j,k,n+4)*rhs(i,j1,k,m) -
     >                          lhs(i,j,k,n+5)*rhs(i,j2,k,m)
                end do
             end do
          end do
       end do

c---------------------------------------------------------------------
c      And the remaining two
c---------------------------------------------------------------------
       do    m = 4, 5
          n = (m-3)*5
          do   k = 1, grid_points(3)-2
             do   j = grid_points(2)-3, 0, -1
                do    i = 1, grid_points(1)-2
                   j1 = j  + 1
                   j2 = j1 + 1
                   rhs(i,j,k,m) = rhs(i,j,k,m) - 
     >                          lhs(i,j,k,n+4)*rhs(i,j1,k,m) -
     >                          lhs(i,j,k,n+5)*rhs(i,j2,k,m)
                end do
             end do
          end do
       end do

       call pinvr

       return
       end
    






