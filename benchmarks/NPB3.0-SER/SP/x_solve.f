
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine x_solve

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c this function performs the solution of the approximate factorization
c step in the x-direction for all five matrix components
c simultaneously. The Thomas algorithm is employed to solve the
c systems for the x-lines. Boundary conditions are non-periodic
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k, i1, i2, m
       double precision  ru1, fac1, fac2

c---------------------------------------------------------------------
c---------------------------------------------------------------------

       if (timeron) call timer_start(t_xsolve)
       do  k = 1, nz2
          do  j = 1, ny2

c---------------------------------------------------------------------
c Computes the left hand side for the three x-factors  
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c      first fill the lhs for the u-eigenvalue                   
c---------------------------------------------------------------------
             do  i = 0, grid_points(1)-1
                ru1 = c3c4*rho_i(i,j,k)
                cv(i) = us(i,j,k)
                rhon(i) = dmax1(dx2+con43*ru1, 
     >                          dx5+c1c5*ru1,
     >                          dxmax+ru1,
     >                          dx1)
             end do

             call lhsinit(grid_points(1)-1)
             do  i = 1, nx2
                lhs(1,i) =   0.0d0
                lhs(2,i) = - dttx2 * cv(i-1) - dttx1 * rhon(i-1)
                lhs(3,i) =   1.0d0 + c2dttx1 * rhon(i)
                lhs(4,i) =   dttx2 * cv(i+1) - dttx1 * rhon(i+1)
                lhs(5,i) =   0.0d0
             end do

c---------------------------------------------------------------------
c      add fourth order dissipation                             
c---------------------------------------------------------------------

             i = 1
             lhs(3,i) = lhs(3,i) + comz5
             lhs(4,i) = lhs(4,i) - comz4
             lhs(5,i) = lhs(5,i) + comz1
  
             lhs(2,i+1) = lhs(2,i+1) - comz4
             lhs(3,i+1) = lhs(3,i+1) + comz6
             lhs(4,i+1) = lhs(4,i+1) - comz4
             lhs(5,i+1) = lhs(5,i+1) + comz1

             do   i=3, grid_points(1)-4
                lhs(1,i) = lhs(1,i) + comz1
                lhs(2,i) = lhs(2,i) - comz4
                lhs(3,i) = lhs(3,i) + comz6
                lhs(4,i) = lhs(4,i) - comz4
                lhs(5,i) = lhs(5,i) + comz1
             end do


             i = grid_points(1)-3
             lhs(1,i) = lhs(1,i) + comz1
             lhs(2,i) = lhs(2,i) - comz4
             lhs(3,i) = lhs(3,i) + comz6
             lhs(4,i) = lhs(4,i) - comz4

             lhs(1,i+1) = lhs(1,i+1) + comz1
             lhs(2,i+1) = lhs(2,i+1) - comz4
             lhs(3,i+1) = lhs(3,i+1) + comz5

c---------------------------------------------------------------------
c      subsequently, fill the other factors (u+c), (u-c) by adding to 
c      the first  
c---------------------------------------------------------------------
             do   i = 1, nx2
                lhsp(1,i) = lhs(1,i)
                lhsp(2,i) = lhs(2,i) - 
     >                            dttx2 * speed(i-1,j,k)
                lhsp(3,i) = lhs(3,i)
                lhsp(4,i) = lhs(4,i) + 
     >                            dttx2 * speed(i+1,j,k)
                lhsp(5,i) = lhs(5,i)
                lhsm(1,i) = lhs(1,i)
                lhsm(2,i) = lhs(2,i) + 
     >                            dttx2 * speed(i-1,j,k)
                lhsm(3,i) = lhs(3,i)
                lhsm(4,i) = lhs(4,i) - 
     >                            dttx2 * speed(i+1,j,k)
                lhsm(5,i) = lhs(5,i)
             end do

c---------------------------------------------------------------------
c                          FORWARD ELIMINATION  
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c      perform the Thomas algorithm; first, FORWARD ELIMINATION     
c---------------------------------------------------------------------

             do    i = 0, grid_points(1)-3
                i1 = i  + 1
                i2 = i  + 2
                fac1      = 1.d0/lhs(3,i)
                lhs(4,i)  = fac1*lhs(4,i)
                lhs(5,i)  = fac1*lhs(5,i)
                do    m = 1, 3
                   rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                end do
                lhs(3,i1) = lhs(3,i1) -
     >                         lhs(2,i1)*lhs(4,i)
                lhs(4,i1) = lhs(4,i1) -
     >                         lhs(2,i1)*lhs(5,i)
                do    m = 1, 3
                   rhs(m,i1,j,k) = rhs(m,i1,j,k) -
     >                         lhs(2,i1)*rhs(m,i,j,k)
                end do
                lhs(2,i2) = lhs(2,i2) -
     >                         lhs(1,i2)*lhs(4,i)
                lhs(3,i2) = lhs(3,i2) -
     >                         lhs(1,i2)*lhs(5,i)
                do    m = 1, 3
                   rhs(m,i2,j,k) = rhs(m,i2,j,k) -
     >                         lhs(1,i2)*rhs(m,i,j,k)
                end do
             end do

c---------------------------------------------------------------------
c      The last two rows in this grid block are a bit different, 
c      since they do not have two more rows available for the
c      elimination of off-diagonal entries
c---------------------------------------------------------------------

             i  = grid_points(1)-2
             i1 = grid_points(1)-1
             fac1      = 1.d0/lhs(3,i)
             lhs(4,i)  = fac1*lhs(4,i)
             lhs(5,i)  = fac1*lhs(5,i)
             do    m = 1, 3
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             end do
             lhs(3,i1) = lhs(3,i1) -
     >                      lhs(2,i1)*lhs(4,i)
             lhs(4,i1) = lhs(4,i1) -
     >                      lhs(2,i1)*lhs(5,i)
             do    m = 1, 3
                rhs(m,i1,j,k) = rhs(m,i1,j,k) -
     >                      lhs(2,i1)*rhs(m,i,j,k)
             end do
c---------------------------------------------------------------------
c            scale the last row immediately 
c---------------------------------------------------------------------
             fac2             = 1.d0/lhs(3,i1)
             do    m = 1, 3
                rhs(m,i1,j,k) = fac2*rhs(m,i1,j,k)
             end do

c---------------------------------------------------------------------
c      do the u+c and the u-c factors                 
c---------------------------------------------------------------------

             do    i = 0, grid_points(1)-3
                i1 = i  + 1
                i2 = i  + 2
		m = 4
                fac1       = 1.d0/lhsp(3,i)
                lhsp(4,i)  = fac1*lhsp(4,i)
                lhsp(5,i)  = fac1*lhsp(5,i)
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                lhsp(3,i1) = lhsp(3,i1) -
     >                        lhsp(2,i1)*lhsp(4,i)
                lhsp(4,i1) = lhsp(4,i1) -
     >                        lhsp(2,i1)*lhsp(5,i)
                rhs(m,i1,j,k) = rhs(m,i1,j,k) -
     >                        lhsp(2,i1)*rhs(m,i,j,k)
                lhsp(2,i2) = lhsp(2,i2) -
     >                        lhsp(1,i2)*lhsp(4,i)
                lhsp(3,i2) = lhsp(3,i2) -
     >                        lhsp(1,i2)*lhsp(5,i)
                rhs(m,i2,j,k) = rhs(m,i2,j,k) -
     >                        lhsp(1,i2)*rhs(m,i,j,k)
		m = 5
                fac1       = 1.d0/lhsm(3,i)
                lhsm(4,i)  = fac1*lhsm(4,i)
                lhsm(5,i)  = fac1*lhsm(5,i)
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                lhsm(3,i1) = lhsm(3,i1) -
     >                        lhsm(2,i1)*lhsm(4,i)
                lhsm(4,i1) = lhsm(4,i1) -
     >                        lhsm(2,i1)*lhsm(5,i)
                rhs(m,i1,j,k) = rhs(m,i1,j,k) -
     >                        lhsm(2,i1)*rhs(m,i,j,k)
                lhsm(2,i2) = lhsm(2,i2) -
     >                        lhsm(1,i2)*lhsm(4,i)
                lhsm(3,i2) = lhsm(3,i2) -
     >                        lhsm(1,i2)*lhsm(5,i)
                rhs(m,i2,j,k) = rhs(m,i2,j,k) -
     >                        lhsm(1,i2)*rhs(m,i,j,k)
             end do

c---------------------------------------------------------------------
c         And again the last two rows separately
c---------------------------------------------------------------------
             i  = grid_points(1)-2
             i1 = grid_points(1)-1
	     m = 4
             fac1       = 1.d0/lhsp(3,i)
             lhsp(4,i)  = fac1*lhsp(4,i)
             lhsp(5,i)  = fac1*lhsp(5,i)
             rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             lhsp(3,i1) = lhsp(3,i1) -
     >                      lhsp(2,i1)*lhsp(4,i)
             lhsp(4,i1) = lhsp(4,i1) -
     >                      lhsp(2,i1)*lhsp(5,i)
             rhs(m,i1,j,k) = rhs(m,i1,j,k) -
     >                      lhsp(2,i1)*rhs(m,i,j,k)
	     m = 5
             fac1       = 1.d0/lhsm(3,i)
             lhsm(4,i)  = fac1*lhsm(4,i)
             lhsm(5,i)  = fac1*lhsm(5,i)
             rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             lhsm(3,i1) = lhsm(3,i1) -
     >                      lhsm(2,i1)*lhsm(4,i)
             lhsm(4,i1) = lhsm(4,i1) -
     >                      lhsm(2,i1)*lhsm(5,i)
             rhs(m,i1,j,k) = rhs(m,i1,j,k) -
     >                      lhsm(2,i1)*rhs(m,i,j,k)
c---------------------------------------------------------------------
c               Scale the last row immediately
c---------------------------------------------------------------------
             rhs(4,i1,j,k) = rhs(4,i1,j,k)/lhsp(3,i1)
             rhs(5,i1,j,k) = rhs(5,i1,j,k)/lhsm(3,i1)


c---------------------------------------------------------------------
c                         BACKSUBSTITUTION 
c---------------------------------------------------------------------


             i  = grid_points(1)-2
             i1 = grid_points(1)-1
             do   m = 1, 3
                rhs(m,i,j,k) = rhs(m,i,j,k) -
     >                             lhs(4,i)*rhs(m,i1,j,k)
             end do

             rhs(4,i,j,k) = rhs(4,i,j,k) -
     >                          lhsp(4,i)*rhs(4,i1,j,k)
             rhs(5,i,j,k) = rhs(5,i,j,k) -
     >                          lhsm(4,i)*rhs(5,i1,j,k)

c---------------------------------------------------------------------
c      The first three factors
c---------------------------------------------------------------------
             do    i = grid_points(1)-3, 0, -1
                i1 = i  + 1
                i2 = i  + 2
                do   m = 1, 3
                   rhs(m,i,j,k) = rhs(m,i,j,k) - 
     >                          lhs(4,i)*rhs(m,i1,j,k) -
     >                          lhs(5,i)*rhs(m,i2,j,k)
                end do

c---------------------------------------------------------------------
c      And the remaining two
c---------------------------------------------------------------------
                rhs(4,i,j,k) = rhs(4,i,j,k) - 
     >                          lhsp(4,i)*rhs(4,i1,j,k) -
     >                          lhsp(5,i)*rhs(4,i2,j,k)
                rhs(5,i,j,k) = rhs(5,i,j,k) - 
     >                          lhsm(4,i)*rhs(5,i1,j,k) -
     >                          lhsm(5,i)*rhs(5,i2,j,k)
             end do
          end do
       end do
       if (timeron) call timer_stop(t_xsolve)

c---------------------------------------------------------------------
c      Do the block-diagonal inversion          
c---------------------------------------------------------------------
       if (timeron) call timer_start(t_ninvr)
       call ninvr
       if (timeron) call timer_stop(t_ninvr)

       return
       end
    






