
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine lhsx

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c This function computes the left hand side for the three x-factors  
c---------------------------------------------------------------------

       include 'header.h'

       double precision ru1
       integer          i, j, k


c---------------------------------------------------------------------
c      first fill the lhs for the u-eigenvalue                   
c---------------------------------------------------------------------
       do  k = 1, grid_points(3)-2
          do  j = 1, grid_points(2)-2
             do  i = 0, grid_points(1)-1
                ru1 = c3c4*rho_i(i,j,k)
                cv(i) = us(i,j,k)
                rhon(i) = dmax1(dx2+con43*ru1, 
     >                          dx5+c1c5*ru1,
     >                          dxmax+ru1,
     >                          dx1)
             end do

             do  i = 1, grid_points(1)-2
                lhs(i,j,k,1) =   0.0d0
                lhs(i,j,k,2) = - dttx2 * cv(i-1) - dttx1 * rhon(i-1)
                lhs(i,j,k,3) =   1.0d0 + c2dttx1 * rhon(i)
                lhs(i,j,k,4) =   dttx2 * cv(i+1) - dttx1 * rhon(i+1)
                lhs(i,j,k,5) =   0.0d0
             end do
          end do
       end do

c---------------------------------------------------------------------
c      add fourth order dissipation                             
c---------------------------------------------------------------------

       i = 1
       do   k = 1, grid_points(3)-2
          do   j = 1, grid_points(2)-2
             lhs(i,j,k,3) = lhs(i,j,k,3) + comz5
             lhs(i,j,k,4) = lhs(i,j,k,4) - comz4
             lhs(i,j,k,5) = lhs(i,j,k,5) + comz1
  
             lhs(i+1,j,k,2) = lhs(i+1,j,k,2) - comz4
             lhs(i+1,j,k,3) = lhs(i+1,j,k,3) + comz6
             lhs(i+1,j,k,4) = lhs(i+1,j,k,4) - comz4
             lhs(i+1,j,k,5) = lhs(i+1,j,k,5) + comz1
          end do
       end do

       do   k = 1, grid_points(3)-2
          do   j = 1, grid_points(2)-2
             do   i=3, grid_points(1)-4
                lhs(i,j,k,1) = lhs(i,j,k,1) + comz1
                lhs(i,j,k,2) = lhs(i,j,k,2) - comz4
                lhs(i,j,k,3) = lhs(i,j,k,3) + comz6
                lhs(i,j,k,4) = lhs(i,j,k,4) - comz4
                lhs(i,j,k,5) = lhs(i,j,k,5) + comz1
             end do
          end do
       end do


       i = grid_points(1)-3
       do   k = 1, grid_points(3)-2
          do   j = 1, grid_points(2)-2
             lhs(i,j,k,1) = lhs(i,j,k,1) + comz1
             lhs(i,j,k,2) = lhs(i,j,k,2) - comz4
             lhs(i,j,k,3) = lhs(i,j,k,3) + comz6
             lhs(i,j,k,4) = lhs(i,j,k,4) - comz4

             lhs(i+1,j,k,1) = lhs(i+1,j,k,1) + comz1
             lhs(i+1,j,k,2) = lhs(i+1,j,k,2) - comz4
             lhs(i+1,j,k,3) = lhs(i+1,j,k,3) + comz5
          end do
       end do

c---------------------------------------------------------------------
c      subsequently, fill the other factors (u+c), (u-c) by adding to 
c      the first  
c---------------------------------------------------------------------
       do   k = 1, grid_points(3)-2
          do   j = 1, grid_points(2)-2
             do   i = 1, grid_points(1)-2
                lhs(i,j,k,1+5)  = lhs(i,j,k,1)
                lhs(i,j,k,2+5)  = lhs(i,j,k,2) - 
     >                            dttx2 * speed(i-1,j,k)
                lhs(i,j,k,3+5)  = lhs(i,j,k,3)
                lhs(i,j,k,4+5)  = lhs(i,j,k,4) + 
     >                            dttx2 * speed(i+1,j,k)
                lhs(i,j,k,5+5) = lhs(i,j,k,5)
                lhs(i,j,k,1+10) = lhs(i,j,k,1)
                lhs(i,j,k,2+10) = lhs(i,j,k,2) + 
     >                            dttx2 * speed(i-1,j,k)
                lhs(i,j,k,3+10) = lhs(i,j,k,3)
                lhs(i,j,k,4+10) = lhs(i,j,k,4) - 
     >                            dttx2 * speed(i+1,j,k)
                lhs(i,j,k,5+10) = lhs(i,j,k,5)
             end do
          end do
       end do

       return
       end



