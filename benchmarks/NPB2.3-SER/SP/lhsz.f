
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine lhsz

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c This function computes the left hand side for the three z-factors   
c---------------------------------------------------------------------

       include 'header.h'

       double precision ru1
       integer i, j, k

c---------------------------------------------------------------------
c first fill the lhs for the u-eigenvalue                          
c---------------------------------------------------------------------
       do   j = 1, grid_points(2)-2
          do   i = 1, grid_points(1)-2

             do   k = 1-1, grid_points(3)-1
                ru1 = c3c4*rho_i(i,j,k)
                cv(k) = ws(i,j,k)
                rhos(k) = dmax1(dz4 + con43 * ru1,
     >                          dz5 + c1c5 * ru1,
     >                          dzmax + ru1,
     >                          dz1)
             end do

             do   k =  1, grid_points(3)-2
                lhs(i,j,k,1) =  0.0d0
                lhs(i,j,k,2) = -dttz2 * cv(k-1) - dttz1 * rhos(k-1)
                lhs(i,j,k,3) =  1.0 + c2dttz1 * rhos(k)
                lhs(i,j,k,4) =  dttz2 * cv(k+1) - dttz1 * rhos(k+1)
                lhs(i,j,k,5) =  0.0d0
             end do
          end do
       end do

c---------------------------------------------------------------------
c      add fourth order dissipation                                  
c---------------------------------------------------------------------

       k = 1
       do    j = 1, grid_points(2)-2
          do    i = 1, grid_points(1)-2
             lhs(i,j,k,3) = lhs(i,j,k,3) + comz5
             lhs(i,j,k,4) = lhs(i,j,k,4) - comz4
             lhs(i,j,k,5) = lhs(i,j,k,5) + comz1

             lhs(i,j,k+1,2) = lhs(i,j,k+1,2) - comz4
             lhs(i,j,k+1,3) = lhs(i,j,k+1,3) + comz6
             lhs(i,j,k+1,4) = lhs(i,j,k+1,4) - comz4
             lhs(i,j,k+1,5) = lhs(i,j,k+1,5) + comz1
          end do
       end do

       do    k = 3, grid_points(3)-4
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2
                lhs(i,j,k,1) = lhs(i,j,k,1) + comz1
                lhs(i,j,k,2) = lhs(i,j,k,2) - comz4
                lhs(i,j,k,3) = lhs(i,j,k,3) + comz6
                lhs(i,j,k,4) = lhs(i,j,k,4) - comz4
                lhs(i,j,k,5) = lhs(i,j,k,5) + comz1
             end do
          end do
       end do

       k = grid_points(3)-3 
       do    j = 1, grid_points(2)-2
          do    i = 1, grid_points(1)-2
             lhs(i,j,k,1) = lhs(i,j,k,1) + comz1
             lhs(i,j,k,2) = lhs(i,j,k,2) - comz4
             lhs(i,j,k,3) = lhs(i,j,k,3) + comz6
             lhs(i,j,k,4) = lhs(i,j,k,4) - comz4

             lhs(i,j,k+1,1) = lhs(i,j,k+1,1) + comz1
             lhs(i,j,k+1,2) = lhs(i,j,k+1,2) - comz4
             lhs(i,j,k+1,3) = lhs(i,j,k+1,3) + comz5
          end do
       end do


c---------------------------------------------------------------------
c      subsequently, fill the other factors (u+c), (u-c) 
c---------------------------------------------------------------------
       do    k = 1, grid_points(3)-2
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2
                lhs(i,j,k,1+5)  = lhs(i,j,k,1)
                lhs(i,j,k,2+5)  = lhs(i,j,k,2) - 
     >                            dttz2 * speed(i,j,k-1)
                lhs(i,j,k,3+5)  = lhs(i,j,k,3)
                lhs(i,j,k,4+5)  = lhs(i,j,k,4) + 
     >                            dttz2 * speed(i,j,k+1)
                lhs(i,j,k,5+5) = lhs(i,j,k,5)
                lhs(i,j,k,1+10) = lhs(i,j,k,1)
                lhs(i,j,k,2+10) = lhs(i,j,k,2) + 
     >                            dttz2 * speed(i,j,k-1)
                lhs(i,j,k,3+10) = lhs(i,j,k,3)
                lhs(i,j,k,4+10) = lhs(i,j,k,4) - 
     >                            dttz2 * speed(i,j,k+1)
                lhs(i,j,k,5+10) = lhs(i,j,k,5)
             end do
          end do
       end do

       return
       end


