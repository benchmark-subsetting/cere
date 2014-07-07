
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine lhsy

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c This function computes the left hand side for the three y-factors   
c---------------------------------------------------------------------

       include 'header.h'

       double precision ru1
       integer          i, j, k


c---------------------------------------------------------------------
c      first fill the lhs for the u-eigenvalue         
c---------------------------------------------------------------------
       do  k = 1, grid_points(3)-2
          do  i = 1, grid_points(1)-2

             do  j = 1-1, grid_points(2)-1
                ru1 = c3c4*rho_i(i,j,k)
                cv(j) = vs(i,j,k)
                rhoq(j) = dmax1( dy3 + con43 * ru1,
     >                           dy5 + c1c5*ru1,
     >                           dymax + ru1,
     >                           dy1)
             end do
            
             do  j = 1, grid_points(2)-2
                lhs(i,j,k,1) =  0.0d0
                lhs(i,j,k,2) = -dtty2 * cv(j-1) - dtty1 * rhoq(j-1)
                lhs(i,j,k,3) =  1.0 + c2dtty1 * rhoq(j)
                lhs(i,j,k,4) =  dtty2 * cv(j+1) - dtty1 * rhoq(j+1)
                lhs(i,j,k,5) =  0.0d0
             end do
          end do
       end do

c---------------------------------------------------------------------
c      add fourth order dissipation                             
c---------------------------------------------------------------------

       j = 1
       do   k = 1, grid_points(3)-2
          do   i = 1, grid_points(1)-2

             lhs(i,j,k,3) = lhs(i,j,k,3) + comz5
             lhs(i,j,k,4) = lhs(i,j,k,4) - comz4
             lhs(i,j,k,5) = lhs(i,j,k,5) + comz1
       
             lhs(i,j+1,k,2) = lhs(i,j+1,k,2) - comz4
             lhs(i,j+1,k,3) = lhs(i,j+1,k,3) + comz6
             lhs(i,j+1,k,4) = lhs(i,j+1,k,4) - comz4
             lhs(i,j+1,k,5) = lhs(i,j+1,k,5) + comz1
          end do
       end do

       do   k = 1, grid_points(3)-2
          do   j=3, grid_points(2)-4
             do   i = 1, grid_points(1)-2

                lhs(i,j,k,1) = lhs(i,j,k,1) + comz1
                lhs(i,j,k,2) = lhs(i,j,k,2) - comz4
                lhs(i,j,k,3) = lhs(i,j,k,3) + comz6
                lhs(i,j,k,4) = lhs(i,j,k,4) - comz4
                lhs(i,j,k,5) = lhs(i,j,k,5) + comz1
             end do
          end do
       end do

       j = grid_points(2)-3
       do   k = 1, grid_points(3)-2
          do   i = 1, grid_points(1)-2
             lhs(i,j,k,1) = lhs(i,j,k,1) + comz1
             lhs(i,j,k,2) = lhs(i,j,k,2) - comz4
             lhs(i,j,k,3) = lhs(i,j,k,3) + comz6
             lhs(i,j,k,4) = lhs(i,j,k,4) - comz4

             lhs(i,j+1,k,1) = lhs(i,j+1,k,1) + comz1
             lhs(i,j+1,k,2) = lhs(i,j+1,k,2) - comz4
             lhs(i,j+1,k,3) = lhs(i,j+1,k,3) + comz5
          end do
       end do

c---------------------------------------------------------------------
c      subsequently, do the other two factors                    
c---------------------------------------------------------------------
       do    k = 1, grid_points(3)-2
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2
                lhs(i,j,k,1+5)  = lhs(i,j,k,1)
                lhs(i,j,k,2+5)  = lhs(i,j,k,2) - 
     >                            dtty2 * speed(i,j-1,k)
                lhs(i,j,k,3+5)  = lhs(i,j,k,3)
                lhs(i,j,k,4+5)  = lhs(i,j,k,4) + 
     >                            dtty2 * speed(i,j+1,k)
                lhs(i,j,k,5+5) = lhs(i,j,k,5)
                lhs(i,j,k,1+10) = lhs(i,j,k,1)
                lhs(i,j,k,2+10) = lhs(i,j,k,2) + 
     >                            dtty2 * speed(i,j-1,k)
                lhs(i,j,k,3+10) = lhs(i,j,k,3)
                lhs(i,j,k,4+10) = lhs(i,j,k,4) - 
     >                            dtty2 * speed(i,j+1,k)
                lhs(i,j,k,5+10) = lhs(i,j,k,5)
             end do
          end do
       end do

       return
       end



