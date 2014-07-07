
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  ninvr

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   block-diagonal matrix-vector multiplication              
c---------------------------------------------------------------------

       include 'header.h'

       integer  i, j, k
       double precision r1, r2, r3, r4, r5, t1, t2

       do k = 1, grid_points(3)-2
          do j = 1, grid_points(2)-2
             do i = 1, grid_points(1)-2

                r1 = rhs(i,j,k,1)
                r2 = rhs(i,j,k,2)
                r3 = rhs(i,j,k,3)
                r4 = rhs(i,j,k,4)
                r5 = rhs(i,j,k,5)
               
                t1 = bt * r3
                t2 = 0.5d0 * ( r4 + r5 )

                rhs(i,j,k,1) = -r2
                rhs(i,j,k,2) =  r1
                rhs(i,j,k,3) = bt * ( r4 - r5 )
                rhs(i,j,k,4) = -t1 + t2
                rhs(i,j,k,5) =  t1 + t2
             enddo    
          enddo
       enddo

       return
       end
