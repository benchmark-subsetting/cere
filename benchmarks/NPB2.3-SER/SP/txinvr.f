
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  txinvr

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c block-diagonal matrix-vector multiplication                  
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k
       double precision t1, t2, t3, ac, ru1, uu, vv, ww, r1, r2, r3, 
     >                  r4, r5, ac2inv


       do    k = 1, grid_points(3)-2
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2

                ru1 = rho_i(i,j,k)
                uu = us(i,j,k)
                vv = vs(i,j,k)
                ww = ws(i,j,k)
                ac = speed(i,j,k)
                ac2inv = ainv(i,j,k)*ainv(i,j,k)

                r1 = rhs(i,j,k,1)
                r2 = rhs(i,j,k,2)
                r3 = rhs(i,j,k,3)
                r4 = rhs(i,j,k,4)
                r5 = rhs(i,j,k,5)

                t1 = c2 * ac2inv * ( qs(i,j,k)*r1 - uu*r2  - 
     >                  vv*r3 - ww*r4 + r5 )
                t2 = bt * ru1 * ( uu * r1 - r2 )
                t3 = ( bt * ru1 * ac ) * t1

                rhs(i,j,k,1) = r1 - t1
                rhs(i,j,k,2) = - ru1 * ( ww*r1 - r4 )
                rhs(i,j,k,3) =   ru1 * ( vv*r1 - r3 )
                rhs(i,j,k,4) = - t2 + t3
                rhs(i,j,k,5) =   t2 + t3

             end do
          end do
       end do

       return
       end


