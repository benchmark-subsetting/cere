
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  tzetar

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   block-diagonal matrix-vector multiplication                       
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k
       double precision  t1, t2, t3, ac, xvel, yvel, zvel, r1, r2, r3, 
     >                   r4, r5, btuz, acinv, ac2u, uzik1


       do    k = 1, grid_points(3)-2
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2

                xvel = us(i,j,k)
                yvel = vs(i,j,k)
                zvel = ws(i,j,k)
                ac   = speed(i,j,k)
                acinv = ainv(i,j,k)

                ac2u = ac*ac

                r1 = rhs(i,j,k,1)
                r2 = rhs(i,j,k,2)
                r3 = rhs(i,j,k,3)
                r4 = rhs(i,j,k,4)
                r5 = rhs(i,j,k,5)      

                uzik1 = u(i,j,k,1)
                btuz  = bt * uzik1

                t1 = btuz*acinv * (r4 + r5)
                t2 = r3 + t1
                t3 = btuz * (r4 - r5)

                rhs(i,j,k,1) = t2
                rhs(i,j,k,2) = -uzik1*r2 + xvel*t2
                rhs(i,j,k,3) =  uzik1*r1 + yvel*t2
                rhs(i,j,k,4) =  zvel*t2  + t3
                rhs(i,j,k,5) =  uzik1*(-xvel*r2 + yvel*r1) + 
     >                    qs(i,j,k)*t2 + c2iv*ac2u*t1 + zvel*t3

             end do
          end do
       end do

       return
       end
