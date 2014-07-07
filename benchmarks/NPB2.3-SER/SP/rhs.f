c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine compute_rhs

c---------------------------------------------------------------------
c---------------------------------------------------------------------

       include 'header.h'

       integer i, j, k, m
       double precision aux, rho_inv, uijk, up1, um1, vijk, vp1, vm1,
     >                  wijk, wp1, wm1


c---------------------------------------------------------------------
c      compute the reciprocal of density, and the kinetic energy, 
c      and the speed of sound. 
c---------------------------------------------------------------------

       do    k = 0, grid_points(3)-1
          do    j = 0, grid_points(2)-1
             do    i = 0, grid_points(1)-1
                rho_inv = 1.0d0/u(i,j,k,1)
                rho_i(i,j,k) = rho_inv
                us(i,j,k) = u(i,j,k,2) * rho_inv
                vs(i,j,k) = u(i,j,k,3) * rho_inv
                ws(i,j,k) = u(i,j,k,4) * rho_inv
                square(i,j,k)     = 0.5d0* (
     >                        u(i,j,k,2)*u(i,j,k,2) + 
     >                        u(i,j,k,3)*u(i,j,k,3) +
     >                        u(i,j,k,4)*u(i,j,k,4) ) * rho_inv
                qs(i,j,k) = square(i,j,k) * rho_inv
c---------------------------------------------------------------------
c               (don't need speed and ainx until the lhs computation)
c---------------------------------------------------------------------
                aux = c1c2*rho_inv* (u(i,j,k,5) - square(i,j,k))
                aux = dsqrt(aux)
                speed(i,j,k) = aux
                ainv(i,j,k)  = 1.0d0/aux
             end do
          end do
       end do

c---------------------------------------------------------------------
c copy the exact forcing term to the right hand side;  because 
c this forcing term is known, we can store it on the whole grid
c including the boundary                   
c---------------------------------------------------------------------

       do   m = 1, 5
          do   k = 0, grid_points(3)-1
             do   j = 0, grid_points(2)-1
                do   i = 0, grid_points(1)-1
                   rhs(i,j,k,m) = forcing(i,j,k,m)
                end do
             end do
          end do
       end do


c---------------------------------------------------------------------
c      compute xi-direction fluxes 
c---------------------------------------------------------------------
       do    k = 1, grid_points(3)-2
          do    j = 1, grid_points(2)-2
             do    i = 1, grid_points(1)-2
                uijk = us(i,j,k)
                up1  = us(i+1,j,k)
                um1  = us(i-1,j,k)

                rhs(i,j,k,1) = rhs(i,j,k,1) + dx1tx1 * 
     >                    (u(i+1,j,k,1) - 2.0d0*u(i,j,k,1) + 
     >                     u(i-1,j,k,1)) -
     >                    tx2 * (u(i+1,j,k,2) - u(i-1,j,k,2))

                rhs(i,j,k,2) = rhs(i,j,k,2) + dx2tx1 * 
     >                    (u(i+1,j,k,2) - 2.0d0*u(i,j,k,2) + 
     >                     u(i-1,j,k,2)) +
     >                    xxcon2*con43 * (up1 - 2.0d0*uijk + um1) -
     >                    tx2 * (u(i+1,j,k,2)*up1 - 
     >                           u(i-1,j,k,2)*um1 +
     >                           (u(i+1,j,k,5)- square(i+1,j,k)-
     >                            u(i-1,j,k,5)+ square(i-1,j,k))*
     >                            c2)

                rhs(i,j,k,3) = rhs(i,j,k,3) + dx3tx1 * 
     >                    (u(i+1,j,k,3) - 2.0d0*u(i,j,k,3) +
     >                     u(i-1,j,k,3)) +
     >                    xxcon2 * (vs(i+1,j,k) - 2.0d0*vs(i,j,k) +
     >                              vs(i-1,j,k)) -
     >                    tx2 * (u(i+1,j,k,3)*up1 - 
     >                           u(i-1,j,k,3)*um1)

                rhs(i,j,k,4) = rhs(i,j,k,4) + dx4tx1 * 
     >                    (u(i+1,j,k,4) - 2.0d0*u(i,j,k,4) +
     >                     u(i-1,j,k,4)) +
     >                    xxcon2 * (ws(i+1,j,k) - 2.0d0*ws(i,j,k) +
     >                              ws(i-1,j,k)) -
     >                    tx2 * (u(i+1,j,k,4)*up1 - 
     >                           u(i-1,j,k,4)*um1)

                rhs(i,j,k,5) = rhs(i,j,k,5) + dx5tx1 * 
     >                    (u(i+1,j,k,5) - 2.0d0*u(i,j,k,5) +
     >                     u(i-1,j,k,5)) +
     >                    xxcon3 * (qs(i+1,j,k) - 2.0d0*qs(i,j,k) +
     >                              qs(i-1,j,k)) +
     >                    xxcon4 * (up1*up1 -       2.0d0*uijk*uijk + 
     >                              um1*um1) +
     >                    xxcon5 * (u(i+1,j,k,5)*rho_i(i+1,j,k) - 
     >                              2.0d0*u(i,j,k,5)*rho_i(i,j,k) +
     >                              u(i-1,j,k,5)*rho_i(i-1,j,k)) -
     >                    tx2 * ( (c1*u(i+1,j,k,5) - 
     >                             c2*square(i+1,j,k))*up1 -
     >                            (c1*u(i-1,j,k,5) - 
     >                             c2*square(i-1,j,k))*um1 )
             end do
          end do
       end do

c---------------------------------------------------------------------
c      add fourth order xi-direction dissipation               
c---------------------------------------------------------------------

       i = 1
       do    m = 1, 5
          do    k = 1, grid_points(3)-2
             do    j = 1, grid_points(2)-2
                rhs(i,j,k,m) = rhs(i,j,k,m)- dssp * 
     >                    ( 5.0d0*u(i,j,k,m) - 4.0d0*u(i+1,j,k,m) +
     >                            u(i+2,j,k,m))
             end do
          end do
       end do

       i = 2
       do    m = 1, 5
          do    k = 1, grid_points(3)-2
             do    j = 1, grid_points(2)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp * 
     >                    (-4.0d0*u(i-1,j,k,m) + 6.0d0*u(i,j,k,m) -
     >                      4.0d0*u(i+1,j,k,m) + u(i+2,j,k,m))
             end do
          end do
       end do

       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     j = 1, grid_points(2)-2
                do  i = 3*1,grid_points(1)-3*1-1
                   rhs(i,j,k,m) = rhs(i,j,k,m) - dssp * 
     >                    (  u(i-2,j,k,m) - 4.0d0*u(i-1,j,k,m) + 
     >                     6.0*u(i,j,k,m) - 4.0d0*u(i+1,j,k,m) + 
     >                         u(i+2,j,k,m) )
                end do
             end do
          end do
       end do

       i = grid_points(1)-3
       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     j = 1, grid_points(2)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp *
     >                    ( u(i-2,j,k,m) - 4.0d0*u(i-1,j,k,m) + 
     >                      6.0d0*u(i,j,k,m) - 4.0d0*u(i+1,j,k,m) )
             end do
          end do
       end do

       i = grid_points(1)-2
       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     j = 1, grid_points(2)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp *
     >                    ( u(i-2,j,k,m) - 4.d0*u(i-1,j,k,m) +
     >                      5.d0*u(i,j,k,m) )
             end do
          end do
       end do

c---------------------------------------------------------------------
c      compute eta-direction fluxes 
c---------------------------------------------------------------------
       do     k = 1, grid_points(3)-2
          do     j = 1, grid_points(2)-2
             do     i = 1, grid_points(1)-2
                vijk = vs(i,j,k)
                vp1  = vs(i,j+1,k)
                vm1  = vs(i,j-1,k)
                rhs(i,j,k,1) = rhs(i,j,k,1) + dy1ty1 * 
     >                   (u(i,j+1,k,1) - 2.0d0*u(i,j,k,1) + 
     >                    u(i,j-1,k,1)) -
     >                   ty2 * (u(i,j+1,k,3) - u(i,j-1,k,3))
                rhs(i,j,k,2) = rhs(i,j,k,2) + dy2ty1 * 
     >                   (u(i,j+1,k,2) - 2.0d0*u(i,j,k,2) + 
     >                    u(i,j-1,k,2)) +
     >                   yycon2 * (us(i,j+1,k) - 2.0d0*us(i,j,k) + 
     >                             us(i,j-1,k)) -
     >                   ty2 * (u(i,j+1,k,2)*vp1 - 
     >                          u(i,j-1,k,2)*vm1)
                rhs(i,j,k,3) = rhs(i,j,k,3) + dy3ty1 * 
     >                   (u(i,j+1,k,3) - 2.0d0*u(i,j,k,3) + 
     >                    u(i,j-1,k,3)) +
     >                   yycon2*con43 * (vp1 - 2.0d0*vijk + vm1) -
     >                   ty2 * (u(i,j+1,k,3)*vp1 - 
     >                          u(i,j-1,k,3)*vm1 +
     >                          (u(i,j+1,k,5) - square(i,j+1,k) - 
     >                           u(i,j-1,k,5) + square(i,j-1,k))
     >                          *c2)
                rhs(i,j,k,4) = rhs(i,j,k,4) + dy4ty1 * 
     >                   (u(i,j+1,k,4) - 2.0d0*u(i,j,k,4) + 
     >                    u(i,j-1,k,4)) +
     >                   yycon2 * (ws(i,j+1,k) - 2.0d0*ws(i,j,k) + 
     >                             ws(i,j-1,k)) -
     >                   ty2 * (u(i,j+1,k,4)*vp1 - 
     >                          u(i,j-1,k,4)*vm1)
                rhs(i,j,k,5) = rhs(i,j,k,5) + dy5ty1 * 
     >                   (u(i,j+1,k,5) - 2.0d0*u(i,j,k,5) + 
     >                    u(i,j-1,k,5)) +
     >                   yycon3 * (qs(i,j+1,k) - 2.0d0*qs(i,j,k) + 
     >                             qs(i,j-1,k)) +
     >                   yycon4 * (vp1*vp1       - 2.0d0*vijk*vijk + 
     >                             vm1*vm1) +
     >                   yycon5 * (u(i,j+1,k,5)*rho_i(i,j+1,k) - 
     >                             2.0d0*u(i,j,k,5)*rho_i(i,j,k) +
     >                             u(i,j-1,k,5)*rho_i(i,j-1,k)) -
     >                   ty2 * ((c1*u(i,j+1,k,5) - 
     >                           c2*square(i,j+1,k)) * vp1 -
     >                          (c1*u(i,j-1,k,5) - 
     >                           c2*square(i,j-1,k)) * vm1)
             end do
          end do
       end do

c---------------------------------------------------------------------
c      add fourth order eta-direction dissipation         
c---------------------------------------------------------------------

       j = 1
       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m)- dssp * 
     >                    ( 5.0d0*u(i,j,k,m) - 4.0d0*u(i,j+1,k,m) +
     >                            u(i,j+2,k,m))
             end do
          end do
       end do

       j = 2
       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp * 
     >                    (-4.0d0*u(i,j-1,k,m) + 6.0d0*u(i,j,k,m) -
     >                      4.0d0*u(i,j+1,k,m) + u(i,j+2,k,m))
             end do
          end do
       end do

       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do    j = 3*1, grid_points(2)-3*1-1
                do  i = 1,grid_points(1)-2
                   rhs(i,j,k,m) = rhs(i,j,k,m) - dssp * 
     >                    (  u(i,j-2,k,m) - 4.0d0*u(i,j-1,k,m) + 
     >                     6.0*u(i,j,k,m) - 4.0d0*u(i,j+1,k,m) + 
     >                         u(i,j+2,k,m) )
                end do
             end do
          end do
       end do
 
       j = grid_points(2)-3
       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp *
     >                    ( u(i,j-2,k,m) - 4.0d0*u(i,j-1,k,m) + 
     >                      6.0d0*u(i,j,k,m) - 4.0d0*u(i,j+1,k,m) )
             end do
          end do
       end do

       j = grid_points(2)-2
       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp *
     >                    ( u(i,j-2,k,m) - 4.d0*u(i,j-1,k,m) +
     >                      5.d0*u(i,j,k,m) )
             end do
          end do
       end do

c---------------------------------------------------------------------
c      compute zeta-direction fluxes 
c---------------------------------------------------------------------
       do    k = 1, grid_points(3)-2
          do     j = 1, grid_points(2)-2
             do     i = 1, grid_points(1)-2
                wijk = ws(i,j,k)
                wp1  = ws(i,j,k+1)
                wm1  = ws(i,j,k-1)

                rhs(i,j,k,1) = rhs(i,j,k,1) + dz1tz1 * 
     >                   (u(i,j,k+1,1) - 2.0d0*u(i,j,k,1) + 
     >                    u(i,j,k-1,1)) -
     >                   tz2 * (u(i,j,k+1,4) - u(i,j,k-1,4))
                rhs(i,j,k,2) = rhs(i,j,k,2) + dz2tz1 * 
     >                   (u(i,j,k+1,2) - 2.0d0*u(i,j,k,2) + 
     >                    u(i,j,k-1,2)) +
     >                   zzcon2 * (us(i,j,k+1) - 2.0d0*us(i,j,k) + 
     >                             us(i,j,k-1)) -
     >                   tz2 * (u(i,j,k+1,2)*wp1 - 
     >                          u(i,j,k-1,2)*wm1)
                rhs(i,j,k,3) = rhs(i,j,k,3) + dz3tz1 * 
     >                   (u(i,j,k+1,3) - 2.0d0*u(i,j,k,3) + 
     >                    u(i,j,k-1,3)) +
     >                   zzcon2 * (vs(i,j,k+1) - 2.0d0*vs(i,j,k) + 
     >                             vs(i,j,k-1)) -
     >                   tz2 * (u(i,j,k+1,3)*wp1 - 
     >                          u(i,j,k-1,3)*wm1)
                rhs(i,j,k,4) = rhs(i,j,k,4) + dz4tz1 * 
     >                   (u(i,j,k+1,4) - 2.0d0*u(i,j,k,4) + 
     >                    u(i,j,k-1,4)) +
     >                   zzcon2*con43 * (wp1 - 2.0d0*wijk + wm1) -
     >                   tz2 * (u(i,j,k+1,4)*wp1 - 
     >                          u(i,j,k-1,4)*wm1 +
     >                          (u(i,j,k+1,5) - square(i,j,k+1) - 
     >                           u(i,j,k-1,5) + square(i,j,k-1))
     >                          *c2)
                rhs(i,j,k,5) = rhs(i,j,k,5) + dz5tz1 * 
     >                   (u(i,j,k+1,5) - 2.0d0*u(i,j,k,5) + 
     >                    u(i,j,k-1,5)) +
     >                   zzcon3 * (qs(i,j,k+1) - 2.0d0*qs(i,j,k) + 
     >                             qs(i,j,k-1)) +
     >                   zzcon4 * (wp1*wp1 - 2.0d0*wijk*wijk + 
     >                             wm1*wm1) +
     >                   zzcon5 * (u(i,j,k+1,5)*rho_i(i,j,k+1) - 
     >                             2.0d0*u(i,j,k,5)*rho_i(i,j,k) +
     >                             u(i,j,k-1,5)*rho_i(i,j,k-1)) -
     >                   tz2 * ( (c1*u(i,j,k+1,5) - 
     >                            c2*square(i,j,k+1))*wp1 -
     >                           (c1*u(i,j,k-1,5) - 
     >                            c2*square(i,j,k-1))*wm1)
             end do
          end do
       end do

c---------------------------------------------------------------------
c      add fourth order zeta-direction dissipation                
c---------------------------------------------------------------------

       k = 1
       do     m = 1, 5
          do     j = 1, grid_points(2)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m)- dssp * 
     >                    ( 5.0d0*u(i,j,k,m) - 4.0d0*u(i,j,k+1,m) +
     >                            u(i,j,k+2,m))
             end do
          end do
       end do

       k = 2
       do     m = 1, 5
          do     j = 1, grid_points(2)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp * 
     >                    (-4.0d0*u(i,j,k-1,m) + 6.0d0*u(i,j,k,m) -
     >                      4.0d0*u(i,j,k+1,m) + u(i,j,k+2,m))
             end do
          end do
       end do

       do     m = 1, 5
          do     k = 3*1, grid_points(3)-3*1-1
             do     j = 1, grid_points(2)-2
                do     i = 1,grid_points(1)-2
                   rhs(i,j,k,m) = rhs(i,j,k,m) - dssp * 
     >                    (  u(i,j,k-2,m) - 4.0d0*u(i,j,k-1,m) + 
     >                     6.0*u(i,j,k,m) - 4.0d0*u(i,j,k+1,m) + 
     >                         u(i,j,k+2,m) )
                end do
             end do
          end do
       end do
 
       k = grid_points(3)-3
       do     m = 1, 5
          do     j = 1, grid_points(2)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp *
     >                    ( u(i,j,k-2,m) - 4.0d0*u(i,j,k-1,m) + 
     >                      6.0d0*u(i,j,k,m) - 4.0d0*u(i,j,k+1,m) )
             end do
          end do
       end do

       k = grid_points(3)-2
       do     m = 1, 5
          do     j = 1, grid_points(2)-2
             do     i = 1, grid_points(1)-2
                rhs(i,j,k,m) = rhs(i,j,k,m) - dssp *
     >                    ( u(i,j,k-2,m) - 4.d0*u(i,j,k-1,m) +
     >                      5.d0*u(i,j,k,m) )
             end do
          end do
       end do

       do     m = 1, 5
          do     k = 1, grid_points(3)-2
             do     j = 1, grid_points(2)-2
                do    i = 1, grid_points(1)-2
                   rhs(i,j,k,m) = rhs(i,j,k,m) * dt
                end do
             end do
          end do
       end do
    
       return
       end




