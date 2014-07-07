
c---------------------------------------------------------------------
c---------------------------------------------------------------------

       subroutine  initialize

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c This subroutine initializes the field variable u using 
c tri-linear transfinite interpolation of the boundary values     
c---------------------------------------------------------------------

       include 'header.h'
  
       integer i, j, k, m, ix, iy, iz
       double precision  xi, eta, zeta, Pface(5,3,2), Pxi, Peta, 
     >                   Pzeta, temp(5)

c---------------------------------------------------------------------
c  Later (in compute_rhs) we compute 1/u for every element. A few of 
c  the corner elements are not used, but it convenient (and faster) 
c  to compute the whole thing with a simple loop. Make sure those 
c  values are nonzero by initializing the whole thing here. 
c---------------------------------------------------------------------

      do k = 0, IMAX-1
         do j = 0, IMAX-1
            do i = 0, IMAX-1
               u(i, j, k, 1) = 1.0
               u(i, j, k, 2) = 0.0
               u(i, j, k, 3) = 0.0
               u(i, j, k, 4) = 0.0
               u(i, j, k, 5) = 1.0
            end do
         end do
      end do

c---------------------------------------------------------------------
c first store the "interpolated" values everywhere on the grid    
c---------------------------------------------------------------------

          do  k = 0, grid_points(3)-1
             zeta = dble(k) * dnzm1
             do  j = 0, grid_points(2)-1
                eta = dble(j) * dnym1
                do   i = 0, grid_points(1)-1
                   xi = dble(i) * dnxm1
                  
                   do ix = 1, 2
                      call exact_solution(dble(ix-1), eta, zeta, 
     >                                    Pface(1,1,ix))
                   end do

                   do    iy = 1, 2
                      call exact_solution(xi, dble(iy-1) , zeta, 
     >                                    Pface(1,2,iy))
                   end do

                   do    iz = 1, 2
                      call exact_solution(xi, eta, dble(iz-1),   
     >                                    Pface(1,3,iz))
                   end do

                   do   m = 1, 5
                      Pxi   = xi   * Pface(m,1,2) + 
     >                        (1.0d0-xi)   * Pface(m,1,1)
                      Peta  = eta  * Pface(m,2,2) + 
     >                        (1.0d0-eta)  * Pface(m,2,1)
                      Pzeta = zeta * Pface(m,3,2) + 
     >                        (1.0d0-zeta) * Pface(m,3,1)
 
                      u(i,j,k,m) = Pxi + Peta + Pzeta - 
     >                          Pxi*Peta - Pxi*Pzeta - Peta*Pzeta + 
     >                          Pxi*Peta*Pzeta

                   end do
                end do
             end do
          end do


c---------------------------------------------------------------------
c now store the exact values on the boundaries        
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c west face                                                  
c---------------------------------------------------------------------

       xi = 0.0d0
       i  = 0
       do  k = 0, grid_points(3)-1
          zeta = dble(k) * dnzm1
          do   j = 0, grid_points(2)-1
             eta = dble(j) * dnym1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(i,j,k,m) = temp(m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c east face                                                      
c---------------------------------------------------------------------

       xi = 1.0d0
       i  = grid_points(1)-1
       do   k = 0, grid_points(3)-1
          zeta = dble(k) * dnzm1
          do   j = 0, grid_points(2)-1
             eta = dble(j) * dnym1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(i,j,k,m) = temp(m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c south face                                                 
c---------------------------------------------------------------------

       eta = 0.0d0
       j   = 0
       do  k = 0, grid_points(3)-1
          zeta = dble(k) * dnzm1
          do   i = 0, grid_points(1)-1
             xi = dble(i) * dnxm1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(i,j,k,m) = temp(m)
             end do
          end do
       end do


c---------------------------------------------------------------------
c north face                                    
c---------------------------------------------------------------------

       eta = 1.0d0
       j   = grid_points(2)-1
       do   k = 0, grid_points(3)-1
          zeta = dble(k) * dnzm1
          do   i = 0, grid_points(1)-1
             xi = dble(i) * dnxm1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(i,j,k,m) = temp(m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c bottom face                                       
c---------------------------------------------------------------------

       zeta = 0.0d0
       k    = 0
       do   i =0, grid_points(1)-1
          xi = dble(i) *dnxm1
          do   j = 0, grid_points(2)-1
             eta = dble(j) * dnym1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(i,j,k,m) = temp(m)
             end do
          end do
       end do

c---------------------------------------------------------------------
c top face     
c---------------------------------------------------------------------

       zeta = 1.0d0
       k    = grid_points(3)-1
       do   i =0, grid_points(1)-1
          xi = dble(i) * dnxm1
          do   j = 0, grid_points(2)-1
             eta = dble(j) * dnym1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(i,j,k,m) = temp(m)
             end do
          end do
       end do

       return
       end


       subroutine lhsinit

       include 'header.h'
       
       integer i, j, k, c, n

       c = 1

c---------------------------------------------------------------------
c     zap the whole left hand side for starters
c---------------------------------------------------------------------
       do  n = 1, 15
          do  k = 0, grid_points(3)-1
             do  j = 0, grid_points(2)-1
                do  i = 0, grid_points(1)-1
                   lhs(i,j,k,n) = 0.0d0
                end do
             end do
          end do
       end do

c---------------------------------------------------------------------
c      next, set all diagonal values to 1. This is overkill, but 
c      convenient
c---------------------------------------------------------------------
       do   n = 1, 3
          do   k = 0, grid_points(3)-1
             do   j = 0, grid_points(2)-1
                do   i = 0, grid_points(1)-1
                   lhs(i,j,k,5*n-2) = 1.0d0
                end do
             end do
          end do
       end do

 
       return
       end



