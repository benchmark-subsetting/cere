c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine lhsx

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c     This function computes the left hand side in the xi-direction
c---------------------------------------------------------------------

      include 'header.h'

      integer i, j, k

c---------------------------------------------------------------------
c     determine a (labeled f) and n jacobians
c---------------------------------------------------------------------
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 0, grid_points(1)-1

               tmp1 = 1.0d+00 / u(1,i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2
c---------------------------------------------------------------------
c     
c---------------------------------------------------------------------
               fjac(1, 1, i, j, k) = 0.0d+00
               fjac(1, 2, i, j, k) = 1.0d+00
               fjac(1, 3, i, j, k) = 0.0d+00
               fjac(1, 4, i, j, k) = 0.0d+00
               fjac(1, 5, i, j, k) = 0.0d+00

               fjac(2, 1, i, j, k) = -(u(2,i,j,k) * tmp2 * 
     >              u(2,i,j,k))
     >              + c2 * 0.50d+00 * (u(2,i,j,k) * u(2,i,j,k)
     >              + u(3,i,j,k) * u(3,i,j,k)
     >              + u(4,i,j,k) * u(4,i,j,k) ) * tmp2
               fjac(2,2,i,j,k) = ( 2.0d+00 - c2 )
     >              * ( u(2,i,j,k) / u(1,i,j,k) )
               fjac(2,3,i,j,k) = - c2 * ( u(3,i,j,k) * tmp1 )
               fjac(2,4,i,j,k) = - c2 * ( u(4,i,j,k) * tmp1 )
               fjac(2,5,i,j,k) = c2

               fjac(3,1,i,j,k) = - ( u(2,i,j,k)*u(3,i,j,k) ) * tmp2
               fjac(3,2,i,j,k) = u(3,i,j,k) * tmp1
               fjac(3,3,i,j,k) = u(2,i,j,k) * tmp1
               fjac(3,4,i,j,k) = 0.0d+00
               fjac(3,5,i,j,k) = 0.0d+00

               fjac(4,1,i,j,k) = - ( u(2,i,j,k)*u(4,i,j,k) ) * tmp2
               fjac(4,2,i,j,k) = u(4,i,j,k) * tmp1
               fjac(4,3,i,j,k) = 0.0d+00
               fjac(4,4,i,j,k) = u(2,i,j,k) * tmp1
               fjac(4,5,i,j,k) = 0.0d+00

               fjac(5,1,i,j,k) = ( c2 * (  u(2,i,j,k) * u(2,i,j,k)
     >              + u(3,i,j,k) * u(3,i,j,k)
     >              + u(4,i,j,k) * u(4,i,j,k) ) * tmp2
     >              - c1 * ( u(5,i,j,k) * tmp1 ) )
     >              * ( u(2,i,j,k) * tmp1 )
               fjac(5,2,i,j,k) = c1 *  u(5,i,j,k) * tmp1 
     >              - 0.50d+00 * c2
     >              * (  3.0d+00*u(2,i,j,k)*u(2,i,j,k)
     >              + u(3,i,j,k)*u(3,i,j,k)
     >              + u(4,i,j,k)*u(4,i,j,k) ) * tmp2
               fjac(5,3,i,j,k) = - c2 * ( u(3,i,j,k)*u(2,i,j,k) )
     >              * tmp2
               fjac(5,4,i,j,k) = - c2 * ( u(4,i,j,k)*u(2,i,j,k) )
     >              * tmp2
               fjac(5,5,i,j,k) = c1 * ( u(2,i,j,k) * tmp1 )

               njac(1,1,i,j,k) = 0.0d+00
               njac(1,2,i,j,k) = 0.0d+00
               njac(1,3,i,j,k) = 0.0d+00
               njac(1,4,i,j,k) = 0.0d+00
               njac(1,5,i,j,k) = 0.0d+00

               njac(2,1,i,j,k) = - con43 * c3c4 * tmp2 * u(2,i,j,k)
               njac(2,2,i,j,k) =   con43 * c3c4 * tmp1
               njac(2,3,i,j,k) =   0.0d+00
               njac(2,4,i,j,k) =   0.0d+00
               njac(2,5,i,j,k) =   0.0d+00

               njac(3,1,i,j,k) = - c3c4 * tmp2 * u(3,i,j,k)
               njac(3,2,i,j,k) =   0.0d+00
               njac(3,3,i,j,k) =   c3c4 * tmp1
               njac(3,4,i,j,k) =   0.0d+00
               njac(3,5,i,j,k) =   0.0d+00

               njac(4,1,i,j,k) = - c3c4 * tmp2 * u(4,i,j,k)
               njac(4,2,i,j,k) =   0.0d+00 
               njac(4,3,i,j,k) =   0.0d+00
               njac(4,4,i,j,k) =   c3c4 * tmp1
               njac(4,5,i,j,k) =   0.0d+00

               njac(5,1,i,j,k) = - ( con43 * c3c4
     >              - c1345 ) * tmp3 * (u(2,i,j,k)**2)
     >              - ( c3c4 - c1345 ) * tmp3 * (u(3,i,j,k)**2)
     >              - ( c3c4 - c1345 ) * tmp3 * (u(4,i,j,k)**2)
     >              - c1345 * tmp2 * u(5,i,j,k)

               njac(5,2,i,j,k) = ( con43 * c3c4
     >              - c1345 ) * tmp2 * u(2,i,j,k)
               njac(5,3,i,j,k) = ( c3c4 - c1345 ) * tmp2 * u(3,i,j,k)
               njac(5,4,i,j,k) = ( c3c4 - c1345 ) * tmp2 * u(4,i,j,k)
               njac(5,5,i,j,k) = ( c1345 ) * tmp1

            enddo
c---------------------------------------------------------------------
c     now jacobians set, so form left hand side in x direction
c---------------------------------------------------------------------
            do i = 1, grid_points(1)-2

               tmp1 = dt * tx1
               tmp2 = dt * tx2

               lhs(1,1,aa,i,j,k) = - tmp2 * fjac(1,1,i-1,j,k)
     >              - tmp1 * njac(1,1,i-1,j,k)
     >              - tmp1 * dx1 
               lhs(1,2,aa,i,j,k) = - tmp2 * fjac(1,2,i-1,j,k)
     >              - tmp1 * njac(1,2,i-1,j,k)
               lhs(1,3,aa,i,j,k) = - tmp2 * fjac(1,3,i-1,j,k)
     >              - tmp1 * njac(1,3,i-1,j,k)
               lhs(1,4,aa,i,j,k) = - tmp2 * fjac(1,4,i-1,j,k)
     >              - tmp1 * njac(1,4,i-1,j,k)
               lhs(1,5,aa,i,j,k) = - tmp2 * fjac(1,5,i-1,j,k)
     >              - tmp1 * njac(1,5,i-1,j,k)

               lhs(2,1,aa,i,j,k) = - tmp2 * fjac(2,1,i-1,j,k)
     >              - tmp1 * njac(2,1,i-1,j,k)
               lhs(2,2,aa,i,j,k) = - tmp2 * fjac(2,2,i-1,j,k)
     >              - tmp1 * njac(2,2,i-1,j,k)
     >              - tmp1 * dx2
               lhs(2,3,aa,i,j,k) = - tmp2 * fjac(2,3,i-1,j,k)
     >              - tmp1 * njac(2,3,i-1,j,k)
               lhs(2,4,aa,i,j,k) = - tmp2 * fjac(2,4,i-1,j,k)
     >              - tmp1 * njac(2,4,i-1,j,k)
               lhs(2,5,aa,i,j,k) = - tmp2 * fjac(2,5,i-1,j,k)
     >              - tmp1 * njac(2,5,i-1,j,k)

               lhs(3,1,aa,i,j,k) = - tmp2 * fjac(3,1,i-1,j,k)
     >              - tmp1 * njac(3,1,i-1,j,k)
               lhs(3,2,aa,i,j,k) = - tmp2 * fjac(3,2,i-1,j,k)
     >              - tmp1 * njac(3,2,i-1,j,k)
               lhs(3,3,aa,i,j,k) = - tmp2 * fjac(3,3,i-1,j,k)
     >              - tmp1 * njac(3,3,i-1,j,k)
     >              - tmp1 * dx3 
               lhs(3,4,aa,i,j,k) = - tmp2 * fjac(3,4,i-1,j,k)
     >              - tmp1 * njac(3,4,i-1,j,k)
               lhs(3,5,aa,i,j,k) = - tmp2 * fjac(3,5,i-1,j,k)
     >              - tmp1 * njac(3,5,i-1,j,k)

               lhs(4,1,aa,i,j,k) = - tmp2 * fjac(4,1,i-1,j,k)
     >              - tmp1 * njac(4,1,i-1,j,k)
               lhs(4,2,aa,i,j,k) = - tmp2 * fjac(4,2,i-1,j,k)
     >              - tmp1 * njac(4,2,i-1,j,k)
               lhs(4,3,aa,i,j,k) = - tmp2 * fjac(4,3,i-1,j,k)
     >              - tmp1 * njac(4,3,i-1,j,k)
               lhs(4,4,aa,i,j,k) = - tmp2 * fjac(4,4,i-1,j,k)
     >              - tmp1 * njac(4,4,i-1,j,k)
     >              - tmp1 * dx4
               lhs(4,5,aa,i,j,k) = - tmp2 * fjac(4,5,i-1,j,k)
     >              - tmp1 * njac(4,5,i-1,j,k)

               lhs(5,1,aa,i,j,k) = - tmp2 * fjac(5,1,i-1,j,k)
     >              - tmp1 * njac(5,1,i-1,j,k)
               lhs(5,2,aa,i,j,k) = - tmp2 * fjac(5,2,i-1,j,k)
     >              - tmp1 * njac(5,2,i-1,j,k)
               lhs(5,3,aa,i,j,k) = - tmp2 * fjac(5,3,i-1,j,k)
     >              - tmp1 * njac(5,3,i-1,j,k)
               lhs(5,4,aa,i,j,k) = - tmp2 * fjac(5,4,i-1,j,k)
     >              - tmp1 * njac(5,4,i-1,j,k)
               lhs(5,5,aa,i,j,k) = - tmp2 * fjac(5,5,i-1,j,k)
     >              - tmp1 * njac(5,5,i-1,j,k)
     >              - tmp1 * dx5

               lhs(1,1,bb,i,j,k) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(1,1,i,j,k)
     >              + tmp1 * 2.0d+00 * dx1
               lhs(1,2,bb,i,j,k) = tmp1 * 2.0d+00 * njac(1,2,i,j,k)
               lhs(1,3,bb,i,j,k) = tmp1 * 2.0d+00 * njac(1,3,i,j,k)
               lhs(1,4,bb,i,j,k) = tmp1 * 2.0d+00 * njac(1,4,i,j,k)
               lhs(1,5,bb,i,j,k) = tmp1 * 2.0d+00 * njac(1,5,i,j,k)

               lhs(2,1,bb,i,j,k) = tmp1 * 2.0d+00 * njac(2,1,i,j,k)
               lhs(2,2,bb,i,j,k) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(2,2,i,j,k)
     >              + tmp1 * 2.0d+00 * dx2
               lhs(2,3,bb,i,j,k) = tmp1 * 2.0d+00 * njac(2,3,i,j,k)
               lhs(2,4,bb,i,j,k) = tmp1 * 2.0d+00 * njac(2,4,i,j,k)
               lhs(2,5,bb,i,j,k) = tmp1 * 2.0d+00 * njac(2,5,i,j,k)

               lhs(3,1,bb,i,j,k) = tmp1 * 2.0d+00 * njac(3,1,i,j,k)
               lhs(3,2,bb,i,j,k) = tmp1 * 2.0d+00 * njac(3,2,i,j,k)
               lhs(3,3,bb,i,j,k) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(3,3,i,j,k)
     >              + tmp1 * 2.0d+00 * dx3
               lhs(3,4,bb,i,j,k) = tmp1 * 2.0d+00 * njac(3,4,i,j,k)
               lhs(3,5,bb,i,j,k) = tmp1 * 2.0d+00 * njac(3,5,i,j,k)

               lhs(4,1,bb,i,j,k) = tmp1 * 2.0d+00 * njac(4,1,i,j,k)
               lhs(4,2,bb,i,j,k) = tmp1 * 2.0d+00 * njac(4,2,i,j,k)
               lhs(4,3,bb,i,j,k) = tmp1 * 2.0d+00 * njac(4,3,i,j,k)
               lhs(4,4,bb,i,j,k) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(4,4,i,j,k)
     >              + tmp1 * 2.0d+00 * dx4
               lhs(4,5,bb,i,j,k) = tmp1 * 2.0d+00 * njac(4,5,i,j,k)

               lhs(5,1,bb,i,j,k) = tmp1 * 2.0d+00 * njac(5,1,i,j,k)
               lhs(5,2,bb,i,j,k) = tmp1 * 2.0d+00 * njac(5,2,i,j,k)
               lhs(5,3,bb,i,j,k) = tmp1 * 2.0d+00 * njac(5,3,i,j,k)
               lhs(5,4,bb,i,j,k) = tmp1 * 2.0d+00 * njac(5,4,i,j,k)
               lhs(5,5,bb,i,j,k) = 1.0d+00
     >              + tmp1 * 2.0d+00 * njac(5,5,i,j,k)
     >              + tmp1 * 2.0d+00 * dx5

               lhs(1,1,cc,i,j,k) =  tmp2 * fjac(1,1,i+1,j,k)
     >              - tmp1 * njac(1,1,i+1,j,k)
     >              - tmp1 * dx1
               lhs(1,2,cc,i,j,k) =  tmp2 * fjac(1,2,i+1,j,k)
     >              - tmp1 * njac(1,2,i+1,j,k)
               lhs(1,3,cc,i,j,k) =  tmp2 * fjac(1,3,i+1,j,k)
     >              - tmp1 * njac(1,3,i+1,j,k)
               lhs(1,4,cc,i,j,k) =  tmp2 * fjac(1,4,i+1,j,k)
     >              - tmp1 * njac(1,4,i+1,j,k)
               lhs(1,5,cc,i,j,k) =  tmp2 * fjac(1,5,i+1,j,k)
     >              - tmp1 * njac(1,5,i+1,j,k)

               lhs(2,1,cc,i,j,k) =  tmp2 * fjac(2,1,i+1,j,k)
     >              - tmp1 * njac(2,1,i+1,j,k)
               lhs(2,2,cc,i,j,k) =  tmp2 * fjac(2,2,i+1,j,k)
     >              - tmp1 * njac(2,2,i+1,j,k)
     >              - tmp1 * dx2
               lhs(2,3,cc,i,j,k) =  tmp2 * fjac(2,3,i+1,j,k)
     >              - tmp1 * njac(2,3,i+1,j,k)
               lhs(2,4,cc,i,j,k) =  tmp2 * fjac(2,4,i+1,j,k)
     >              - tmp1 * njac(2,4,i+1,j,k)
               lhs(2,5,cc,i,j,k) =  tmp2 * fjac(2,5,i+1,j,k)
     >              - tmp1 * njac(2,5,i+1,j,k)

               lhs(3,1,cc,i,j,k) =  tmp2 * fjac(3,1,i+1,j,k)
     >              - tmp1 * njac(3,1,i+1,j,k)
               lhs(3,2,cc,i,j,k) =  tmp2 * fjac(3,2,i+1,j,k)
     >              - tmp1 * njac(3,2,i+1,j,k)
               lhs(3,3,cc,i,j,k) =  tmp2 * fjac(3,3,i+1,j,k)
     >              - tmp1 * njac(3,3,i+1,j,k)
     >              - tmp1 * dx3
               lhs(3,4,cc,i,j,k) =  tmp2 * fjac(3,4,i+1,j,k)
     >              - tmp1 * njac(3,4,i+1,j,k)
               lhs(3,5,cc,i,j,k) =  tmp2 * fjac(3,5,i+1,j,k)
     >              - tmp1 * njac(3,5,i+1,j,k)

               lhs(4,1,cc,i,j,k) =  tmp2 * fjac(4,1,i+1,j,k)
     >              - tmp1 * njac(4,1,i+1,j,k)
               lhs(4,2,cc,i,j,k) =  tmp2 * fjac(4,2,i+1,j,k)
     >              - tmp1 * njac(4,2,i+1,j,k)
               lhs(4,3,cc,i,j,k) =  tmp2 * fjac(4,3,i+1,j,k)
     >              - tmp1 * njac(4,3,i+1,j,k)
               lhs(4,4,cc,i,j,k) =  tmp2 * fjac(4,4,i+1,j,k)
     >              - tmp1 * njac(4,4,i+1,j,k)
     >              - tmp1 * dx4
               lhs(4,5,cc,i,j,k) =  tmp2 * fjac(4,5,i+1,j,k)
     >              - tmp1 * njac(4,5,i+1,j,k)

               lhs(5,1,cc,i,j,k) =  tmp2 * fjac(5,1,i+1,j,k)
     >              - tmp1 * njac(5,1,i+1,j,k)
               lhs(5,2,cc,i,j,k) =  tmp2 * fjac(5,2,i+1,j,k)
     >              - tmp1 * njac(5,2,i+1,j,k)
               lhs(5,3,cc,i,j,k) =  tmp2 * fjac(5,3,i+1,j,k)
     >              - tmp1 * njac(5,3,i+1,j,k)
               lhs(5,4,cc,i,j,k) =  tmp2 * fjac(5,4,i+1,j,k)
     >              - tmp1 * njac(5,4,i+1,j,k)
               lhs(5,5,cc,i,j,k) =  tmp2 * fjac(5,5,i+1,j,k)
     >              - tmp1 * njac(5,5,i+1,j,k)
     >              - tmp1 * dx5

            enddo
         enddo
      enddo

      return
      end



