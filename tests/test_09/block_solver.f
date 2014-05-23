
        subroutine bi_cgstab_block(x,b,a,axp,ayp,azp,axm,aym,azm,
     $  epsilon,nb,nx,ny,nz,maxit)
c       /* Bi-CGSTAB */
        implicit none
        integer nb,nx,ny,nz,kit, i,j,k,l,m
        integer maxit

        real*8 x(nb,nx,ny,nz),b(nb,nx,ny,nz),r(nb,nx,ny,nz),
     1  rhat(nb,nx,ny,nz),p(nb,nx,ny,nz),v(nb,nx,ny,nz),t(nb,nx,ny,nz)

        real*8 a(nb,nb,nx,ny,nz),
     1  axp(nb,nb,nx,ny,nz),ayp(nb,nb,nx,ny,nz),azp(nb,nb,nx,ny,nz),
     2  axm(nb,nb,nx,ny,nz),aym(nb,nb,nx,ny,nz),azm(nb,nb,nx,ny,nz)

        real*8 epsilon,r2,alpha,beta,rho,oldrho,omega,oldomega,tmp,tmp1


        kit = 0

        do k=1,nz
           do j=1,ny
              do i=1,nx
                 do l=1,nb
                    r(l,i,j,k)=0.0d0
                 enddo
              enddo
           enddo
        enddo

        call mat_times_vec(r,x,a,axp,ayp,azp,axm,aym,azm,
     $  nb,nx,ny,nz)
        r2=0.0d0

        do k=1,nz
           do j=1,ny
              do i=1,nx
                 do l=1,nb
                    r(l,i,j,k) = b(l,i,j,k) - r(l,i,j,k)
                    r2 =r2+r(l,i,j,k)**2
                    p(l,i,j,k) = 0.0d0
                    v(l,i,j,k) = 0.0d0
                    t(l,i,j,k)=0.0d0
                    rhat(l,i,j,k) = r(l,i,j,k)
                 enddo
              enddo
           enddo
        enddo

        oldrho = 1
        oldomega = 1
        alpha = 1

c       write(10,*) 'r2==', r2
        DO WHILE ( (r2 - epsilon). GT. 0.0d0 )
                kit = kit + 1
                rho=0.0d0

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            rho =rho+rhat(l,i,j,k)*r(l,i,j,k)
                         enddo
                      enddo
                   enddo
                enddo
                beta = (rho / oldrho)*(alpha/oldomega)

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            p(l,i,j,k) = r(l,i,j,k) + beta * 
     1                      ( p(l,i,j,k) - oldomega * v(l,i,j,k) )
                         enddo
                      enddo
                   enddo
                enddo
                call mat_times_vec(v,p,a,axp,ayp,azp,axm,aym,azm,
     $          nb,nx,ny,nz)
                tmp=0.0d0

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            tmp=tmp+rhat(l,i,j,k)*v(l,i,j,k)
                         enddo
                      enddo
                   enddo
                enddo
                alpha = rho / tmp

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            r(l,i,j,k)=r(l,i,j,k)-alpha * v(l,i,j,k)
                         enddo
                      enddo
                   enddo
                enddo
                call mat_times_vec(t,r,a,axp,ayp,azp,axm,aym,azm,
     $          nb,nx,ny,nz)
                tmp=0.0d0
                tmp1=0.0d0

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            tmp=tmp+t(l,i,j,k)*r(l,i,j,k)
                            tmp1=tmp1+t(l,i,j,k)*t(l,i,j,k)
                         enddo
                      enddo
                   enddo
                enddo
                omega = tmp/tmp1

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            x(l,i,j,k)= x(l,i,j,k)+alpha*p(l,i,j,k)+
     1                      omega*r(l,i,j,k)
                            r(l,i,j,k)=r(l,i,j,k)-omega*t(l,i,j,k)
                         enddo
                      enddo
                   enddo
                enddo
                oldrho = rho
                oldomega = omega
                r2=0

                do k=1,nz
                   do j=1,ny
                      do i=1,nx
                         do l=1,nb
                            r2=r2+r(l,i,j,k)*r(l,i,j,k)
                         enddo
                      enddo
                   enddo
                enddo

c               print *,'iteration ',kit,'  |residual|^2 =',r2
        END DO
c       write(30,*) 'convergence after ',kit,' iterations.'
       write(10,*) '|residual|^2 =', r2
        maxit = maxit + kit

        RETURN
        END

        subroutine mat_times_vec(y,x,a,axp,ayp,azp,axm,aym,azm,
     $  nb,nx,ny,nz)
        implicit none
        integer nb,nx,ny,nz,i,j,k,m,l,kit,im1,ip1,jm1,jp1,km1,kp1

        real*8 y(nb,nx,ny,nz),x(nb,nx,ny,nz)

        real*8 a(nb,nb,nx,ny,nz),
     1  axp(nb,nb,nx,ny,nz),ayp(nb,nb,nx,ny,nz),azp(nb,nb,nx,ny,nz),
     2  axm(nb,nb,nx,ny,nz),aym(nb,nb,nx,ny,nz),azm(nb,nb,nx,ny,nz)


      do k=1,nz
         km1=mod(k+nz-2,nz)+1
         kp1=mod(k,nz)+1
         do j=1,ny
            jm1=mod(j+ny-2,ny)+1
            jp1=mod(j,ny)+1
            do i=1,nx
               im1=mod(i+nx-2,nx)+1
               ip1=mod(i,nx)+1
               do l=1,nb
                  y(l,i,j,k)=0.0d0
                  do m=1,nb
                     y(l,i,j,k)=y(l,i,j,k)+
     1               a(l,m,i,j,k)*x(m,i,j,k)+
     2               axp(l,m,i,j,k)*x(m,ip1,j,k)+
     3               ayp(l,m,i,j,k)*x(m,i,jp1,k)+
     4               azp(l,m,i,j,k)*x(m,i,j,kp1)+
     5               axm(l,m,i,j,k)*x(m,im1,j,k)+
     6               aym(l,m,i,j,k)*x(m,i,jm1,k)+
     7               azm(l,m,i,j,k)*x(m,i,j,km1)
                  enddo
               enddo
            enddo
         enddo
        enddo          

 

c        y=x
c        where (mask) y=tmp
        return
        end

