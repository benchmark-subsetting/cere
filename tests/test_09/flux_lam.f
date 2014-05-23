      subroutine flux(q,e,f,g,ev,fv,gv,Re,Pr,gm,nx,ny,nz,
     $     dx,dy,dz)
      implicit none
      integer nx,ny,nz
      real*8 gm,Re,Pr,dx,dy,dz

      real*8 q(5,nx,ny,nz),e(5,nx,ny,nz),f(5,nx,ny,nz),g(5,nx,ny,nz),
     1     ev(5,nx,ny,nz),fv(5,nx,ny,nz),gv(5,nx,ny,nz)

      real*8 u(nx,ny,nz),v(nx,ny,nz),w(nx,ny,nz),p(nx,ny,nz),
     1     ro(nx,ny,nz),
     2     mu(nx,ny,nz)

      real*8 t0,t1,t2,t3
      integer im1,im2,ip1,ip2,jm1,jm2,jp1,jp2,km1,km2,kp1,kp2,i,j,k,l
      real*8 dx2,dy2,dz2

      dx2=2.0d0*dx
      dy2=2.0d0*dy
      dz2=2.0d0*dz

      do k=1,nz
         do j=1,ny
            do i=1,nx

               ro(i,j,k)=q(1,i,j,k)
               u(i,j,k)=q(2,i,j,k)/ro(i,j,k)
               v(i,j,k)=q(3,i,j,k)/ro(i,j,k)
               w(i,j,k)=q(4,i,j,k)/ro(i,j,k)
               p(i,j,k)=(gm-1.0d0)*(q(5,i,j,k)-0.5d0*ro(i,j,k)*
     1         (u(i,j,k)**2+v(i,j,k)**2+w(i,j,k)**2))
               mu(i,j,k)=(gm*p(i,j,k)/ro(i,j,k))**0.75d0

C     Euler's fluxes
               e(1,i,j,k)=ro(i,j,k)*u(i,j,k)
               e(2,i,j,k)=ro(i,j,k)*u(i,j,k)*u(i,j,k)+p(i,j,k)
               e(3,i,j,k)=ro(i,j,k)*u(i,j,k)*v(i,j,k)
               e(4,i,j,k)=ro(i,j,k)*u(i,j,k)*w(i,j,k)
               e(5,i,j,k)=u(i,j,k)*(q(5,i,j,k)+p(i,j,k))


               f(1,i,j,k)=ro(i,j,k)*v(i,j,k)
               f(2,i,j,k)=ro(i,j,k)*v(i,j,k)*u(i,j,k)
               f(3,i,j,k)=ro(i,j,k)*v(i,j,k)*v(i,j,k)+p(i,j,k)
               f(4,i,j,k)=ro(i,j,k)*v(i,j,k)*w(i,j,k)
               f(5,i,j,k)=v(i,j,k)*(q(5,i,j,k)+p(i,j,k))

               g(1,i,j,k)=ro(i,j,k)*w(i,j,k)
               g(2,i,j,k)=ro(i,j,k)*w(i,j,k)*u(i,j,k)
               g(3,i,j,k)=ro(i,j,k)*w(i,j,k)*v(i,j,k)
               g(4,i,j,k)=ro(i,j,k)*w(i,j,k)*w(i,j,k)+p(i,j,k)
               g(5,i,j,k)=w(i,j,k)*(q(5,i,j,k)+p(i,j,k))
            enddo
         enddo
      enddo


      do k=1,nz
         km2=mod(k+nz-3,nz)+1
         km1=mod(k+nz-2,nz)+1
         kp1=mod(k,nz)+1
         kp2=mod(k+1,nz)+1
         do j=1,ny
            jm2=mod(j+ny-3,ny)+1
            jm1=mod(j+ny-2,ny)+1
            jp1=mod(j,ny)+1
            jp2=mod(j+1,ny)+1
            do i=1,nx
               im2=mod(i+nx-3,nx)+1
               im1=mod(i+nx-2,nx)+1
               ip1=mod(i,nx)+1
               ip2=mod(i+1,nx)+1      

         

C     Viscous fluxes
               ev(1,i,j,k)=0.0d0
               t0=0.5*(mu(i,j,k)+mu(ip1,j,k))
               t3=gm*p(i,j,k)/ro(i,j,k)

               t1=(v(i,jp1,k)-v(i,jm1,k))/dy2
               t2=(w(i,j,kp1)-w(i,j,km1))/dz2
               ev(2,i,j,k)=t0/3.0d0*(4.0d0*(u(ip1,j,k)-u(i,j,k))/dx-
     1              (t1+(v(ip1,jp1,k)-v(ip1,jm1,k))/dy2+
     2              t2+(w(ip1,j,kp1)-w(ip1,j,km1))/dz2))

               t1=(u(i,jp1,k)-u(i,jm1,k))/dy2
               ev(3,i,j,k)=t0*((t1+(u(ip1,jp1,k)
     1                        -u(ip1,jm1,k))/dy2)/2.0d0+
     2              (v(ip1,j,k)-v(i,j,k))/dx)

               t2=(u(i,j,kp1)-u(i,j,km1))/dz2
               ev(4,i,j,k)=t0*((t2+(u(ip1,j,kp1)
     1              -u(ip1,j,km1))/dz2)/2.0d0+
     2              (w(ip1,j,k)-w(i,j,k))/dx)

               ev(5,i,j,k)=0.5d0*((u(ip1,j,k)+u(i,j,k))*ev(2,i,j,k)+
     1              (v(ip1,j,k)+v(i,j,k))*ev(3,i,j,k)+
     2              (w(ip1,j,k)+w(i,j,k))*ev(4,i,j,k))+
     3              t0/Pr/(gm-1.0d0)*(gm*p(ip1,j,k)/ro(ip1,j,k)-t3)/dx

c     ************************************************************   
               fv(1,i,j,k)=0.0d0
               t0=(mu(i,j,k)+mu(i,jp1,k))/2.0d0

               t1=(v(ip1,j,k)-v(im1,j,k))/dx2
               fv(2,i,j,k)=t0*(((v(ip1,jp1,k)
     2              -v(im1,jp1,k))/dx2+t1)/2.0d0+
     1              (u(i,jp1,k)-u(i,j,k))/dy)

               t1=(u(ip1,j,k)-u(im1,j,k))/dx2
               t2=(w(i,j,kp1)-w(i,j,km1))/dz2
               fv(3,i,j,k)=t0/3.0d0*(4.0d0*(v(i,jp1,k)-v(i,j,k))/dy-
     1         ((u(ip1,jp1,k)-u(im1,jp1,k))/dx2+t1+
     2         (w(i,jp1,kp1)-w(i,jp1,km1))/dz2+t2))

               fv(4,i,j,k)=t0*(
     1         0.5d0*((u(ip1,jp1,k)-u(im1,jp1,k))/dx2+t1)+
     2         (w(i,jp1,k)-w(i,j,k))/dy)

               fv(5,i,j,k)=0.5*(
     1         (u(i,jp1,k)+u(i,j,k))*fv(2,i,j,k)+
     2         (v(i,jp1,k)+v(i,j,k))*fv(3,i,j,k)+
     3         (w(i,jp1,k)+w(i,j,k))*fv(4,i,j,k))+
     4         t0/Pr/(gm-1.0d0)*(gm*p(i,jp1,k)/ro(i,jp1,k)-t3)/dy

C     *************************************************************

               gv(1,i,j,k)=0.0d0
               t0=(mu(i,j,k)+mu(i,j,kp1))/2.0d0
               t1=(w(ip1,j,k)-w(im1,j,k))/dx2  
               gv(2,i,j,k)=t0*(
     1         ((w(ip1,j,kp1)-w(im1,j,kp1))/dx2+t1)/2.0d0+
     2         (u(i,j,kp1)-u(i,j,k))/dz)

               t1=(w(i,jp1,k)-w(i,jm1,k))/dy2
               gv(3,i,j,k)=t0*(
     1         ((w(i,jp1,kp1)-w(i,jm1,kp1))/dy2+t1)/2.0d0+
     2         (v(i,j,kp1)-v(i,j,k))/dz)

               t1=(u(ip1,j,k)-u(im1,j,k))/dx2
               t2=(v(i,jp1,k)-v(i,jm1,k))/dy2
               gv(4,i,j,k)=t0/3.0d0*(4.0d0*(w(i,j,kp1)-w(i,j,k))/dz-
     1         ((u(ip1,j,kp1)-u(im1,j,kp1))/dx2+t1+
     2          (v(i,jp1,kp1)-v(i,jm1,kp1))/dy2+t2))

                gv(5,i,j,k)=0.5d0*(
     1          (u(i,j,kp1)+u(i,j,k))*gv(2,i,j,k)+
     2          (v(i,j,kp1)+v(i,j,k))*gv(3,i,j,k)+
     4          (w(i,j,kp1)+w(i,j,k))*gv(4,i,j,k))+
     5          t0/Pr/(gm-1.0d0)*(gm*p(i,j,kp1)/ro(i,j,kp1)-t3)/dz

                

             enddo
          enddo
       enddo 


       
      return
      end
 


