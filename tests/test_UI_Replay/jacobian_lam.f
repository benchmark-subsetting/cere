      subroutine jacobian(q,je,jv,kt,kx,ky,kz,Re,Pr,gm,nx,ny,nz,
     $step,ax)

      implicit none
      integer nx,ny,nz,ax,ish,jsh,ksh,i,j,k,ip1,jp1,kp1
      real*8 kt,kx,ky,kz,gm,Re,Pr,al0,al1,al2,al3,al4,al5,al6,step

      real*8 q(5,nx,ny,nz)

      real*8 je(5,5,nx,ny,nz),jv(5,5,nx,ny,nz)

      real*8 u,v,w,fi2,alf,tht,a1,mu,ro,us,vs,ws,ros
C     Compute shift distance
      ish=0
      jsh=0
      ksh=0
      if (ax.eq.1) ish=1
      if (ax.eq.2) jsh=1
      if (ax.eq.3) ksh=1

      al0=(kx**2+ky**2+kz**2)/Pr
      al1=4.0d0/3.0d0*kx**2+ky**2+kz**2
      al2=kx*ky
      al3=1./3.0d0*kx*kz
      al4=kx**2+4.0d0/3.0d0*ky**2+kz**2
      al5=1.0d0/3.0d0*ky*kz
      al6=kx**2+ky**2+4.0d0/3.0d0*kz**2


      do k=1,nz
         kp1=mod(k,nz+1-ksh)+ksh
         do j=1,ny
            jp1=mod(j,ny+1-jsh)+jsh
            do i=1,nx
               ip1=mod(i,nx+1-ish)+ish

C     Initialize support variables
               ro=q(1,i,j,k)
               u=q(2,i,j,k)/ro
               v=q(3,i,j,k)/ro
               w=q(4,i,j,k)/ro
               fi2=0.5d0*(gm-1.0d0)*(u*u+v*v+w*w)
               alf=gm*q(5,i,j,k)/ro
               a1=alf-fi2
               tht=kx*u+ky*v+kz*w
               mu=((gm-1.0d0)*(q(5,i,j,k)/ro-0.5d0*
     1              (u*u+v*v+w*w)))**0.75d0

C     Initialize an jacobian for euiler part

               je(1,1,i,j,k)= kt
               je(2,1,i,j,k)= -u*tht+kx*fi2
               je(3,1,i,j,k)= -v*tht+ky*fi2
               je(4,1,i,j,k)= -w*tht+kz*fi2
               je(5,1,i,j,k)= tht*(2.0d0*fi2-alf)

               je(1,2,i,j,k)= kx
               je(2,2,i,j,k)= kt+tht-(gm-2.0d0)*kx*u
               je(3,2,i,j,k)= kx*v-(gm-1.0d0)*ky*u
               je(4,2,i,j,k)= kx*w-(gm-1.0d0)*kz*u
               je(5,2,i,j,k)= kx*a1-(gm-1.0d0)*u*tht

               je(1,3,i,j,k)= ky
               je(2,3,i,j,k)= ky*u-(gm-1.0d0)*kx*v
               je(3,3,i,j,k)= kt+tht-(gm-2.0d0)*ky*v
               je(4,3,i,j,k)= ky*w-(gm-1.0d0)*kz*v
               je(5,3,i,j,k)= ky*a1-(gm-1.0d0)*v*tht

               je(1,4,i,j,k)= kz
               je(2,4,i,j,k)= kz*u-(gm-1.0d0)*kx*w
               je(3,4,i,j,k)= kz*v-(gm-1.0d0)*ky*w
               je(4,4,i,j,k)= kt+tht-(gm-2.0d0)*kz*w
               je(5,4,i,j,k)= kz*a1-(gm-1.0d0)*w*tht

               je(1,5,i,j,k)= 0.0d0
               je(2,5,i,j,k)= (gm-1.0d0)*kx     
               je(3,5,i,j,k)= (gm-1.0d0)*ky
               je(4,5,i,j,k)= (gm-1.0d0)*kz
               je(5,5,i,j,k)= gm*tht+kt

C     Initialize an jacobian for viscous part


               ros=q(1,ip1,jp1,kp1)
               us=q(2,ip1,jp1,kp1)/ros
               vs=q(3,ip1,jp1,kp1)/ros
               ws=q(4,ip1,jp1,kp1)/ros
               a1=(1.0d0/ros-1.0d0/ro)/step               
               mu=(mu +((gm-1.0d0)*(q(5,ip1,jp1,kp1)/ros-
     1         0.5d0*(us*us+vs*vs+ws*ws)))**0.75d0)/2.0d0


               jv(1,1,i,j,k)=0.0d0
               jv(2,1,i,j,k)=mu/step*(
     $         al1*(u/ro-us/ros)+al2*(v/ro-vs/ros)+al3*(w/ro-ws/ros))
               jv(3,1,i,j,k)=mu/step*(
     $         al2*(u/ro-us/ros)+al4*(v/ro-vs/ros)+al5*(w/ro-ws/ros))
               jv(4,1,i,j,k)=mu/step*(
     $         al3*(u/ro-us/ros)+al5*(v/ro-vs/ros)+al6*(w/ro-ws/ros))
               jv(5,1,i,j,k)=mu/step*(
     $         al1*(u**2/ro-us**2/ros)+2.0d0*al2*(u*v/ro-us*vs/ros)+
     $         2.0d0*al3*(u*w/ro-us*ws/ros)+al4*(v**2/ro-vs**2/ros)+
     $         al6*(w**2/ro-ws**2/ros)+2.0d0*al5*(v*w/ro-vs*ws/ros)+
     $         al0*(q(5,i,j,k)/ro**2-q(5,ip1,jp1,kp1)/ros**2)+
     $         al0*((u*u+v*v+w*w)/ro-(us**2+vs**2+ws**2)/ros))

               jv(1,2,i,j,k)=0.0d0
               jv(2,2,i,j,k)=mu*al1*a1
               jv(3,2,i,j,k)=mu*al2*a1
               jv(4,2,i,j,k)=mu*al3*a1
               jv(5,2,i,j,k)=-jv(2,1,i,j,k)-mu*al0*(us/ros-u/ro)/step

               jv(1,3,i,j,k)=0.0d0
               jv(2,3,i,j,k)=mu*al2*a1
               jv(3,3,i,j,k)=mu*al4*a1
               jv(4,3,i,j,k)=mu*al5*a1
               jv(5,3,i,j,k)=-jv(3,1,i,j,k)-mu*al0*(vs/ros-v/ro)/step

               jv(1,4,i,j,k)=0.0d0
               jv(2,4,i,j,k)=mu*al3*a1
               jv(3,4,i,j,k)=mu*al5*a1
               jv(4,4,i,j,k)=mu*al6*a1
               jv(5,4,i,j,k)=-jv(4,1,i,j,k)-mu*al0*(ws/ros-w/ro)/step

               jv(1,5,i,j,k)=0.0d0
               jv(2,5,i,j,k)=0.0d0
               jv(3,5,i,j,k)=0.0d0
               jv(4,5,i,j,k)=0.0d0
               jv(5,5,i,j,k)=mu*al0*a1
            enddo
         enddo
      enddo
      return
      end
      
