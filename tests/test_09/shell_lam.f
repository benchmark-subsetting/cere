      subroutine shell(Re,Pr,nx,ny,nz,
     $nuim,nuex2,nuex4,cfl,scheme,conf,ni,maxit) 
      implicit none
      COMMON /scale/ ro1,p1,ro2,p2
      integer nx,ny,nz,ni,n,i,j,k,scheme,conf,si,sj,sk,l,m
      integer im2,im1,ip1,ip2,jm2,jm1,jp1,jp2,km2,km1,kp1,kp2
      integer maxit
      real*8 nuim,nuex2,nuex4,cfl,epsilon,dx,dy,dz,dt,rad
      real*8 gm,u1,v1,w1,p1,ro1,u2,v2,w2,p2,ro2,Re,Pr,cfll
     $,time,t1,t2,t3,t4,t5,t6,t7,t8,mu
      real*8 dqnorm

      real*8 q(5,nx,ny,nz),dq(5,nx,ny,nz),rhs(5,nx,ny,nz),e(5,nx,ny,nz),
     1     f(5,nx,ny,nz),g(5,nx,ny,nz),ev(5,nx,ny,nz),fv(5,nx,ny,nz),
     2     gv(5,nx,ny,nz),diss(5,nx,ny,nz)

      real*8 a(5,5,nx,ny,nz),be(5,5,nx,ny,nz),ce(5,5,nx,ny,nz),
     1     av(5,5,nx,ny,nz),bv(5,5,nx,ny,nz),cv(5,5,nx,ny,nz)

      real*8 ae(5,5,nx,ny,nz),axp(5,5,nx,ny,nz),ayp(5,5,nx,ny,nz),
     1     azp(5,5,nx,ny,nz),axm(5,5,nx,ny,nz),aym(5,5,nx,ny,nz),
     2     azm(5,5,nx,ny,nz),ident(5,5,nx,ny,nz)


       real*8 u(nx,ny,nz),v(nx,ny,nz),w(nx,ny,nz),p(nx,ny,nz),
     1 ro(nx,ny,nz),at(nx,ny,nz)

      real*8 xyz(3,nx,ny,nz)


C       This particular problem is periodic only




C     Data initialization
      epsilon=1.d-16

      si=nx/8
      sj=ny/8
      sk=nz/8
      dx=1.0d0/(nx-1)
      dy=1.0d0/(ny-1)
      dz=1.0d0/(nz-1)
      time=0.0d0



        do k = 1,nz
           t3=(k-1)*dz
           do j=1,ny
              t2=(j-1)*dy
              do i=1,nx
                 t1=(i-1)*dx
                 xyz(1,i,j,k)=t1
                 xyz(2,i,j,k)=t2
                 xyz(3,i,j,k)=t3
                 do l=1,5
                    rhs(l,i,j,k)=0.0d0
                    do m=1,5
                       a(m,l,i,j,k)=0.0d0
                       axp(m,l,i,j,k)=0.0d0
                       ayp(m,l,i,j,k)=0.0d0
                       azp(m,l,i,j,k)=0.0d0
                       axm(m,l,i,j,k)=0.0d0
                       aym(m,l,i,j,k)=0.0d0
                       azm(m,l,i,j,k)=0.0d0
                       ident(m,l,i,j,k)=0.0d0
                    enddo
                    ident(l,l,i,j,k)=1.0d0
                    rhs(l,i,j,k)=0.0d0
                    dq(l,i,j,k)=0.0d0
                 enddo
              enddo
           enddo
        enddo              

C Flow Initialization

      gm=1.4d0
      u1=0.0d0
      v1=0.0d0
      w1=0.0d0
      p1=1.0d0
      ro1=1.0d0
      u2=0.0d0
      v2=0.0d0
      w2=0.0d0
      p2=0.1d0
      ro2=0.1d0

      t1=ro2
      t2=ro2*u2
      t3=ro2*v2
      t4=ro2*w2
      t5=p2/(gm-1.0d0)+0.5d0*ro2*(u2**2+v2**2+w2**2)


        do k = 1,nz
           do j=1,ny
              do i=1,nx
                 q(1,i,j,k)=t1
                 q(2,i,j,k)=t2
                 q(3,i,j,k)=t3
                 q(4,i,j,k)=t4
                 q(5,i,j,k)=t5
              enddo
           enddo
        enddo

      t1=ro1
      t2=ro1*u1
      t3=ro1*v1
      t4=ro1*w1
      t5=p1/(gm-1.0d0)+0.5d0*ro1*(u1**2+v1**2+w1**2)

      if (conf.EQ.0) then

         do k=nz/2-sk,nz/2+sk
            do j=ny/2-sj,ny/2+sj
               do i=nx/2-si,nx/2+si
                 q(1,i,j,k)=t1
                 q(2,i,j,k)=t2
                 q(3,i,j,k)=t3
                 q(4,i,j,k)=t4
                 q(5,i,j,k)=t5
              enddo
           enddo
        enddo
      else
         rad=DMIN1((si*dx)**2,(sj*dy)**2,(sk*dz)**2)

         do k=1,nz
            t8=((k-nz/2)*dz)**2
            do j=1,ny
               t7=((j-ny/2)*dy)**2
               do i=1,nx
                  t6=((i-nx/2)*dx)**2 +t7 + t8
                  if (t6.LE.rad) then
                     q(1,i,j,k)=t1
                     q(2,i,j,k)=t2
                     q(3,i,j,k)=t3
                     q(4,i,j,k)=t4
                     q(5,i,j,k)=t5
                  endif
               enddo
            enddo
         enddo
      endif

      

C Propagation in time



      time = 0.0d0

      do n=1,ni
C     Time step definition

      cfll=0.1d0+(n-1.0d0)*cfl/20.0d0
      if (cfll.ge.cfl) cfll=cfl
      t8=0.0d0

      do k=1,nz
         do j=1,ny
            do i=1,nx
               t1=q(1,i,j,k)
               t2=q(2,i,j,k)/t1
               t3=q(3,i,j,k)/t1
               t4=q(4,i,j,k)/t1
               t5=(gm-1.0d0)*(q(5,i,j,k)-0.5d0*t1*(t2*t2+t3*t3+t4*t4))
               t6=dSQRT(gm*t5/t1)
               mu=gm*Pr*(gm*t5/t1)**0.75d0*2.0d0/Re/t1
               t7=((dabs(t2)+t6)/dx+mu/dx**2)**2 +
     1            ((dabs(t3)+t6)/dy+mu/dy**2)**2 +
     2            ((dabs(t4)+t6)/dz+mu/dz**2)**2
               t7=DSQRT(t7)
               t8=max(t8,t7)
            enddo
         enddo
      enddo
      dt=cfll / t8


C Left hand side
      call jacobian(q,ae,av,0.0d0,1.0d0,0.0d0,0.0d0,Re,Pr,gm,
     1     nx,ny,nz,dx,1)
      call jacobian(q,be,bv,0.0d0,0.0d0,1.0d0,0.0d0,Re,Pr,gm,
     1     nx,ny,nz,dy,2)
      call jacobian(q,ce,cv,0.0d0,0.0d0,0.0d0,1.0d0,Re,Pr,gm,
     1     nx,ny,nz,dz,3)


      do k=1,nz
         km1=mod(k-2+nz,nz)+1
         kp1=mod(k,nz)+1
         do j=1,ny
            jm1=mod(j-2+ny,ny)+1
            jp1=mod(j,ny)+1
            do i=1,nx
               im1=mod(i-2+nx,nx)+1
               ip1=mod(i,nx)+1
               do l=1,5
                  do m=1,5
                     t1=ident(m,l,i,j,k)
                     a(m,l,i,j,k)=t1 - 0.5d0*dt*(
     1               (av(m,l,i,j,k)-av(m,l,im1,j,k))/dx+
     2               (bv(m,l,i,j,k)-bv(m,l,i,jm1,k))/dy+
     3               (cv(m,l,i,j,k)-cv(m,l,i,j,km1))/dz)/Re
     4               + 2.0d0*nuim*dt*(1.0d0/dx+1.0d0/dy+1.0d0/dz)*t1     

                     axp(m,l,i,j,k)=0.5d0*dt/dx*
     1               (ae(m,l,ip1,j,k)-av(m,l,i,j,k)/Re) 
     2               - nuim*dt/dx * t1
                     axm(m,l,i,j,k)=-0.5d0*dt/dx*
     1                    (ae(m,l,im1,j,k)-av(m,l,im1,j,k)/Re)
     2               - nuim*dt/dx * t1 

                     ayp(m,l,i,j,k)=0.5d0*dt/dy*
     1               (be(m,l,i,jp1,k)-bv(m,l,i,j,k)/Re)
     2               - nuim*dt/dy * t1
                     aym(m,l,i,j,k)=-0.5d0*dt/dy*
     1               (be(m,l,i,jm1,k)-bv(m,l,i,jm1,k)/Re)
     2               - nuim*dt/dy * t1

                     azp(m,l,i,j,k)=0.5d0*dt/dz*
     1               (ce(m,l,i,j,kp1)-cv(m,l,i,j,k)/Re)
     2               - nuim*dt/dz * t1
                     azm(m,l,i,j,k)=-0.5d0*dt/dz*
     1               (ce(m,l,i,j,km1)-cv(m,l,i,j,km1)/Re)
     2               - nuim*dt/dz * t1

                  enddo
               enddo
            enddo
         enddo
      enddo

      

C Right hand side
      call flux(q,e,f,g,ev,fv,gv,Re,Pr,gm,nx,ny,nz,dx,dy,dz)


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
               do l=1,5
                  t1= -0.5d0*dt*(
     1            (e(l,ip1,j,k)-e(l,im1,j,k))/dx +
     2            (f(l,i,jp1,k)-f(l,i,jm1,k))/dy +
     3            (g(l,i,j,kp1)-g(l,i,j,km1))/dz) +
     4            dt/Re*((ev(l,i,j,k)-ev(l,im1,j,k))/dx +
     5                  (fv(l,i,j,k)-fv(l,i,jm1,k))/dy +
     6                  (gv(l,i,j,k)-gv(l,i,j,km1))/dz)

C     Artificial Viscosity
C     Explicit dissipation - second order
                  t2=dt*nuex2*(
     1            (q(l,ip1,j,k)-2.0d0*q(l,i,j,k)+q(l,im1,j,k))/dx +
     2            (q(l,i,jp1,k)-2.0d0*q(l,i,j,k)+q(l,i,jm1,k))/dy +
     3            (q(l,i,j,kp1)-2.0d0*q(l,i,j,k)+q(l,i,j,km1))/dz)

C     Explicit dissipation - fourth order
                  t2=t2-dt*nuex4*(
     1            (q(l,ip2,j,k)-4.0d0*q(l,ip1,j,k)+6.0d0*q(l,i,j,k)-
     2            4.0d0*q(l,im1,j,k)+q(l,im2,j,k))/dx +
     3            (q(l,i,jp2,k)-4.0d0*q(l,i,jp1,k)+6.0d0*q(l,i,j,k)-
     4            4.0d0*q(l,i,jm1,k)+q(l,i,jm2,k))/dy +
     5            (q(l,i,j,kp2)-4.0d0*q(l,i,j,kp1)+6.0d0*q(l,i,j,k)-
     6            4.0d0*q(l,i,j,km1)+q(l,i,j,km2))/dx )

                  rhs(l,i,j,k)=t1+t2
C     initial guess is rhs
                  dq(l,i,j,k)=rhs(l,i,j,k)
               enddo
            enddo
         enddo
      enddo
     
c      write(10,*) 'Time step: ',n,'  dt: ',dt 

      if (scheme.eq.1) call bi_cgstab_block(dq,rhs,a,axp,
     $ ayp,azp,axm,aym,azm,epsilon,5,nx,ny,nz,maxit)


      do k=1,nz
         do j=1,ny
            do i=1,nx
               do l=1,5
                  q(l,i,j,k)=q(l,i,j,k)+dq(l,i,j,k)
               enddo
            enddo
         enddo
      enddo

c      write(10,*) n, q(1,2,2,2)
c      write(10,*) q(2,2,2,2),q(3,2,2,2),q(4,2,2,2)
c      write(10,*) q(5,2,2,2)



      time=time+dt
      enddo

      dqnorm = 0.0d0
      do k=1,nz
         do j=1,ny
            do i=1,nx
               do l=1,5
                  dqnorm = dqnorm + dq(l,i,j,k)*dq(l,i,j,k)
               enddo
            enddo
         enddo
      enddo

      write(30,*) 'dqnorm ==', dqnorm 

      return
      end

      









      
