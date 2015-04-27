        program driver
        implicit none

        character*80 title
        integer nx,ny,nz,scheme,conf,time_steps 
        integer maxit
        real*8 nuim,nuex2,nuex4,cfl, Re, Pr
        real*8 tm,gtm

        maxit = 0

c       print *,'BI-CGSTAB & symmetric difference scheme '
c       print *, '3D  Laminar shock wave propagation'

        open(unit=50,file='bwaves.in',form='formatted')
        open(unit=10,file='bwaves.out',form='formatted')
        open(unit=20,file='bwaves2.out',form='formatted')
        open(unit=30,file='bwaves3.out',form='formatted')

c        print *, 'Re, Pr'
        read(50,*) title
        read (50,*) Re, Pr
c        print *, 'Re: ',Re, '    Pr: ',Pr


c       print *,'(nx,ny,nz) ?'
        read(50,*) title
        read(50,*) nx,ny,nz
c       print *,'grid size is: ',nx,ny,nz

c       print *,'(CFL, nuim, nuex2, nuex4) ?'
        read(50,*) title
        read(50,*) cfl, nuim, nuex2, nuex4
c        print *,'CFL:',cfl,'   ', 'nuim:',nuim,'  ',
c     $  'nuex2:',nuex2,' nuex4:', nuex4

c        print *,'What scheme you will use -explicit(0) or implicit(1)'
        read(50,*) title
        read (50,*) scheme
c       if (scheme.EQ.0) then
c       print *, 'Explicit scheme is working'
c       else
c       print *, 'Implicit scheme is working'   
c        endif

c        print *,'What initial configuration do you want-'
        read(50,*) title
c        print *, 'cubic(0) or spheric(1) ?'
        read (50,*) conf
c       if (conf.EQ.0) then
c       print *, 'Cubic initial configuration'
c       else
c       print *, 'Spheric initial configuration'        
c        endif

c        print *, 'Number of Time Steps ?'
        read(50,*) title
        read (50,*) time_steps
c        print *, 'Number of Time Steps:', time_steps
        

        call shell(Re,Pr,nx,ny,nz,
     $  nuim,nuex2,nuex4,cfl,scheme,conf, time_steps,maxit)
        
        write(20,*) maxit

        end






