












































































c=======================================================================
c
c    \\\\\\\\\\     B E G I N   S U B R O U T I N E       //////////
c    //////////                F O U R N                  \\\\\\\\\\
c
c                            Developed by
c                Laboratory of Computational Astrophysics
c               University of Illinois at Urbana-Champaign
c
c=======================================================================
c
      subroutine fourn(data,nn,ndim,isign)
c
c    rklessen:zeus3d.vel_init <-----------------------------------------
c                                                      
c
c    written by: Ralf Klessen 9. June 1997
c    modified 1: Mordecai-Mark MacLow
c    modified 2: PSLi, MPIzed for ZeusMP
c
c  PURPOSE: Fouriertrafo
c
c  INPUT VARIABLES: 
c
c  OUTPUT VARIABLES:
c
c  LOCAL VARIABLES: 
c
c  EXTERNALS: 
c
c========================================================================

      integer isign,ndim,nn(ndim)
      REAL data(*)
      integer i1,i2,i2rev,i3,i3rev,ibit,idim,ifp1,ifp2,ip1,ip2,ip3,k1,
     *        k2,n,nprev,nrem,ntot
      REAL    tempi,tempr
     1      , theta,wi,wpi,wpr,wr,wtemp

      ntot=1
      do 11 idim=1,ndim
        ntot=ntot*nn(idim)
 11   continue
      nprev=1
      do 18 idim=1,ndim
        n=nn(idim)
        nrem=ntot/(n*nprev)
        ip1=2*nprev
        ip2=ip1*n
        ip3=ip2*nrem
        i2rev=1
        do 14 i2=1,ip2,ip1
          if(i2.lt.i2rev)then
            do 13 i1=i2,i2+ip1-2,2
              do 12 i3=i1,ip3,ip2
                i3rev=i2rev+i3-i2
                tempr=data(i3)
                tempi=data(i3+1)
                data(i3)=data(i3rev)
                data(i3+1)=data(i3rev+1)
                data(i3rev)=tempr
                data(i3rev+1)=tempi
 12           continue
 13         continue
          endif
          ibit=ip2/2
 1        if ((ibit.ge.ip1).and.(i2rev.gt.ibit)) then
            i2rev=i2rev-ibit
            ibit=ibit/2
          goto 1
          endif
          i2rev=i2rev+ibit
 14     continue
        ifp1=ip1
 2      if(ifp1.lt.ip2)then
          ifp2=2*ifp1
          theta=isign*6.28318530717959d0/(ifp2/ip1)
          wpr=-2.d0*sin(0.5d0*theta)**2
          wpi=sin(theta)
          wr=1.d0
          wi=0.d0
          do 17 i3=1,ifp1,ip1
            do 16 i1=i3,i3+ip1-2,2
              do 15 i2=i1,ip3,ifp2
                k1=i2
                k2=k1+ifp1
                tempr=(wr)*data(k2)-(wi)*data(k2+1)
                tempi=(wr)*data(k2+1)+(wi)*data(k2)
                data(k2)=data(k1)-tempr
                data(k2+1)=data(k1+1)-tempi
                data(k1)=data(k1)+tempr
                data(k1+1)=data(k1+1)+tempi
 15           continue
 16         continue
            wtemp=wr
            wr=wr*wpr-wi*wpi+wr
            wi=wi*wpr+wtemp*wpi+wi
 17       continue
          ifp1=ifp2
        goto 2
        endif
        nprev=n*nprev 
 18   continue
      return
      end
c
c=======================================================================
c
c    \\\\\\\\\\         E N D   S U B R O U T I N E       //////////
c    //////////                F O U R N                  \\\\\\\\\\
c
c=======================================================================

