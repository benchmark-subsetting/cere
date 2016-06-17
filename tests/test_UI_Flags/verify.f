!
! FT verification routine.
!
      subroutine verify(n1, n2, n3, nt, cksum, verified)
        implicit none
        include 'npbparams.h'
!
! Arguments.
!

         integer n1, n2, n3, nt
         double complex cksum(0:nt)
	 logical verified

!
! Local variables.
!
         integer kt
         double complex cexpd(20)
         real*8 epsilon, err
!
! Initialize tolerance level and success flag.
!
         epsilon = 1.0d-12
         verified = .true.
!
         if ((n1 .eq. 64) .and. (n2 .eq. 64) .and.                 
     &            (n3 .eq. 64) .and. (nt .eq. 6)) then
!
! Class S reference values.
!
            cexpd(1) = dcmplx(554.6087004964D0, 484.5363331978D0)
            cexpd(2) = dcmplx(554.6385409189D0, 486.5304269511D0)
            cexpd(3) = dcmplx(554.6148406171D0, 488.3910722336D0)
            cexpd(4) = dcmplx(554.5423607415D0, 490.1273169046D0)
            cexpd(5) = dcmplx(554.4255039624D0, 491.7475857993D0)
            cexpd(6) = dcmplx(554.2683411902D0, 493.2597244941D0)
            
         else if ((n1 .eq. 128) .and. (n2 .eq. 128) .and.                 
     &            (n3 .eq. 32) .and. (nt .eq. 6)) then
!
! Class W reference values.
!
            cexpd(1) = dcmplx(567.3612178944D0, 529.3246849175D0)
            cexpd(2) = dcmplx(563.1436885271D0, 528.2149986629D0)
            cexpd(3) = dcmplx(559.4024089970D0, 527.0996558037D0)
            cexpd(4) = dcmplx(556.0698047020D0, 526.0027904925D0)
            cexpd(5) = dcmplx(553.0898991250D0, 524.9400845633D0)
            cexpd(6) = dcmplx(550.4159734538D0, 523.9212247086D0)
!
         else if ((n1 .eq. 256) .and. (n2 .eq. 256) .and.               
     &            (n3 .eq. 128) .and. (nt .eq. 6)) then
!
! Class A reference values.
!
            cexpd(1) = dcmplx(504.6735008193D0, 511.4047905510D0)
            cexpd(2) = dcmplx(505.9412319734D0, 509.8809666433D0)
            cexpd(3) = dcmplx(506.9376896287D0, 509.8144042213D0)
            cexpd(4) = dcmplx(507.7892868474D0, 510.1336130759D0)
            cexpd(5) = dcmplx(508.5233095391D0, 510.4914655194D0)
            cexpd(6) = dcmplx(509.1487099959D0, 510.7917842803D0)
!
         else if ((n1 .eq. 512) .and. (n2 .eq. 256) .and.               
     &            (n3 .eq. 256) .and. (nt .eq. 20)) then
!
! Class B reference values.
!
            cexpd(1)  = dcmplx(517.7643571579D0, 507.7803458597D0)
            cexpd(2)  = dcmplx(515.4521291263D0, 508.8249431599D0)
            cexpd(3)  = dcmplx(514.6409228649D0, 509.6208912659D0)
            cexpd(4)  = dcmplx(514.2378756213D0, 510.1023387619D0)
            cexpd(5)  = dcmplx(513.9626667737D0, 510.3976610617D0)
            cexpd(6)  = dcmplx(513.7423460082D0, 510.5948019802D0)
            cexpd(7)  = dcmplx(513.5547056878D0, 510.7404165783D0)
            cexpd(8)  = dcmplx(513.3910925466D0, 510.8576573661D0)
            cexpd(9)  = dcmplx(513.2470705390D0, 510.9577278523D0)
            cexpd(10) = dcmplx(513.1197729984D0, 511.0460304483D0)
            cexpd(11) = dcmplx(513.0070319283D0, 511.1252433800D0)
            cexpd(12) = dcmplx(512.9070537032D0, 511.1968077718D0)
            cexpd(13) = dcmplx(512.8182883502D0, 511.2616233064D0)
            cexpd(14) = dcmplx(512.7393733383D0, 511.3203605551D0)
            cexpd(15) = dcmplx(512.6691062020D0, 511.3735928093D0)
            cexpd(16) = dcmplx(512.6064276004D0, 511.4218460548D0)
            cexpd(17) = dcmplx(512.5504076570D0, 511.4656139760D0)
            cexpd(18) = dcmplx(512.5002331720D0, 511.5053595966D0)
            cexpd(19) = dcmplx(512.4551951846D0, 511.5415130407D0)
            cexpd(20) = dcmplx(512.4146770029D0, 511.5744692211D0)
!
         else if ((n1 .eq. 512) .and. (n2 .eq. 512) .and.               
     &            (n3 .eq. 512) .and. (nt .eq. 20)) then
!
! Class C reference values.
!
            cexpd(1)  = dcmplx(519.5078707457D0, 514.9019699238D0)
            cexpd(2)  = dcmplx(515.5422171134D0, 512.7578201997D0)
            cexpd(3)  = dcmplx(514.4678022222D0, 512.2251847514D0)
            cexpd(4)  = dcmplx(514.0150594328D0, 512.1090289018D0)
            cexpd(5)  = dcmplx(513.7550426810D0, 512.1143685824D0)
            cexpd(6)  = dcmplx(513.5811056728D0, 512.1496764568D0)
            cexpd(7)  = dcmplx(513.4569343165D0, 512.1870921893D0)
            cexpd(8)  = dcmplx(513.3651975661D0, 512.2193250322D0)
            cexpd(9)  = dcmplx(513.2955192805D0, 512.2454735794D0)
            cexpd(10) = dcmplx(513.2410471738D0, 512.2663649603D0)
            cexpd(11) = dcmplx(513.1971141679D0, 512.2830879827D0)
            cexpd(12) = dcmplx(513.1605205716D0, 512.2965869718D0)
            cexpd(13) = dcmplx(513.1290734194D0, 512.3075927445D0)
            cexpd(14) = dcmplx(513.1012720314D0, 512.3166486553D0)
            cexpd(15) = dcmplx(513.0760908195D0, 512.3241541685D0)
            cexpd(16) = dcmplx(513.0528295923D0, 512.3304037599D0)
            cexpd(17) = dcmplx(513.0310107773D0, 512.3356167976D0)
            cexpd(18) = dcmplx(513.0103090133D0, 512.3399592211D0)
            cexpd(19) = dcmplx(512.9905029333D0, 512.3435588985D0)
            cexpd(20) = dcmplx(512.9714421109D0, 512.3465164008D0)
!
         else
!
            write (*,    120) 'NOT DONE'
            verified = .false.
!
         end if
!
! Verification test for results.
!
         if (verified) then

            do kt = 1, nt
              err = abs((cksum(kt)-cexpd(kt))/cexpd(kt))
              if (.not.(err.le.epsilon)) then
                verified = .false.
                goto 100
	      endif     
            end do
  100       continue

            if (verified) then
               write (*,    120) 'PASSED'
            else
               write (*,    120) 'FAILED'
            end if

  120       format (' Verification test for FT ', a)
         end if
!
         return
      end
