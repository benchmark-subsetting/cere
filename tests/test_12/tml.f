      MODULE LES3D_DATA

      IMPLICIT REAL*8 (A-H,O-Z)

      PARAMETER ( NSPECI = 1, ND = 7 + NSPECI )
      PARAMETER ( NSCHEME = 4, ICHEM = 0, ISGSK = 0, IVISC = 1 )
      PARAMETER ( IPERIOD = 1, JPERIOD = 0, KPERIOD = 1 )

      DIMENSION CPK(NSPECI), CVK(NSPECI), RGK(NSPECI),
     >                  HFK(NSPECI), RMWT(NSPECI), CONC0(NSPECI)

      DOUBLE PRECISION DT, TIME, STATTIME, CFL, RELNO, TSTND, ALREF,
     >   GAMMA, CP, CV, RMACH, PSTAG, TSTAG, RSTAG, ASTR2,
     >   RHOREF, TREF, UREF, CREF, PREF, PEXIT, CMU, RMUREF, RNUREF,
     >   EKREF, CNUK, CEPSK, CPROD, URMSFAC,
     >   XLEN, YLEN, ZLEN, DX, DY, DZ, DXI, DYI, DZI, DELTA, SAREA,
     >      XAREA, YAREA, ZAREA, VOLUME, DTVOL,
     >   SLREF, EFORM, TPROD, GDIFFAC

      CHARACTER*60 PREFIX

      INTEGER N, M, NM1, ITIME, NADV, NSTART, NEND, IHEAT,
     >   IADD, JADD, KADD, IBDD, ICDD, JBDD, JCDD, KBDD, KCDD, IFLOW,
     >   NFLOW, IRESTART, IREST, NREST, ISTAT, NSTAT, LENGTH,
     >   ITRACE, NTRACE, IPTS, JPTS, KPTS, ILOC(50), JLOC(50), KLOC(50),
     >   IDYN, IMAX, JMAX, KMAX

      PARAMETER( RUNIV =  8.3145D3,
     >             BIG =  1.0000D10,
     >           SMALL =  1.0000D-6,
     >          RMOLWT = 28.9700D0,
     >           AIRCP =  1.0045D3,
     >         PRANDLT =    0.72D0,
     >        TPRANDLT =    0.91D0)

      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:) ::
     >             U, V, W, P, T, H, EK,
     >         UAV, VAV, WAV, PAV, TAV, HAV, EKAV

      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:) ::
     >             CONC, HF, QAV, COAV, HFAV, DU

      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:,:,:) ::
     >             Q

      END MODULE LES3D_DATA
!---------------------------------------------------------------------
      PROGRAM LES3D

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE WORK(:)
      CHARACTER FLNAME*80
      LOGICAL YES

      READ(5,*)
      READ(5,*)
      READ(5,*) PREFIX
      READ(5,*)
      READ(5,*)
      READ(5,*) IMAX, JMAX, KMAX, TAUMAX
      READ(5,*)
      READ(5,*)
      READ(5,*) IRESTART, NEND, IREST, IFLOW, ITIME, ISTAT
      READ(5,*)
      READ(5,*)
      READ(5,*) XLEN, YLEN, ZLEN
      READ(5,*)
      READ(5,*)
      READ(5,*) CFL, UREF, TREF, PREF, PEXIT
      READ(5,*)
      READ(5,*)
!      READ(5,*) TSTND, RELNO, ALREF
      READ(5,*) TSTND, RNUREF, ALREF
      READ(5,*)
      READ(5,*)
      READ(5,*) CNUK, CE, CEPSK, CPROD, EKREF
      READ(5,*)
      READ(5,*)
      READ(5,*) ITRACE, IPTS, JPTS, KPTS
      READ(5,*)
      READ(5,*)
      READ(5,*) (ILOC(I),I=1, IPTS)
      READ(5,*)
      READ(5,*)
      READ(5,*) (JLOC(J),J=1, JPTS)
      READ(5,*)
      READ(5,*)
      READ(5,*) (KLOC(K),K=1, KPTS)
      READ(5,*)
      READ(5,*)
      READ(5,*)
      READ(5,*)
      IF(ICHEM .EQ. 2) THEN
         DO NS = 1, NSPECI
            READ(5,*) CONC0(NS), RMWT(NS), CPK(NS), HFK(NS)
         END DO
      END IF

!      ALLOCATE( Q( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2,ND,2),
!     >          U( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ), 
!     >          V( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ), 
!     >          W( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          P( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ), 
!     >          T( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          H( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >       CONC( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2,NSPECI),
!     >         HF( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2,4),
!     >         EK( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2))

!      ALLOCATE( QAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2,ND),
!     >          UAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          VAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          WAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          PAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          TAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          HAV( -2:IMAX+2,-2:JMAX+2,-2:KMAX+2 ),
!     >          COAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,NSPECI),
!     >          EKAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
!     >          HFAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,4))

!      ALLOCATE(DU(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,ND))

      ALLOCATE(Q(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,ND,2),
     >        DU(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,ND),
     >         U(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2), 
     >         V(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2), 
     >         W(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >         P(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2), 
     >         T(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >        HF(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,4),
     >       QAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,ND),
     >       UAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >       VAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >       WAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >       PAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >       TAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >      HFAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,4))
      Q=0D0
      DU=0D0
      U=0D0
      V=0D0
      W=0D0
      P=0D0
      T=0D0
      HF=0D0
      QAV=0D0
      UAV=0D0
      VAV=0D0
      WAV=0D0
      PAV=0D0
      TAV=0D0
      HFAV=0D0

      NWORDS = SIZE(Q) + SIZE(DU) + SIZE(U) + SIZE(V) + SIZE(W) +
     >         SIZE(P) + SIZE(T) + SIZE(HF) + SIZE(QAV) + 
     >         SIZE(UAV) + SIZE(VAV) + SIZE(WAV) + SIZE(PAV) +
     >         SIZE(TAV) + SIZE(HFAV)

      IF(ISGSK == 1) THEN
         ALLOCATE(EK(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >             H(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >          EKAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2),
     >           HAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2))
         EK=0D0
         H=0D0
         EKAV=0D0
         HAV=0D0
         NWORDS = NWORDS + SIZE(EK) + SIZE(H) + SIZE(EKAV) + SIZE(HAV)
      ENDIF

      IF(ICHEM > 0) THEN
         ALLOCATE(CONC(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,NSPECI),
     >            COAV(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2,NSPECI))
         CONC=0D0
         COAV=0D0
         NWORDS = NWORDS + SIZE(CONC) + SIZE(COAV)
      ENDIF

      LENGTH = ILENGTH(PREFIX, 60)

      IWORK = 0
      IF(ISTAT .GT. 0) THEN
         NSTATS = 10
         IWORK = IWORK + NSTATS * (IMAX+1) * (JMAX+1) * (KMAX+1)
      ENDIF
      IF(IWORK .GT. 0) THEN
         ALLOCATE(WORK(IWORK), STAT = ISTATUS)
         WORK=0D0
         IF(ISTATUS .EQ. 0) THEN
            NWORDS = NWORDS + IWORK
         ELSE
            WRITE(6,*) "ERROR IN WORK SPACE ALLOCATE!!"
            STOP
         ENDIF
         WORK = 0.0D0
      ENDIF

      DX = XLEN / (IMAX-1)
      DY = YLEN / (JMAX-1)
      ZLEN = DX * (KMAX - 1)
      DZ = ZLEN / (KMAX-1)

      DXI = 1.0D0 / DX
      DYI = 1.0D0 / DY
      DZI = 1.0D0 / DZ

      XAREA = DY * DZ
      YAREA = DX * DZ
      ZAREA = DX * DY

      VOLUME = DX * DY * DZ
      DELTA  = VOLUME**(1.0D0 / 3.0D0)
      SAREA  = SQRT(XAREA**2 + YAREA**2 + ZAREA**2)

      CALL SETIV()

      NSTART   = 1
      DT       = 0.0D0
      NADV     = 0
      M        = 1
      NREST    = 0
      NFLOW    = 0
      NTRACE   = 0
      TIME     = 0.0D0

c      WRITE(6,'(//15X,A)')"3D TEMPORAL MIXING LAYER"

c      WRITE(6,'(//," 1. GEOMETRY"/)')
c      WRITE(6,'(/9X,"IMAX =",I4,3X,"JMAX =",I4,3X,"KMAX =",I4)')
c     >                                    IMAX, JMAX, KMAX

c      WRITE(6,'(/9X,3(A,F6.3,3X))') "XLEN = ", XLEN,
c     >                              "YLEN = ", YLEN,
c     >                              "ZLEN = ", ZLEN

c      WRITE(6,'(//," 2. SIMULATION PARAMETERS"/)')

c      WRITE(6,'(9X,A,F12.5)') "UREF         = ", UREF
c      WRITE(6,'(9X,A,F12.5)') "TREF         = ", TREF
c      WRITE(6,'(9X,A,F12.5)') "PREF         = ", PREF
c      WRITE(6,'(9X,A,F12.5)') "REYNOLDS #   = ", RELNO
c      WRITE(6,'(9X,A,F12.5)') "LENGTH SCALE = ", ALREF
c      WRITE(6,'(9X,A,F12.5)') "CFL          = ", CFL

c      WRITE(6,'(//," 3. NUMERICAL SCHEME"/)')
c      IF(NSCHEME .EQ. 4) THEN
c         WRITE(6,'(9X,"SPATIAL ORDER   =  FOURTH")')
c      ELSE
c         WRITE(6,'(9X,"SPATIAL ORDER   =  SECOUND")')
c      ENDIF
c      IF(ISGSK .EQ. 1) THEN
c         IF(IDYN .GT. 0) THEN
c            WRITE(6,'(9X,"LES MODELING    =  DYNAMIC KSGS")')
c         ELSE
c            WRITE(6,'(9X,"LES MODELING    =  KSGS")')
c         ENDIF
c      ELSE
c         WRITE(6,'(9X,"LES MODELING    =  NONE")')
c      ENDIF

c      IF(ICHEM .EQ. 1) THEN
c         WRITE(6,'(//," 4. G-EQUATION FLAMELET MODEL",/)')
c         WRITE(6,'(9X,"FLAME SPEED = ",F6.3)') SLREF
c         WRITE(6,'(9X,"FLAME TEMP  = ",F6.1)') TPROD
c      ELSE IF(ICHEM .EQ. 2) THEN
c         WRITE(6,'(//," 4. SPECIES PROPERTIES"/)')
c         WRITE(6,'(/9X,A,I1/)')"NUMBER OF SPECIES = ", NSPECI
c         WRITE(6,'(9X,A,3X,A,3X,A,5X,A,9X,A/)')
c     >         "SPECIES(K):","M-MASS(K)","Y(K)","CP(K)","HF(K)"
c         DO L = 1,NSPECI
c           WRITE(6,'(13X,I1,6X,F6.2,6X,F5.3,3X,F9.2,3X,F9.2)')
c     >                 L, RMWT(L), CONC0(L), CPK(L), HFK(L)
c         END DO
c      ENDIF

c      WRITE(6,'(/,5X,"ALLOCATED MEMORY (KBYTES) = ", I7/)')
c     >                                      NWORDS * 8 / 2**10

      IF(IRESTART .EQ. 1) CALL RESTART(1)

      IF(ITRACE .GT. 0) THEN
         WRITE(FLNAME,'(A,"RHO_",I3.3,".TRACE")')
     >                               PREFIX(1:LENGTH), NTRACE
         NN = ILENGTH(FLNAME, 80)
         CALL TRACE(0, Q(-2,-2,-2,1,1), FLNAME(1:NN), 60)

         WRITE(FLNAME,'(A,"U_",I3.3,".TRACE")')
     >                               PREFIX(1:LENGTH), NTRACE
         NN = ILENGTH(FLNAME, 80)
         CALL TRACE(0, U(-2,-2,-2), FLNAME(1:NN), 61)

         WRITE(FLNAME,'(A,"V_",I3.3,".TRACE")')
     >                               PREFIX(1:LENGTH), NTRACE
         NN = ILENGTH(FLNAME, 80)
         CALL TRACE(0, V(-2,-2,-2), FLNAME(1:NN), 62)

         WRITE(FLNAME,'(A,"W_",I3.3,".TRACE")')
     >                               PREFIX(1:LENGTH), NTRACE
         NN = ILENGTH(FLNAME, 80)
         CALL TRACE(0, W(-2,-2,-2), FLNAME(1:NN), 63)

         WRITE(FLNAME,'(A,"P_",I3.3,".TRACE")')
     >                               PREFIX(1:LENGTH), NTRACE
         NN = ILENGTH(FLNAME, 80)
         CALL TRACE(0, P(-2,-2,-2), FLNAME(1:NN), 64)
      ENDIF

      CALL SETBC()

      IF(IRESTART .EQ. 0) THEN
         NFLOW = 0
!         CALL FLOWIO()
      END IF

      OPEN(99, FILE = 'leslie3d.out')

      TAU  = 0.0D0
      NADV = NSTART

      TIME_START = GET_TIME()

      DO WHILE (NADV .LE. NEND .AND. TAU < TAUMAX)

         IF(MOD(NADV,ITIME) .EQ. 0 .OR. NADV .EQ. NSTART) CALL TMSTEP()

         TIME = TIME + DT 

         CALL ANALYSIS(TAU, DELM)
         WRITE(99,'(2F12.6)') TAU, DELM

c         IF(MOD(NADV,ITIME) == 0) WRITE(6,10) NADV, DT, TIME, TAU, DELM
10       FORMAT(1X,'NADV = ',I7,' DT = ',E12.6, ' TIME = ',F10.6,
     >       ' TAU = ',F10.6,' DEL MOM = ', F10.6)

         NADV = NADV + 1

         IADD = MOD(NADV,2)
         JADD = MOD((NADV+IADD)/2,2)
         KADD = MOD(((2*JADD+IADD+NADV)/4),2)

         IF (IADD .EQ. 1) THEN
            IBDD = 1
            ICDD = 0
         ELSE
            IBDD = -1
            ICDD = -2
         END IF
         IF (JADD .EQ. 1) THEN
            JBDD = 1
            JCDD = 0
         ELSE
            JBDD = -1
            JCDD = -2
         END IF
         IF (KADD .EQ. 1) THEN
            KBDD = 1
            KCDD = 0
         ELSE
            KBDD = -1
            KCDD = -2
         END IF

         DO N = 1, 2
            M = 3 - N
            NM1 = N - 1

            CALL FLUXI()

            CALL FLUXJ()

            CALL FLUXK()

            IF(ISGSK .EQ. 1) CALL KSOURCE()

            CALL UPDATE()

            CALL SETBC()

            IADD = MOD((IADD+1),2)
            JADD = MOD((JADD+1),2)
            KADD = MOD((KADD+1),2)

            IF(IADD .EQ. 1) THEN
               IBDD = 1
               ICDD = 0
            ELSE
               IBDD = -1
               ICDD = -2
            END IF
            IF(JADD .EQ. 1) THEN
               JBDD = 1
               JCDD = 0
            ELSE
               JBDD = -1
               JCDD = -2
            END IF
            IF(KADD .EQ. 1) THEN
               KBDD = 1
               KCDD = 0
            ELSE
               KBDD = -1
               KCDD = -2
            END IF
        END DO

        IF(ISTAT .GT. 0) CALL STATS(WORK(1), WORK(2))

        IF(IFLOW .GT. 0) THEN
           IF(MOD(NADV,IFLOW) .EQ. 0) THEN
              NFLOW = NFLOW + 1
              CALL FLOWIO()
           ENDIF
        ENDIF

        IF(IREST .GT. 0) THEN
           IF(MOD(NADV,IREST) .EQ. 0) CALL RESTART(0)
        ENDIF
       
        IF(ITRACE .GT. 0) THEN
           IF(MOD(NADV,ITRACE) .EQ. 0) THEN
              CALL TRACE(1, Q(-2,-2,-2,1,1), FLNAME, 60)
              CALL TRACE(1, U(-2,-2,-2)    , FLNAME, 61)
              CALL TRACE(1, V(-2,-2,-2)    , FLNAME, 62)
              CALL TRACE(1, W(-2,-2,-2)    , FLNAME, 63)
              CALL TRACE(1, P(-2,-2,-2)    , FLNAME, 64)
           ENDIF
        ENDIF

      ENDDO

      IF(ITRACE .GT. 0) THEN
         CALL TRACE(2, Q(-2,-2,-2,1,1), FLNAME, 60)
         CALL TRACE(2, U(-2,-2,-2)    , FLNAME, 61)
         CALL TRACE(2, V(-2,-2,-2)    , FLNAME, 62)
         CALL TRACE(2, W(-2,-2,-2)    , FLNAME, 63)
         CALL TRACE(2, P(-2,-2,-2)    , FLNAME, 64)
      ENDIF

      TIME_END = GET_TIME()
      TOTAL_TIME = TIME_END - TIME_START

      NSECS = NINT(TOTAL_TIME)
      NHOUR = NSECS / 3600
      NSECS = NSECS - 3600 * NHOUR
      NMINS = NSECS / 60
      NSECS = NSECS - 60 * NMINS

c      WRITE(6,'(//5X,"TOTAL TIME = ",I2.2,":",I2.2,":",I2.2,/)')
c     >                        NHOUR, NMINS, NSECS
c      WRITE(6,'(5X,"TOTAL TIME (SEC) = ",F10.3)') TOTAL_TIME
c      WRITE(6,'(5X,"TIME PER STEP = ",F8.5,/)')
c     >                   TOTAL_TIME / DBLE(NADV - NSTART + 1)

      stop

      END
!------------------------------------------------------------------------
      SUBROUTINE FLUXI()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE QS(:), FSI(:,:)
      ALLOCATABLE QDIFFX(:), RMU(:), EKCOEF(:)
      ALLOCATABLE DUDX(:), DUDY(:), DUDZ(:)
      ALLOCATABLE DVDX(:), DVDY(:), DVDZ(:)
      ALLOCATABLE DWDX(:), DWDY(:), DWDZ(:)
      ALLOCATABLE DTDX(:), DCDX(:,:)
      ALLOCATABLE DHDX(:), DKDX(:)

      PARAMETER (  R12I = 1.0D0 / 12.0D0,
     >              R6I = 1.0D0 / 6.0D0,
     >            THIRD = 1.0D0 / 3.0D0,
     >             TWO3 = 2.0D0 / 3.0D0 )

      ALLOCATE( QS(0:IMAX-1), FSI(0:IMAX-1,ND))
      ALLOCATE( QDIFFX(0:IMAX-1), RMU(0:IMAX-1), EKCOEF(0:IMAX-1))
      ALLOCATE( DUDX(0:IMAX-1), DUDY(0:IMAX-1), DUDZ(0:IMAX-1))
      ALLOCATE( DVDX(0:IMAX-1), DVDY(0:IMAX-1), DVDZ(0:IMAX-1))
      ALLOCATE( DWDX(0:IMAX-1), DWDY(0:IMAX-1), DWDZ(0:IMAX-1))
      ALLOCATE( DTDX(0:IMAX-1), DCDX(0:IMAX-1,NSPECI))
      ALLOCATE( DHDX(0:IMAX-1), DKDX(0:IMAX-1))
      QS=0D0
      FSI=0D0
      QDIFFX=0D0
      RMU=0D0
      EKCOEF=0D0
      DUDX=0D0
      DUDY=0D0
      DUDZ=0D0
      DVDX=0D0
      DVDY=0D0
      DVDZ=0D0
      DWDX=0D0
      DWDY=0D0
      DWDZ=0D0
      DTDX=0D0
      DCDX=0D0
      DHDX=0D0
      DKDX=0D0

      CALL EXTRAPI()

      I1 = 0
      I2 = IMAX - 1

      J1 = 1
      J2 = JMAX - 1

      K1 = 1
      K2 = KMAX - 1

      DO K = K1,K2
         DO J = J1,J2

            ABD = DBLE(IBDD)
!
! EULER STUFF
!
            DO I = I1,I2
               QS(I) = UAV(I,J,K) * XAREA
            END DO
 
            IF(NSCHEME .EQ. 2) THEN
               DO I = I1,I2
                  II = I + 1 - IADD
                  QSP = U(II,J,K) * XAREA
                  QSPI = (QSP - QS(I)) * DBLE((1 - 2 * IADD))
                  IF(QSPI .GT. 0.0D0) QS(I) = 0.5D0 * (QS(I) + QSP)
               END DO
            END IF

            DO L = 1,5
               DO I = I1,I2
                  FSI(I,L) = QAV(I,J,K,L) * QS(I)
               END DO
            END DO

            IF(ISGSK .EQ. 1) THEN
               DO I = I1,I2
                  FSI(I,7) = QAV(I,J,K,7) * QS(I)
               END DO
            END IF

            IF(ICHEM .GT. 0) THEN
               DO NS = 1, NSPECI
                  DO I = I1,I2
                     FSI(I,7+NS) = QAV(I,J,K,7+NS) * QS(I)
                  END DO
               END DO
            END IF
 
            DO I = I1,I2
               FSI(I,2) = FSI(I,2) + PAV(I,J,K) * XAREA
            END DO

            DO I = I1,I2
               FSI(I,5) = FSI(I,5) + PAV(I,J,K) * QS(I)
            END DO
!
! VISCOUS STUFF
!
            IF(NSCHEME .EQ. 2) THEN      ! I-DIRECTION DERIVATIVES
 
               DO I = I1,I2
                  DUDX(I) = DXI * (U(I+1,J,K) - U(I,J,K))
                  DVDX(I) = DXI * (V(I+1,J,K) - V(I,J,K))
                  DWDX(I) = DXI * (W(I+1,J,K) - W(I,J,K))
                  DTDX(I) = DXI * (T(I+1,J,K) - T(I,J,K))
               ENDDO

               DO I = I1, I2 
                  DUDY(I) = DYI * (UAV(I,J+1,K) - UAV(I,J-1,K)) * 0.5D0
                  DVDY(I) = DYI * (VAV(I,J+1,K) - VAV(I,J-1,K)) * 0.5D0
                  DWDY(I) = DYI * (WAV(I,J+1,K) - WAV(I,J-1,K)) * 0.5D0
               ENDDO

               DO I = I1, I2 
                  DUDZ(I) = DZI * (UAV(I,J,K+1) - UAV(I,J,K-1)) * 0.5D0
                  DVDZ(I) = DZI * (VAV(I,J,K+1) - VAV(I,J,K-1)) * 0.5D0
                  DWDZ(I) = DZI * (WAV(I,J,K+1) - WAV(I,J,K-1)) * 0.5D0
               END DO
 
               IF(ISGSK .EQ. 1) THEN
                  DO I = I1,I2
                     DKDX(I) = DXI * (EK(I+1,J,K) - EK(I,J,K))
                     DHDX(I) = DXI * ( H(I+1,J,K) -  H(I,J,K))
                  END DO
               END IF
 
               IF(ICHEM .GT. 0) THEN
                  DO NS = 1,NSPECI
                     DO I = I1,I2
                        DCDX(I,NS) = 
     >                   DXI * (CONC(I+1,J,K,NS) - CONC(I,J,K,NS))
                     END DO
                  END DO
               END IF

            ELSE

               DO I = I1,I2
                  II  =  I + IADD
                  IBD = II - IBDD
                  ICD = II + IBDD
   
                  DUDX(I) = 
     >               DXI * ABD * ((U(IBD,J,K) - U(ICD,J,K)) +
     >                    8.0D0 * (U( II,J,K) - U(IBD,J,K))) * R6I
                  DVDX(I) = 
     >               DXI * ABD * ((V(IBD,J,K) - V(ICD,J,K)) +
     >                    8.0D0 * (V( II,J,K) - V(IBD,J,K))) * R6I
                  DWDX(I) = 
     >               DXI * ABD * ((W(IBD,J,K) - W(ICD,J,K)) +
     >                    8.0D0 * (W( II,J,K) - W(IBD,J,K))) * R6I
                  DTDX(I) = 
     >               DXI * ABD * ((T(IBD,J,K) - T(ICD,J,K)) +
     >                    8.0D0 * (T( II,J,K) - T(IBD,J,K))) * R6I
               END DO
 
               IF(JPERIOD .EQ. 1 .AND.
     >           (J .EQ. 1 .OR. J .EQ. JMAX-1)) THEN
                  DO I = I1,I2
                     DUDY(I) = DYI * (UAV(I,J+1,K) - UAV(I,J-1,K))*0.5D0
                     DVDY(I) = DYI * (VAV(I,J+1,K) - VAV(I,J-1,K))*0.5D0
                     DWDY(I) = DYI * (WAV(I,J+1,K) - WAV(I,J-1,K))*0.5D0
                  END DO
               ELSE
                  DO I = I1,I2
                     DUDY(I) =
     >                    DYI * R12I * (UAV(I,J-2,K) - UAV(I,J+2,K) +
     >                         8.0D0 * (UAV(I,J+1,K) - UAV(I,J-1,K)))
                     DVDY(I) =
     >                    DYI * R12I * (VAV(I,J-2,K) - VAV(I,J+2,K) +
     >                         8.0D0 * (VAV(I,J+1,K) - VAV(I,J-1,K)))
                     DWDY(I) =
     >                    DYI * R12I * (WAV(I,J-2,K) - WAV(I,J+2,K) +
     >                         8.0D0 * (WAV(I,J+1,K) - WAV(I,J-1,K)))
                  END DO
               END IF
 
               DO I = I1,I2
                  DUDZ(I) = DZI * R12I * (UAV(I,J,K-2) - UAV(I,J,K+2) +
     >                           8.0D0 * (UAV(I,J,K+1) - UAV(I,J,K-1)))
                  DVDZ(I) = DZI * R12I * (VAV(I,J,K-2) - VAV(I,J,K+2) +
     >                           8.0D0 * (VAV(I,J,K+1) - VAV(I,J,K-1)))
                  DWDZ(I) = DZI * R12I * (WAV(I,J,K-2) - WAV(I,J,K+2) +
     >                           8.0D0 * (WAV(I,J,K+1) - WAV(I,J,K-1)))
               END DO
 
               IF(ISGSK .EQ. 1) THEN
                  DO I = I1,I2
                     II  =  I + IADD
                     IBD = II - IBDD
                     ICD = II + IBDD
 
                     DKDX(I) = 
     >                 DXI * ABD * ((EK(IBD,J,K) - EK(ICD,J,K)) +
     >                      8.0D0 * (EK(II,J,K)  - EK(IBD,J,K))) * R6I
                     DHDX(I) = 
     >                 DXI * ABD * (( H(IBD,J,K) -  H(ICD,J,K)) +
     >                      8.0D0 * ( H(II,J,K)  -  H(IBD,J,K))) * R6I
                  END DO
               END IF

               IF(ICHEM .GT. 0) THEN
                  DO NS = 1,NSPECI
                     DO I = I1,I2
                        II  =  I + IADD
                        IBD = II - IBDD
                        ICD = II + IBDD
 
                        DCDX(I,NS) = DXI *
     >              ABD * ((CONC(IBD,J,K,NS) - CONC(ICD,J,K,NS)) +
     >            8.0D0 *  (CONC( II,J,K,NS) - CONC(IBD,J,K,NS))) * R6I
                     END DO
                  END DO
               END IF

            END IF

            DO I = I1,I2
               QDIFFX(I) = 0.0D0
            END DO
 
            DO I = I1,I2
               TEMP   = 0.5D0 * (T(I,J,K) + T(I+1,J,K))
               RMU(I) = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
            END DO
 
            IF(ICHEM .EQ. 2) THEN
               DO NS = 1,NSPECI
                  DO I = I1,I2
                     TEMP = 0.5D0 * (T(I,J,K) + T(I+1,J,K))
                     RD   = RMU(I) / PRANDLT
                     HK   = RD * (CPK(NS) * TEMP + HFK(NS))
                     QDIFFX(I) = QDIFFX(I) + HK * DCDX(I,NS)
                  END DO
               END DO
            END IF
 
            DO I = I1,I2
               UAVE = 0.5D0 * (U(I,J,K)    + U(I+1,J,K))
               VAVE = 0.5D0 * (V(I,J,K)    + V(I+1,J,K))
               WAVE = 0.5D0 * (W(I,J,K)    + W(I+1,J,K))
               CPAV = 0.5D0 * (HF(I,J,K,2) + HF(I+1,J,K,2))
 
               DIV    =  DUDX(I) + DVDY(I) + DWDZ(I)
               RK     =  CPAV * RMU(I) / PRANDLT
               RLMBDA = -TWO3 * RMU(I)
 
               TXX = -2.0D0 * RMU(I) * DUDX(I) - RLMBDA * DIV
               TXY = -RMU(I) * (DUDY(I) + DVDX(I))
               TXZ = -RMU(I) * (DWDX(I) + DUDZ(I))
               TYX =  TXY
               TZX =  TXZ
 
               FSI(I,2) = FSI(I,2) + TXX * XAREA
               FSI(I,3) = FSI(I,3) + TYX * XAREA
               FSI(I,4) = FSI(I,4) + TZX * XAREA
 
               QX = -RK * DTDX(I) - QDIFFX(I)
 
               FSI(I,5) = FSI(I,5) +
     >         (TXX * UAVE + TXY * VAVE + TXZ * WAVE + QX) * XAREA
            END DO

            IF(ICHEM .EQ. 1) THEN
               DO I = I1,I2
                  RD = RMU(I) * GDIFFAC
                  FSI(I,8) = FSI(I,8) - RD * DCDX(I,1) * XAREA
               END DO
            ELSE IF(ICHEM .EQ. 2) THEN
               DO NS = 1,NSPECI
                  DO I = I1,I2
                     RD = RMU(I) / PRANDLT
                     FSI(I,7+NS) =
     >                     FSI(I,7+NS) - RD * DCDX(I,NS) * XAREA
                  END DO
               END DO
            END IF
 
            IF(ISGSK .EQ. 1) THEN
               DO I = I1,I2
                  RHAVE  = 0.5D0 * (Q(I,J,K,1,N) + Q(I+1,J,K,1,N))
                  EKAVE  = 0.5D0 * (EK(I,J,K)    + EK(I+1,J,K))
!                  CNUAVE = 0.5D0 * (CNU(I,J,K)   + CNU(I+1,J,K))
!                  CENAVE = 0.5D0 * (CEN(I,J,K)   + CEN(I+1,J,K))
 
!                  EKCOEF(I) = RHAVE * CNUAVE * SQRT(EKAVE) * DELTA
                  EKCOEF(I) = RHAVE * CNUK * SQRT(EKAVE) * DELTA
                  RHEV  = 2.0D0 * EKCOEF(I)
!                  RDENG = CENAVE * EKCOEF(I)
                  RHEK  = TWO3 * RHAVE * EKAVE
 
                  SXX = DUDX(I)
                  SYY = DVDY(I)
                  SZZ = DWDZ(I)
                  SXY = 0.5D0 * (DUDY(I) + DVDX(I))
                  SXZ = 0.5D0 * (DUDZ(I) + DWDX(I))
 
                  DIV  = (SXX + SYY + SZZ) * THIRD

                  SGSXX = - RHEV * (SXX - DIV) + RHEK
                  SGSXY = - RHEV * SXY
                  SGSXZ = - RHEV * SXZ
 
                  SGSEX = - RDENG * DHDX(I)
 
                  FSI(I,2) = FSI(I,2) + SGSXX * XAREA
                  FSI(I,3) = FSI(I,3) + SGSXY * XAREA
                  FSI(I,4) = FSI(I,4) + SGSXZ * XAREA
                  FSI(I,5) = FSI(I,5) + SGSEX * XAREA
               END DO
 
               DO I = I1,I2
!                  CENAVE = 0.5D0 * (CEN(I,J,K) + CEN(I+1,J,K))
!                  RDENG  = CENAVE * EKCOEF(I) + RMU(I) / PRANDLT
                  RDENG = EKCOEF(I) + RMU(I) / PRANDLT
                  FSI(I,7) = FSI(I,7) - RDENG * DKDX(I) * XAREA
               END DO
 
               IF(ICHEM .GT. 0) THEN
                  DO NS = 1,NSPECI
                     DO I = I1,I2
                        FSI(I,7+NS) = FSI(I,7+NS) -
     >                       EKCOEF(I) * DCDX(I,NS) * XAREA
                     END DO
                  END DO
               END IF

            END IF
!
! DF/DX = (DT / VOL) * (F_I+1 - F_I)
!
            DO L = 1, 5 
               DO I = I1+1, I2
                  DU(I,J,K,L) = -DTVOL * (FSI(I,L) - FSI(I-1,L))
               END DO
            END DO

            IF(ISGSK .EQ. 1) THEN
               DO I = I1+1, I2
                  DU(I,J,K,7) = -DTVOL * (FSI(I,7) - FSI(I-1,7))
               END DO
            END IF

            IF(ICHEM .GT. 0) THEN
               DO NS = 1, NSPECI
                  DO I = I1+1, I2
                     DU(I,J,K,7+NS) =
     >                     -DTVOL * (FSI(I,7+NS) - FSI(I-1,7+NS))
                  END DO
               END DO
            END IF

         END DO
      END DO

      DEALLOCATE( QS, FSI,
     >            QDIFFX, RMU, EKCOEF,
     >            DUDX, DUDY, DUDZ,
     >            DVDX, DVDY, DVDZ,
     >            DWDX, DWDY, DWDZ,
     >            DTDX, DCDX,
     >            DHDX, DKDX)

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE FLUXJ()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE QS(:), FSJ(:,:,:)
      ALLOCATABLE QDIFFY(:), RMU(:), EKCOEF(:)
      ALLOCATABLE DUDX(:),DUDY(:),DUDZ(:)
      ALLOCATABLE DVDX(:),DVDY(:),DVDZ(:)
      ALLOCATABLE DWDX(:),DWDY(:),DWDZ(:)
      ALLOCATABLE DTDY(:), DCDY(:,:)
      ALLOCATABLE DHDY(:), DKDY(:)

      PARAMETER (  R12I = 1.0D0 / 12.0D0,
     >              R6I = 1.0D0 / 6.0D0,
     >            THIRD = 1.0D0 / 3.0D0,
     >             TWO3 = 2.0D0 / 3.0D0 )
 
      ALLOCATE( QS(IMAX-1), FSJ(IMAX-1,0:JMAX-1,ND))
      ALLOCATE( QDIFFY(IMAX-1), RMU(IMAX-1), EKCOEF(IMAX-1))
      ALLOCATE( DUDX(IMAX-1),DUDY(IMAX-1),DUDZ(IMAX-1))
      ALLOCATE( DVDX(IMAX-1),DVDY(IMAX-1),DVDZ(IMAX-1))
      ALLOCATE( DWDX(IMAX-1),DWDY(IMAX-1),DWDZ(IMAX-1))
      ALLOCATE( DTDY(IMAX-1), DCDY(IMAX-1,NSPECI))
      ALLOCATE( DHDY(IMAX-1), DKDY(IMAX-1))
      QS=0D0
      FSJ=0D0
      QDIFFY=0D0
      RMU=0D0
      EKCOEF=0D0
      DUDX=0D0
      DUDY=0D0
      DUDZ=0D0
      DVDX=0D0
      DVDY=0D0
      DVDZ=0D0
      DWDX=0D0
      DWDY=0D0
      DWDZ=0D0
      DTDY=0D0
      DCDY=0D0
      DHDY=0D0
      DKDY=0D0

      CALL EXTRAPJ()

      I1 = 1
      I2 = IMAX - 1
 
      J1 = 0
      J2 = JMAX - 1

      K1 = 1
      K2 = KMAX - 1

      DO K = K1,K2
         DO J = J1,J2

            DO I = I1, I2
               QS(I) = VAV(I,J,K) * YAREA
            END DO
 
            IF(NSCHEME .EQ. 2) THEN
               JJ = J + 1 - JADD
               DO I = I1, I2
                  QSP = V(I,JJ,K) * YAREA
                  QSPI = (QSP - QS(I)) * (1 - 2 * JADD)
                  IF(QSPI.GT.0.0D0) QS(I) = 0.5D0 * (QS(I) + QSP)
               ENDDO
            END IF

            DO L = 1,5
               DO I = I1, I2
                  FSJ(I,J,L) = QAV(I,J,K,L) * QS(I)
               END DO
            END DO

            IF(ISGSK .EQ. 1) THEN
               DO I = I1, I2
                  FSJ(I,J,7) = QAV(I,J,K,7) * QS(I)
               END DO
            END IF
 
            IF(ICHEM .GT. 0) THEN
               DO NS = 1, NSPECI
                  DO I = I1, I2
                     FSJ(I,J,7+NS) = QAV(I,J,K,7+NS) * QS(I)
                  END DO
               END DO
            END IF
 
            DO I = I1, I2
               FSJ(I,J,3) = FSJ(I,J,3) + PAV(I,J,K) * YAREA
            END DO

            DO I = I1, I2
               FSJ(I,J,5) = FSJ(I,J,5) + PAV(I,J,K) * QS(I)
            END DO

            JJ  =  J + JADD
            JBD = JJ - JBDD
            JCD = JJ + JBDD
            ABD = DBLE(JBDD)

            IF(NSCHEME .EQ. 2) THEN
 
               DO I = I1, I2
                  DUDX(I) = DXI * (UAV(I+1,J,K) - UAV(I-1,J,K)) * 0.5D0
                  DVDX(I) = DXI * (VAV(I+1,J,K) - VAV(I-1,J,K)) * 0.5D0
                  DWDX(I) = DXI * (WAV(I+1,J,K) - WAV(I-1,J,K)) * 0.5D0
 
                  DUDY(I) = DYI * (U(I,J+1,K) - U(I,J,K))
                  DVDY(I) = DYI * (V(I,J+1,K) - V(I,J,K))
                  DWDY(I) = DYI * (W(I,J+1,K) - W(I,J,K))
                  DTDY(I) = DYI * (T(I,J+1,K) - T(I,J,K))
 
                  DUDZ(I) = DZI * (UAV(I,J,K+1) - UAV(I,J,K-1)) * 0.5D0
                  DVDZ(I) = DZI * (VAV(I,J,K+1) - VAV(I,J,K-1)) * 0.5D0
                  DWDZ(I) = DZI * (WAV(I,J,K+1) - WAV(I,J,K-1)) * 0.5D0
               END DO
 
               IF(ICHEM .GT. 0) THEN
                  DO NS = 1,NSPECI
                     DO I = I1, I2
                        DCDY(I,NS) = 
     >                     DYI * (CONC(I,J+1,K,NS) - CONC(I,J,K,NS))
                     END DO
                  END DO
               END IF
 
               IF(ISGSK .EQ. 1) THEN
                  DO I = I1, I2
                     DKDY(I) = DYI * (EK(I,J+1,K) - EK(I,J,K))
                     DHDY(I) = DYI * ( H(I,J+1,K) -  H(I,J,K))
                  END DO
               END IF
 
            ELSE
 
               DO I = I1, I2
                  DUDX(I) = DXI * R12I * (UAV(I-2,J,K) - UAV(I+2,J,K) +
     >                           8.0D0 * (UAV(I+1,J,K) - UAV(I-1,J,K)))
                  DVDX(I) = DXI * R12I * (VAV(I-2,J,K) - VAV(I+2,J,K) +
     >                           8.0D0 * (VAV(I+1,J,K) - VAV(I-1,J,K)))
                  DWDX(I) = DXI * R12I * (WAV(I-2,J,K) - WAV(I+2,J,K) +
     >                           8.0D0 * (WAV(I+1,J,K) - WAV(I-1,J,K)))
               END DO
 
               IF(JPERIOD .EQ. 0 .AND.
     >           (J .EQ. 0 .OR. J .EQ. JMAX-1)) THEN
                  DO I = I1, I2
                     DUDY(I) = DYI * (U(I,J+1,K) - U(I,J,K))
                     DVDY(I) = DYI * (V(I,J+1,K) - V(I,J,K))
                     DWDY(I) = DYI * (W(I,J+1,K) - W(I,J,K))
                     DTDY(I) = DYI * (T(I,J+1,K) - T(I,J,K))
                  END DO
               ELSE
                  DO I = I1, I2
                     DUDY(I) = 
     >                 DYI * ABD * ((U(I,JBD,K) - U(I,JCD,K)) +
     >                      8.0D0 * (U(I, JJ,K) - U(I,JBD,K))) * R6I
                     DVDY(I) = 
     >                 DYI * ABD * ((V(I,JBD,K) - V(I,JCD,K)) +
     >                      8.0D0 * (V(I, JJ,K) - V(I,JBD,K))) * R6I
                     DWDY(I) = 
     >                 DYI * ABD * ((W(I,JBD,K) - W(I,JCD,K)) +
     >                      8.0D0 * (W(I, JJ,K) - W(I,JBD,K))) * R6I
                     DTDY(I) = 
     >                 DYI * ABD * ((T(I,JBD,K) - T(I,JCD,K)) +
     >                      8.0D0 * (T(I, JJ,K) - T(I,JBD,K))) * R6I
                  END DO
               END IF
 
               DO I = I1, I2
                  DUDZ(I) = DZI * R12I * (UAV(I,J,K-2) - UAV(I,J,K+2) +
     >                           8.0D0 * (UAV(I,J,K+1) - UAV(I,J,K-1)))
                  DVDZ(I) = DZI * R12I * (VAV(I,J,K-2) - VAV(I,J,K+2) +
     >                           8.0D0 * (VAV(I,J,K+1) - VAV(I,J,K-1)))
                  DWDZ(I) = DZI * R12I * (WAV(I,J,K-2) - WAV(I,J,K+2) +
     >                           8.0D0 * (WAV(I,J,K+1) - WAV(I,J,K-1)))
               END DO
 
               IF(ISGSK .EQ. 1) THEN
                  IF(JPERIOD .EQ. 0 .AND.
     >              (J .EQ. 0 .OR. J .EQ. JMAX-1)) THEN
                     DO I = I1, I2
                        DKDY(I) = DYI * (EK(I,J+1,K) - EK(I,J,K))
                        DHDY(I) = DYI * ( H(I,J+1,K) -  H(I,J,K))
                     END DO
                  ELSE
                     DO I = I1, I2
                        DKDY(I) = 
     >                 DYI * ABD * ((EK(I,JBD,K) - EK(I,JCD,K)) +
     >                      8.0D0 * (EK(I, JJ,K) - EK(I,JBD,K))) * R6I
                        DHDY(I) = 
     >                 DYI * ABD * (( H(I,JBD,K) -  H(I,JCD,K)) +
     >                      8.0D0 * ( H(I, JJ,K) -  H(I,JBD,K))) * R6I
                     END DO
                  END IF
               END IF

               IF(ICHEM .GT. 0) THEN
                  IF(JPERIOD .EQ. 0 .AND.
     >              (J .EQ. 0 .OR. J .EQ. JMAX-1)) THEN
                     DO NS = 1,NSPECI
                        DO I = I1, I2
                           DCDY(I,NS) = 
     >                      DYI * (CONC(I,J+1,K,NS) - CONC(I,J,K,NS))
                        END DO
                     END DO
                  ELSE
                     DO NS = 1,NSPECI
                        DO I = I1, I2
                           DCDY(I,NS) = DYI *
     >              ABD * ((CONC(I,JBD,K,NS) - CONC(I,JCD,K,NS)) +
     >              8.0D0 *(CONC(I, JJ,K,NS) - CONC(I,JBD,K,NS))) * R6I
                        END DO
                     END DO
                  END IF
               END IF
 
           END IF
 
           DO I = I1, I2
              TEMP   = 0.5D0 * (T(I,J,K) + T(I,J+1,K))
              RMU(I) = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
           END DO
 
           DO I = I1, I2
              QDIFFY(I) = 0.0D0
           END DO
 
           IF(ICHEM .EQ. 2) THEN
              DO NS = 1,NSPECI
                 DO I = I1, I2
                    TEMP = 0.5D0 * (T(I,J,K) + T(I,J+1,K))
                    RD   = RMU(I) / PRANDLT
                    HK   = RD * (CPK(NS) * TEMP + HFK(NS))
                    QDIFFY(I) = QDIFFY(I) + HK * DCDY(I,NS)
                 END DO
              END DO
           END IF
 
           DO I = I1, I2
              UAVE = 0.5D0 * (U(I,J,K) + U(I,J+1,K))
              VAVE = 0.5D0 * (V(I,J,K) + V(I,J+1,K))
              WAVE = 0.5D0 * (W(I,J,K) + W(I,J+1,K))
              CPAV = 0.5D0 * (HF(I,J,K,2) + HF(I,J+1,K,2))
 
              DIV    = DUDX(I) + DVDY(I) + DWDZ(I)
              RK     = RMU(I) * CPAV /PRANDLT
              RLMBDA = - TWO3 * RMU(I)
     
              TXY = -RMU(I) * (DUDY(I) + DVDX(I))
              TYY = -2.0D0 * RMU(I) * DVDY(I) - RLMBDA * DIV
              TYX =  TXY
              TYZ = -RMU(I) * (DVDZ(I) + DWDY(I))
              TZY =  TYZ
     
              FSJ(I,J,2) = FSJ(I,J,2) + TXY * YAREA
              FSJ(I,J,3) = FSJ(I,J,3) + TYY * YAREA
              FSJ(I,J,4) = FSJ(I,J,4) + TZY * YAREA
     
              QY = -RK * DTDY(I) - QDIFFY(I)
     
              FSJ(I,J,5) = FSJ(I,J,5) +
     >       (TYX * UAVE + TYY * VAVE + TYZ * WAVE + QY) * YAREA
           END DO

 
           IF(ICHEM .EQ. 1) THEN
              DO I = I1, I2
                 RD = RMU(I) * GDIFFAC
                 FSJ(I,J,8) = FSJ(I,J,8) - RD * DCDY(I,1) * YAREA
              END DO
           ELSE IF(ICHEM .EQ. 2) THEN
              DO NS = 1,NSPECI
                 DO I = I1, I2
                    RD = RMU(I) / PRANDLT
                    FSJ(I,J,7+NS) = FSJ(I,J,7+NS) -
     >                                    RD * DCDY(I,NS) * YAREA
                 END DO
              END DO
           END IF
 
           IF(ISGSK .EQ. 1) THEN
              DO I = I1, I2
                 RHAVE  = 0.5D0 * (Q(I,J,K,1,N) + Q(I,J+1,K,1,N))
                 EKAVE  = 0.5D0 * (EK(I,J,K) + EK(I,J+1,K))
!                 CNUAVE = 0.5D0 * (CNU(I,J,K)+ CNU(I,J+1,K))
!                 CENAVE = 0.5D0 * (CEN(I,J,K)+ CEN(I,J+1,K))
 
!                 EKCOEF(I) = RHAVE * CNUAVE * SQRT(EKAVE) * DELTA
                 EKCOEF(I) = RHAVE * CNUK * SQRT(EKAVE) * DELTA
                 RHEV   = 2.0D0 * EKCOEF(I)
!                 RDENG  = CENAVE * EKCOEF(I)
                 RHEK   = TWO3 * RHAVE * EKAVE
     
                 SXX = DUDX(I)
                 SYY = DVDY(I)
                 SZZ = DWDZ(I)
                 SXY = 0.5D0 * (DUDY(I) + DVDX(I))
                 SYZ = 0.5D0 * (DVDZ(I) + DWDY(I))
     
                 DIV = (SXX + SYY + SZZ) * THIRD
     
                 SGSYY = - RHEV * (SYY - DIV) + RHEK
                 SGSXY = - RHEV * SXY
                 SGSYZ = - RHEV * SYZ
     
                 SGSEY = - RDENG * DHDY(I)
     
                 FSJ(I,J,2) = FSJ(I,J,2) + SGSXY * YAREA
                 FSJ(I,J,3) = FSJ(I,J,3) + SGSYY * YAREA
                 FSJ(I,J,4) = FSJ(I,J,4) + SGSYZ * YAREA
                 FSJ(I,J,5) = FSJ(I,J,5) + SGSEY * YAREA
              END DO 
     
              DO I = I1, I2
!                 CENAVE = 0.5D0 * (CEN(I,J,K)+ CEN(I,J+1,K))
!                 RDENG  = CENAVE * EKCOEF(I) + RMU(I) / PRANDLT
                 RDENG = EKCOEF(I) + RMU(I) / PRANDLT
                 FSJ(I,J,7) = FSJ(I,J,7) - RDENG * DKDY(I) * YAREA
              END DO
     
              IF(ICHEM .GT. 0) THEN
                 DO NS = 1,NSPECI
                    DO I = I1, I2
                       FSJ(I,J,7+NS) = FSJ(I,J,7+NS) -
     >                       EKCOEF(I) * DCDY(I,NS) * YAREA
                    END DO
                 END DO
              END IF
     
           END IF

        END DO

        DO L = 1,5
           DO J = J1+1,J2
               DO I = I1, I2
                  DU(I,J,K,L) = DU(I,J,K,L) -
     >                   DTVOL * (FSJ(I,J,L) - FSJ(I,J-1,L))
               END DO
            END DO
        END DO

        IF(ISGSK .EQ. 1) THEN
           DO J = J1+1,J2
              DO I = I1, I2
                 DU(I,J,K,7) = DU(I,J,K,7) -
     >                      DTVOL * (FSJ(I,J,7) - FSJ(I,J-1,7))
              END DO
           END DO
        END IF

        IF(ICHEM .GT. 0) THEN
           DO NS = 1, NSPECI
              DO J = J1+1, J2
                 DO I = I1, I2
                    DU(I,J,K,7+NS) = DU(I,J,K,7+NS) -
     >                     DTVOL * (FSJ(I,J,7+NS) - FSJ(I,J-1,7+NS))
                 END DO
              END DO
           END DO
        END IF

      END DO

      DEALLOCATE( QS, FSJ,
     >            QDIFFY, RMU, EKCOEF,
     >            DUDX,DUDY,DUDZ,
     >            DVDX,DVDY,DVDZ,
     >            DWDX,DWDY,DWDZ,
     >            DTDY, DCDY,
     >            DHDY, DKDY)

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE FLUXK() 

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE QS(:),FSK(:,:,:)
      ALLOCATABLE QDIFFZ(:), RMU(:), EKCOEF(:)
      ALLOCATABLE DUDX(:),DUDY(:),DUDZ(:)
      ALLOCATABLE DVDX(:),DVDY(:),DVDZ(:)
      ALLOCATABLE DWDX(:),DWDY(:),DWDZ(:)
      ALLOCATABLE DTDZ(:), DCDZ(:,:)
      ALLOCATABLE DHDZ(:), DKDZ(:)

      PARAMETER (  R12I = 1.0D0 / 12.0D0,
     >              R6I = 1.0D0 / 6.0D0,
     >            THIRD = 1.0D0 / 3.0D0,
     >             TWO3 = 2.0D0 / 3.0D0 )

      ALLOCATE( QS(IMAX-1),FSK(IMAX-1,0:KMAX,ND),
     >      QDIFFZ(IMAX-1), RMU(IMAX-1), EKCOEF(IMAX-1),
     >      DUDX(IMAX-1),DUDY(IMAX-1),DUDZ(IMAX-1),
     >      DVDX(IMAX-1),DVDY(IMAX-1),DVDZ(IMAX-1),
     >      DWDX(IMAX-1),DWDY(IMAX-1),DWDZ(IMAX-1),
     >      DTDZ(IMAX-1), DCDZ(IMAX-1,NSPECI),
     >      DHDZ(IMAX-1), DKDZ(IMAX-1))
      QS=0D0
      FSK=0D0
      QDIFFZ=0D0
      RMU=0D0
      EKCOEF=0D0
      DUDX=0D0
      DUDY=0D0
      DUDZ=0D0
      DVDX=0D0
      DVDY=0D0
      DVDZ=0D0
      DWDX=0D0
      DWDY=0D0
      DWDZ=0D0
      DTDZ=0D0
      DCDZ=0D0
      DHDZ=0D0
      DKDZ=0D0

      CALL EXTRAPK()

      I1 = 1
      I2 = IMAX - 1

      K1 = 0
      K2 = KMAX - 1

      J1 = 1
      J2 = JMAX - 1

      DO J = J1, J2
         DO K = K1, K2
!
! EULER STUFF
!
            DO I = I1, I2
               QS(I) = WAV(I,J,K) * ZAREA
            END DO
 
            IF(NSCHEME .EQ. 2 ) THEN
               KK = K + 1 - KADD
               DO I = I1, I2
                  QSP = W(I,J,KK) * ZAREA
                  QSPI = (QSP - QS(I)) * (1 - 2 * KADD)
                  IF(QSPI.GT.0.0D0) QS(I) = 0.5D0 * (QS(I) + QSP)
               END DO
            END IF

            DO L = 1, 5
               DO I = I1, I2
                  FSK(I,K,L) = QAV(I,J,K,L) * QS(I)
               END DO
            END DO

            IF(ISGSK .EQ. 1) THEN
               DO I = I1, I2
                  FSK(I,K,7) = QAV(I,J,K,7) * QS(I)
               END DO
            END IF

            IF(ICHEM .GT. 0) THEN
               DO NS = 1, NSPECI
                  DO I = I1, I2
                     FSK(I,K,7+NS) = QAV(I,J,K,7+NS) * QS(I)
                  END DO
               END DO
            END IF
 
            DO I = I1, I2
               FSK(I,K,4) = FSK(I,K,4) + PAV(I,J,K) * ZAREA
            END DO
            DO I = I1, I2
               FSK(I,K,5) = FSK(I,K,5) + PAV(I,J,K) * QS(I)
            END DO
!
! VISCOUS STUFF
! 
            KK  =  K + KADD
            KBD = KK - KBDD
            KCD = KK + KBDD
            ABD = DBLE(KBDD)
 
            IF(NSCHEME .EQ. 2) THEN
               DO I = I1, I2
                  DUDX(I) = DXI * (UAV(I+1,J,K) - UAV(I-1,J,K)) * 0.5D0
                  DVDX(I) = DXI * (VAV(I+1,J,K) - VAV(I-1,J,K)) * 0.5D0
                  DWDX(I) = DXI * (WAV(I+1,J,K) - WAV(I-1,J,K)) * 0.5D0
     
                  DUDY(I) = DYI * (UAV(I,J+1,K) - UAV(I,J-1,K)) * 0.5D0
                  DVDY(I) = DYI * (VAV(I,J+1,K) - VAV(I,J-1,K)) * 0.5D0
                  DWDY(I) = DYI * (WAV(I,J+1,K) - WAV(I,J-1,K)) * 0.5D0
 
                  DUDZ(I) = DZI * (U(I,J,K+1) - U(I,J,K))
                  DVDZ(I) = DZI * (V(I,J,K+1) - V(I,J,K))
                  DWDZ(I) = DZI * (W(I,J,K+1) - W(I,J,K))
                  DTDZ(I) = DZI * (T(I,J,K+1) - T(I,J,K))
               END DO
 
               IF(ICHEM .GT. 0) THEN
                  DO NS = 1, NSPECI
                     DO I = I1, I2
                        DCDZ(I,NS) = 
     >                     DZI * (CONC(I,J,K+1,NS) - CONC(I,J,K,NS))
                     END DO
                  END DO
               END IF
 
               IF(ISGSK .EQ. 1) THEN
                  DO I = I1, I2
                     DKDZ(I) = DZI * (EK(I,J,K+1) - EK(I,J,K))
                     DHDZ(I) = DZI * ( H(I,J,K+1) -  H(I,J,K))
                  END DO
               END IF
 
            ELSE

               DO I = I1, I2
                  DUDX(I) = DXI * R12I * (UAV(I-2,J,K) - UAV(I+2,J,K) +
     >                           8.0D0 * (UAV(I+1,J,K) - UAV(I-1,J,K)))
                  DVDX(I) = DXI * R12I * (VAV(I-2,J,K) - VAV(I+2,J,K) +
     >                           8.0D0 * (VAV(I+1,J,K) - VAV(I-1,J,K)))
                  DWDX(I) = DXI * R12I * (WAV(I-2,J,K) - WAV(I+2,J,K) +
     >                           8.0D0 * (WAV(I+1,J,K) - WAV(I-1,J,K)))
               END DO
 
               IF(J .EQ. 1 .OR. J .EQ. JMAX-1) THEN
                  DO I = I1, I2
                     DUDY(I) =
     >                   DYI * (UAV(I,J+1,K) - UAV(I,J-1,K)) * 0.5D0
                     DVDY(I) =
     >                   DYI * (VAV(I,J+1,K) - VAV(I,J-1,K)) * 0.5D0
                     DWDY(I) =
     >                   DYI * (WAV(I,J+1,K) - WAV(I,J-1,K)) * 0.5D0
                  END DO
               ELSE
                  DO I = I1, I2
                     DUDY(I) =
     >                       DYI * R12I * (UAV(I,J-2,K) - UAV(I,J+2,K) +
     >                            8.0D0 * (UAV(I,J+1,K) - UAV(I,J-1,K)))
                     DVDY(I) =
     >                       DYI * R12I * (VAV(I,J-2,K) - VAV(I,J+2,K) +
     >                            8.0D0 * (VAV(I,J+1,K) - VAV(I,J-1,K)))
                     DWDY(I) =
     >                       DYI * R12I * (WAV(I,J-2,K) - WAV(I,J+2,K) +
     >                            8.0D0 * (WAV(I,J+1,K) - WAV(I,J-1,K)))
                  END DO
               END IF
 
               DO I = I1, I2
                  DUDZ(I) =
     >                  DZI * ABD * ((U(I,J,KBD) - U(I,J,KCD)) +
     >                       8.0D0 * (U(I,J, KK) - U(I,J,KBD))) * R6I
                  DVDZ(I) =
     >                  DZI * ABD * ((V(I,J,KBD) - V(I,J,KCD)) +
     >                       8.0D0 * (V(I,J, KK) - V(I,J,KBD))) * R6I
                  DWDZ(I) =
     >                  DZI * ABD * ((W(I,J,KBD) - W(I,J,KCD)) +
     >                       8.0D0 * (W(I,J, KK) - W(I,J,KBD))) * R6I
                  DTDZ(I) =
     >                  DZI * ABD * ((T(I,J,KBD) - T(I,J,KCD)) +
     >                       8.0D0 * (T(I,J, KK) - T(I,J,KBD))) * R6I
               END DO

               IF(ICHEM .GT. 0) THEN
                  DO NS = 1, NSPECI
                     DO I = I1, I2
                        DCDZ(I,NS) =
     >         DZI * ABD * ((CONC(I,J,KBD,NS) - CONC(I,J,KCD,NS)) +
     >              8.0D0 * (CONC(I,J, KK,NS) - CONC(I,J,KBD,NS))) * R6I
                     END DO
                  END DO
               END IF

               IF(ISGSK .EQ. 1) THEN
                  DO I = I1, I2
                     DKDZ(I) = 
     >                  DZI * ABD * ((EK(I,J,KBD) - EK(I,J,KCD)) +
     >                       8.0D0 * (EK(I,J, KK) - EK(I,J,KBD))) * R6I
                     DHDZ(I) = 
     >                  DZI * ABD * (( H(I,J,KBD) -  H(I,J,KCD)) +
     >                       8.0D0 * ( H(I,J, KK) -  H(I,J,KBD))) * R6I
                  END DO
               END IF
            END IF

            DO I = I1, I2
               TEMP   = 0.5D0 * (T(I,J,K) + T(I,J,K+1))
               RMU(I) = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
            END DO
 
            DO I = I1, I2
               QDIFFZ(I) = 0.0D0
            END DO
 
            IF(ICHEM .EQ. 2) THEN
               DO NS = 1, NSPECI
                  DO I = I1, I2
                     TEMP = 0.5D0 * (T(I,J,K) + T(I,J,K+1))
                     RD   = RMU(I) / PRANDLT
                     HK   = RD * (CPK(NS) * TEMP + HFK(NS))
                     QDIFFZ(I) = QDIFFZ(I) + HK * DCDZ(I,NS)
                  END DO
               END DO
            END IF

            DO I = I1, I2
               UAVE = 0.5D0 * (U(I,J,K) + U(I,J,K+1))
               VAVE = 0.5D0 * (V(I,J,K) + V(I,J,K+1))
               WAVE = 0.5D0 * (W(I,J,K) + W(I,J,K+1))
               CPAV = 0.5D0 * (HF(I,J,K,2) + HF(I,J,K+1,2))
 
               DIV    = DUDX(I) + DVDY(I) + DWDZ(I)
               RK     = RMU(I) * CPAV / PRANDLT
               RLMBDA = -TWO3 * RMU(I)
 
               TXZ = -RMU(I) * (DWDX(I) + DUDZ(I))
               TYZ = -RMU(I) * (DVDZ(I) + DWDY(I))
               TZZ = -2.0D0 * RMU(I) * DWDZ(I) - RLMBDA * DIV
               TZX =  TXZ
               TZY =  TYZ
 
               FSK(I,K,2) = FSK(I,K,2) + TXZ * ZAREA
               FSK(I,K,3) = FSK(I,K,3) + TYZ * ZAREA
               FSK(I,K,4) = FSK(I,K,4) + TZZ * ZAREA
 
               QZ = -RK * DTDZ(I) - QDIFFZ(I)
 
               FSK(I,K,5) = FSK(I,K,5) +
     >          (TZX * UAVE + TZY * VAVE + TZZ * WAVE + QZ) * ZAREA
             END DO
 
             IF(ICHEM .EQ. 1) THEN
                DO I = I1, I2
                   RD = RMU(I) * GDIFFAC
                   FSK(I,K,8) = FSK(I,K,8) - RD * DCDZ(I,1) * ZAREA
                END DO
             ELSE IF(ICHEM .EQ. 2) THEN
                DO NS = 1, NSPECI
                   DO I = I1, I2
                      RD = RMU(I) / PRANDLT
                      FSK(I,K,7+NS) = FSK(I,K,7+NS) -
     >                              RD * DCDZ(I,NS) * ZAREA
                   END DO
                END DO
             END IF

             IF(ISGSK .EQ. 1) THEN
                DO I = I1, I2
                   RHAVE  = 0.5D0 * (Q(I,J,K,1,N) + Q(I,J,K+1,1,N))
                   EKAVE  = 0.5D0 * (EK(I,J,K) + EK(I,J,K+1))
!                   CNUAVE = 0.5D0 * (CNU(I,J,K) + CNU(I,J,K+1))
!                   CENAVE = 0.5D0 * (CEN(I,J,K) + CEN(I,J,K+1))
 
!                   EKCOEF(I) = RHAVE * CNUAVE * SQRT(EKAVE) * DELTA
                   EKCOEF(I) = RHAVE * CNUK * SQRT(EKAVE) * DELTA
                   RHEV  = 2.0D0 * EKCOEF(I)
!                   RDENG = CENAVE * EKCOEF(I)
                   RHEK  = TWO3 * RHAVE * EKAVE
 
                   SXX = DUDX(I)
                   SYY = DVDY(I)
                   SZZ = DWDZ(I)
                   SXZ = 0.5D0 * (DUDZ(I) + DWDX(I))
                   SYZ = 0.5D0 * (DVDZ(I) + DWDY(I))
 
                   DIV = (SXX + SYY + SZZ) * THIRD
 
                   SGSZZ = - RHEV * (SZZ - DIV) + RHEK
                   SGSXZ = - RHEV * SXZ
                   SGSYZ = - RHEV * SYZ
 
                   SGSEZ = - RDENG * DHDZ(I)
 
                   FSK(I,K,2) = FSK(I,K,2) + SGSXZ * ZAREA
                   FSK(I,K,3) = FSK(I,K,3) + SGSYZ * ZAREA
                   FSK(I,K,4) = FSK(I,K,4) + SGSZZ * ZAREA
                   FSK(I,K,5) = FSK(I,K,5) + SGSEZ * ZAREA
                END DO
 
                DO I = I1, I2
!                   CENAVE = 0.5D0 * (CEN(I,J,K) + CEN(I,J,K+1))
!                   RDENG  = CENAVE * EKCOEF(I) + RMU(I) / PRANDLT
                   RDENG = EKCOEF(I) + RMU(I) / PRANDLT
                   FSK(I,K,7) = FSK(I,K,7) - RDENG * DKDZ(I) * ZAREA
                END DO
 
                IF(ICHEM .GT. 0) THEN
                   DO NS = 1, NSPECI
                      DO I = I1, I2
                         FSK(I,K,7+NS) = FSK(I,K,7+NS) -
     >                         EKCOEF(I) * DCDZ(I,NS) * ZAREA
                      END DO
                   END DO
               END IF
            END IF
         END DO

         DO L = 1, 5
             DO K = K1+1, K2
                DO I = I1, I2
                   DU(I,J,K,L) = DU(I,J,K,L) -
     >                  DTVOL * (FSK(I,K,L) - FSK(I,K-1,L))
                END DO
             END DO
         END DO

         IF(ISGSK .EQ. 1) THEN
            DO K = K1+1, K2
               DO I = I1, I2
                  DU(I,J,K,7) = DU(I,J,K,7) -
     >                DTVOL * (FSK(I,K,7) - FSK(I,K-1,7))
               END DO
            END DO
         END IF

         IF(ICHEM .GT. 0) THEN
            DO NS = 1, NSPECI
               DO K = K1+1, K2
                  DO I = I1, I2
                     DU(I,J,K,7+NS) = DU(I,J,K,7+NS) -
     >                   DTVOL * (FSK(I,K,7+NS) - FSK(I,K-1,7+NS))
                  END DO
               END DO
            END DO
         END IF
      END DO

      DEALLOCATE( QS,FSK,
     >      QDIFFZ, RMU, EKCOEF,
     >      DUDX,DUDY,DUDZ,
     >      DVDX,DVDY,DVDZ,
     >      DWDX,DWDY,DWDZ,
     >      DTDZ, DCDZ,
     >      DHDZ, DKDZ)

      RETURN
      END

!------------------------------------------------------------------------
      SUBROUTINE EXTRAPI()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER ( R6I = 1.0D0 / 6.0D0 )

      I1 = 0
      I2 = IMAX - 1
 
      J1 = 0
      J2 = JMAX
 
      K1 = -1
      K2 = KMAX + 1

      IF(NSCHEME .EQ. 2) THEN
         DO L = 1, 5
            DO K = K1, K2
               DO J = J1, J2
                  DO I = I1, I2
                     II = I + IADD
                     QAV(I,J,K,L) = Q(II,J,K,L,N)
                  END DO
               END DO
            END DO
         END DO

         IF(ISGSK .EQ. 1) THEN
            DO K = K1, K2
               DO J = J1, J2
                  DO I = I1, I2
                     II = I + IADD
                     QAV(I,J,K,7) = Q(II,J,K,7,N)
                  END DO
               END DO
            END DO
         END IF

         IF(ICHEM .GT. 0) THEN
            DO NS = 1, NSPECI
               DO K = K1, K2
                  DO J = J1, J2
                     DO I = I1, I2
                        II = I + IADD
                        QAV(I,J,K,7+NS) = Q(II,J,K,7+NS,N)
                     END DO
                  END DO
               END DO
            END DO
         END IF

      ELSE

         DO L = 1, 5
            DO K = K1, K2
               DO J = J1, J2
                  DO I = I1, I2
                     II  =  I + IADD
                     IBD = II - IBDD
                     ICD = II + IBDD

                     QAV(I,J,K,L) = R6I * (2.0D0 * Q(IBD,J,K,L,N) +
     >                                     5.0D0 * Q( II,J,K,L,N) -
     >                                             Q(ICD,J,K,L,N))
                  END DO
               END DO
            END DO
         END DO

         IF(ICHEM .GT. 0) THEN
            DO NS = 1, NSPECI
               DO K = K1, K2
                  DO J = J1, J2
                     DO I = I1, I2
                        II  =  I + IADD
                        IBD = II - IBDD
                        ICD = II + IBDD

                        QAV(I,J,K,7+NS) =
     >                             R6I * (2.0D0 * Q(IBD,J,K,7+NS,N) +
     >                                    5.0D0 * Q( II,J,K,7+NS,N) -
     >                                            Q(ICD,J,K,7+NS,N))
                     END DO
                  END DO
               END DO
            END DO
         END IF

         IF(ISGSK .EQ. 1) THEN
            DO K = K1,K2
               DO J = J1,J2
                  DO I = I1,I2
                     II  =  I + IADD
                     IBD = II - IBDD
                     ICD = II + IBDD

                     QAV(I,J,K,7) = R6I * (2.0D0 * Q(IBD,J,K,7,N) +
     >                                     5.0D0 * Q( II,J,K,7,N) -
     >                                             Q(ICD,J,K,7,N))
                  END DO
               END DO
            END DO
         END IF
      END IF

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               UAV(I,J,K) = QAV(I,J,K,2) / QAV(I,J,K,1)
            END DO
         END DO
      END DO
      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               VAV(I,J,K) = QAV(I,J,K,3) / QAV(I,J,K,1)
            END DO
         END DO
      END DO
      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               WAV(I,J,K) = QAV(I,J,K,4) / QAV(I,J,K,1)
            END DO
         END DO
      END DO

      IF(ISGSK .EQ. 1) THEN
         DO K = K1, K2
            DO J = J1, J2
               DO I = I1, I2
                  EKAV(I,J,K) = QAV(I,J,K,7) / QAV(I,J,K,1)
               END DO
            END DO
         END DO
      END IF

      IF(ICHEM .GT. 0) THEN
         DO NS = 1, NSPECI
            DO K = K1, K2
               DO J = J1, J2
                  DO I = I1, I2
                     COAV(I,J,K,NS) = QAV(I,J,K,7+NS) / QAV(I,J,K,1)
                  END DO
               END DO
            END DO
         END DO

         IF(ICHEM .EQ. 1) THEN
            DO K = K1, K2
               DO J = J1, J2
                  DO I = I1, I2
                     HFAV(I,J,K,1) = COAV(I,J,K,1) * EFORM
                  END DO
               END DO
            END DO
         ELSE IF(ICHEM .EQ. 2) THEN
            DO L = 1, 4
               DO K = K1, K2
                  DO J = J1, J2
                     DO I = I1, I2
                        HFAV(I,J,K,L) = 0.0D0
                     END DO
                  END DO
               END DO
            END DO
            DO NS = 1, NSPECI
               DO K = K1, K2
                  DO J = J1, J2
                     DO I = I1, I2
                        HFAV(I,J,K,1) = HFAV(I,J,K,1) + 
     >                                      HFK(NS) * COAV(I,J,K,NS)
                        HFAV(I,J,K,2) = HFAV(I,J,K,2) + 
     >                                      CPK(NS) * COAV(I,J,K,NS)
                        HFAV(I,J,K,3) = HFAV(I,J,K,3) + 
     >                                      CVK(NS) * COAV(I,J,K,NS)
                        HFAV(I,J,K,4) = HFAV(I,J,K,4) + 
     >                                      RGK(NS) * COAV(I,J,K,NS)
                     END DO
                  END DO
               END DO
            END DO
         END IF
      END IF

      IF(ISGSK .EQ. 1) THEN
         DO K = K1, K2
            DO J = J1, J2
               DO I = I1, I2
                  RKE = 0.5D0 * (UAV(I,J,K) * UAV(I,J,K) +
     >                          VAV(I,J,K) * VAV(I,J,K) +
     >                          WAV(I,J,K) * WAV(I,J,K) ) + EKAV(I,J,K)
                  EI = QAV(I,J,K,5) / QAV(I,J,K,1) - RKE
                  TAV(I,J,K) = (EI - HFAV(I,J,K,1)) / HFAV(I,J,K,3)
                  PAV(I,J,K) = QAV(I,J,K,1) * HFAV(I,J,K,4) * TAV(I,J,K)
                  HAV(I,J,K) = (QAV(I,J,K,5) + PAV(I,J,K)) /
     >                                                     QAV(I,J,K,1)
               END DO
            END DO
         END DO
      ELSE
         DO K = K1, K2
            DO J = J1, J2
               DO I = I1, I2
                  RKE = 0.5D0 * (UAV(I,J,K) * UAV(I,J,K) +
     >                          VAV(I,J,K) * VAV(I,J,K) +
     >                          WAV(I,J,K) * WAV(I,J,K) )
                  EI = QAV(I,J,K,5) / QAV(I,J,K,1) - RKE
                  TAV(I,J,K) = (EI - HFAV(I,J,K,1)) / HFAV(I,J,K,3)
                  PAV(I,J,K) = QAV(I,J,K,1) * HFAV(I,J,K,4) * TAV(I,J,K)
               END DO
            END DO
         END DO
      END IF

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE EXTRAPJ()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER ( R6I = 1.0D0 / 6.0D0 )

      I1 = -1
      I2 = IMAX + 1

      J1 = 0
      J2 = JMAX - 1

      K1 = -1
      K2 = KMAX + 1

      IF(NSCHEME .EQ. 2) THEN  !2ND-ORDER

        DO NN = 1,5
          DO K = K1,K2
            DO J = J1,J2
              JJ  = J + JADD
              DO I = I1,I2
                QAV(I,J,K,NN) = Q(I,JJ,K,NN,N)
              END DO
            END DO
          END DO
        END DO

        IF(ICHEM .GT. 0) THEN
          DO NN = 1,NSPECI
            DO K = K1,K2
              DO J = J1,J2
                JJ = J + JADD
                DO I = I1,I2
                  QAV(I,J,K,7+NN) = Q(I,JJ,K,7+NN,N)
                END DO
              END DO
            END DO
          END DO
        END IF
 
        IF(ISGSK .EQ. 1) THEN
          DO K = K1,K2
            DO J = J1,J2
              JJ = J + JADD
              DO I = I1,I2
                QAV(I,J,K,7) = Q(I,JJ,K,7,N)
              END DO
            END DO
          END DO
        END IF

      ELSE    !4TH-ORDER

        DO NN = 1,5
          DO K = K1,K2
            DO J = J1,J2
              JJ  =  J + JADD
              JBD = JJ - JBDD
              JCD = JJ + JBDD

              IF(J .EQ. 0 .OR. J .EQ. JMAX-1) THEN
                DO I = I1,I2
                  QAV(I,J,K,NN) = Q(I,JJ,K,NN,N)
                END DO
              ELSE
                DO I = I1,I2
                  QAV(I,J,K,NN) = R6I * (2.0D0  * Q(I,JBD,K,NN,N) +
     >                                   5.0D0 * Q(I, JJ,K,NN,N) -
     >                                          Q(I,JCD,K,NN,N))
                END DO
              END IF

            END DO
          END DO
        END DO

        IF(ICHEM .GT. 0) THEN
          DO NN = 8,7+NSPECI
            DO K = K1,K2
              DO J = J1,J2
                JJ  =  J + JADD
                JBD = JJ - JBDD
                JCD = JJ + JBDD
 
                IF(J .EQ. 0 .OR. J .EQ. JMAX-1) THEN
                  DO I = I1,I2
                    QAV(I,J,K,NN) = Q(I,JJ,K,NN,N)
                  END DO
                ELSE
                  DO I = I1,I2
                    QAV(I,J,K,NN) = R6I * (2.0D0  * Q(I,JBD,K,NN,N) +
     >                                     5.0D0 * Q(I, JJ,K,NN,N) -
     >                                            Q(I,JCD,K,NN,N))
                  END DO
                END IF

              END DO
            END DO
          END DO
        END IF
 
        IF(ISGSK .EQ. 1) THEN
          DO K = K1,K2
            DO J = J1,J2
              JJ  =  J + JADD
              JBD = JJ - JBDD
              JCD = JJ + JBDD
 
              IF(J .EQ. 0 .OR. J .EQ. JMAX-1) THEN
                DO I = I1,I2
                  QAV(I,J,K,7) = Q(I,JJ,K,7,N)
                END DO
              ELSE
                DO I = I1,I2
                  QAV(I,J,K,7) = R6I * (2.0D0  * Q(I,JBD,K,7,N) +
     >                                  5.0D0 * Q(I, JJ,K,7,N) -
     >                                         Q(I,JCD,K,7,N))
                END DO
              END IF
 
            END DO
          END DO
        END IF

      END IF

      DO K = K1,K2
        DO J = J1,J2
          DO I = I1,I2
            UAV(I,J,K) = QAV(I,J,K,2) / QAV(I,J,K,1)
          END DO
        END DO
      END DO
      DO K = K1,K2
        DO J = J1,J2
          DO I = I1,I2
            VAV(I,J,K) = QAV(I,J,K,3) / QAV(I,J,K,1)
          END DO
        END DO
      END DO
      DO K = K1,K2
        DO J = J1,J2
          DO I = I1,I2
            WAV(I,J,K) = QAV(I,J,K,4) / QAV(I,J,K,1)
          END DO
        END DO
      END DO
 
      IF(ISGSK .EQ. 1) THEN
        DO K = K1,K2
          DO J = J1,J2
            DO I = I1,I2
              EKAV(I,J,K) = QAV(I,J,K,7) / QAV(I,J,K,1)
            END DO
          END DO
        END DO
      END IF

      IF(ICHEM .GT. 0) THEN
        DO NN = 1,NSPECI
          DO K = K1,K2
            DO J = J1,J2
              DO I = I1,I2
                COAV(I,J,K,NN) = QAV(I,J,K,7+NN) / QAV(I,J,K,1)
              END DO
            END DO
          END DO
        END DO
        IF(ICHEM .EQ. 1) THEN 
          DO K = K1,K2
            DO J = J1,J2
              DO I = I1,I2
                HFAV(I,J,K,1) = COAV(I,J,K,1) * EFORM
              END DO
            END DO
          END DO
        ELSE IF(ICHEM .EQ. 2) THEN
          DO NN = 1,4
            DO K = K1,K2
              DO J = J1,J2
                DO I = I1,I2
                  HFAV(I,J,K,NN) = 0.0D0
                END DO
              END DO
            END DO
          END DO
          DO NN = 1,NSPECI
            DO K = K1,K2
              DO J = J1,J2
                DO I = I1,I2
                  HFAV(I,J,K,1) = HFAV(I,J,K,1) + 
     >                                      HFK(NN) * COAV(I,J,K,NN)
                  HFAV(I,J,K,2) = HFAV(I,J,K,2) + 
     >                                      CPK(NN) * COAV(I,J,K,NN)
                  HFAV(I,J,K,3) = HFAV(I,J,K,3) + 
     >                                      CVK(NN) * COAV(I,J,K,NN)
                  HFAV(I,J,K,4) = HFAV(I,J,K,4) + 
     >                                      RGK(NN) * COAV(I,J,K,NN)
                END DO
              END DO
            END DO
          END DO
        END IF 
      END IF

      IF(ISGSK .EQ. 1) THEN
        DO K = K1,K2
          DO J = J1,J2
            DO I = I1,I2
              RKE  = 0.5D0 * (UAV(I,J,K) * UAV(I,J,K) +
     >                       VAV(I,J,K) * VAV(I,J,K) +
     >                       WAV(I,J,K) * WAV(I,J,K)) + EKAV(I,J,K)
              EI = QAV(I,J,K,5) / QAV(I,J,K,1) - RKE
              TAV(I,J,K) = (EI - HFAV(I,J,K,1)) / HFAV(I,J,K,3)
              PAV(I,J,K) = QAV(I,J,K,1) * HFAV(I,J,K,4) * TAV(I,J,K)
              HAV(I,J,K) = (QAV(I,J,K,5) + PAV(I,J,K)) / QAV(I,J,K,1)
            END DO
          END DO
        END DO
      ELSE
        DO K = K1,K2
          DO J = J1,J2
            DO I = I1,I2
              RKE  = 0.5D0 * (UAV(I,J,K) * UAV(I,J,K) +
     >                       VAV(I,J,K) * VAV(I,J,K) +
     >                       WAV(I,J,K) * WAV(I,J,K))
              EI = QAV(I,J,K,5) / QAV(I,J,K,1) - RKE
              TAV(I,J,K) = (EI - HFAV(I,J,K,1)) / HFAV(I,J,K,3)
              PAV(I,J,K) = QAV(I,J,K,1) * HFAV(I,J,K,4) * TAV(I,J,K)
            END DO
          END DO
        END DO
      END IF

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE EXTRAPK()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER ( R6I = 1.0D0 / 6.0D0 )

      I1 = -1
      I2 = IMAX + 1

      J1 = 0
      J2 = JMAX

      K1 = 0
      K2 = KMAX - 1

      IF(NSCHEME .EQ. 2) THEN   !2ND-ORDER 

        DO NN = 1,5
          DO K = K1,K2
            KK = K + KADD
            DO J = J1,J2
              DO I = I1,I2
                QAV(I,J,K,NN) = Q(I,J,KK,NN,N)
              END DO
            END DO
          END DO
        END DO

        IF(ICHEM .GT. 0) THEN
          DO NN = 8,7+NSPECI
            DO K = K1,K2
              KK = K + KADD
              DO J = J1,J2
                DO I = I1,I2
                  QAV(I,J,K,NN) = Q(I,J,KK,NN,N)
                END DO
              END DO
            END DO
          END DO
        END IF

        IF(ISGSK .EQ. 1) THEN
          DO K = K1,K2
            KK = K + KADD
            DO J = J1,J2
              DO I = I1,I2
                QAV(I,J,K,7) = Q(I,J,KK,7,N)
              END DO
            END DO
          END DO
        END IF

      ELSE !4TH-ORDER

        DO NN = 1,5
          DO K = K1,K2
            KK  =  K + KADD
            KBD = KK - KBDD
            KCD = KK + KBDD

            DO J = J1,J2
              DO I = I1,I2
                QAV(I,J,K,NN) = R6I * (2.0D0  * Q(I,J,KBD,NN,N) +
     >                                 5.0D0 * Q(I,J, KK,NN,N) -
     >                                        Q(I,J,KCD,NN,N))
              END DO
            END DO
          END DO
        END DO

        IF(ICHEM .GT. 0) THEN
          DO NN = 8,7+NSPECI
            DO K = K1,K2
              KK  =  K + KADD
              KBD = KK - KBDD
              KCD = KK + KBDD
 
              DO J = J1,J2
                DO I = I1,I2
                  QAV(I,J,K,NN) = R6I * (2.0D0  * Q(I,J,KBD,NN,N) +
     >                                   5.0D0 * Q(I,J, KK,NN,N) -
     >                                          Q(I,J,KCD,NN,N))
                END DO
              END DO
            END DO
          END DO
        END IF

        IF(ISGSK .EQ. 1) THEN
          DO K = K1,K2
            KK  =  K + KADD
            KBD = KK - KBDD
            KCD = KK + KBDD
 
            DO J = J1,J2
              DO I = I1,I2
                QAV(I,J,K,7) = R6I * (2.0D0  * Q(I,J,KBD,7,N) +
     >                                5.0D0 * Q(I,J, KK,7,N) -
     >                                       Q(I,J,KCD,7,N))
              END DO
            END DO
          END DO
        END IF

      END IF

      DO K = K1,K2
        DO J = J1,J2
          DO I = I1,I2
            UAV(I,J,K) = QAV(I,J,K,2) / QAV(I,J,K,1)
          END DO
        END DO
      END DO
      DO K = K1,K2
        DO J = J1,J2
          DO I = I1,I2
            VAV(I,J,K) = QAV(I,J,K,3) / QAV(I,J,K,1)
          END DO
        END DO
      END DO
      DO K = K1,K2
        DO J = J1,J2
          DO I = I1,I2
            WAV(I,J,K) = QAV(I,J,K,4) / QAV(I,J,K,1)
          END DO
        END DO
      END DO
 
      IF(ISGSK .EQ. 1) THEN
        DO K = K1,K2
          DO J = J1,J2
            DO I = I1,I2
              EKAV(I,J,K) = QAV(I,J,K,7) / QAV(I,J,K,1)
            END DO
          END DO
        END DO
      END IF
 
      IF(ICHEM .GT. 0) THEN
        DO NN = 1,NSPECI
          DO K = K1,K2
            DO J = J1,J2
              DO I = I1,I2
                COAV(I,J,K,NN) = QAV(I,J,K,7+NN) / QAV(I,J,K,1)
              END DO
            END DO
          END DO
        END DO
        IF(ICHEM .EQ. 1) THEN
          DO K = K1,K2
            DO J = J1,J2
              DO I = I1,I2
                HFAV(I,J,K,1) = COAV(I,J,K,1) * EFORM
              END DO
            END DO
          END DO
        ELSE IF(ICHEM .EQ. 2) THEN
          DO NN = 1,4
            DO K = K1,K2
              DO J = J1,J2
                DO I = I1,I2
                  HFAV(I,J,K,NN) = 0.0D0
                END DO
              END DO
            END DO
          END DO
          DO NN = 1,NSPECI
            DO K = K1,K2
              DO J = J1,J2
                DO I = I1,I2
                  HFAV(I,J,K,1) = HFAV(I,J,K,1) + 
     >                                    HFK(NN) * COAV(I,J,K,NN)
                  HFAV(I,J,K,2) = HFAV(I,J,K,2) + 
     >                                    CPK(NN) * COAV(I,J,K,NN)
                  HFAV(I,J,K,3) = HFAV(I,J,K,3) + 
     >                                    CVK(NN) * COAV(I,J,K,NN)
                  HFAV(I,J,K,4) = HFAV(I,J,K,4) + 
     >                                    RGK(NN) * COAV(I,J,K,NN)
                END DO
              END DO
            END DO
          END DO
        END IF
      END IF
 
      IF(ISGSK .EQ. 1) THEN
        DO K = K1,K2
          DO J = J1,J2
            DO I = I1,I2
              RKE  = 0.5D0 * (UAV(I,J,K) * UAV(I,J,K) +
     >                       VAV(I,J,K) * VAV(I,J,K) +
     >                       WAV(I,J,K) * WAV(I,J,K)) + EKAV(I,J,K)
              EI = QAV(I,J,K,5) / QAV(I,J,K,1) - RKE
              TAV(I,J,K) = (EI - HFAV(I,J,K,1)) / HFAV(I,J,K,3)
              PAV(I,J,K) = QAV(I,J,K,1) * HFAV(I,J,K,4) * TAV(I,J,K)
              HAV(I,J,K) = (QAV(I,J,K,5) + PAV(I,J,K)) / QAV(I,J,K,1)
            END DO
          END DO
        END DO
      ELSE
        DO K = K1,K2
          DO J = J1,J2
            DO I = I1,I2
              RKE  = 0.5D0 * (UAV(I,J,K) * UAV(I,J,K) +
     >                       VAV(I,J,K) * VAV(I,J,K) +
     >                       WAV(I,J,K) * WAV(I,J,K))
              EI = QAV(I,J,K,5) / QAV(I,J,K,1) - RKE
              TAV(I,J,K) = (EI - HFAV(I,J,K,1)) / HFAV(I,J,K,3)
              PAV(I,J,K) = QAV(I,J,K,1) * HFAV(I,J,K,4) * TAV(I,J,K)
            END DO
          END DO
        END DO
      END IF

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE FLOWIO()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER ( NUNIT = 35 )
      CHARACTER FLNAME*80

!      WRITE(FLNAME,'(A,"FLOW_",I3.3,".DATA")')
!     >                           PREFIX(1:LENGTH), NFLOW
!      WRITE(6,'(/4X,A,A/)') "WRITING FLOW FILE: ", FLNAME
!      OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
!      WRITE(NUNIT) IMAX, JMAX, KMAX
!      WRITE(NUNIT) ISGSK, ICHEM, NSPECI, IDYN
!      WRITE(NUNIT) NADV, TIME

!      WRITE(NUNIT) ((( Q(I,J,K,1,1),I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      WRITE(NUNIT) ((( U(I,J,K)    ,I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      WRITE(NUNIT) ((( V(I,J,K)    ,I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      WRITE(NUNIT) ((( W(I,J,K)    ,I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      WRITE(NUNIT) ((( T(I,J,K)    ,I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      IF(ISGSK .EQ. 1)
!     >WRITE(NUNIT) (((EK(I,J,K)    ,I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      DO NS = 1, NSPECI
!         WRITE(NUNIT) (((CONC(I,J,K,NS),I=0,IMAX),J=0,JMAX),K=0,KMAX)
!      ENDDO
!      CLOSE(NUNIT)

!      IF(NADV .EQ. NSTART) THEN
         OPEN(NUNIT, FILE = 'grid.dat', FORM = "UNFORMATTED")
         WRITE(NUNIT) IMAX-1, JMAX-1, KMAX-1
         WRITE(NUNIT)
     >     (((SNGL((I-1)*DX),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >     (((SNGL((J-1)*DY),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >     (((SNGL((K-1)*DZ),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1)
         CLOSE(NUNIT)
!      ENDIF

      RGAS = 287.2
      GAMA = 1.4
      CV   = RGAS * (GAMA - 1.0)

      WRITE(FLNAME,'(A,"flow_",I3.3,".dat")')
     >                           PREFIX(1:LENGTH), NFLOW
      WRITE(6,'(/4X,A,A/)') "WRITING FLOW FILE: ", FLNAME
      OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
      WRITE(NUNIT) IMAX-1, JMAX-1, KMAX-1
      WRITE(NUNIT) (SNGL(TIME),I=1,4)
      WRITE(NUNIT)
     >  (((SNGL(Q(I,J,K,1,1)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >  (((SNGL(Q(I,J,K,2,1)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >  (((SNGL(Q(I,J,K,3,1)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >  (((SNGL(Q(I,J,K,4,1)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
     >  (((SNGL(Q(I,J,K,1,1) * (CV * T(I,J,K) +
     >    0.5 * (U(I,J,K)**2 + V(I,J,K)**2 + W(I,J,K)**2))),
     >                        I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1)
!     > ,(((SNGL(EK(I,J,K)),I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),
!     > ((((SNGL(CONC(I,J,K,NS)),
!     >     I=1,IMAX-1),J=1,JMAX-1),K=1,KMAX-1),NS=1,NSPECI)
      CLOSE(NUNIT)

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE KSOURCE()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE DUDX(:), DUDY(:), DUDZ(:),
     >          DVDX(:), DVDY(:), DVDZ(:),
     >          DWDX(:), DWDY(:), DWDZ(:)

      PARAMETER ( R12I = 1.0D0 / 12.0D0,
     >           THIRD = 1.0D0 / 3.0D0,
     >            TWO3 = 2.0D0 / 3.0D0 )

      ALLOCATE( DUDX(IMAX-1), DUDY(IMAX-1), DUDZ(IMAX-1),
     >          DVDX(IMAX-1), DVDY(IMAX-1), DVDZ(IMAX-1),
     >          DWDX(IMAX-1), DWDY(IMAX-1), DWDZ(IMAX-1))
      DUDX=0D0
      DUDY=0D0
      DUDZ=0D0
      DVDX=0D0
      DVDY=0D0
      DVDZ=0D0
      DWDX=0D0
      DWDY=0D0
      DWDZ=0D0

      DO K = 1, KMAX - 1
         DO J = 1, JMAX - 1

            DX2 = DXI * 0.5D0
            DY2 = DYI * 0.5D0
            DZ2 = DZI * 0.5D0

            DX4 = DXI * R12I
            DY4 = DYI * R12I
            DZ4 = DZI * R12I

            IF(NSCHEME .EQ. 2) THEN

               DO I = 1, IMAX-1
                  DUDX(I) = DX2 * (U(I+1,J,K) - U(I-1,J,K))
               ENDDO
               DO I = 1, IMAX-1
                  DVDX(I) = DX2 * (V(I+1,J,K) - V(I-1,J,K))
               ENDDO
               DO I = 1, IMAX-1
                  DWDX(I) = DX2 * (W(I+1,J,K) - W(I-1,J,K))
               ENDDO

               DO I = 1, IMAX-1
                  DUDY(I) = DY2 * (U(I,J+1,K) - U(I,J-1,K))
               ENDDO
               DO I = 1, IMAX-1
                  DVDY(I) = DY2 * (V(I,J+1,K) - V(I,J-1,K))
               ENDDO
               DO I = 1, IMAX-1
                  DWDY(I) = DY2 * (W(I,J+1,K) - W(I,J-1,K))
               ENDDO

               DO I = 1, IMAX-1
                  DUDZ(I) = DZ2 * (U(I,J,K+1) - U(I,J,K-1))
               ENDDO
               DO I = 1, IMAX-1
                  DVDZ(I) = DZ2 * (V(I,J,K+1) - V(I,J,K-1))
               ENDDO
               DO I = 1, IMAX-1
                  DWDZ(I) = DZ2 * (W(I,J,K+1) - W(I,J,K-1))
               ENDDO
            ELSE

               DO I = 1, IMAX-1
                  DUDX(I) = DX4 * (U(I-2,J,K) - U(I+2,J,K) +
     >                    8.0D0 * (U(I+1,J,K) - U(I-1,J,K)))
               ENDDO

               DO I = 1, IMAX-1
                  DVDX(I) = DX4 * (V(I-2,J,K) - V(I+2,J,K) +
     >                    8.0D0 * (V(I+1,J,K) - V(I-1,J,K)))
               ENDDO

               DO I = 1, IMAX-1
                  DWDX(I) = DX4 * (W(I-2,J,K) - W(I+2,J,K) +
     >                    8.0D0 * (W(I+1,J,K) - W(I-1,J,K)))
               ENDDO

               IF(J .EQ. 1 .OR. J .EQ. JMAX-1) THEN
                  DO I = 1, IMAX-1
                     DUDY(I) = DY2 * (U(I,J+1,K) - U(I,J-1,K))
                  ENDDO
                  DO I = 1, IMAX-1
                     DVDY(I) = DY2 * (V(I,J+1,K) - V(I,J-1,K))
                  ENDDO
                  DO I = 1, IMAX-1
                     DWDY(I) = DY2 * (W(I,J+1,K) - W(I,J-1,K))
                  ENDDO
               ELSE
                  DO I = 1, IMAX-1
                     DUDY(I) = DY4 * (U(I,J-2,K) - U(I,J+2,K) +
     >                       8.0D0 * (U(I,J+1,K) - U(I,J-1,K)))
                  ENDDO

                  DO I = 1, IMAX-1
                     DVDY(I) = DY4 * (V(I,J-2,K) - V(I,J+2,K) +
     >                       8.0D0 * (V(I,J+1,K) - V(I,J-1,K)))
                  ENDDO

                  DO I = 1, IMAX-1
                     DWDY(I) = DY4 * (W(I,J-2,K) - W(I,J+2,K) +
     >                       8.0D0 * (W(I,J+1,K) - W(I,J-1,K)))
                  ENDDO
               ENDIF

               DO I = 1, IMAX-1
                  DUDZ(I) = DZ4 * (U(I,J,K-2) - U(I,J,K+2) +
     >                    8.0D0 * (U(I,J,K+1) - U(I,J,K-1)))
               ENDDO

               DO I = 1, IMAX-1
                  DVDZ(I) = DZ4 * (V(I,J,K-2) - V(I,J,K+2) +
     >                    8.0D0 * (V(I,J,K+1) - V(I,J,K-1)))
               ENDDO

               DO I = 1, IMAX-1
                  DWDZ(I) = DZ4 * (W(I,J,K-2) - W(I,J,K+2) +
     >                    8.0D0 * (W(I,J,K+1) - W(I,J,K-1)))
               ENDDO
            ENDIF

            DO I = 1, IMAX-1
               SXX = DUDX(I)
               SYY = DVDY(I)
               SZZ = DWDZ(I)
               SXY = 0.5D0 * (DUDY(I) + DVDX(I))
               SXZ = 0.5D0 * (DUDZ(I) + DWDX(I))
               SYZ = 0.5D0 * (DVDZ(I) + DWDY(I))

               DIVM = (SXX + SYY + SZZ) * THIRD

               SEK    = SQRT(EK(I,J,K))
!               EKCOEF = Q(I,J,K,1,N) * CNU(I,J,K) * SEK * DELTA
               EKCOEF = Q(I,J,K,1,N) * CNUK * SEK * DELTA
               RHEV   = 2.0D0 * EKCOEF
               RHEK   = TWO3 * Q(I,J,K,1,N) * EK(I,J,K)

               SGSXX = - RHEV * (SXX - DIVM) + RHEK
               SGSYY = - RHEV * (SYY - DIVM) + RHEK
               SGSZZ = - RHEV * (SZZ - DIVM) + RHEK
               SGSXY = - RHEV * SXY
               SGSXZ = - RHEV * SXZ
               SGSYZ = - RHEV * SYZ

               PRODK = - CPROD *
     >          (SGSXX * DUDX(I) + SGSXY * DVDX(I) + SGSXZ * DWDX(I) +
     >           SGSXY * DUDY(I) + SGSYY * DVDY(I) + SGSYZ * DWDY(I) +
     >           SGSXZ * DUDZ(I) + SGSYZ * DVDZ(I) + SGSZZ * DWDZ(I))

!               DISSK = CEP(I,J,K) * Q(I,J,K,1,N) * EK(I,J,K) * SEK /
               DISSK = CEPSK * Q(I,J,K,1,N) * EK(I,J,K) * SEK /
     >                                                          DELTA
               DU(I,J,K,7) = DU(I,J,K,7) + DT * (PRODK - DISSK)
            END DO
         END DO
      END DO

      DEALLOCATE( DUDX, DUDY, DUDZ,
     >          DVDX, DVDY, DVDZ,
     >          DWDX, DWDY, DWDZ)

      RETURN
      END
!----------------------------------------------------------------------
      SUBROUTINE MAP(NSTART, NEND, N, NP, N1, N2)

      NMAX = N2 - N1 + 1

      NCHUNK = NMAX / NP
      NR = MOD(NMAX, NP)
 
      NSTART = N1 + N * NCHUNK
      NEND   = NSTART  + NCHUNK - 1
      IF(N .LE. NR-1)THEN
         NSTART = NSTART + N
         NEND   = NEND   + N + 1
      ELSE
         NSTART = NSTART + NR
         NEND   = NEND   + NR
      END IF
      IF(N .EQ. NP-1) NEND = NMAX
 
      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE RESTART(ISWITCH)

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      PARAMETER (NUNIT = 35)
      CHARACTER FLNAME*80
      LOGICAL YES

      IF(ISWITCH .EQ. 0) THEN
         NREST = NREST + 1

         OPEN(55, FILE = PREFIX(1:LENGTH)//'RESTART.DATA')
         WRITE(55,*) NREST
         CLOSE(55)

         WRITE(FLNAME,'(A,"REST_",I3.3,".DATA")')
     >                                 PREFIX(1:LENGTH), NREST
         WRITE(6,'(5X,A,A,/)')"WRITING RESTART FILE: ", FLNAME
         OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
         WRITE(NUNIT) IMAX, JMAX, KMAX
         WRITE(NUNIT) ISGSK, ICHEM, NSPECI, IDYN
         WRITE(NUNIT) NADV, NSTAT, NFLOW, NTRACE
         WRITE(NUNIT) TIME

         DO L = 1, ND
            WRITE(NUNIT) (((Q(I,J,K,L,1),I=0,IMAX),J=0,JMAX),K=0,KMAX)
         ENDDO

         CLOSE(NUNIT)

      ELSE

         INQUIRE(FILE = PREFIX(1:LENGTH)//'RESTART.DATA', EXIST = YES)
         IF(.NOT. YES) THEN
            WRITE(6,*) "NO RESTART.DATA FILE"
            STOP
         ENDIF
         OPEN(55, FILE = PREFIX(1:LENGTH)//'RESTART.DATA')
         READ(55,*) NREST
         CLOSE(55)

         WRITE(FLNAME,'(A,"REST_",I3.3,".DATA")')
     >                                 PREFIX(1:LENGTH), NREST
         WRITE(6,'(5X,A,A,/)')"READING RESTART FILE: ", FLNAME
         OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
         INQUIRE(FILE = FLNAME, EXIST = YES)
         IF(.NOT. YES) THEN
            WRITE(6,*) "FILE :", FLNAME," DOES NOT EXISTS"
            STOP
         ENDIF
         READ(NUNIT) I, J, K
         READ(NUNIT) I, J, K, L
         READ(NUNIT) NADV, NSTAT, NFLOW, NTRACE
         READ(NUNIT) TIME

         DO L = 1, ND
            READ(NUNIT) (((Q(I,J,K,L,1),I=0,IMAX),J=0,JMAX),K=0,KMAX)
         ENDDO

         CLOSE(NUNIT)

         NSTART = NADV + 1
         NEND   = NSTART + NEND - 1

         DO K = 0, KMAX
            DO J = 0, JMAX
               DO I = 0, IMAX

                  DO L = 1, ND
                     Q(I,J,K,L,2) = Q(I,J,K,L,1)
                  ENDDO

                  U(I,J,K) = Q(I,J,K,2,1) / Q(I,J,K,1,1)
                  V(I,J,K) = Q(I,J,K,3,1) / Q(I,J,K,1,1)
                  W(I,J,K) = Q(I,J,K,4,1) / Q(I,J,K,1,1)
 
                  IF(ICHEM .GT. 0) THEN
                     DO L = 1, NSPECI
                        CONC(I,J,K,L) = Q(I,J,K,7+L,1) / Q(I,J,K,1,1)
                     ENDDO
 
                     IF(ICHEM .EQ. 1) THEN
                        HF(I,J,K,1) = EFORM * CONC(I,J,K,1)
                     ELSE IF(ICHEM .EQ. 2) THEN
                        DO L = 1,4
                           HF(I,J,K,L) = 0.0D0
                        ENDDO
                        DO NS = 1,NSPECI
                           HF(I,J,K,1) = HF(I,J,K,1) +
     >                                         CONC(I,J,K,NS) * HFK(NS)
                           HF(I,J,K,2) = HF(I,J,K,2) +
     >                                         CONC(I,J,K,NS) * CPK(NS)
                           HF(I,J,K,3) = HF(I,J,K,3) +
     >                                         CONC(I,J,K,NS) * CVK(NS)
                           HF(I,J,K,4) = HF(I,J,K,4) +
     >                                         CONC(I,J,K,NS) * RGK(NS)
                        ENDDO
                     ENDIF
                  ENDIF
 
                  IF(ISGSK .EQ. 1) THEN
                     EK(I,J,K) = MAX(Q(I,J,K,7,1) / Q(I,J,K,1,1), 0.0D0)

                     RKE = 0.5D0 * (U(I,J,K) * U(I,J,K) +
     >                            V(I,J,K) * V(I,J,K) +
     >                            W(I,J,K) * W(I,J,K)) + EK(I,J,K)

                     EI = Q(I,J,K,5,1) / Q(I,J,K,1,1) - RKE
                     T(I,J,K) = (EI - HF(I,J,K,1)) / HF(I,J,K,3)
                     P(I,J,K) = Q(I,J,K,1,1) * HF(I,J,K,4) * T(I,J,K)
                     H(I,J,K) = (Q(I,J,K,5,1) + P(I,J,K)) / Q(I,J,K,1,1)
                  ELSE
                     RKE = 0.5D0 * ( U(I,J,K) * U(I,J,K) +
     >                             V(I,J,K) * V(I,J,K) +
     >                             W(I,J,K) * W(I,J,K))

                     EI = Q(I,J,K,5,1) / Q(I,J,K,1,1) - RKE
                     T(I,J,K) = (EI - HF(I,J,K,1)) / HF(I,J,K,3)
                     P(I,J,K) = Q(I,J,K,1,1) * HF(I,J,K,4) * T(I,J,K)
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE SETBC()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      J = 0

      I1 = 0
      I2 = IMAX

      K1 = 0
      K2 = KMAX

      DO K = K1, K2
         DO I = I1, I2
            U(I,J,K) =  U(I,J+1,K)
            V(I,J,K) = -V(I,J+1,K)
            W(I,J,K) =  W(I,J+1,K)
            P(I,J,K) =  P(I,J+1,K)
            T(I,J,K) =  T(I,J+1,K)

            EI  = HF(I,J,K,1) + HF(I,J,K,3) * T(I,J,K)
            RKE = 0.5D0 * (U(I,J,K)**2 +
     >                    V(I,J,K)**2 +
     >                    W(I,J,K)**2)

            Q(I,J,K,1,M) = P(I,J,K) / (HF(I,J,K,4) * T(I,J,K))
            Q(I,J,K,2,M) = Q(I,J,K,1,M) * U(I,J,K)
            Q(I,J,K,3,M) = Q(I,J,K,1,M) * V(I,J,K)
            Q(I,J,K,4,M) = Q(I,J,K,1,M) * W(I,J,K)
            Q(I,J,K,5,M) = Q(I,J,K,1,M) * (EI + RKE)
         ENDDO

         IF(ICHEM .GT. 0) THEN
            DO NS = 1, NSPECI
               DO I = I1, I2
                  CONC(I,J,K,NS) = CONC(I,J+1,K,NS)
                  Q(I,J,K,7+NS,M) = CONC(I,J,K,NS) * Q(I,J,K,1,M)
               ENDDO
            ENDDO

            IF(ICHEM .EQ. 1) THEN
               DO I = I1, I2
                  HF(I,J,K,1) = HF(I,J+1,K,1)
               ENDDO
            ELSE IF(ICHEM .EQ. 2) THEN
               DO L = 1, 4
                  DO I = I1, I2
                     HF(I,J,K,L) = HF(I,J+1,K,L)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

         IF(ISGSK .EQ. 1) THEN
            DO I = I1, I2
               EK(I,J,K) = EK(I,J+1,K)
               Q(I,J,K,7,M) = Q(I,J,K,1,M) * EK(I,J,K)
               Q(I,J,K,5,M) = Q(I,J,K,5,M) + Q(I,J,K,7,M)
               H(I,J,K) = (Q(I,J,K,5,M) + P(I,J,K)) / Q(I,J,K,1,M)
            ENDDO
         ENDIF
      ENDDO

      J = JMAX

      I1 = 0
      I2 = IMAX

      K1 = 0
      K2 = KMAX

      DO K = K1, K2
         DO I = I1, I2
            U(I,J,K) =  U(I,J-1,K)
            V(I,J,K) = -V(I,J-1,K)
            W(I,J,K) =  W(I,J-1,K)
            P(I,J,K) =  P(I,J-1,K)
            T(I,J,K) =  T(I,J-1,K)

            EI  = HF(I,J,K,1) + HF(I,J,K,3) * T(I,J,K)
            RKE = 0.5D0 * (U(I,J,K)**2 +
     >                    V(I,J,K)**2 +
     >                    W(I,J,K)**2)

            Q(I,J,K,1,M) = P(I,J,K) / (HF(I,J,K,4) * T(I,J,K))
            Q(I,J,K,2,M) = Q(I,J,K,1,M) * U(I,J,K)
            Q(I,J,K,3,M) = Q(I,J,K,1,M) * V(I,J,K)
            Q(I,J,K,4,M) = Q(I,J,K,1,M) * W(I,J,K)
            Q(I,J,K,5,M) = Q(I,J,K,1,M) * (EI + RKE)
         ENDDO

         IF(ICHEM .GT. 0) THEN
            DO NS = 1, NSPECI
               DO I = I1, I2
                  CONC(I,J,K,NS) = CONC(I,J-1,K,NS)
                  Q(I,J,K,7+NS,M) = CONC(I,J,K,NS) * Q(I,J,K,1,M)
               ENDDO
            ENDDO

            IF(ICHEM .EQ. 1) THEN
               DO I = I1, I2
                  HF(I,J,K,1) = HF(I,J-1,K,L)
               ENDDO
            ELSE IF(ICHEM .EQ. 2) THEN
               DO L = 1, 4
                  DO I = I1, I2
                     HF(I,J,K,L) = HF(I,J-1,K,L)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
           
         IF(ISGSK .EQ. 1) THEN
            DO I = I1, I2
               EK(I,J,K) = EK(I,J-1,K)
               Q(I,J,K,7,M) = Q(I,J,K,1,M) * EK(I,J,K)
               Q(I,J,K,5,M) = Q(I,J,K,5,M) + Q(I,J,K,7,M)
               H(I,J,K) = (Q(I,J,K,5,M) + P(I,J,K)) / Q(I,J,K,1,M)
            ENDDO
         ENDIF
      ENDDO

      J1 = 0
      J2 = JMAX

      K1 = -1
      K2 = KMAX + 1

      IF(NSCHEME .EQ. 4) THEN
         I1 = -1
      ELSE
         I1 = 0
      ENDIF
      I2 = 0

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               U(I,J,K) = U(IMAX-1+I,J,K)
               V(I,J,K) = V(IMAX-1+I,J,K)
               W(I,J,K) = W(IMAX-1+I,J,K)
               P(I,J,K) = P(IMAX-1+I,J,K)
               T(I,J,K) = T(IMAX-1+I,J,K)

               Q(I,J,K,1,M) = Q(IMAX-1+I,J,K,1,M)
               Q(I,J,K,2,M) = Q(IMAX-1+I,J,K,2,M)
               Q(I,J,K,3,M) = Q(IMAX-1+I,J,K,3,M)
               Q(I,J,K,4,M) = Q(IMAX-1+I,J,K,4,M)
               Q(I,J,K,5,M) = Q(IMAX-1+I,J,K,5,M)

               IF(ISGSK .EQ. 1) THEN
                  EK(I,J,K) = EK(IMAX-1+I,J,K)
                  Q(I,J,K,7,M) = Q(IMAX-1+I,J,K,7,M)
               ENDIF

               IF(ICHEM .GT. 0) THEN
                  DO NS = 1, NSPECI
                     CONC(I,J,K,NS) = CONC(IMAX-1+I,J,K,NS)
                     Q(I,J,K,7+NS,M) = Q(IMAX-1+I,J,K,7+NS,M)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      J1 = 0
      J2 = JMAX

      K1 = -1
      K2 = KMAX + 1

      I1 = IMAX
      IF(NSCHEME .EQ. 4) THEN
         I2 = IMAX + 1
      ELSE
         I2 = IMAX
      ENDIF

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               U(I,J,K) = U(I-IMAX+1,J,K)
               V(I,J,K) = V(I-IMAX+1,J,K)
               W(I,J,K) = W(I-IMAX+1,J,K)
               P(I,J,K) = P(I-IMAX+1,J,K)
               T(I,J,K) = T(I-IMAX+1,J,K)

               Q(I,J,K,1,M) = Q(I-IMAX+1,J,K,1,M)
               Q(I,J,K,2,M) = Q(I-IMAX+1,J,K,2,M)
               Q(I,J,K,3,M) = Q(I-IMAX+1,J,K,3,M)
               Q(I,J,K,4,M) = Q(I-IMAX+1,J,K,4,M)
               Q(I,J,K,5,M) = Q(I-IMAX+1,J,K,5,M)

               IF(ISGSK .EQ. 1) THEN
                  EK(I,J,K) = EK(I-IMAX+1,J,K)
                  Q(I,J,K,7,M) = Q(I-IMAX+1,J,K,7,M)
               ENDIF

               IF(ICHEM .GT. 0) THEN
                  DO NS = 1, NSPECI
                     CONC(I,J,K,NS) = CONC(I-IMAX+1,J,K,NS)
                     Q(I,J,K,7+NS,M) = Q(I-IMAX+1,J,K,7+NS,M)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      I1 = -1
      I2 = IMAX + 1

      J1 = 0
      J2 = JMAX

      K1 = KMAX
      K2 = KMAX
      IF(NSCHEME .EQ. 4) K2 = KMAX + 1

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               U(I,J,K) = U(I,J,K-KMAX+1)
               V(I,J,K) = V(I,J,K-KMAX+1)
               W(I,J,K) = W(I,J,K-KMAX+1)
               P(I,J,K) = P(I,J,K-KMAX+1)
               T(I,J,K) = T(I,J,K-KMAX+1)

               Q(I,J,K,1,M) = Q(I,J,K-KMAX+1,1,M)
               Q(I,J,K,2,M) = Q(I,J,K-KMAX+1,2,M)
               Q(I,J,K,3,M) = Q(I,J,K-KMAX+1,3,M)
               Q(I,J,K,4,M) = Q(I,J,K-KMAX+1,4,M)
               Q(I,J,K,5,M) = Q(I,J,K-KMAX+1,5,M)

               IF(ISGSK .EQ. 1) THEN
                  EK(I,J,K) = EK(I,J,K-KMAX+1)
                  Q(I,J,K,7,M) = Q(I,J,K-KMAX+1,7,M)
               ENDIF

               IF(ICHEM .GT. 0) THEN
                  DO NS = 1, NSPECI
                     CONC(I,J,K,NS) = CONC(I,J,K-KMAX+1,NS)
                     Q(I,J,K,7+NS,M) = Q(I,J,K-KMAX+1,7+NS,M)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      I1 = -1
      I2 = IMAX + 1

      J1 = 0
      J2 = JMAX
      
      IF(NSCHEME .EQ. 4) THEN
         K1 = -1
      ELSE
         K1 = 0
      ENDIF
      K2 = 0

      DO K = K1, K2
         DO J = J1, J2
            DO I = I1, I2
               U(I,J,K) = U(I,J,KMAX-1+K)
               V(I,J,K) = V(I,J,KMAX-1+K)
               W(I,J,K) = W(I,J,KMAX-1+K)
               P(I,J,K) = P(I,J,KMAX-1+K)
               T(I,J,K) = T(I,J,KMAX-1+K)

               Q(I,J,K,1,M) = Q(I,J,KMAX-1+K,1,M)
               Q(I,J,K,2,M) = Q(I,J,KMAX-1+K,2,M)
               Q(I,J,K,3,M) = Q(I,J,KMAX-1+K,3,M)
               Q(I,J,K,4,M) = Q(I,J,KMAX-1+K,4,M)
               Q(I,J,K,5,M) = Q(I,J,KMAX-1+K,5,M)

               IF(ISGSK .EQ. 1) THEN
                  EK(I,J,K) = EK(I,J,KMAX-1+K)
                  Q(I,J,K,7,M) = Q(I,J,KMAX-1+K,7,M)
               ENDIF

               IF(ICHEM .GT. 0) THEN
                  DO NS = 1, NSPECI
                     CONC(I,J,K,NS) = CONC(I,J,KMAX-1+K,NS)
                     Q(I,J,K,7+NS,M) = Q(I,J,KMAX-1+K,7+NS,M)
                  ENDDO
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE SETIV()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE UM(:),UT(:,:,:),SECJ(:)
      ALLOCATABLE AEX(:),BEX(:)
      ALLOCATABLE EPS(:),ALPA(:),PHASE(:),RK1(:),RK2(:),XPHS(:)
      ALLOCATABLE SK1(:),SK2(:),TK1(:),TK2(:)
      ALLOCATABLE C1I(:),C2I(:)
      ALLOCATABLE SI(:,:,:),SIX(:,:,:)
      ALLOCATABLE COX(:,:,:)
      ALLOCATABLE YA(:),YB(:)

!      DO K = -2, KMAX+2
!         DO J = -2, JMAX+2
!            DO I = -2, IMAX+2
!               CNU(I,J,K) = CNUK
!               CEN(I,J,K) = 1.0D0 / TPRANDLT
!               CEP(I,J,K) = CEPSK
!            END DO
!         END DO
!      END DO

      DO L = 1, ND
         DO K = -2, KMAX+2
            DO J = -2, JMAX+2
               DO I = -2, IMAX+2
                  Q(I,J,K,L,1) = 0.0D0
                  Q(I,J,K,L,2) = 0.0D0
                  DU(I,J,K,L)  = 0.0D0
               END DO
            END DO
         END DO
      END DO

      DO K = -2, KMAX+2 
         DO J = -2, JMAX+2
            DO I = -2, IMAX+2
               U(I,J,K)    = 0.0D0
               V(I,J,K)    = UREF
               W(I,J,K)    = 0.0D0
               P(I,J,K)    = PREF
               T(I,J,K)    = TREF 
               IF(ISGSK == 1) THEN
                  H(I,J,K)    = 0.0D0
                  EK(I,J,K)   = 0.0D0
               ENDIF
               HF(I,J,K,1) = 0.0D0
               HF(I,J,K,2) = 0.0D0
               HF(I,J,K,3) = 0.0D0
               HF(I,J,K,4) = 0.0D0
               IF(ICHEM > 0) THEN
                  DO NS = 1, NSPECI
                     CONC(I,J,K,NS) = 0.0D0
                  ENDDO
               ENDIF
            END DO
         END DO
      END DO
!
!-----SPECIES PROPERTIES
!
      GAMMA = 1.4
      RC    = RUNIV / RMOLWT
      CP    = RC * GAMMA / (GAMMA - 1.0D0)
      CV    = CP - RC
      DH    = 0.0D0

      IF(ICHEM .EQ. 2) THEN
         DO NS = 1, NSPECI
            RGK(NS) = RUNIV / RMWT(NS)
            CVK(NS) = CPK(NS) - RGK(NS)
            HFK(NS) = HFK(NS) - CPK(NS) * TSTND
         ENDDO

         CP = 0.0D0
         CV = 0.0D0
         RC = 0.0D0
         DH = 0.0D0
         DO NS = 1, NSPECI
            CP = CP + CONC0(NS) * CPK(NS)
            CV = CV + CONC0(NS) * CVK(NS)
            RC = RC + CONC0(NS) * RGK(NS)
            DH = DH + CONC0(NS) * HFK(NS)
         END DO
         GAMMA = CP / CV
      END IF

      CREF   = SQRT(GAMMA * RC * TREF)
      RHOREF = PREF / (RC * TREF)
!      RMUREF = RHOREF * UREF * ALREF / RELNO
!      RNUREF = RMUREF / RHOREF
      RMUREF = RHOREF * RNUREF
      RELNO  = UREF / RNUREF
      CMU    = RMUREF * (TREF + 110.0) / (TREF * SQRT(TREF))

      GM1 = 1.0D0 / (GAMMA - 1.0D0)
      GP1 = GAMMA + 1.0D0
      GM2 = GAMMA * GM1

      RMACH = UREF / CREF
      TSTAG = TREF * (1.0D0 + (GAMMA - 1.0D0) * RMACH**2 * 0.5D0)
      PSTAG = PREF * (TSTAG / TREF)**GM2
      RSTAG = PSTAG / (RC * TSTAG)
      ASTR2 = (2.0D0 / GP1) * GAMMA * (GAMMA - 1.0D0) * CV * TSTAG

      IF(ICHEM .EQ. 1) EFORM = CP * (TPROD - TREF)

      IF(ICHEM .EQ. 1) THEN
         DO K = -2, KMAX+2
            DO J = -2, JMAX+2
               DO I = -2, IMAX+2
                  HF(I,J,K,2) = CP
                  HF(I,J,K,3) = CV
                  HF(I,J,K,4) = RC
               END DO
            END DO
         END DO
      ELSE IF(ICHEM .EQ. 0) THEN
         DO K = -2, KMAX+2
            DO J = -2, JMAX+2
               DO I = -2, IMAX+2
                  HF(I,J,K,1) = DH
                  HF(I,J,K,2) = CP
                  HF(I,J,K,3) = CV
                  HF(I,J,K,4) = RC
               END DO
            END DO
         END DO
      END IF

      IF(ICHEM .LE. 1) THEN
         DO K = -2, KMAX+2
            DO J = -2, JMAX+2
                DO I = -2, IMAX+2
                   HFAV(I,J,K,1) = DH
                   HFAV(I,J,K,2) = CP
                   HFAV(I,J,K,3) = CV
                   HFAV(I,J,K,4) = RC
                END DO
             END DO
         END DO
      END IF

      IF(IRESTART .EQ. 1) RETURN

      ALLOCATE( UM(-2:IMAX+2),UT(-2:JMAX+2,4,3),SECJ(-2:IMAX+2),
     >         AEX(3),BEX(3),
     >         EPS(3),ALPA(3),PHASE(3),RK1(3),RK2(3),XPHS(3),
     >         SK1(3),SK2(3),TK1(3),TK2(3),
     >         C1I(-2:IMAX+2),C2I(-2:IMAX+2),
     >         SI(-2:JMAX+2,2,3),SIX(-2:JMAX+2,2,3),
     >         COX(-2:JMAX+2,2,3),
     >         YA(-2:JMAX+2),YB(-2:JMAX+2))
      UM=0D0
      UT=0D0
      SECJ=0D0
      AEX=0D0
      BEX=0D0
      EPS=0D0
      ALPA=0D0
      PHASE=0D0
      RK1=0D0
      RK2=0D0
      XPHS=0D0
      SK1=0D0
      SK2=0D0
      TK1=0D0
      TK2=0D0
      C1I=0D0
      C2I=0D0
      SI=0D0
      SIX=0D0
      COX=0D0
      YA=0D0
      YB=0D0

      EPS(1) = 0.130  !0.100
      EPS(2) = 0.130  !0.000
      EPS(3) = 0.000

      RK1(1) = 0.475
      RK1(2) = 0.350
      RK1(3) = 0.475

      RK2(1) = 0.750
      RK2(2) = 0.600
      RK2(3) = 0.750

      ALPA(1) = 0.4446
      ALPA(2) = 0.2223
      ALPA(3) = 0.11115

      PHASE(1) = 0.0
      PHASE(2) = 1.57076
      PHASE(3) = 4.712388

      XPHS(1) = 1.57076
      XPHS(2) = 0.0
      XPHS(3) = 0.0

      BETA = 2.0 * ALPA(1)
      DLET = 0.1
      XZRO = 0.25

      DI = XLEN / DBLE(IMAX-2)
      DJ = YLEN / DBLE(JMAX-1)
      DK = ZLEN / DBLE(KMAX-2)

      L2   = ((JMAX-1)/2 + 1)
      EL   = 1.0 / ALPA(1)
      COEF = SQRT(2.0 / XZRO**2)

      DO J = -2,JMAX+2
         S   =  (J - L2)* DJ * EL
         R   =  (J - 2) * DI * EL
         TT  =  (J - 2) * DK * EL
         C   = EXP(S)
         RIC = 1.0 / C
         UM(J) = 0.5 * UREF * (C - RIC) / (C + RIC)

         DO L = 1,3
            AEX(L)     = EXP(RK1(L) * S)
            BEX(L)     = EXP(RK2(L) * S)
            SI(J,1,L)  = COS(ALPA(L) * R + PHASE(L))
            SI(J,2,L)  = SIN(ALPA(L) * R + PHASE(L))
            SIX(J,1,L) = COS(ALPA(L)*R + XPHS(L))
            SIX(J,2,L) = SIN(ALPA(L)*R + XPHS(L))

            COX(J,1,1)   = COS(BETA*TT)
            SK1(L)       = 2.0/(AEX(L) + 1.0/AEX(L))
            SK2(L)       = 2.0/(BEX(L) + 1.0/BEX(L))
            TK2(L)       = (BEX(L)-1.0/BEX(L))/(BEX(L)+1.0/BEX(L))
            UT(J,1,L) = -RK1(L)*SK1(L)*(AEX(L) - 1.0/AEX(L))/
     >                                  (AEX(L) + 1.0/AEX(L))
            UT(J,2,L) =  RK2(L)*SK2(L)*(TK2(L)**2-SK2(L)**2)
            UT(J,3,L) = -ALPA(L) * SK1(L)
            UT(J,4,L) = -ALPA(L) * SK2(L) * TK2(L)
         END DO
      END DO
!------------------
! 2-D PERTURBATIONS
!------------------
      DO K = -2, KMAX+2
         DO J = -2, JMAX+2
            DO I = -2, IMAX+2

               EQ = EL * UREF
               U(I,J,K) = UM(J) -
     >            EQ * EPS(1) * (UT(J,1,1) * SI(I,1,1) +
     >                           UT(J,2,1) * SI(I,2,1)) -
     >            EQ * EPS(2) * (UT(J,1,2) * SI(I,1,2) +
     >                           UT(J,2,2) * SI(I,2,2)) -
     >            EQ * EPS(3) * (UT(J,1,3) * SI(I,1,3) +
     >                           UT(J,2,3) * SI(I,2,3))

               V(I,J,K) =
     >            EQ * EPS(1) * (UT(J,3,1) * SI(I,2,1) +
     >                           UT(J,4,1) * SI(I,1,1)) +
     >            EQ * EPS(2) * (UT(J,3,2) * SI(I,2,2) +
     >                           UT(J,4,2) * SI(I,1,2)) +
     >            EQ * EPS(3) * (UT(J,3,3) * SI(I,2,3) +
     >                           UT(J,4,3) * SI(I,1,3))

               W(I,J,K) = 0.0
            END DO
         END DO
      END DO
!-------------------
! 3-D  PERTURBATIONS
!-------------------
!      DO K = -2,KMAX+2
!         DO J = -2,JMAX+2
!            DO I = -2,IMAX+2
!
!               EQ = EL * UREF
!               DQ = EQ * DLET * COX(K,1,1)
!
!               U(I,J,K) = UM(J) -
!     >            EQ * EPS(1) * (UT(J,1,1) * SI(I,1,1) +
!     >                           UT(J,2,1) * SI(I,2,1)) -
!     >            EQ * EPS(2) * (UT(J,1,2) * SI(I,1,2) +
!     >                           UT(J,2,2) * SI(I,2,2)) -
!     >            EQ * EPS(3) * (UT(J,1,3) * SI(I,1,3) +
!     >                           UT(J,2,3) * SI(I,2,3)) -
!     >            DQ * EPS(1) * (UT(J,1,1) * SIX(I,1,1) +
!     >                           UT(J,2,1) * SIX(I,2,1)) -
!     >            DQ * EPS(3) * (UT(J,1,2) * SIX(I,1,2) +
!     >                           UT(J,2,2) * SIX(I,2,2)) -
!     >            DQ * EPS(3) * (UT(J,1,3) * SIX(I,1,3) +
!     >                           UT(J,2,3) * SIX(I,2,3))
!
!               V(I,J,K) =
!     >            EQ * EPS(1) * (UT(J,3,1) * SI(I,2,1) +
!     >                           UT(J,4,1) * SI(I,1,1)) +
!     >            EQ * EPS(2) * (UT(J,3,2) * SI(I,2,2) +
!     >                           UT(J,4,2) * SI(I,1,2)) +
!     >            EQ * EPS(3) * (UT(J,3,3) * SI(I,2,3) +
!     >                           UT(J,4,3) * SI(I,1,3)) +
!     >            DQ * EPS(1) * (UT(J,3,1) * SIX(I,2,1) +
!     >                           UT(J,4,1) * SIX(I,1,1)) +
!     >            DQ * EPS(3) * (UT(J,3,2) * SIX(I,2,2) +
!     >                           UT(J,4,2) * SIX(I,1,2)) +
!     >            DQ * EPS(3) * (UT(J,3,3) * SIX(I,2,3) +
!     >                           UT(J,4,3) * SIX(I,1,3))
!
!               W(I,J,K)  = 0.0
!            END DO
!         END DO
!      END DO
!--------------------------------------------------------
! TURBULENT MIXING LAYER WITH SUPER-IMPOSED PERTURBATIONS
!--------------------------------------------------------
!      DO I = 1, 10000
!         DUMMY = RAND()
!      END DO

!      URMS = 0.0

!      DO K = 1, KMAX-1
!         DO J = 1, JMAX-1
!            S1      = (J - L2) * DJ * EL
!            SECJ(J) = 2.0 / (EXP(S1) + 1.0 / EXP(S1))
!
!            DO I = 1, IMAX-1
!               URAND = RAND() * UREF * URMS * SECJ(J)
!               VRAND = RAND() * UREF * URMS * SECJ(J)
!               WRAND = RAND() * UREF * URMS * SECJ(J)
!
!               U(I,J,K) = U(I,J,K) + URAND
!               V(I,J,K) = V(I,J,K) + VRAND
!               W(I,J,K) = W(I,J,K) + WRAND
!            ENDDO
!         ENDDO
!      ENDDO
!-------------
! SCALAR FIELD
!-------------
      IF(ICHEM .EQ. 2) THEN
         DO J = -2, JMAX+2
            YI = (J-1) * DJ - 0.5*YLEN
            YIP1 = J * DJ - 0.5*YLEN
            S  = 0.5*(YI + YIP1)
            AY = ABS(4.0 * S)
            TT = 1.0/(1.0+0.2316419*AY)
            DD = 0.3989423 * EXP(-8.0 * S * S/2.0)
            CC = 1.0 - DD*TT*((((1.330274*TT-1.821256)*TT+1.781478)*TT-
     >                       0.3565638)*TT+0.3193815)
            IF(S .LT. 0.0) CC = 1.0 - CC
            YA(J) = 1.0 - CC
         END DO

         DO K = -2, KMAX+2
            DO J = -2, JMAX+2
               DO I = -2, IMAX+2
                  CONC(I,J,K,1) = YA(J)
                  CONC(I,J,K,2) = 1.0D0 - CONC(I,J,K,1)
               ENDDO
            ENDDO
         ENDDO
      ENDIF

      DEALLOCATE( UM,UT,SECJ,
     >         AEX,BEX,
     >         EPS,ALPA,PHASE,RK1,RK2,XPHS,
     >         SK1,SK2,TK1,TK2,
     >         C1I,C2I,
     >         SI,SIX,
     >         COX,
     >         YA,YB)

!!      SMALL = -0.0005
!      SM = -0.0005
!      XC = 0.5D0 * XLEN
!      YC = 0.5D0 * YLEN
!      ZC = 0.5D0 * ZLEN
!      RC = YLEN * 0.1
!      C  = UREF * YLEN * SM / RMACH
!
!      DO K = -2, KMAX+2
!         DO J = -2, JMAX+2
!            DO I = -2, IMAX+2
!               XX = DX * (I - 1) - XC
!               YY = DY * (J - 1) - YC
!               ZZ = DZ * (K - 1) - ZC
!
!               EXPON = -0.5D0 * (XX * XX + YY * YY) / (RC**2)
!               U(I,J,K) = UREF - C * YY * EXP(EXPON) / (RHOREF * RC**2)
!               V(I,J,K) = C * XX * EXP(EXPON) / (RHOREF * RC**2)
!               W(I,J,K) = 0.0D0
!
!!               EXPON = -0.5D0 * (XX * XX + YY * YY) / (RC**2)
!!               U(I,J,K) = C * YY * EXP(EXPON) / (RHOREF * RC**2)
!!               V(I,J,K) = UREF - C * XX * EXP(EXPON) / (RHOREF * RC**2)
!!               W(I,J,K) = 0.0D0
!
!!               EXPON = -0.5D0 * (XX * XX + ZZ * ZZ) / (RC**2)
!!               U(I,J,K) = UREF - C * ZZ * EXP(EXPON) / (RHOREF * RC**2)
!!               V(I,J,K) = 0.0D0
!!               W(I,J,K) = C * XX * EXP(EXPON) / (RHOREF * RC**2)
!
!               P(I,J,K) = PREF * RHOREF * C**2 * EXP(EXPON) / RC**2
!               T(I,J,K) = TREF
!            ENDDO
!         ENDDO
!      ENDDO

!      WRITE(6,'(2X,A,/)')"INITIALIZATION DONE!!"

      IF(ICHEM .EQ. 2) THEN 
        DO K = -2,KMAX+2
          DO J = -2,JMAX+2
            DO I = -2,IMAX+2
              HF(I,J,K,1) = 0.0D0
              HF(I,J,K,2) = 0.0D0
              HF(I,J,K,3) = 0.0D0
              HF(I,J,K,4) = 0.0D0
            END DO
          END DO
        END DO

        DO NS = 1,NSPECI
          DO K = -2,KMAX+2
            DO J = -2,JMAX+2
              DO I = -2,IMAX+2
                HF(I,J,K,1) = HF(I,J,K,1) + CONC(I,J,K,NS) * HFK(NS)
                HF(I,J,K,2) = HF(I,J,K,2) + CONC(I,J,K,NS) * CPK(NS)
                HF(I,J,K,3) = HF(I,J,K,3) + CONC(I,J,K,NS) * CVK(NS)
                HF(I,J,K,4) = HF(I,J,K,4) + CONC(I,J,K,NS) * RGK(NS)
              END DO
            END DO
          END DO
        END DO
      END IF

      IF(ISGSK .EQ. 1)THEN
        DO K = -2,KMAX+2
          DO J = -2,JMAX+2
            DO I = -2,IMAX+2
              RKE = 0.5D0 * (U(I,J,K)**2 +
     >                      V(I,J,K)**2 +
     >                      W(I,J,K)**2)
              EK(I,J,K) = EKREF * RKE
            END DO
          END DO
        END DO
      END IF 

      DO K = -2,KMAX+2
        DO J = -2,JMAX+2
          DO I = -2,IMAX+2
            T(I,J,K) = TREF
            P(I,J,K) = PREF

            EI  = HF(I,J,K,3) * T(I,J,K) + HF(I,J,K,1)
            RKE = 0.5D0 * (U(I,J,K)**2 + 
     >                    V(I,J,K)**2 + 
     >                    W(I,J,K)**2)

            Q(I,J,K,1,1) = P(I,J,K) / (HF(I,J,K,4) * T(I,J,K))
            Q(I,J,K,2,1) = Q(I,J,K,1,1) * U(I,J,K)
            Q(I,J,K,3,1) = Q(I,J,K,1,1) * V(I,J,K)
            Q(I,J,K,4,1) = Q(I,J,K,1,1) * W(I,J,K)
            Q(I,J,K,5,1) = Q(I,J,K,1,1) * (EI + RKE)

!            H(I,J,K) = (Q(I,J,K,5,1) + P(I,J,K)) / Q(I,J,K,1,1)

            Q(I,J,K,1,2) = Q(I,J,K,1,1)
            Q(I,J,K,2,2) = Q(I,J,K,2,1)
            Q(I,J,K,3,2) = Q(I,J,K,3,1)
            Q(I,J,K,4,2) = Q(I,J,K,4,1)
            Q(I,J,K,5,2) = Q(I,J,K,5,1)
          END DO
        END DO
      END DO

      IF(ISGSK .EQ. 1)THEN
        DO K = -2,KMAX+2
          DO J = -2,JMAX+2
            DO I = -2,IMAX+2
              Q(I,J,K,7,1)  = Q(I,J,K,1,1) * EK(I,J,K)
              Q(I,J,K,7,2)  = Q(I,J,K,7,1)

              Q(I,J,K,5,1)  = Q(I,J,K,5,1) + Q(I,J,K,7,1)
              Q(I,J,K,5,2)  = Q(I,J,K,5,1)
              H(I,J,K)      = (Q(I,J,K,5,1) + P(I,J,K)) / Q(I,J,K,1,1)
            END DO
          END DO
        END DO
      END IF

      IF(ICHEM .GE. 1) THEN
        DO NS = 1,NSPECI
          DO K = -2,KMAX+2
            DO J = -2,JMAX+2
              DO I = -2,IMAX+2
                Q(I,J,K,7+NS,1) = Q(I,J,K,1,1) * CONC(I,J,K,NS)
                Q(I,J,K,7+NS,2) = Q(I,J,K,7+NS,1)
              END DO
            END DO
          END DO
        END DO
      END IF

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE STATS(AVETIME, AVE)

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      CHARACTER FLNAME*80
      DIMENSION AVE(0:IMAX,0:JMAX,0:KMAX,*)

      I1 = 1
      I2 = IMAX - 1

      J1 = 1
      J2 = JMAX - 1

      K1 = 1
      K2 = KMAX - 1

      AVETIME = AVETIME + DT

      AVE(I1:I2,J1:J2,K1:K2,1) = AVE(I1:I2,J1:J2,K1:K2,1) +
     >                        U(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,2) = AVE(I1:I2,J1:J2,K1:K2,2) +
     >                        V(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,3) = AVE(I1:I2,J1:J2,K1:K2,3) +
     >                        W(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,4) = AVE(I1:I2,J1:J2,K1:K2,4) +
     > U(I1:I2,J1:J2,K1:K2) * U(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,5) = AVE(I1:I2,J1:J2,K1:K2,5) +
     > V(I1:I2,J1:J2,K1:K2) * V(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,6) = AVE(I1:I2,J1:J2,K1:K2,6) +
     > W(I1:I2,J1:J2,K1:K2) * W(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,7) = AVE(I1:I2,J1:J2,K1:K2,7) +
     > U(I1:I2,J1:J2,K1:K2) * V(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,8) = AVE(I1:I2,J1:J2,K1:K2,8) +
     > U(I1:I2,J1:J2,K1:K2) * W(I1:I2,J1:J2,K1:K2) * DT

      AVE(I1:I2,J1:J2,K1:K2,9) = AVE(I1:I2,J1:J2,K1:K2,9) +
     > V(I1:I2,J1:J2,K1:K2) * W(I1:I2,J1:J2,K1:K2) * DT

      NSTATS = 9

      IF(MOD(NADV,ISTAT) .EQ. 0) THEN

         NSTAT = NSTAT + 1

         WRITE(FLNAME,'(A,"STAT_",I3.3,".DATA")')
     >                             PREFIX(1:LENGTH), NSTAT
         WRITE(6,'(3X,A,A)') "WRITING STATISTICS FILE:", FLNAME
 
         OPEN(24, FILE = FLNAME, FORM = 'UNFORMATTED')
         WRITE(24) IMAX, JMAX, KMAX
         WRITE(24) ISGSK, ICHEM, NSPECI, IDYN
         WRITE(24) NADV, TIME
         WRITE(24) AVETIME, NSTATS

         AVETIME = 0.0
         DO L = 1, NSTATS
            WRITE(24) (((AVE(I,J,K,L),I=I1,I2),J=J1,J2),K=K1,K2)
            AVE(:,:,:,L) = 0.0D0
         ENDDO
         CLOSE(24)
      END IF
     
      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE TMSTEP()

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      DTMIN = 5.0D0

      DO K = 1, KMAX-1
         DO J = 1, JMAX-1
            DO I = 1, IMAX-1
               QQSI = U(I,J,K) * XAREA
               QQSJ = V(I,J,K) * YAREA
               QQSK = W(I,J,K) * ZAREA

               GM   = HF(I,J,K,2) / HF(I,J,K,3)
               RC   = HF(I,J,K,4)
               TEMP = T(I,J,K)
               CC   = SQRT(GM * RC * TEMP)

               RMU  = CMU * TEMP * SQRT(TEMP) / (TEMP + 110.0D0)
               RNU  = GM * RMU / (PRANDLT * Q(I,J,K,1,M))
               VVIS = 2.0D0 * RNU * SAREA * SAREA / VOLUME

               DTIJK = VOLUME / 
     >       (ABS(QQSI) + ABS(QQSJ) + ABS(QQSK) + SAREA * CC + VVIS)

               DTMIN = MIN(DTMIN, DTIJK)
            END DO
         END DO
      END DO

      DT = CFL * DTMIN
      DTVOL = DT / VOLUME

      RETURN
      END
!------------------------------------------------------------------------
      SUBROUTINE UPDATE()
 
      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      ALLOCATABLE SUMYK(:), RHOI(:), RKE(:)

      ALLOCATE( SUMYK(IMAX), RHOI(IMAX), RKE(IMAX))
      SUMYK=0D0
      RHOI=0D0
      RKE=0D0

      RN   = DBLE(N)
      RNM1 = RN - 1.0D0
      RNI  = 1.0D0 / RN

      I2 = IMAX - 1

      DO K = 1, KMAX - 1
         DO J = 1, JMAX - 1

            Q(1:I2,J,K,1,M) = (RNM1 * Q(1:I2,J,K,1,M) +
     >                         Q(1:I2,J,K,1,N) + DU(1:I2,J,K,1)) * RNI
            RHOI(1:I2) = 1.0D0 / Q(1:I2,J,K,1,M)

            Q(1:I2,J,K,2,M) = (RNM1 * Q(1:I2,J,K,2,M) + 
     >                         Q(1:I2,J,K,2,N) + DU(1:I2,J,K,2)) * RNI
            U(1:I2,J,K) = Q(1:I2,J,K,2,M) * RHOI(1:I2)
            RKE(1:I2) = U(1:I2,J,K) * U(1:I2,J,K)

            Q(1:I2,J,K,3,M) = (RNM1 * Q(1:I2,J,K,3,M) + 
     >                         Q(1:I2,J,K,3,N) + DU(1:I2,J,K,3)) * RNI
            V(1:I2,J,K) = Q(1:I2,J,K,3,M) * RHOI(1:I2)
            RKE(1:I2) = RKE(1:I2) + V(1:I2,J,K) * V(1:I2,J,K)

            Q(1:I2,J,K,4,M) = (RNM1 * Q(1:I2,J,K,4,M) + 
     >                         Q(1:I2,J,K,4,N) + DU(1:I2,J,K,4)) * RNI
            W(1:I2,J,K) = Q(1:I2,J,K,4,M) * RHOI(1:I2)
            RKE(1:I2) = RKE(1:I2) + W(1:I2,J,K) * W(1:I2,J,K)
            RKE(1:I2) = 0.5D0 * RKE(1:I2)

            IF(ISGSK .EQ. 1) THEN
               Q(1:I2,J,K,7,M) = (RNM1 * Q(1:I2,J,K,7,M) +
     >                         Q(1:I2,J,K,7,N) + DU(1:I2,J,K,7)) * RNI
               EK(1:I2,J,K) = MAX(Q(1:I2,J,K,7,M) * RHOI(1:I2), 0.0D0)
               RKE(1:I2) = RKE(1:I2) + EK(1:I2,J,K)
            ENDIF

            IF(ICHEM .GT. 0) THEN
               DO NS = 1, NSPECI
                  L = 7 + NS
                  Q(1:I2,J,K,L,M) = (RNM1 * Q(1:I2,J,K,L,M) +
     >                         Q(1:I2,J,K,L,N) + DU(1:I2,J,K,L)) * RNI
                  CONC(1:I2,J,K,NS) = Q(1:I2,J,K,L,M) * RHOI(1:I2)
                  CONC(1:I2,J,K,NS) = MAX(CONC(1:I2,J,K,NS), 0.0D0)
                  CONC(1:I2,J,K,NS) = MIN(CONC(1:I2,J,K,NS), 1.0D0)
                  IF(NS .EQ. 1) THEN
                     SUMYK(1:I2) = CONC(1:I2,J,K,NS)
                  ELSE
                     SUMYK(1:I2) = SUMYK(1:I2) + CONC(1:I2,J,K,NS)
                  ENDIF
               ENDDO

               SUMYK(1:I2) = 1.0D0 / SUMYK(1:I2)

               DO NS = 1, NSPECI
                  L = 7 + NS
                  CONC(1:I2,J,K,NS) = CONC(1:I2,J,K,NS) * SUMYK(1:I2)
                  Q(1:I2,J,K,L,M) = Q(1:I2,J,K,1,M) * CONC(1:I2,J,K,NS)
               ENDDO

               IF(ICHEM .EQ. 1) THEN
                  HF(1:I2,J,K,1) = EFORM * CONC(1:I2,J,K,1)
               ELSE IF(ICHEM .EQ. 2) THEN
                  HF(1:I2,J,K,1) = CONC(1:I2,J,K,1) * HFK(1)
                  HF(1:I2,J,K,2) = CONC(1:I2,J,K,1) * CPK(1)
                  HF(1:I2,J,K,3) = CONC(1:I2,J,K,1) * CVK(1)
                  HF(1:I2,J,K,4) = CONC(1:I2,J,K,1) * RGK(1)
                  DO NS = 2, NSPECI
                     HF(1:I2,J,K,1) = HF(1:I2,J,K,1) +
     >                                  CONC(1:I2,J,K,NS) * HFK(NS)
                     HF(1:I2,J,K,2) = HF(1:I2,J,K,2) +
     >                                  CONC(1:I2,J,K,NS) * CPK(NS)
                     HF(1:I2,J,K,3) = HF(1:I2,J,K,3) +
     >                                  CONC(1:I2,J,K,NS) * CVK(NS)
                     HF(1:I2,J,K,4) = HF(1:I2,J,K,4) +
     >                                  CONC(1:I2,J,K,NS) * RGK(NS)
                  ENDDO
               ENDIF
            ENDIF

            DO I = 1, I2
               Q(I,J,K,5,M) = (RNM1 * Q(I,J,K,5,M) +
     >                         Q(I,J,K,5,N) + DU(I,J,K,5)) * RNI
               EI = Q(I,J,K,5,M) * RHOI(I) - RKE(I)
               T(I,J,K) = (EI - HF(I,J,K,1)) / HF(I,J,K,3)
            ENDDO

            P(1:I2,J,K) = Q(1:I2,J,K,1,M) * HF(1:I2,J,K,4) * T(1:I2,J,K)

            IF(ISGSK .EQ. 1) H(1:I2,J,K) =
     >                   (Q(1:I2,J,K,5,M) + P(1:I2,J,K)) * RHOI(1:I2)

         END DO
      END DO

      DEALLOCATE( SUMYK, RHOI, RKE)

      RETURN
      END
!--------------------------------------------------------------------
      SUBROUTINE TRACE(KCMD, VAR, FLNAME, NUNIT)

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)

      CHARACTER*(*) FLNAME
      DIMENSION VAR(-2:IMAX+2,-2:JMAX+2,-2:KMAX+2)

      IF(KCMD .EQ. 0) THEN

         OPEN(NUNIT, FILE = FLNAME, FORM = "UNFORMATTED")
         WRITE(NUNIT) IPTS, JPTS, KPTS
         WRITE(NUNIT) (ILOC(I),I=1,IPTS)
         WRITE(NUNIT) (JLOC(J),J=1,JPTS)
         WRITE(NUNIT) (KLOC(K),K=1,KPTS)

         RETURN

      ELSE IF(KCMD .EQ. 2) THEN

         CLOSE(NUNIT)
         RETURN

      ELSE

         WRITE(NUNIT) NADV, TIME
         DO KK = 1, KPTS
            DO JJ = 1, JPTS
               K = KLOC(KK)
               J = JLOC(JJ)
               WRITE(NUNIT) (VAR(ILOC(II),J,K),II=1,IPTS)
            ENDDO
         ENDDO

      ENDIF

      RETURN
      END
!--------------------------------------------------------------------
      FUNCTION ILENGTH(CVAR, N)

      CHARACTER*(*) CVAR

      DO ILENGTH = N, 1, -1
         IF(CVAR(ILENGTH:ILENGTH) .NE. ' ') RETURN
      ENDDO
      WRITE(6,*) "ERROR IN ILENGTH!!"
      STOP

      END
!--------------------------------------------------------------------
      SUBROUTINE ANALYSIS (TAU, DELM)

      USE LES3D_DATA
      IMPLICIT REAL*8(A-H,O-Z)
      ALLOCATABLE UBAR(:), RBAR(:)

      ALLOCATE(UBAR(JMAX-1), RBAR(JMAX-1))
      UBAR=0D0
      RBAR=0D0

      DO J = 1, JMAX-1
         RBAR(J) = 0.0D0
         UBAR(J) = 0.0D0
         DO K = 1, KMAX-1
            DO I = 1, IMAX-1
               UBAR(J) = UBAR(J) + U(I,J,K) * DX * DZ
               RBAR(J) = RBAR(J) + Q(I,J,K,1,1) * DX * DZ
            ENDDO
         ENDDO
         UBAR(J) = UBAR(J) / UREF / RBAR(J)
      ENDDO

      DELM = 0.0D0
      DO J = 1, JMAX-1
         DELM = DELM + (0.25D0 - UBAR(J) * UBAR(J)) * DY
      ENDDO

      TAU = TIME / (ALREF / UREF)

      RETURN
      END
!---------------------------------------------------------------------
! GET NUMBER OF SECONDS SINCE 1990 TO WITHIN MILLISECOND PRECISION.
! THIS BECOMES OBSELETE IN 2060.
!---------------------------------------------------------------------
      REAL*8 FUNCTION GET_TIME()

      IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER :: IDATE(8), DAYS(12)
      CHARACTER (LEN = 10) CDATE(3)
      LOGICAL LEAP

      CALL DATE_AND_TIME(CDATE(1), CDATE(2), CDATE(3), IDATE)

      NDAYS = 0

      DO NY = 1990, IDATE(1)
         IF(MOD(NY,4) == 0) THEN     
            IF(MOD(NY,100) == 0) THEN
               IF(MOD(NY,400) == 0) THEN
                  LEAP = .TRUE.
               ELSE
                  LEAP = .FALSE.
               ENDIF
            ELSE
               LEAP = .TRUE.
            ENDIF
         ELSE
            LEAP = .FALSE.
         ENDIF

         DAYS( 1) = 31
         DAYS( 2) = 28
         IF (LEAP) DAYS(2) = 29
         DAYS( 3) = 31
         DAYS( 4) = 30
         DAYS( 5) = 31
         DAYS( 6) = 30
         DAYS( 7) = 31
         DAYS( 8) = 31
         DAYS( 9) = 30
         DAYS(10) = 31
         DAYS(11) = 30
         DAYS(12) = 31

         IF(NY == IDATE(1)) THEN
            DO NM = 1, IDATE(2)-1
               NDAYS = NDAYS + DAYS(NM)
            ENDDO
            NDAYS = NDAYS + IDATE(3)
         ELSE
            DO NM = 1, 12
               NDAYS = NDAYS + DAYS(NM)
            ENDDO
         ENDIF
      ENDDO

      NSECS = 24 * 3600 * NDAYS
      NSECS = NSECS + 3600 * IDATE(5)
      NSECS = NSECS + 60 * IDATE(6)
      NSECS = NSECS + IDATE(7)
      GET_TIME = (1000.0 * DBLE(NSECS) + DBLE(IDATE(8))) / 1000.0

      RETURN
      END
