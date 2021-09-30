C########################################################
      PROGRAM FD
C########################################################
C     This program solves the unsteady one-dimensional 
C     convection-diffusion equation with Dirichlet boundary 
C     conditions on both ends. Initial solution is zero
C     field, boundary conditions are time-independent; the
C     transition from initial to steady solution is 
C     simulated using different time integration schemes.
C   
C     1995             M. Peric, Institut fuer Schiffbau
C########################################################
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),FIO(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
      DIMENSION FIOO(NX)
      CHARACTER FILOUT*20
C
C.....READ FILE NAMES AND OPEN FILES
C
      PRINT *,' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILOUT
    1 FORMAT(A)
      OPEN (UNIT=8,FILE=FILOUT)
      REWIND 8
C
C.....READ INPUT DATA
C
      PRINT *,' ENTER: DEN (density), VEL (velocity), '
      PRINT *,'        DIF (diffusion coefficient), DT (time step), '
      PRINT *,'        NTMAX (max. no. of time steps), '
      PRINT *,'        NTPR (print each NTPRth step) :    '
      READ(*,*) DEN,VEL,DIF,DT,NTMAX,NTPR
C
      PRINT *,' ENTER BOUNDARY VALUES: FI @ X=Xmin, FI @ X=Xmax:   '
      READ(*,*) FI0,FIN
C
      PRINT *,' CHOOSE CONVECTION SCHEME: 1 = CDS, 2 = UDS   '
      READ(*,*) IC
C
      PRINT *,' CHOOSE TIME SCHEME: 1 = EXPLICIT EULER,  '
      PRINT *,'                     2 = IMPLICIT EULER,  '
      PRINT *,'                     3 = CRANK-NICOLSON,  '
      PRINT *,'                     4 = IMPLICIT 3 TIME LEVELS:  '
      READ(*,*) IT
C
C.....READ GRID DATA, CALCULATE NODE COORDINATES
C
      PRINT *,' ENTER: XMIN, XMAX, EXP. FACTOR, NUMBER OF NODES:  '
      READ(*,*) XMIN,XMAX,EXP,N
C
      NM=N-1
      IF(EXP.EQ.1.) THEN
        DX=(XMAX-XMIN)/REAL(N-1)
      ELSE
        DX=(XMAX-XMIN)*(1.-EXP)/(1.-EXP**(N-1))
      ENDIF
C
      X(1)=XMIN
      DO I=2,N
        X(I)=X(I-1)+DX
        DX=DX*EXP
      END DO
C
C.....INITIALIZE SOLUTION, SET BOUNDARY VALUES
C
      DO I=1,N
        FI(I)=0.
        FIO(I)=0.
      END DO
      FI(1)=FI0
      FI(N)=FIN
      DENVEL=DEN*VEL
      ZERO=0.
C
C.....START TIME LOOP (BETA = 0. FOR THE FIRST STEP & 3 LEVEL SCHEME)
C
      TIME=0.
      DDTR=DEN/DT
      BETA=0.
C
      DO 100 NT=1,NTMAX
      TIME=TIME+DT
C
C.....SHIFT SOLUTIONS TO OLDER LEVELS
C
      DO I=1,N
        FIOO(I)=FIO(I)
        FIO(I)=FI(I)
      END DO
      FI(1)=FI0
      FI(N)=FIN
C
C.....DISCRETIZE CONVECTION TERM USING CDS (NO DEFERRED CORRECTION)
C
      IF(IC.EQ.1) THEN
        DO I=2,NM
          DXR=1./(X(I+1)-X(I-1))
          AE(I)= DENVEL*DXR
          AW(I)=-DENVEL*DXR
        END DO
C
C.....DISCRETIZE CONVECTION TERM USING UDS 
C
      ELSEIF(IC.EQ.2) THEN
        DO I=2,NM
          AE(I)= MIN(DENVEL,ZERO)/(X(I+1)-X(I))
          AW(I)=-MAX(DENVEL,ZERO)/(X(I)-X(I-1))
        END DO
      ENDIF
C
C.....DISCRETIZE DIFFUSION TERM (CDS)
C
      DO I=2,NM
        DXR=2./(X(I+1)-X(I-1))
        AE(I)=AE(I)-DIF*DXR/(X(I+1)-X(I))
        AW(I)=AW(I)-DIF*DXR/(X(I)-X(I-1))
        Q(I)=0.
      END DO
C
C.....TIME DISCRETIZATION -> EXPLICIT EULER
C
      IF(IT.EQ.1) THEN
        DO I=2,NM
          Q(I)=Q(I)-AE(I)*FIO(I+1)-AW(I)*FIO(I-1)+
     *         (DDTR+AE(I)+AW(I))*FIO(I)
          AP(I)=DDTR
          AE(I)=0.
          AW(I)=0.
        END DO
C
C.....TIME DISCRETIZATION -> IMPLICIT EULER
C
      ELSEIF(IT.EQ.2) THEN
        DO I=2,NM
          Q(I)=Q(I)+DDTR*FIO(I)
          AP(I)=DDTR-AE(I)-AW(I)
        END DO
C
C.....TIME DISCRETIZATION -> CRANK-NICOLSON
C
      ELSEIF(IT.EQ.3) THEN
        DO I=2,NM
          AE(I)=0.5*AE(I)
          AW(I)=0.5*AW(I)
          Q(I)=Q(I)-AE(I)*FIO(I+1)-AW(I)*FIO(I-1)+
     *              (DDTR+AE(I)+AW(I))*FIO(I)
          AP(I)=DDTR-AE(I)-AW(I)
        END DO
C
C.....TIME DISCRETIZATION -> IMPLICIT 3 LEVEL
C
      ELSEIF(IT.EQ.4) THEN
        DO I=2,NM
          Q(I)=Q(I)+DDTR*((1.+BETA)*FIO(I)-0.5*BETA*FIOO(I))
          AP(I)=DDTR*(1.+0.5*BETA)-AE(I)-AW(I)
        END DO
      ENDIF
C
C.....ADJUST BOUNDARY SOURCE & COEFF.
C
      Q(2)=Q(2)-AW(2)*FI(1)
      AW(2)=0.
      Q(NM)=Q(NM)-AE(NM)*FI(N)
      AE(NM)=0.
C
C.....SOLVE EQUATION SYSTEM
C
      CALL TDMA
C
C.....PRINT THE RESULT (EVERY NTPR-TH TIME STEP)
C
      IF(MOD(NT,NTPR).EQ.0) THEN
        PE=DEN*VEL*(XMAX-XMIN)/DIF
        DFAC=DIF*DT/(DEN*DX**2)
        CFAC=VEL*DT/DX
        WRITE(8,*) '#        PECLET NUMBER:  PE = ',PE
        WRITE(8,*) '#     DIFFUSION NUMBER:   D = ',DFAC
        WRITE(8,*) '#      COURANT NUMBER :  Co = ',CFAC
        WRITE(8,*) '#           TIME STEP :  DT = ',DT  
        WRITE(8,*) '#                TIME :   T = ',TIME
        IF(IC.EQ.2) WRITE(8,*) '#   UPWIND CONVECTION '
        IF(IC.EQ.1) WRITE(8,*) '#   CDS CONVECTION '
        IF(IT.EQ.1) WRITE(8,*) '#   EXPLICIT EULER TIME DISCR. '
        IF(IT.EQ.2) WRITE(8,*) '#   IMPLICIT EULER TIME DISCR. '
        IF(IT.EQ.3) WRITE(8,*) '#   CRANK-NICOLSON TIME DISCR. '
        IF(IT.EQ.4) WRITE(8,*) '#   IMPLICIT 3 LEVELS TIME DISCR. '
        WRITE(8,*) '#  '
        WRITE(8,*) '#      X             FI '
        DO I=1,N
          WRITE(8,'(1P2E13.5)') X(I),FI(I)
        END DO
        WRITE(8,*) '  '
      ENDIF
C
C.....SET BETA=1.0 FOR SUBSEQUENT TIME STEPS
C
      BETA=1.0
C
  100 CONTINUE
C
      STOP
      END
C
C
C######################################################
      SUBROUTINE TDMA
C######################################################
C.....  INITIALIZATION BY ZERO ASSUMED!
C.....  UPR(I) = RECIPROCAL VALUE OF UP(I)
C.....  QS(I)  = MODIFIED SOURCE TERM
C######################################################
C
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),FIO(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
      DIMENSION UPR(NX),QS(NX)
C
C.....FORWARD SUBSTITUTION
C
      DO I=2,NM
        UPR(I)=1./(AP(I)-AW(I)*AE(I-1)*UPR(I-1))
        QS(I)=Q(I)-AW(I)*QS(I-1)*UPR(I-1)
      END DO
C
C.....BACKWARD SUBSTITUTION
C
      DO I=NM,2,-1
        FI(I)=(-AE(I)*FI(I+1)+QS(I))*UPR(I)
      END DO
C
      RETURN
      END
