C###########################################################
      PROGRAM FV
C###########################################################
C     FINITE VOLUME METHOD FOR SOLVING CONSERVATION EQUATION
C     FOR SCALAR TRANSPORT USING CARTESIAN GRIDS AND KNOWN
C     VELOCITY FIELD (HERE: STAGNATION POINT FLOW, U=X AND
C     V=-Y; AT X=0, SCALAR VARIES FROM 0.0 AT Y=Ymax TO 1.0 AT
C     Y=0; AT X=Xmax, OUTFLOW - ZERO GRADIENT EXTRAPOLATION
C     FROM INSIDE; AT Y=Ymax, INFLOW, SCALAR = 0.0; AT Y=0,
C     NEUMANN BOUNDARY CONDITION, ZERO GRADIENT IN 
C     Y-DIRECTION). UNSTEADY VERSION, SOLVING FOR THE
C     TRANSITION FROM INITIAL SOLUTION (ZERO FIELD) TO STEADY
C     SOLUTION. FOUR SCHEMES FOR TIME INTEGRATION ARE
C     IMPLEMENTED (IMPLICIT EULER, EXPLICIT EULER, CRANK-
C     NICOLSON AND THREE TIME LEVELS IMPLICIT).
C
C                               M. PERIC, IfS, 1996
C###########################################################
      PARAMETER (NX=42,NY=42)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *       AS(NX,NY),AP(NX,NY),Q(NX,NY),FIO(NX,NY),FIOO(NX,NY)
      DIMENSION X(NX),Y(NY),XC(NX),YC(NY),CU(NX,NY),CV(NX,NY),
     *       FX(NX),FY(NY),DW(NY)
      CHARACTER FILEO*20
C
C.....READ FILE NAMES, OPEN FILES
C
    1 FORMAT(A20)
      PRINT *, ' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILEO
      OPEN (UNIT=8,FILE=FILEO)
      REWIND 8
C
C.....READ INPUT DATA
C
      PRINT *,' ENTER: DEN (density), DIF (diff. coeff.), '
      PRINT *,'        DT (time step), NTMAX (max. no. time steps), '
      PRINT *,'        NTPR (print each NTPRth step) :   '
      READ(*,*) DEN,DIF,DT,NTMAX,NTPR
C
      PRINT *,' CHOOSE CONVECTION SCHEME: 1 - CDS; 2 - UDS  '
      READ(*,*) ID
C
      PRINT *,' CHOOSE TIME SCHEME: 1 - EE, 2 - IE, 3 - CN, 4 - I3L  '
      READ(*,*) IT
C
      PRINT *,' ALFA PARAMETER IN SIP - SOLVER:  '
      READ(*,*) ALFA
C
C.....DEFINE GRID IN X-DIRECTION
C
      PRINT *,' ENTER: Xmin, Xmax, Expansion factor, No. of CV:  '
      READ(*,*) XMIN,XMAX,EXX,N
C
      NI=N+2
      NIM=NI-1
      IF(EXX.EQ.1.) THEN
        DX=(XMAX-XMIN)/REAL(N)
      ELSE
        DX=(XMAX-XMIN)*(1.-EXX)/(1.-EXX**N)
      ENDIF
C
      X(1)=XMIN
      DO I=2,NIM
        X(I)=X(I-1)+DX
        DX=DX*EXX
      END DO
      X(NI)=X(NIM)
C
C.....COORDINATES OF CELL CENTERS
C
      XC(1)=X(1)
      DO I=2,NIM
        XC(I)=0.5*(X(I)+X(I-1))
      END DO
      XC(NI)=X(NIM)
C
C.....DEFINE GRID IN Y-DIRECTION
C
      PRINT *,' ENTER: Ymin, Ymax, Expansion factor, No. of CV:  '
      READ(*,*) YMIN,YMAX,EXY,M
C
      NJ=M+2
      NJM=NJ-1
      IF(EXY.EQ.1.) THEN
        DY=(YMAX-YMIN)/REAL(M)
      ELSE
        DY=(YMAX-YMIN)*(1.-EXY)/(1.-EXY**M)
      ENDIF
C
      Y(1)=YMIN
      DO J=2,NJM
        Y(J)=Y(J-1)+DY
        DY=DY*EXY
      END DO
      Y(NJ)=Y(NJM)
C
C.....COORDINATES OF CELL CENTERS
C
      YC(1)=Y(1)
      DO J=2,NJM
        YC(J)=0.5*(Y(J)+Y(J-1))
      END DO
      YC(NJ)=Y(NJM)
C
C.....CALCULATE INTERPOLATION FACTORS (FX = (X_e - X_P)/(X_E - X_P))
C
      FX(1)=0.
      DO I=2,NIM
        FX(I)=(X(I)-XC(I))/(XC(I+1)-XC(I))
      END DO
C
      FY(1)=0.
      DO J=2,NJM
        FY(J)=(Y(J)-YC(J))/(YC(J+1)-YC(J))
      END DO
C
C.....INITIALIZE VARIABLE VALUES
C
      DO I=1,NI
        DO J=1,NJ
          FI(I,J)=0.
          FIO(I,J)=0.
        END DO
      END DO
C
      DO J=2,NJM
        FI(1,J)=1.-(YC(J)-YMIN)/(YMAX-YMIN)
      END DO
      FI(1,1)=1.
C
C.....CALCULATE MASS FLUXES (CU = DEN*U*DY; CV = DEN*V*DX;
C     U = X(I) at cell face 'e'; V = Y(J) at cell face 'n')
C
      DO I=1,NIM
        DO J=2,NJM
          CU(I,J)=DEN*X(I)*(Y(J)-Y(J-1))
        END DO
      END DO
C
      DO J=1,NJM
        DO I=2,NIM
          CV(I,J)=-DEN*Y(J)*(X(I)-X(I-1))
        END DO
      END DO
C
C.....START TIME LOOP (BETA = 0 for the first step)
C
      BETA=0.
      TIME=0.
      DDTR=DEN/DT
C
      DO 100 NT=1,NTMAX
      TIME=TIME+DT
C
C.....SHIFT SOLUTIONS IN TIME; SET BOUNDARY VALUES
C
      DO I=1,NI
        DO J=1,NJ
          FIOO(I,J)=FIO(I,J)
          FIO(I,J)=FI(I,J)
        END DO
      END DO
C
      DO J=2,NJM
        FI(1,J)=1.-(YC(J)-YMIN)/(YMAX-YMIN)
      END DO
      FI(1,1)=1.
C
C.....DISCRETIZE DIFFUSION TERM
C
      DO J=2,NJM
        DW(J)=-DIF*(Y(J)-Y(J-1))/(XC(2)-XC(1))
      END DO
C
      DO I=2,NIM
        DS=-DIF*(X(I)-X(I-1))/(YC(2)-YC(1))
C
        DO J=2,NJM
          DE=-DIF*(Y(J)-Y(J-1))/(XC(I+1)-XC(I))
          DN=-DIF*(X(I)-X(I-1))/(YC(J+1)-YC(J))
C
C.....DISCRETIZE CONVECTION TERM (UDS or CDS, no deferred correction)
C
          IF(ID.EQ.2) THEN
            AE(I,J)=DE+MIN(CU(I,J),0.)
            AW(I,J)=DW(J)-MAX(CU(I-1,J),0.)
            AN(I,J)=DN+MIN(CV(I,J),0.)
            AS(I,J)=DS-MAX(CV(I,J-1),0.)
          ELSE
            AE(I,J)=DE+CU(I,J)*FX(I)
            AW(I,J)=DW(J)-CU(I-1,J)*(1.-FX(I-1))
            AN(I,J)=DN+CV(I,J)*FY(J)
            AS(I,J)=DS-CV(I,J-1)*(1.-FY(J-1))
          ENDIF
C
C.....SOURCE TERM
C
          Q(I,J)=0.
C
          DW(J)=DE
          DS=DN
        END DO
      END DO
C
C.....BOUNDARY CONDITIONS (WEST - wall; EAST - outlet, zero gradient;
C                          NORTH - inlet; SOUTH - symmetry plane)
C
      DO J=2,NJM
        AE(NIM,J)=0.
      END DO
C
      DO I=2,NIM
        AS(I,2)=0.
      END DO
C
C.....TIME DISCRETIZATION -> EULER EXPLICIT
C
      IF(IT.EQ.1) THEN
        DO I=2,NIM
          DX=X(I)-X(I-1)
          DO J=2,NJM
            CT=DDTR*DX*(Y(J)-Y(J-1))
            Q(I,J)=Q(I,J)-(AE(I,J)*FIO(I+1,J)+AW(I,J)*FIO(I-1,J)+
     *             AN(I,J)*FIO(I,J+1)+AS(I,J)*FIO(I,J-1))+
     *            (CT+AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))*FIO(I,J)
            AP(I,J)=CT
            AE(I,J)=0.
            AW(I,J)=0.
            AN(I,J)=0.
            AS(I,J)=0.
          END DO
        END DO
C
C.....TIME DISCRETIZATION -> EULER IMPLICIT
C
      ELSEIF(IT.EQ.2) THEN
        DO I=2,NIM
          DX=X(I)-X(I-1)
          DO J=2,NJM
            CT=DDTR*DX*(Y(J)-Y(J-1))
            Q(I,J)=Q(I,J)+CT*FIO(I,J)
            AP(I,J)=CT-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))
          END DO
        END DO
C
C.....TIME DISCRETIZATION -> CRANK-NICOLSON
C
      ELSEIF(IT.EQ.3) THEN
        DO I=2,NIM
          DX=X(I)-X(I-1)
          DO J=2,NJM
            CT=DDTR*DX*(Y(J)-Y(J-1))
            AE(I,J)=0.5*AE(I,J)
            AW(I,J)=0.5*AW(I,J)
            AN(I,J)=0.5*AN(I,J)
            AS(I,J)=0.5*AS(I,J)
            Q(I,J)=Q(I,J)-(AE(I,J)*FIO(I+1,J)+AW(I,J)*FIO(I-1,J)+
     *             AN(I,J)*FIO(I,J+1)+AS(I,J)*FIO(I,J-1))+
     *             (CT+AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))*FIO(I,J)
            AP(I,J)=CT-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))
          END DO
        END DO
C
C.....TIME DISCRETIZATION -> IMPLICIT 3 LEVELS
C
      ELSEIF(IT.EQ.4) THEN
        DO I=2,NIM
          DX=X(I)-X(I-1)
          DO J=2,NJM
            CT=DDTR*DX*(Y(J)-Y(J-1))
            Q(I,J)=Q(I,J)+CT*((1.+BETA)*FIO(I,J)-0.5*BETA*FIOO(I,J))
            AP(I,J)=CT*(1.+0.5*BETA)-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))
          END DO
        END DO
C
      ENDIF
C
C.....SOLVE EQUATION SYSTEM
C
      CALL SIPSOL(ALFA)
C
C.....PRINT THE RESULT (set symmetry & outflow values first)
C
      IF(MOD(NT,NTPR).EQ.0) THEN
C
        DO I=2,NIM
          FI(I,1)=FI(I,2)
        END DO
        DO J=1,NJ
          FI(NI,J)=FI(NIM,J)
        END DO
C
        dfac=dif*dt/(den*dx**2)
        cfac=dt/dx
C
        WRITE(8,*) '  '
        write(8,*) '  d  = ',dfac
        write(8,*) '  Co = ',cfac
        write(8,*) '  dt = ',dt
        write(8,*) '  Time = ',time
        if(id.eq.1) write(8,*) '  CDS for convection  '
        if(id.eq.2) write(8,*) '  UDS for convection  '
        if(it.eq.1) write(8,*) '  Explicit Euler '
        if(it.eq.2) write(8,*) '  Implicit Euler '
        if(it.eq.3) write(8,*) '  Crank-Nicolson '
        if(it.eq.4) write(8,*) '  Implicit 3 levels  '
        write(8,*) '  '
        CALL PRINT(FI,' TEMP ')
C
C.....CALCULATE WALL FLUX
C
        QWALL=0.
        DO J=2,NJM
          DE=DIF*(Y(J)-Y(J-1))/(XC(2)-XC(1))
          QWALL=QWALL+DE*(FI(2,J)-FI(1,J))
        END DO
        WRITE(8,*) '  '
        WRITE(8,*) '      QWALL = ',QWALL
      ENDIF
C
      BETA=1.0
  100 CONTINUE
      STOP
      END
C
C
C############################################################
      SUBROUTINE SIPSOL(ALFA)
C############################################################
      PARAMETER (NX=42,NY=42,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *       AS(NX,NY),AP(NX,NY),Q(NX,NY),FIO(NX,NY),FIOO(NX,NY)
      REAL LW,LS,LPR
      DIMENSION LW(NX,NY),LS(NX,NY),LPR(NX,NY),UN(NX,NY),
     *          UE(NX,NY),RES(NX,NY)
      DATA UN,UE /NXY*0.,NXY*0./
C
C.....CALCULATE COEFFICIENTS OF [L] AND [U] MATRICES
C
      DO I=2,NIM
        DO J=2,NJM
          LW(I,J)=AW(I,J)/(1.+ALFA*UN(I-1,J))
          LS(I,J)=AS(I,J)/(1.+ALFA*UE(I,J-1))
          P1=ALFA*LW(I,J)*UN(I-1,J)
          P2=ALFA*LS(I,J)*UE(I,J-1)
          LPR(I,J)=1./(AP(I,J)+P1+P2-LW(I,J)*UE(I-1,J)-
     *             LS(I,J)*UN(I,J-1)+1.E-20)
          UN(I,J)=(AN(I,J)-P1)*LPR(I,J)
          UE(I,J)=(AE(I,J)-P2)*LPR(I,J)
        END DO
      END DO
C
      DO L=1,1000
C
C.....CALCULATE RESIDUALS AND  {R}=[L^-1]{RES}
C
        RESL=0.
        DO I=2,NIM
          DO J=2,NJM
            RES(I,J)=Q(I,J)-AP(I,J)*FI(I,J)-AN(I,J)*FI(I,J+1)-
     *           AS(I,J)*FI(I,J-1)-AE(I,J)*FI(I+1,J)-AW(I,J)*FI(I-1,J)
            RESL=RESL+ABS(RES(I,J))
            RES(I,J)=(RES(I,J)-LS(I,J)*RES(I,J-1)-
     *      LW(I,J)*RES(I-1,J))*LPR(I,J)
          END DO
        END DO
C
        IF(L.EQ.1) RESNOR=RESL
        RSM=RESL/RESNOR
C
C.....CALCULATE INCREMENT {DEL}=[U^-1]{R}
C
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            RES(I,J)=RES(I,J)-UN(I,J)*RES(I,J+1)-UE(I,J)*RES(I+1,J)
            FI(I,J)=FI(I,J)+RES(I,J)
          END DO
        END DO
C
C.....CHECK CONVERGENCE
C
        WRITE(8,*)  '     ',L,' ITER., RSM =',RSM
        IF(RSM.LT.1.E-4) RETURN
C
      END DO
C
      RETURN
      END
C
C###########################################################
      SUBROUTINE PRINT(PHI,TITLE)
C###########################################################
      PARAMETER (NX=42,NY=42,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *       AS(NX,NY),AP(NX,NY),Q(NX,NY),FIO(NX,NY),FIOO(NX,NY)
      DIMENSION PHI(NX,NY)
      CHARACTER*6 TITLE
C
      WRITE(8,20) TITLE
      IE=1
      NL=NI/12+1
      IF(MOD(NI,12).EQ.0) NL=NI/12
C
      DO L=1,NL
        IS=IE
        IE=MIN(NI,IS+11)
        WRITE(8,21) (I,I=IS,IE)
        WRITE(8,*) '  J'
        DO J=NJ,1,-1
          WRITE(8,23) J,(PHI(I,J),I=IS,IE)
        END DO
      END DO
C
   20 FORMAT(2X,26('*-'),7X,A6,7X,26('-*'))
   21 FORMAT(3X,'I = ',I3,11I10)
   23 FORMAT(1X,I3,1P12E10.2)
      RETURN
      END
