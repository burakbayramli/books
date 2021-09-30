C=============================================================
      PROGRAM LAPL2DSIN
C=============================================================
C     This program sets up the matrix equation [A]{f}={Q} which
C     results from finite volume discretization of the
C     Poissson equation in 2D using Cartesian grid and
C     central difference approximation of the second derivative.
C     Boundary conditions are of Dirichlet type on east and north
C     boundary: f(1,y) = 0, f(x,1) = 0., and of Neumann type on
C     west and south boundary (zero gradient normal to boundary).
C     The equation can be solved by a variety of iterative
C===============================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDI/ NI,NJ,NIM,NJM,MAXIT,LI(NX)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DIMENSION FI(NXY),XC(NX),YC(NY)
      DATA FI /NXY*0./
      CHARACTER FILEO*20
      REAL PI
c
C     Define constant Pi
      PI=4.0*ATAN(1.0)
C
C.....INPUT DATA
C
      PRINT *, ' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILEO
    1 FORMAT(A20)
      OPEN (UNIT=8,FILE=FILEO)
      REWIND 8
C
C.....Maximum number of allowed iteration and normalized residuel sum
C     at which converged solution is assumed
c
      PRINT *, ' Enter:  MAXIT, RESMAX '
      PRINT *, '        '
      READ(*,*) MAXIT,RESMAX
C
C.....DEFINE GRID IN X-DIRECTION
C
      PRINT *,' ENTER: XMAX, EXX, NICV  '
      READ(*,*) XMAX,EXX,N
C
      NI=N+2
      NIM=NI-1
      IF(EXX.EQ.1.) THEN
        DX=XMAX/REAL(N)
      ELSE
        DX=XMAX*(1.-EXX)/(1.-EXX**N)
      ENDIF
      X(1)=0.
      DO I=2,NIM
        X(I)=X(I-1)+DX
        DX=DX*EXX
      END DO
      X(NI)=X(NIM)
C
C.....DEFINE GRID IN Y-DIRECTION
C
      PRINT *,' ENTER: YMAX, EXY, NJCV  '
      READ(*,*) YMAX,EXY,M
C
      NJ=M+2
      NJM=NJ-1
      IF(EXY.EQ.1.) THEN
        DY=YMAX/REAL(M)
      ELSE
        DY=YMAX*(1.-EXY)/(1.-EXY**M)
      ENDIF
      Y(1)=0.
      DO J=2,NJM
        Y(J)=Y(J-1)+DY
        DY=DY*EXY
      END DO
      Y(NJ)=Y(NJM)
C
C.....COORDINATES OF CELL CENTERS
C
      XC(1)=X(1)
      XC(NI)=X(NIM)
      DO I=2,NIM
        XC(I)=0.5*(X(I)+X(I-1))
      END DO
C
      YC(1)=Y(1)
      YC(NJ)=Y(NJM)
      DO J=2,NJM
        YC(J)=0.5*(Y(J)+Y(J-1))
      END DO
C
C.....WORKING ARRAY FOR CONVERTING 2D INDICES TO 1D
C
      NIJ=NI*NJ
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
C
C.....INITIALIZE FIELD VALUES (ZERO)
C
      DO I=1,NI
      DO J=1,NJ
          IJ=LI(I)+J
          FI(IJ)=XC(I)*YC(J)
      END DO
      END DO
C
C.....From the earlier version: Laplace equation with exact solution
C     Fi = x * y when Dirichlet boundary condition is applied at all
C     boundaries (linear profiles, CDS gives exact solution on any grid).
C
      DO I=2,NI
        FI(LI(I)+NJ)=XC(I)*YC(NJ)
      END DO
      DO J=2,NJM
        FI(LI(NI)+J)=YC(J)*XC(NI)
      END DO
C
C.....CALCULATE ELEMENTS OF MATRIX [A] (For Laplace equation, Q(IJ)=0.;
C     for Poisson equation, the source term should be chosen so that
C     the sum of all sources = 0 if Neumann conditions on all boundaries.)
C
      DO I=2,NIM
        DO J=2,NJM
          IJ=LI(I)+J
          AE(IJ)=-(Y(J)-Y(J-1))/(XC(I+1)-XC(I))
          AN(IJ)=-(X(I)-X(I-1))/(YC(J+1)-YC(J))
          AW(IJ)=-(Y(J)-Y(J-1))/(XC(I)-XC(I-1))
          AS(IJ)=-(X(I)-X(I-1))/(YC(J)-YC(J-1))
          AP(IJ)=-(AE(IJ)+AW(IJ)+AN(IJ)+AS(IJ))
          VOL=(X(I)-X(I-1))*(Y(J)-Y(J-1))
          Q(IJ)=0.
C          Q(IJ)=SIN(2*PI*XC(I)/XMAX)*SIN(2*PI*YC(J)/YMAX)*VOL
        END DO
      END DO
C
C.....NEUMANN CONDITION AT BOUNDARIES X=0 & Y=0: quadratic extrapolation
C     from inside is used, boundary value is expressed through a weighted
C     average of two inner nodes, and the product of the coefficient 
C     multiplying boundary noal value leads then to the modification of
C     the coefficients for two inner nodes involved. The Neumann b.c. is
C     thus implicitly taken into account; the boundary values have to be
C     computed in each iteration if multigrid method is used, otherwise
C     only at the end.
C
c      I=2
c      QUO=(XC(3)-XC(1))**2-(XC(2)-XC(1))**2
c      DO J=2,NJM
c        IJ=LI(I)+J
c        AP(IJ)=AP(IJ)+AW(IJ)*(XC(3)-XC(1))**2/QUO
c        AE(IJ)=AE(IJ)-AW(IJ)*(XC(2)-XC(1))**2/QUO
c        AW(IJ)=0.
c      ENDDO
c      J=2
c      QUO=(YC(3)-YC(1))**2-(YC(2)-YC(1))**2
c      DO I=2,NIM
c        IJ=LI(I)+J
c        AP(IJ)=AP(IJ)+AS(IJ)*(YC(3)-YC(1))**2/QUO
c        AN(IJ)=AN(IJ)-AS(IJ)*(YC(2)-YC(1))**2/QUO
c        AS(IJ)=0
c      ENDDO
C
C.....CHOOSE THE SOLVER
C
      PRINT *, ' Enter solver ID: '
      PRINT *, ' 1 - Gauss-Seidel solver '
      PRINT *, ' 2 - Line-by-Line TDMA '
      PRINT *, ' 3 - Stone SIP solver '
      PRINT *, ' 4 - Conjugate gradient solver '
      PRINT *, ' 5 - ADI '
      PRINT *, ' 6 - Multigrid solver with GS '
      PRINT *, ' 7 - Multigrid solver with SIP '
      PRINT *, ' 8 - Multigrid solver with ICCG '
      PRINT *, '  '
      READ(*,*) ISOL
        PRINT *, ' Enter: ALFA, BETA (relevant only for SIP or ADI) '
        PRINT *, '        '
        READ(*,*) ALFA,BETA
C
C.....CALCULATE INITIAL ERROR NORM
C
      ERRNOR=0.
      DO I=2,NIM
        DO J=2,NJM
          IJ=LI(I)+J
          ERRNOR=ERRNOR+ABS(FI(IJ)-XC(I)*YC(J))
        END DO
      END DO
      WRITE(8,*) '  Initial Error norm:  ERR = ',ERRNOR
C
C.....SOLVE EQUATION SYSTEM
C
      IF(ISOL.EQ.1) THEN
        CALL GSS(FI)
      ELSEIF(ISOL.EQ.2) THEN
        CALL LSOL(FI)
      ELSEIF(ISOL.EQ.3) THEN
        CALL SIPSOL(FI)
      ELSEIF(ISOL.EQ.4) THEN
        CALL CGS(FI)
      ELSEIF(ISOL.EQ.5) THEN
        CALL ADI(BETA,FI)
      ELSEIF(ISOL.GT.5) THEN
        CALL MG(NI,NJ,ISOL,FI)
      ENDIF
C
C.....PRINT ERROR NORM AND SOLUTION FIELD (applies only to Laplace
C     equation with Dirichlet b.c.; for the Poisson equation, the exact
C     solution is not known
C
      ERR=0.
      DO I=2,NIM
        DO J=2,NJM
          IJ=LI(I)+J
          ERR=ERR+ABS(FI(IJ)-XC(I)*YC(J))
        END DO
      END DO
      ERR=ERR/ERRNOR
      WRITE(8,*) '     Final error norm:  ERR = ',ERR
C
      IF(ISOL.EQ.1) WRITE(8,*) '  GAUSS-SEIDEL SOLVER'
      IF(ISOL.EQ.2) WRITE(8,*) '  LINE-BY-LINE (ADI) TDMA SOLVER'
      IF(ISOL.EQ.3) WRITE(8,*) '  SIP SOLVER'
      IF(ISOL.EQ.4) WRITE(8,*) '  ICCG SOLVER'
      IF(ISOL.EQ.5) WRITE(8,*) '  ADI SOLVER'
      IF(ISOL.EQ.6) WRITE(8,*) '  MG-GS SOLVER'
      IF(ISOL.EQ.7) WRITE(8,*) '  MG-SIP SOLVER'
      IF(ISOL.EQ.8) WRITE(8,*) '  MG-ICCG SOLVER'
      WRITE(8,*) '   '
      CALL PRINT(NI,NJ,0,FI,'FI SOL')
      STOP
      END
C
C##############################################################
      SUBROUTINE GSS(FI)
C##############################################################
C     This routine contains the Gauss-Seidel solver
C==============================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDI/ NI,NJ,NIM,NJM,MAXIT,LI(NX)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DIMENSION FI(NXY)
C
      DO N=1,MAXIT
C
C.....CALCULATE "FALSE" RESIDUAL AND UPDATE VARIABLE
C
        DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          RES=Q(IJ)-AP(IJ)*FI(IJ)-AE(IJ)*FI(IJ+NJ)-
     *        AW(IJ)*FI(IJ-NJ)-AS(IJ)*FI(IJ-1)-AN(IJ)*FI(IJ+1)
          FI(IJ)=FI(IJ)+RES/AP(IJ)
        END DO
        END DO
C
C.....CHECK CONVERGENCE
C      
        RESN=0.
        DO I=2,NIM
          DO J=2,NJM
            IJ=LI(I)+J
            RES=Q(IJ)-AP(IJ)*FI(IJ)-AE(IJ)*FI(IJ+NJ)-
     *          AW(IJ)*FI(IJ-NJ)-AS(IJ)*FI(IJ-1)-AN(IJ)*FI(IJ+1)
            RESN=RESN+ABS(RES)
          END DO
        END DO
C
        IF(N.EQ.1) RES0=RESN
        RSM=RESN/RES0
        WRITE(8,*) N,' SWEEP, RSM  = ',RSM
        IF(RSM.LT.RESMAX) RETURN
C
      END DO
C
      RETURN
      END
C
C###########################################################     
      SUBROUTINE LSOL(FI)
C###########################################################
C     In this solver the TDMA algorithm is applied line-by-line
C     alternately along J and I lines.
C     Warning: arrays A() and C() should be dimensioned
C     as the maximum of NX and NY!
C===========================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDI/ NI,NJ,NIM,NJM,MAXIT,LI(NX)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DIMENSION A(NX),C(NX),FI(NXY) 
      DATA A,C /NX*0.,NX*0./
C
      DO N=1,MAXIT
C      
C.....SOLVE WITH "TDMA" ALONG  J-LINES
C
        DO I=2,NIM 
          DO J=2,NJM
            IJ=LI(I)+J
            APR=1./(AP(IJ)-AS(IJ)*A(J-1)) 
            A(J)=AN(IJ)*APR
            C(J)=(Q(IJ)-AE(IJ)*FI(IJ+NJ)-AW(IJ)*FI(IJ-NJ)-
     *           AS(IJ)*C(J-1))*APR
          END DO
C
          DO J=NJM,2,-1 
            IJ=LI(I)+J
            FI(IJ)=C(J)-A(J)*FI(IJ+1) 
          END DO
        END DO
C
C.....SOLVE WITH "TDMA" ALONG  I-LINES
C
        DO J=2,NJM 
          DO I=2,NIM 
            IJ=LI(I)+J
            APR=1./(AP(IJ)-AW(IJ)*A(I-1)) 
            A(I)=AE(IJ)*APR
            C(I)=(Q(IJ)-AN(IJ)*FI(IJ+1)-AS(IJ)*FI(IJ-1)-
     *           AW(IJ)*C(I-1))*APR
          END DO
C
          DO I=NIM,2,-1
            IJ=LI(I)+J
            FI(IJ)=C(I)-A(I)*FI(IJ+NJ)
          END DO
        END DO
C
C.....CHECK CONVERGENCE OF INNER ITERATIONS
C
        RESN=0.
        DO I=2,NIM
          DO J=2,NJM
            IJ=LI(I)+J
            RES=Q(IJ)-AP(IJ)*FI(IJ)-AE(IJ)*FI(IJ+NJ)-
     *          AW(IJ)*FI(IJ-NJ)-AS(IJ)*FI(IJ-1)-AN(IJ)*FI(IJ+1)
            RESN=RESN+ABS(RES)
          END DO
        END DO
C
        IF(N.EQ.1) RES0=RESN
        RSM=RESN/RES0
        WRITE(8,*) N,' SWEEP, RSM  = ',RSM
        IF(RSM.LT.RESMAX) RETURN 
C
      END DO
C
      RETURN
      END
C
C###########################################################     
      SUBROUTINE ADI(BETA,FI)
C###########################################################
C     This is the standard ADI solver, as described in Sect.
C     5.3.5. The original version of this code was written
C     by Joel H. Ferziger, Stanford University, 1995.
C===========================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDI/ NI,NJ,NIM,NJM,MAXIT,LI(NX)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DIMENSION AI(NX),CI(NX),AJ(NY),CJ(NY),API(NX),QI(NX),
     *          APJ(NY),QJ(NY),FI(NXY),FF(NXY) 
      DATA API,AI,APJ,AJ /NX*0.,NX*0.,NY*0.,NY*0./
C
C.....CALCULATE ADI COEFFICIENTS
C
      DO I=1,NI
      DO J=1,NJ
        IJ=LI(I)+J
        FF(IJ)=FI(IJ)
      END DO
      END DO
C
C.....START ITERATION LOOP
C
      DO N=1,MAXIT
C
C.....SOLVE WITH "TDMA" ALONG  I-LINES
C
        DO J=2,NJM 
          DO I=2,NIM
            IJ=LI(I)+J
            QI(I)=BETA+AN(IJ)+AS(IJ)
            API(I)=AP(IJ)+QI(I)
          END DO
C
          DO I=2,NIM 
            IJ=LI(I)+J
            APR=1./(API(I)-AW(IJ)*AI(I-1)) 
            AI(I)=AE(IJ)*APR
            CI(I)=(Q(IJ)+QI(I)*FI(IJ)-AN(IJ)*FI(IJ+1)-AS(IJ)*FI(IJ-1)-
     *           AW(IJ)*CI(I-1))*APR
          END DO
          DO I=NIM,2,-1
            IJ=LI(I)+J
            FF(IJ)=CI(I)-AI(I)*FF(IJ+NJ)
          END DO
        END DO
C      
      CALL PRINT(NI,NJ,0,FF,'FF SOL')
c
C.....SOLVE WITH "TDMA" ALONG  J-LINES
C
        DO I=2,NIM 
          DO J=2,NJM
            IJ=LI(I)+J
            QJ(J)=BETA+AE(IJ)+AW(IJ)
            APJ(J)=AP(IJ)+QJ(J)
          END DO
C
          DO J=2,NJM
            IJ=LI(I)+J
            APR=1./(APJ(J)-AS(IJ)*AJ(J-1)) 
            AJ(J)=AN(IJ)*APR
            CJ(J)=(Q(IJ)+QJ(J)*FF(IJ)-AE(IJ)*FF(IJ+NJ)-
     *            AW(IJ)*FF(IJ-NJ)-AS(IJ)*CJ(J-1))*APR
          END DO
          DO J=NJM,2,-1 
            IJ=LI(I)+J
            FI(IJ)=CJ(J)-AJ(J)*FI(IJ+1) 
          END DO
        END DO
C
      CALL PRINT(NI,NJ,0,FI,'FI SOL')
c
C.....CHECK CONVERGENCE OF INNER ITERATIONS
C
        RESN=0.
        DO I=2,NIM
          DO J=2,NJM
            IJ=LI(I)+J
            RES=Q(IJ)-AP(IJ)*FI(IJ)-AE(IJ)*FI(IJ+NJ)-
     *          AW(IJ)*FI(IJ-NJ)-AS(IJ)*FI(IJ-1)-AN(IJ)*FI(IJ+1)
            RESN=RESN+ABS(RES)
          END DO
        END DO
C
        IF(N.EQ.1) RES0=RESN
        RSM=RESN/RES0
        WRITE(8,*) N,' SWEEP, RSM  = ',RSM
        IF(RSM.LT.RESMAX) RETURN 
C
      END DO
C
      RETURN
      END
C
C#############################################################
      SUBROUTINE SIPSOL(FI)
C#############################################################
C     This is the ILU solver after Stone; see Sect. 5.3.4 for
C     a description of the algorithm.
C
C     M. Peric, Institut fuer Schiffbau, Hamburg, 1995
C=============================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDI/ NI,NJ,NIM,NJM,MAXIT,LI(NX)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      REAL LW,LS,LPR
      DIMENSION FI(NXY),UN(NXY),UE(NXY),RES(NXY),
     *          LW(NXY),LS(NXY),LPR(NXY)
      DATA UE,UN,RES /NXY*0.,NXY*0.,NXY*0./
C
C.....CALCULATE ELEMENTS OF [L] AND [U] MATRICES
C
      DO I=2,NIM
      DO IJ=LI(I)+2,LI(I)+NJM
        LW(IJ)=AW(IJ)/(1.+ALFA*UN(IJ-NJ))
        LS(IJ)=AS(IJ)/(1.+ALFA*UE(IJ-1))
        P1=ALFA*LW(IJ)*UN(IJ-NJ)
        P2=ALFA*LS(IJ)*UE(IJ-1)
        LPR(IJ)=1./(AP(IJ)+P1+P2-LW(IJ)*UE(IJ-NJ)-LS(IJ)*UN(IJ-1))
        UN(IJ)=(AN(IJ)-P1)*LPR(IJ)
        UE(IJ)=(AE(IJ)-P2)*LPR(IJ)
      END DO
      END DO
C      
C.....CALCULATE RESIDUAL AND AUXILLIARY VECTORS; INNER ITERATION LOOP
C
      DO N=1,MAXIT
C
        RESN=0.
        DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          RES(IJ)=Q(IJ)-AP(IJ)*FI(IJ)-AN(IJ)*FI(IJ+1)-
     *            AS(IJ)*FI(IJ-1)-AE(IJ)*FI(IJ+NJ)-AW(IJ)*FI(IJ-NJ)
          RESN=RESN+ABS(RES(IJ))
          RES(IJ)=(RES(IJ)-LS(IJ)*RES(IJ-1)-LW(IJ)*RES(IJ-NJ))*LPR(IJ)
        END DO
        END DO
        IF(N.EQ.1) RES0=RESN
C
C.....CALCULATE INCREMENT AND CORRECT VARIABLE
C
        DO I=NIM,2,-1
        DO IJ=LI(I)+NJM,LI(I)+2,-1
          RES(IJ)=RES(IJ)-UN(IJ)*RES(IJ+1)-UE(IJ)*RES(IJ+NJ)
          FI(IJ)=FI(IJ)+RES(IJ)
        END DO
        END DO
C
C.....CONVERGENCE CHECK
C
        RSM=RESN/(RES0+1.E-20)
        WRITE(8,*) N,' SWEEP, RSM = ',RSM
        IF(RSM.LT.RESMAX) RETURN
C
      END DO
C
      RETURN
      END
C
C##############################################################
      SUBROUTINE CGS(FI)
C##############################################################
C     This is a pre-conditioned conjugate gradient solver for
C     symmetric matrices (e.g., pressure or pressure-correction
C     equation, heat conduction, etc.). A 3D version is included
C     in LAPL3D.F file. The original code was written by Ismet
C     Demirdzic, Masinski Fakultet, Sarajevo, 1987 (see Sect.
C     5.3.6 for a description of the algorithm).
C===============================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDI/ NI,NJ,NIM,NJM,MAXIT,LI(NX)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DIMENSION PK(NXY),ZK(NXY),D(NXY),RES(NXY),FI(NXY)
      DATA D,ZK,PK,RES /NXY*0.,NXY*0.,NXY*0.,NXY*0./
C
C.....CALCULATE INITIAL RESIDUAL VECTOR
C
      RES0=0.
      DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          RES(IJ)=Q(IJ)-AP(IJ)*FI(IJ)-AE(IJ)*FI(IJ+NJ)-
     *           AW(IJ)*FI(IJ-NJ)-AN(IJ)*FI(IJ+1)-AS(IJ)*FI(IJ-1)
          RES0=RES0+ABS(RES(IJ))
        END DO
      END DO
C
C.....PRECONDITIONING MATRIX DIAGONAL
C
      DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          D(IJ)=1./(AP(IJ)-D(IJ-NJ)*AW(IJ)**2-D(IJ-1)*AS(IJ)**2)
        END DO
      END DO
C
C.....CALCULATION OF  ZK; INNER ITERATION LOOP
C
      S0=1.E20
      DO N=1,MAXIT
C
C.....FORWARD SUBSTITUTION
C
        DO I=2,NIM
          DO IJ=LI(I)+2,LI(I)+NJM
            ZK(IJ)=(RES(IJ)-AW(IJ)*ZK(IJ-NJ)-AS(IJ)*ZK(IJ-1))*D(IJ)
          END DO
        END DO
C
        DO I=2,NIM
          DO IJ=LI(I)+2,LI(I)+NJM
            ZK(IJ)=ZK(IJ)/(D(IJ)+1.E-20)
          END DO
        END DO
C
C.....BACKWARD SUBSTITUTION
C
        SK=0.
        DO I=NIM,2,-1
          DO IJ=LI(I)+NJM,LI(I)+2,-1
            ZK(IJ)=(ZK(IJ)-AE(IJ)*ZK(IJ+NJ)-AN(IJ)*ZK(IJ+1))*D(IJ)
            SK=SK+RES(IJ)*ZK(IJ) 
          END DO
        END DO
C
C.....CALCULATE BETA AND NEW SEARCH VECTOR
C
        BET=SK/S0
        DO I=2,NIM
          DO IJ=LI(I)+2,LI(I)+NJM
            PK(IJ)=ZK(IJ)+BET*PK(IJ)
          END DO
        END DO
C
C.....CALCULATE SCALAR PRODUCT (PK . A PK) AND ALPHA (A PK OVERWRITES ZK)
C
        S2=0.
        DO I=2,NIM
          DO IJ=LI(I)+2,LI(I)+NJM
            ZK(IJ)=AP(IJ)*PK(IJ)+AE(IJ)*PK(IJ+NJ)+AW(IJ)*PK(IJ-NJ)+ 
     *             AN(IJ)*PK(IJ+1)+AS(IJ)*PK(IJ-1) 
            S2=S2+PK(IJ)*ZK(IJ)
          END DO
        END DO
C
        ALF=SK/S2
C
C.....CALCULATE NEW RESIDUAL AND UPDATE VARIABLE
C
        RESN=0.
        DO I=2,NIM
          DO IJ=LI(I)+2,LI(I)+NJM
            FI(IJ)=FI(IJ)+ALF*PK(IJ) 
            RES(IJ)=RES(IJ)-ALF*ZK(IJ)
            RESN=RESN+ABS(RES(IJ))
          END DO
        END DO
        S0=SK
C
C....CHECK CONVERGENCE
C
        RSM=RESN/(RES0+1.E-20)
        WRITE(8,*) N,' SWEEP, RSM = ',RSM
        IF(RSM.LT.RESMAX) RETURN 
C
      END DO
C
      RETURN
      END
C
C#########################################################
      SUBROUTINE PRINT(NI,NJ,IJST,FI,NAME)
C#########################################################
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FI(NXYA),LI(NX)
      CHARACTER*6 NAME
C
      DO I=1,NI
        LI(I)=IJST+(I-1)*NJ
      END DO
C
      WRITE(8,20) NAME
      IE=1
      NL=NI/12+1
      IF(MOD(NI,12).EQ.0) NL=NI/12
      DO L=1,NL
        IS=IE
        IE=MIN(NI,IS+11)
        WRITE(8,21) (I,I=IS,IE)
        WRITE(8,22)
        DO J=NJ,1,-1
          WRITE(8,23) J,(FI(LI(I)+J),I=IS,IE)
        END DO
      END DO
   20 FORMAT(2X,26('*-'),7X,A6,7X,26('-*'))
   21 FORMAT(3X,'I = ',I3,11I10)
   22 FORMAT(2X,'J')
   23 FORMAT(1X,I3,1P12E10.2)
      RETURN
      END
C
C#########################################################
      BLOCK DATA
C#########################################################
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DATA FIA,QA,AEA,AWA,ANA,ASA,APA /NXYA*0.,NXYA*0.,NXYA*0.,
     *        NXYA*0.,NXYA*0.,NXYA*0.,NXYA*0./
      DATA AE,AW,AN,AS,AP,Q /NXY*0.,NXY*0.,NXY*0.,NXY*0.,NXY*0.,
     *        NXY*0./
      END
C
C#########################################################
      SUBROUTINE MG(NI,NJ,ISOL,FI)
C#########################################################
C    This routine controls the execution of the correction
C    scheme multigrid method based on V-cycles. Variable
C    LTEST allows printing of some quantities for the
C    analysis of performance and error checking. Arrays
C    of dimension NXYA contain variable values on all grids;
C    NXY is the dimension of the finest grid, for which 
C    the solution is sought.
C=========================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      COMMON /MGI/ NLEV,IJGR(10),NIGR(10),NJGR(10),
     *       N1(10),N2(10),N3(10),ITER,LTEST
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),Q(NXY)
      DIMENSION FI(NXY)
C
C.....INITIALIZATION
C
      PRINT *, ' Number of grid levels?  '
      READ(*,*) NLEV
      PRINT *, ' Maximum number of V-Cycles, LTEST (1 or 0):  '
      READ(*,*) NVCMAX,LTEST
      PRINT *, ' Number of iter. on the finest grid: N1(I), I=1,NLEV  '
      READ(*,*) (N1(I),I=1,NLEV)
      PRINT *, ' Number of iter. during Restr. N2(I),I=1,NLEV  '
      READ(*,*) (N2(I),I=1,NLEV)
      PRINT *, ' Number of iter. during Prolong. N3(I),I=1,NLEV  '
      READ(*,*) (N3(I),I=1,NLEV)
      ITER=0
C
C.....DEFINE COARSE GRID SIZES AND POINTERS
C
C    IJGR(K) is the pointer showing the index of the last value
C    from grid (K-1); it is the sum of the number of nodes (or,
C    to be more precise, of all storage locations) on all previous
C    grids. NIGR(K) is the number of nodes in I-direction on grid
C    K (number of CV plus 2 for boundary nodes); NJGR(K) is the
C    number of nodes in J-direction on grid K.
C
      ND=2**(NLEV-1)
      IJGR(1)=0
      NIGR(1)=(NI-2)/ND+2
      NJGR(1)=(NJ-2)/ND+2
      DO L=2,NLEV
        IJGR(L)=IJGR(L-1)+NIGR(L-1)*NJGR(L-1)
        NIGR(L)=2*NIGR(L-1)-2
        NJGR(L)=2*NJGR(L-1)-2
      ENDDO
      IF(LTEST.EQ.1) THEN
        WRITE(8,'(A8,8I4)') ' NIGR = ',(NIGR(L),L=1,NLEV)
        WRITE(8,'(A8,8I4)') ' NJGR = ',(NJGR(L),L=1,NLEV)
        WRITE(8,'(A8,8I4)') ' IJGR = ',(IJGR(L),L=1,NLEV)
      ENDIF
C
C.....FILL ALL-GRIDS ARRAYS WITH FINEST GRID VALUES
C
      DO I=1,NI
        II=(I-1)*NJ
        DO J=1,NJ
          IJ=II+J
          IJA=IJGR(NLEV)+IJ
          QA(IJA)=Q(IJ)
          FIA(IJA)=FI(IJ)
          AEA(IJA)=AE(IJ)
          AWA(IJA)=AW(IJ)
          ANA(IJA)=AN(IJ)
          ASA(IJA)=AS(IJ)
          APA(IJA)=AP(IJ)
        ENDDO
      ENDDO
      IF(LTEST.EQ.1) THEN
        CALL PRINT(NI,NJ,0,AW,'AW FIN')
        CALL PRINT(NI,NJ,0,AE,'AE FIN')
        CALL PRINT(NI,NJ,0,AS,'AS FIN')
        CALL PRINT(NI,NJ,0,AN,'AN FIN')
        CALL PRINT(NI,NJ,0,AP,'AP FIN')
        CALL PRINT(NI,NJ,0,Q,' Q FIN')
      ENDIF
C
C.....TRANSFER COEFFICIENT MATRIX TO COARSE GRIDS
C
      DO L=NLEV,2,-1
        NJF=NJGR(L)
        NIC=NIGR(L-1)
        NJC=NJGR(L-1)
C
        DO IC=2,NIC-1
          IIC=(IC-1)*NJC+IJGR(L-1)
          IF=2*IC-2
          IIF=(IF-1)*NJF+IJGR(L)
          DO JC=2,NJC-1
C
C    IJC is the global index of a node on the coarse grid, which
C    is computed by adding the pointer IJGR for that grid to the
C    local index value, which is (I-1)*NJ+J (see Table 3.1). IJF
C    is the corresponding index on the finer grid.
C
            IJC=IIC+JC
            JF=2*JC-2
            IJF=IIF+JF
            AWA(IJC)=0.5*(AWA(IJF)+AWA(IJF+1))
            AEA(IJC)=0.5*(AEA(IJF+NJF)+AEA(IJF+NJF+1))
            ASA(IJC)=0.5*(ASA(IJF)+ASA(IJF+NJF))
            ANA(IJC)=0.5*(ANA(IJF+1)+ANA(IJF+NJF+1))
            APA(IJC)=-(AWA(IJC)+AEA(IJC)+ASA(IJC)+ANA(IJC))
          END DO
        END DO
        IF(LTEST.EQ.1) THEN
          CALL PRINT(NIC,NJC,IJGR(L-1),AWA,'AW COR')
          CALL PRINT(NIC,NJC,IJGR(L-1),AEA,'AE COR')
          CALL PRINT(NIC,NJC,IJGR(L-1),ASA,'AS COR')
          CALL PRINT(NIC,NJC,IJGR(L-1),ANA,'AN COR')
          CALL PRINT(NIC,NJC,IJGR(L-1),APA,'AP COR')
        ENDIF
      END DO
C
C.....SOLVE ON FINEST GRID N1 TIMES - START MG V-CYCLE
C
      DO NVC=1,NVCMAX
        WRITE(8,*) '  '
        IF(ISOL.EQ.6) CALL GSMG(NLEV,N1(NLEV))
        IF(ISOL.EQ.7) CALL SIPMG(NLEV,N1(NLEV))
        IF(ISOL.EQ.8) CALL CGSMG(NLEV,N1(NLEV))
c        CALL NEUMANN(NLEV)
        IF(RSM.LT.RESMAX) GO TO 110
        IF(LTEST.EQ.1) THEN
          WRITE(8,*) '  CYCLE ',NVC
          WRITE(8,*) '  SOLUTION AFTER N1(NLEV) ITER. ON FINEST GRID '
          CALL PRINT(NIGR(NLEV),NJGR(NLEV),IJGR(NLEV),FIA,'FI FIN')
        ENDIF
C
C.....RESTRICTION
C
        DO L=NLEV-1,1,-1
          CALL RESTR(ISOL,L)
        END DO
C
C.....PROLONGATION
C
        DO L=1,NLEV-1
          CALL INTER(ISOL,L)
        END DO
C
        IF(RSM.LT.RESMAX) GO TO 110
      END DO
  110 CONTINUE
      WRITE(8,*) '  TOTAL NO. OF ITER. ON FINE GRID:  ',ITER
C
C.....TRANSFER SOLUTION BACK TO 2D ARRAY
C
      DO I=1,NI
        II=(I-1)*NJ
        DO J=1,NJ
          IJ=II+J
          IJA=IJGR(NLEV)+IJ
          FI(IJ)=FIA(IJA)
        END DO
      END DO
C
      RETURN
      END
C
C############################################################
      SUBROUTINE RESTR(ISOL,L)
C############################################################
C    This routine performs restriction of the residual from
C    fine to coarse grid and solves for the correction on the
C    coarse grid (level L)
C============================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /MGI/ NLEV,IJGR(10),NIGR(10),NJGR(10),
     *       N1(10),N2(10),N3(10),ITER,LTEST
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      DIMENSION RES(NXYA)
      DATA RES /NXYA*0./
C
C.....CALCULATE RESIDUALS ON FINE GRID
C
      NIF=NIGR(L+1)
      NJF=NJGR(L+1)
      DO IF=2,NIF-1
        IIF=(IF-1)*NJF+IJGR(L+1)
        DO JF=2,NJF-1
          IJF=IIF+JF
          RES(IJF)=-AEA(IJF)*FIA(IJF+NJF)-AWA(IJF)*FIA(IJF-NJF)-
     *              ASA(IJF)*FIA(IJF-1)  -ANA(IJF)*FIA(IJF+1)  +
     *              QA(IJF)-APA(IJF)*FIA(IJF)
        END DO
      END DO
      IF(LTEST.EQ.1) CALL PRINT(NIF,NJF,IJGR(L+1),RES,'RES FI')
C
C.....TRANSFER RESIDUALS TO COARSE GRID
C
      NIC=NIGR(L)
      NJC=NJGR(L)
      DO IC=2,NIC-1
        IIC=(IC-1)*NJC+IJGR(L)
        IF=2*IC-2
        IIF=(IF-1)*NJF+IJGR(L+1)
        DO JC=2,NJC-1
          IJC=IIC+JC
          JF=2*JC-2
          IJF=IIF+JF
          QA(IJC)=RES(IJF)+RES(IJF+1)+RES(IJF+NJF)+RES(IJF+NJF+1)
          FIA(IJC)=0.
        END DO
      END DO
      IF(LTEST.EQ.1) CALL PRINT(NIC,NJC,IJGR(L),QA,'RES CO')
C
C.....SOLVE ON COARSE GRID N2 TIMES
C
      IF(ISOL.EQ.6) CALL GSMG(L,N2(L))
      IF(ISOL.EQ.7) CALL SIPMG(L,N2(L))
      IF(ISOL.EQ.8) CALL CGSMG(L,N2(L))
c      CALL NEUMANN(L)
      IF(LTEST.EQ.1) WRITE(8,*) ' CORRECTION ON GRID ',L
      IF(LTEST.EQ.1) CALL PRINT(NIC,NJC,IJGR(L),FIA,'FI CO ')
C
      RETURN
      END
C
C##########################################################
      SUBROUTINE INTER(ISOL,L)
C##########################################################
C    This routine prolongates the correction calculated on
C    the coarser grid to the finer grid, and solves several
C    times to smooth the interpolation error. Bilinear
C    intrepolation is used.
C==========================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /MGI/ NLEV,IJGR(10),NIGR(10),NJGR(10),
     *       N1(10),N2(10),N3(10),ITER,LTEST
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      DIMENSION COR(NXYA)
      DATA COR /NXYA*0./
C
C.....INTERPOLATE CORRECTION TO FINE GRID: INNER REGION
C
      NIC=NIGR(L)
      NJC=NJGR(L)
      NIF=NIGR(L+1)
      NJF=NJGR(L+1)
      DO IC=2,NIC-2
        IIC=(IC-1)*NJC+IJGR(L)
        IF=2*IC-1
        IIF=(IF-1)*NJF+IJGR(L+1)
        DO JC=2,NJC-2
          IJC=IIC+JC
          JF=2*JC-1
          IJF=IIF+JF
          FIL1=0.75*FIA(IJC)+0.25*FIA(IJC+1)
          FIL2=0.25*FIA(IJC)+0.75*FIA(IJC+1)
          FIR1=0.75*FIA(IJC+NJC)+0.25*FIA(IJC+NJC+1)
          FIR2=0.25*FIA(IJC+NJC)+0.75*FIA(IJC+NJC+1)
          COR(IJF)  =0.75*FIL1+0.25*FIR1
          COR(IJF+1)=0.75*FIL2+0.25*FIR2
          COR(IJF+NJF)  =0.25*FIL1+0.75*FIR1
          COR(IJF+NJF+1)=0.25*FIL2+0.75*FIR2
        END DO
      END DO
C
C.....PROLONGATION ALONG BOUNDARIES - SOUTH & NORTH
C
      DO IC=2,NIC-2
        IIC=(IC-1)*NJC+IJGR(L)
        IF=2*IC-1
        IIF=(IF-1)*NJF+IJGR(L+1)
        IJC=IIC+2
        IJF=IIF+2
        COR(IJF)    =0.375*FIA(IJC)+0.125*FIA(IJC+NJC)
        COR(IJF+NJF)=0.375*FIA(IJC+NJC)+0.125*FIA(IJC)
C
        IJC=IIC+NJC-1
        IJF=IIF+NJF-1
        COR(IJF)    =0.375*FIA(IJC)+0.125*FIA(IJC+NJC)
        COR(IJF+NJF)=0.375*FIA(IJC+NJC)+0.125*FIA(IJC)
      END DO
C
C.....PROLONGATION ALONG BOUNDARIES - WEST & EAST
C
      DO JC=2,NJC-2
        IJC=NJC+IJGR(L)+JC
        JF=2*JC-1
        IJF=NJF+IJGR(L+1)+JF
        COR(IJF)  =0.375*FIA(IJC)+0.125*FIA(IJC+1)
        COR(IJF+1)=0.375*FIA(IJC+1)+0.125*FIA(IJC)
C
        IJC=(NIC-2)*NJC+IJGR(L)+JC
        IJF=(NIF-2)*NJF+IJGR(L+1)+JF
        COR(IJF)  =0.375*FIA(IJC)+0.125*FIA(IJC+1)
        COR(IJF+1)=0.375*FIA(IJC+1)+0.125*FIA(IJC)
      END DO
C
C.....CORNER NODES
C
      IIF=IJGR(L+1)+NJF
      IIC=IJGR(L)+NJC
      COR(IIF+2)    =0.25*FIA(IIC+2)
      COR(IIF+NJF-1)=0.25*FIA(IIC+NJC-1)
      IIF=IJGR(L+1)+(NIF-2)*NJF
      IIC=IJGR(L)  +(NIC-2)*NJC
      COR(IIF+2)    =0.25*FIA(IIC+2)
      COR(IIF+NJF-1)=0.25*FIA(IIC+NJC-1)
C
      IF(LTEST.EQ.1) CALL PRINT(NIF,NJF,IJGR(L+1),COR,'COR FI')
      NIJF=NIF*NJF
      DO IJ=IJGR(L+1)+1,IJGR(L+1)+NIJF
        FIA(IJ)=FIA(IJ)+COR(IJ)
      END DO
C
C.....SOLVE ON FINE GRID N3 TIMES
C
      IF(ISOL.EQ.6) CALL GSMG(L+1,N3(L+1))
      IF(ISOL.EQ.7) CALL SIPMG(L+1,N3(L+1))
      IF(ISOL.EQ.8) CALL CGSMG(L+1,N3(L+1))
c      CALL NEUMANN(L+1)  
      IF(LTEST.EQ.1) 
     *   WRITE(8,*) ' SOLUTION IN PROLONGATION STAGE ON GRID ',L+1
      IF(LTEST.EQ.1) CALL PRINT(NIF,NJF,IJGR(L+1),FIA,'SOL FI')
C
      RETURN
      END

C###########################################################
      SUBROUTINE SIPMG(L,NITER)
C###########################################################
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /MGI/ NLEV,IJGR(10),NIGR(10),NJGR(10),
     *       N1(10),N2(10),N3(10),ITER,LTEST
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      REAL LW,LS,LPR
      DIMENSION UN(NXYA),UE(NXYA),RES(NXYA),
     *          LW(NXYA),LS(NXYA),LPR(NXYA)
      DATA UE,UN,RES /NXYA*0.,NXYA*0.,NXYA*0./
C
C.....CALCULATE ELEMENTS OF [L] AND [U] MATRICES
C
      NI=NIGR(L)
      NJ=NJGR(L)
      NIM=NI-1
      NJM=NJ-1
C
      DO I=2,NIM
      II=(I-1)*NJ+IJGR(L)
      DO IJ=II+2,II+NJM
        LW(IJ)=AWA(IJ)/(1.+ALFA*UN(IJ-NJ))
        LS(IJ)=ASA(IJ)/(1.+ALFA*UE(IJ-1))
        P1=ALFA*LW(IJ)*UN(IJ-NJ)
        P2=ALFA*LS(IJ)*UE(IJ-1)
        LPR(IJ)=1./(APA(IJ)+P1+P2-LW(IJ)*UE(IJ-NJ)-LS(IJ)*UN(IJ-1))
        UN(IJ)=(ANA(IJ)-P1)*LPR(IJ)
        UE(IJ)=(AEA(IJ)-P2)*LPR(IJ)
      END DO
      END DO
C      
C.....CALCULATE RESIDUAL AND AUXILLIARY VECTORS - ITERATE
C
      DO N=1,NITER
        RESN=0.
        DO I=2,NIM
        II=(I-1)*NJ+IJGR(L)
        DO IJ=II+2,II+NJM
          RES(IJ)=QA(IJ)-APA(IJ)*FIA(IJ)-ANA(IJ)*FIA(IJ+1)-
     *    ASA(IJ)*FIA(IJ-1)-AEA(IJ)*FIA(IJ+NJ)-AWA(IJ)*FIA(IJ-NJ)
          RESN=RESN+ABS(RES(IJ))
          RES(IJ)=(RES(IJ)-LS(IJ)*RES(IJ-1)-LW(IJ)*RES(IJ-NJ))*LPR(IJ)
        END DO
        END DO
        IF(ITER.EQ.0) RES0=RESN
C
C.....CALCULATE INCREMENT AND CORRECT VARIABLE
C
        DO I=NIM,2,-1
        II=(I-1)*NJ+IJGR(L)
        DO IJ=II+NJM,II+2,-1
          RES(IJ)=RES(IJ)-UN(IJ)*RES(IJ+1)-UE(IJ)*RES(IJ+NJ)
          FIA(IJ)=FIA(IJ)+RES(IJ)
        END DO
        END DO
C
C.....CONVERGENCE CHECK (ON THE FINEST GRID ONLY)
C
        IF(L.EQ.NLEV) THEN
          RSM=RESN/(RES0+1.E-20)
          ITER=ITER+1
          WRITE(8,*) ITER,' SWEEP, RSM = ',RSM
          IF(RSM.LT.RESMAX) RETURN
        ENDIF
C
      END DO
C
      RETURN
      END
C
C############################################################
      SUBROUTINE GSMG(L,NITER)
C############################################################
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /MGI/ NLEV,IJGR(10),NIGR(10),NJGR(10),
     *       N1(10),N2(10),N3(10),ITER,LTEST
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
C
C.....CALCULATE "FALSE" RESIDUAL AND UPDATE VARIABLE
C
      NI=NIGR(L)
      NJ=NJGR(L)
      NIM=NI-1
      NJM=NJ-1
C
      DO N=1,NITER
        DO I=2,NIM
        II=(I-1)*NJ+IJGR(L)
        DO IJ=II+2,II+NJM
          RES=QA(IJ)-APA(IJ)*FIA(IJ)-AEA(IJ)*FIA(IJ+NJ)-
     *        AWA(IJ)*FIA(IJ-NJ)-ASA(IJ)*FIA(IJ-1)-ANA(IJ)*FIA(IJ+1)
          FIA(IJ)=FIA(IJ)+RES/APA(IJ)
        END DO
        END DO
C
C.....CHECK CONVERGENCE
C      
        IF(L.EQ.NLEV) THEN
          RESN=0.
          DO I=2,NIM
          II=(I-1)*NJ+IJGR(L)
          DO IJ=II+2,II+NJM
            RES=QA(IJ)-APA(IJ)*FIA(IJ)-AEA(IJ)*FIA(IJ+NJ)-
     *        AWA(IJ)*FIA(IJ-NJ)-ASA(IJ)*FIA(IJ-1)-ANA(IJ)*FIA(IJ+1)
            RESN=RESN+ABS(RES)
          END DO
          END DO
C
          IF(ITER.EQ.0) RES0=RESN
          RSM=RESN/(RES0+1.E-20)
          ITER=ITER+1
          WRITE(8,*) ITER,' SWEEP, RSM = ',RSM
          IF(RSM.LT.RESMAX) RETURN
        ENDIF
C
      END DO
C
      RETURN
      END
C
C##############################################################
      SUBROUTINE CGSMG(L,NITER)
C##############################################################
C     This is a pre-conditioned conjugate gradient solver for
C     symmetric matrices (e.g., pressure or pressure-correction
C     equation, heat conduction, etc.). A 3D version is included
C     in LAPL3D.F file. The original code was written by Ismet
C     Demirdzic, Masinski Fakultet, Sarajevo, 1987 (see Sect.
C     5.3.6 for a description of the algorithm).
C===============================================================
      PARAMETER (NX=162,NY=162,NXY=NX*NY,NXYA=NXY+NXY/2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /MGI/ NLEV,IJGR(10),NIGR(10),NJGR(10),
     *       N1(10),N2(10),N3(10),ITER,LTEST
      COMMON /MGR/ RESNOR,RSM,FIA(NXYA),QA(NXYA),AEA(NXYA),
     *       AWA(NXYA),ANA(NXYA),ASA(NXYA),APA(NXYA)
      COMMON /INDR/ RESMAX,ALFA,ERRNOR,RES0,X(NX),Y(NY)
      DIMENSION PK(NXYA),ZK(NXYA),D(NXYA),RES(NXYA)
      DATA D,ZK,PK,RES /NXYA*0.,NXYA*0.,NXYA*0.,NXYA*0./
C
      NI=NIGR(L)
      NJ=NJGR(L)
      NIM=NI-1
      NJM=NJ-1
C
C.....CALCULATE INITIAL RESIDUAL VECTOR
C
      RESN=0.
      DO I=2,NIM
        DO J=2,NJM
          IJ=IJGR(L)+(I-1)*NJ+J
          RES(IJ)=QA(IJ)-APA(IJ)*FIA(IJ)-AEA(IJ)*FIA(IJ+NJ)-
     *        AWA(IJ)*FIA(IJ-NJ)-ANA(IJ)*FIA(IJ+1)-ASA(IJ)*FIA(IJ-1)
          RESN=RESN+ABS(RES(IJ))
        END DO
      END DO
      IF(ITER.EQ.0) RES0=RESN
C
C.....PRECONDITIONING MATRIX DIAGONAL
C
      DO I=2,NIM
        DO J=2,NJM
          IJ=IJGR(L)+(I-1)*NJ+J
          D(IJ)=1./(APA(IJ)-D(IJ-NJ)*AWA(IJ)**2-D(IJ-1)*ASA(IJ)**2)
        END DO
      END DO
C
C.....CALCULATION OF  ZK; INNER ITERATION LOOP
C
      S0=1.E20
      DO N=1,NITER
C
C.....FORWARD SUBSTITUTION
C
        DO I=2,NIM
          DO J=2,NJM
            IJ=IJGR(L)+(I-1)*NJ+J
            ZK(IJ)=(RES(IJ)-AWA(IJ)*ZK(IJ-NJ)-ASA(IJ)*ZK(IJ-1))*D(IJ)
          END DO
        END DO
C
        DO I=2,NIM
          DO J=2,NJM
            IJ=IJGR(L)+(I-1)*NJ+J
            ZK(IJ)=ZK(IJ)/(D(IJ)+1.E-20)
          END DO
        END DO
C
C.....BACKWARD SUBSTITUTION
C
        SK=0.
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            IJ=IJGR(L)+(I-1)*NJ+J
            ZK(IJ)=(ZK(IJ)-AEA(IJ)*ZK(IJ+NJ)-ANA(IJ)*ZK(IJ+1))*D(IJ)
            SK=SK+RES(IJ)*ZK(IJ) 
          END DO
        END DO
C
C.....CALCULATE BETA AND NEW SEARCH VECTOR
C
        BET=SK/S0
        DO I=2,NIM
          DO J=2,NJM
            IJ=IJGR(L)+(I-1)*NJ+J
            PK(IJ)=ZK(IJ)+BET*PK(IJ)
          END DO
        END DO
C
C.....CALCULATE SCALAR PRODUCT (PK . A PK) AND ALPHA (A PK OVERWRITES ZK)
C
        S2=0.
        DO I=2,NIM
          DO J=2,NJM
            IJ=IJGR(L)+(I-1)*NJ+J
            ZK(IJ)=APA(IJ)*PK(IJ)+AEA(IJ)*PK(IJ+NJ)+AWA(IJ)*PK(IJ-NJ)+ 
     *             ANA(IJ)*PK(IJ+1)+ASA(IJ)*PK(IJ-1) 
            S2=S2+PK(IJ)*ZK(IJ)
          END DO
        END DO
C
        ALF=SK/S2
C
C.....CALCULATE NEW RESIDUAL AND UPDATE VARIABLE
C
        RESN=0.
        DO I=2,NIM
          DO J=2,NJM
            IJ=IJGR(L)+(I-1)*NJ+J
            FIA(IJ)=FIA(IJ)+ALF*PK(IJ) 
            RES(IJ)=RES(IJ)-ALF*ZK(IJ)
            RESN=RESN+ABS(RES(IJ))
          END DO
        END DO
        S0=SK
C
C.....CONVERGENCE CHECK (ON THE FINEST GRID ONLY)
C
        IF(L.EQ.NLEV) THEN
          RSM=RESN/(RES0+1.E-20)
          ITER=ITER+1
          WRITE(8,*) ITER,' SWEEP, RSM = ',RSM
          IF(RSM.LT.RESMAX) RETURN
        ENDIF
C
      END DO
C
      RETURN
      END
C


















