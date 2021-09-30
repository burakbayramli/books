C#########################################################
      PROGRAM FV
C#########################################################
C     FINITE VOLUME METHOD FOR SOLVING CONSERVATION EQUATION
C     FOR SCALAR TRANSPORT USING CARTESIAN GRIDS AND KNOWN
C     VELOCITY FIELD (HERE: STAGNATION POINT FLOW, U=X AND
C     V=-Y; AT X=0, SCALAR VARIES FROM 0.0 AT Y=Ymax TO 1.0 AT
C     Y=0; AT X=Xmax, OUTFLOW - ZERO GRADIENT EXTRAPOLATION
C     FROM INSIDE; AT Y=Ymax, INFLOW, SCALAR = 0.0; AT Y=0,
C     NEUMANN BOUNDARY CONDITION, ZERO GRADIENT IN 
C     Y-DIRECTION).
C                               M. PERIC, IfS, 1996
C#########################################################
      PARAMETER (NX=82,NY=82)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY)
      DIMENSION X(NX),Y(NY),XC(NX),YC(NY),CU(NX,NY),CV(NX,NY),
     *       FX(NX),FY(NY),DW(NY)
      CHARACTER FILEO*20
C
C.....READ INPUT DATA
C
      PRINT *, ' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILEO
    1 FORMAT(A20)
      OPEN (UNIT=8,FILE=FILEO)
C
      PRINT *,' ENTER: DEN(sity), DIF(fusion coeff.)  '
      READ(*,*) DEN,DIF
      PRINT *, ' CHOOSE SCHEME: 1 - CDS; 2 - UDS  '
      READ(*,*) ID
C
C.....DEFINE X-GRID (EXX - Exp. factor; NICV - Number of CV)
C
      PRINT *,' ENTER: XMIN, XMAX, EXX, NICV  '
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
C.....DEFINE Y-GRID (EXY - Exp. factor; NJCV - Number of CV)
C
      PRINT *,' ENTER: YMIN, YMAX, EXY, NJCV  '
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
      DO I=2,NI
        DO J=1,NJ
          FI(I,J)=0.
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
C.....INITIALIZE DIFFUSION COEFFICIENT AT WEST AND SOUTH FACE
C
      DO J=2,NJM
        DW(J)=-DIF*(Y(J)-Y(J-1))/(XC(2)-XC(1))
      END DO
C
      DO I=2,NIM
        ASD=-DIF*(X(I)-X(I-1))/(YC(2)-YC(1))
C
C.....LOOP OVER ALL CV's EAST AND NORTH FACE; DIFFUSION COEFFICIENTS
C
        DO J=2,NJM
          AWD=DW(J)
          AED=-DIF*(Y(J)-Y(J-1))/(XC(I+1)-XC(I))
          AND=-DIF*(X(I)-X(I-1))/(YC(J+1)-YC(J))
C
C.....DISCRETIZE CONVECTION TERM (UDS OR CDS)
C
          IF(ID.EQ.2) THEN
            AEC= MIN(CU(I,J),0.)
            AWC=-MAX(CU(I-1,J),0.)
            ANC= MIN(CV(I,J),0.)
            ASC=-MAX(CV(I,J-1),0.)
          ELSE
            AEC= CU(I,J)*FX(I)
            AWC=-CU(I-1,J)*(1.-FX(I-1))
            ANC= CV(I,J)*FY(J)
            ASC=-CV(I,J-1)*(1.-FY(J-1))
          ENDIF
C
C.....SET COEFFICIENTS MATRIX
C
          AE(I,J)=AED+AEC
          AW(I,J)=AWD+AWC
          AN(I,J)=AND+ANC
          AS(I,J)=ASD+ASC
          AP(I,J)=-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))
          Q(I,J)=0.
C
          DW(J)=AED
          ASD=AND
        END DO
      END DO
C
C.....WEST BOUNDARY - DIRICHLET B.C.
C
      DO J=2,NJM
        Q(2,J)=Q(2,J)-AW(2,J)*FI(1,J)
        AW(2,J)=0.
C
C.....EAST BOUNDARY - OTFLOW B.C. (ZERO-GRAD. EXTRAPOLATION)
C
        AP(NIM,J)=AP(NIM,J)+AE(NIM,J)
        AE(NIM,J)=0.
      END DO
C
C.....SOUTH BOUNDARY - SYMMETRY B.C.
C
      DO I=2,NIM
        AP(I,2)=AP(I,2)+AS(I,2)
        AS(I,2)=0.
C
C.....NORTH BOUNDARY - INLET B.C.
C
        Q(I,NJM)=Q(I,NJM)-AN(I,NJM)*FI(I,NJ)
        AN(I,NJM)=0.
      END DO
C
C.....SOLVE EQUATION SYSTEM
C
      PRINT *,' CHOOSE SOLVER: 1 - LBLX, 2 - LBLY, 3 - ILU  '
      READ(*,*) IS
      IF(IS.EQ.1) THEN 
        CALL  LBLX
      ELSEIF(IS.EQ.2) THEN
        CALL LBLY
      ELSEIF(IS.EQ.3) THEN
        PRINT *, '  ENTER: ALFA  '
        READ(*,*) ALFA
        CALL SIPSOL(ALFA)
      ENDIF
C
C.....PRINT THE RESULT
C
      DO I=2,NIM
        FI(I,1)=FI(I,2)
      END DO
      DO J=1,NJ
        FI(NI,J)=FI(NIM,J)
      END DO
C
      CALL PRINT(FI,' TEMP ')
C
C.....CALCULATE WALL HEAT FLUX
C
      QWALL=0.
      DO J=2,NJM
        D=DIF*(Y(J)-Y(J-1))/(XC(2)-XC(1))
        QWALL=QWALL+D*(FI(2,J)-FI(1,J))
      END DO
      WRITE(8,*) '  '
      IF(ID.EQ.2) WRITE(8,*) '      UDS USED FOR CONVECTION '
      IF(ID.EQ.1) WRITE(8,*) '      CDS USED FOR CONVECTION '
      IF(IS.EQ.1) WRITE(8,*) '      TDMA SOLVER IN X-DIRECTION '
      IF(IS.EQ.2) WRITE(8,*) '      TDMA SOLVER IN Y-DIRECTION '
      IF(IS.EQ.3) WRITE(8,*) '      SIP - SOLVER '
      WRITE(8,*) '  '
      WRITE(8,*) '      QWALL = ',QWALL
      STOP
      END
C
C##########################################################
      SUBROUTINE LBLX
C##########################################################
C     Line-by-line application of TDMA along J=const. lines
C==========================================================
      PARAMETER (NX=82,NY=82)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY)
      DIMENSION BPR(NX),V(NX)
C
      DO L=1,1000
C
C.....SOLVE ALONG LINES  J=CONST.
C
        DO J=2,NJM
          DO I=2,NIM
            BPR(I)=1./(AP(I,J)-AW(I,J)*AE(I-1,J)*BPR(I-1))
            V(I)  =Q(I,J)-AN(I,J)*FI(I,J+1)-AS(I,J)*FI(I,J-1)-
     *             AW(I,J)*V(I-1)*BPR(I-1)
          END DO
C
          DO I=NIM,2,-1
            FI(I,J)=(V(I)-AE(I,J)*FI(I+1,J))*BPR(I)
          END DO
        END DO
C
C.....CALCULATE RESIDUAL AND CHECK CONVERGENCE
C
        RESL=0.
        DO I=2,NIM
          DO J=2,NJM
            RESL=RESL+ABS(AE(I,J)*FI(I+1,J)+AW(I,J)*FI(I-1,J)+AN(I,J)*
     *           FI(I,J+1)+AS(I,J)*FI(I,J-1)+AP(I,J)*FI(I,J)-Q(I,J))
          END DO
        END DO
C
        IF(L.EQ.1) RESNOR=RESL
        RSM=RESL/(RESNOR+1.E-20)
        WRITE(8,*) L,' ITER., RSM = ',RSM
        IF(RSM.LT.1.E-4) RETURN
C
      END DO
C
      RETURN
      END
C
C##########################################################
      SUBROUTINE LBLY
C##########################################################
C     Line-by-line application of TDMA along I=const. lines
C==========================================================
      PARAMETER (NX=82,NY=82)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY)
      DIMENSION BPR(NY),V(NY)
C
      DO L=1,1000
C
C.....SOLVE ALONG LINES  I=CONST.
C
        DO I=2,NIM
          DO J=2,NJM
            BPR(J)=1./(AP(I,J)-AS(I,J)*AN(I,J-1)*BPR(J-1))
            V(J)  =Q(I,J)-AE(I,J)*FI(I+1,J)-AW(I,J)*FI(I-1,J)-
     *             AS(I,J)*V(J-1)*BPR(J-1)
          END DO
C
          DO J=NJM,2,-1
            FI(I,J)=(V(J)-AN(I,J)*FI(I,J+1))*BPR(J)
          END DO
        END DO
C      
C......CALCULATE RESIDUAL AND CHECK CONVERGENCE
C
        RESL=0.
        DO I=2,NIM
          DO J=2,NJM
            RESL=RESL+ABS(AE(I,J)*FI(I+1,J)+AW(I,J)*FI(I-1,J)+AN(I,J)*
     *           FI(I,J+1)+AS(I,J)*FI(I,J-1)+AP(I,J)*FI(I,J)-Q(I,J))
          END DO
        END DO
C
        IF(L.EQ.1) RESNOR=RESL
        RSM=RESL/RESNOR
        WRITE(8,*) L,' ITER., RSM = ',RSM
        IF(RSM.LT.1.E-4) RETURN
C
      END DO
C
      RETURN
      END
C
C############################################################
      SUBROUTINE SIPSOL(ALFA)
C############################################################
C     SIP solver of Stone (1968)
C============================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY)
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
      PARAMETER (NX=82,NY=82)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY)
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
