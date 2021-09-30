C#######################################################
      PROGRAM FD
C#######################################################
C     This program solves the one-dimensional convection-
C     diffusion equation with Dirichlet boundary conditions
C     on both ends. The exact solution is compared with
C     solutions obtained using FD method and either UDS
C     or CDS for convection term, CDS for diffusion term.
C   
C     1995             M. Peric, Institut fuer Schiffbau
C#######################################################
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
      DIMENSION FIEX(NX)
      CHARACTER FILOUT*10
C
C.....OPEN FILES
C
      PRINT *,' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILOUT
    1 FORMAT(A10)
      OPEN (UNIT=8,FILE=FILOUT)
C
C.....READ INPUT DATA
C
      PRINT *,' ENTER: DEN(SITY), VEL(OCITY), DIF(FUSION COEFF.)  '
      READ(*,*) DEN,VEL,DIF
      PRINT *,' ENTER BOUNDARY VALUES: FI0, FIN  '
      READ(*,*) FI0,FIN
      PRINT *,' CHOOSE CONVECTION SCHEME: 1 - CDS, 2 - UDS  '
      READ(*,*) IC
C
C.....DEFINE THE GRID: EX - EXPANSION FACTOR; 
C     N - NUMBER OF NODES INCL. BOUNDARY ONES
C
      PRINT *,' ENTER: XMIN, XMAX, EX, N  '
      READ(*,*) XMIN,XMAX,EX,N
      NM=N-1
      IF(EX.EQ.1.) THEN
        DX=(XMAX-XMIN)/REAL(N-1)
      ELSE
        DX=(XMAX-XMIN)*(1.-EX)/(1.-EX**(N-1))
      ENDIF
      X(1)=XMIN
      DO I=2,N
        X(I)=X(I-1)+DX
        DX=DX*EX
      END DO
C
C.....INITIALIZE FIELDS
C
      DO I=1,N
        FI(I)=0.
      END DO
      FI(1)=FI0
      FI(N)=FIN
      DENVEL=DEN*VEL
      ZERO=0.
C
      DO I=2,NM
C
C.....CENTRAL DIFFERENCE CONVECTION APPROX. (CDS)
C
        IF(IC.EQ.1) THEN
          AEC= DENVEL/(X(I+1)-X(I-1))
          AWC=-AEC
C
C.....UPWIND CONVECTION APPROX. (UDS)
C
        ELSEIF(IC.EQ.2) THEN
          AEC= MIN(DENVEL,ZERO)/(X(I+1)-X(I))
          AWC=-MAX(DENVEL,ZERO)/(X(I)-X(I-1))
        ENDIF
C
C.....CENTRAL DIFFERENCE DIFFUSION APPROX. (CDS)
C
        DXR=2./(X(I+1)-X(I-1))
        AED=-DIF*DXR/(X(I+1)-X(I))
        AWD=-DIF*DXR/(X(I)-X(I-1))
C
C.....ASSEMBLE COEFFICIENT MATRIX
C
        AE(I)=AEC+AED
        AW(I)=AWC+AWD
        AP(I)=-AW(I)-AE(I)
        Q(I)=0.
      END DO
C
C.....BOUNDARY CONDITIONS
C
      Q(2)=Q(2)-AW(2)*FI(1)
      AW(2)=0.
      Q(NM)=Q(NM)-AE(NM)*FI(N)
      AE(NM)=0.
C
C.....SOLVE EQUATION SYSTEM
C
      PRINT *,' CHOOSE SOLVER: 1 - JACOBI, 2 - GS, 3 - GSOR, 4 - TDMA'
      READ(*,*) IS
      IF(IS.EQ.1) THEN 
        CALL JACOBI
      ELSEIF(IS.EQ.2) THEN
        CALL GS
      ELSEIF(IS.EQ.3) THEN
        CALL GSOR
      ELSEIF(IS.EQ.4) THEN
        CALL TDMA
      ENDIF
C
C.....CALCULATE EXACT SOLUTION AND ERROR NORM
C
      ERROR=0.
      FIEX(1)=FI0
      FIEX(N)=FIN
      PE=DENVEL*(XMAX-XMIN)/DIF
      RX=1./(XMAX-XMIN)
      DO I=2,NM
        FIEX(I)=FI0+(EXP(PE*X(I)*RX)-1.)/(EXP(PE)-1.)*(FIN-FI0)
        ERROR=ERROR+ABS(FIEX(I)-FI(I))
      END DO
      ERROR=ERROR/REAL(N)
C
C.....PRINT THE RESULT
C
      WRITE(8,*) '  '
      WRITE(8,*) '     PECLET NUMBER:  PE = ',PE
      WRITE(8,*) '     ERROR NORM = ',ERROR
      IF(IC.EQ.1) WRITE(8,*) '     CDS USED FOR CONVECTION '
      IF(IC.EQ.2) WRITE(8,*) '     UDS USED FOR CONVECTION '
      IF(IS.EQ.1) WRITE(8,*) '     JACOBI SOLVER '
      IF(IS.EQ.2) WRITE(8,*) '     GAUSS-SEIDEL SOLVER '
      IF(IS.EQ.3) WRITE(8,*) '     GSOR SOLVER '
      IF(IS.EQ.4) WRITE(8,*) '     TDMA SOLVER '
      WRITE(8,*) '  '
      WRITE(8,*) '      X        FI_EXACT       FI        ERROR '
      WRITE(8,*) '  '
      DO I=1,N
        WRITE(8,65) X(I),FIEX(I),FI(I),FIEX(I)-FI(I)
      END DO
   65 FORMAT(1P4E12.4)
      STOP
      END
C
C#############################################################
      SUBROUTINE TDMA
C#############################################################
C     INITIAL VALUES OF VARIABLES BPR(I) AND V(I) ASSUMED ZERO!
C
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
      DIMENSION BPR(NX),V(NX)
C
C.....CALCULATE 1./U_P (BPR) AND MODIFIED SOURCE TERM (V)
C
      DO I=2,NM
        BPR(I)=1./(AP(I)-AW(I)*AE(I-1)*BPR(I-1))
        V(I)=Q(I)-AW(I)*V(I-1)*BPR(I-1)
      END DO
C
C.....CALCULATE VARIABLE VALUES - BACKWARD SUBSTITUTION
C
      DO I=NM,2,-1
        FI(I)=(V(I)-AE(I)*FI(I+1))*BPR(I)
      END DO
      RETURN
      END
C
C#######################################################
      SUBROUTINE JACOBI
C#######################################################
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
      DIMENSION FIO(NX)
C
      DO IT=1,1000
C
C.....SAVE OLD SOLUTION
C
        DO I=1,N
          FIO(I)=FI(I)
        END DO
C
C.....CALCULATE NEW SOLUTION
C
        DO I=2,NM
          FI(I)=(-AE(I)*FIO(I+1)-AW(I)*FIO(I-1)+Q(I))/AP(I)
        END DO
C
C.....CALCULATE RESIDUAL AND CHECK CONVERGENCE
C
        RES=0.
        DO I=2,NM
          RES=RES+ABS(-AE(I)*FI(I+1)-AW(I)*FI(I-1)+Q(I)-AP(I)*FI(I))
        END DO
        WRITE(8,*) IT,' ITER., RES = ',RES
        IF(RES.LT.1.E-4) RETURN
C
      END DO
      RETURN
      END
C
C##########################################################
      SUBROUTINE GS
C##########################################################
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
C
      DO IT=1,1000
C
C.....CALCULATE NEW SOLUTION
C
        DO I=2,NM
          FI(I)=(-AE(I)*FI(I+1)-AW(I)*FI(I-1)+Q(I))/AP(I)
        END DO
C
C.....CALCULATE RESIDUAL AND CHECK CONVERGENCE
C
        RES=0.
        DO I=2,NM
          RES=RES+ABS(-AE(I)*FI(I+1)-AW(I)*FI(I-1)+Q(I)-AP(I)*FI(I))
        END DO
        WRITE(8,*) IT,' ITER., RES = ',RES
        IF(RES.LT.1.E-4) RETURN
C
      END DO
      RETURN
      END
C
C########################################################
      SUBROUTINE GSOR
C########################################################
      PARAMETER (NX=81)
      COMMON N,NM,FI(NX),AE(NX),AW(NX),AP(NX),Q(NX),X(NX)
C
      PRINT *,' ENTER OVERRELAXATION PARAMETER: OM  '
      READ(*,*) OM
C
      DO IT=1,1000
C
C.....CALCULATE NEW SOLUTION
C
        DO I=2,NM
          FI(I)=FI(I)+
     *          OM*((-AE(I)*FI(I+1)-AW(I)*FI(I-1)+Q(I))/AP(I)-FI(I))
        END DO
C
C.....CALCULATE RESIDUAL AND CHECK CONVERGENCE
C
        RES=0.
        DO I=2,NM
          RES=RES+ABS(-AE(I)*FI(I+1)-AW(I)*FI(I-1)+Q(I)-AP(I)*FI(I))
        END DO
        WRITE(8,*) IT,' ITER., RES = ',RES
        IF(RES.LT.1.E-4) RETURN
C
      END DO
      RETURN
      END
