C=============================================================
      PROGRAM LAPLACE
C=============================================================
C     THIS PROGRAM SETS UP THE MATRIX EQUATION [A]{F}={Q} WHICH
C     RESULTS FROM FINITE DIFFERENCE DISCRETIZATION OF THE
C     LAPLACE EQUATION USING UNIFORM GRID IN EACH DIRECTION AND
C     CENTRAL DIFFERENCE APPROXIMATION OF THE SECOND DERIVATIVES.
C     BOUNDARY CONDITIONS ARE OF DIRICHLET TYPE: T(0,X,Z) = 
C     T(X,0,Z) = T(X,Y,0)=0., T(1,Y,Z)=Y*Z, T(X,1,Z)=X*Z, 
C     T(X,Y,1)=X*Y. THE EXACT SOLUTION IS: T(X,Y,Z) = X*Y*Z.
C     THE EQUATION CAN THEN BE SOLVED BY DIFFERENT ITERATIVE
C     SOLVERS. CENTRAL DIFFERENCES LEAD TO EXACT SOLUTION ON ANY
C     GRID, SO THE ITERATION CONVERGENCEE ERROR CAN BE EASILY
C     DETERMINED.
C                         M. Peric,  IfS,  Hamburg,   1995
C==============================================================
      PARAMETER (NX=41,NY=41,NZ=41,NXYZ=NX*NY*NZ)
      COMMON /INDI/ NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,LI(NX),LK(NZ),
     *       LTEST
      COMMON /INDR/ RESMAX,ALFA
      COMMON /COEF/ AE(NXYZ),AW(NXYZ),AN(NXYZ),AS(NXYZ),
     *       AT(NXYZ),AB(NXYZ),AP(NXYZ),Q(NXYZ)
      DIMENSION X(NX),Y(NY),Z(NZ),T(NXYZ)
C
C.....INPUT DATA
C
      OPEN (UNIT=6,FILE='RESULT')
      REWIND 6
      PRINT *, ' ENTER: NI, NJ, NK, MAXIT, RESMAX :  '
      READ(*,*) NI,NJ,NK,MAXIT,RESMAX
      PRINT *,' ENTER DOMAIN SIZE: XL, YL, ZL:  '
      READ(*,*) XL,YL,ZL
      PRINT *,' ENTER: LTEST (1 - PRINT CONV. HISTORY; 0 - DONT):  '
      READ(*,*) LTEST
C
C    MAXIT is the limit on iterations in the solver (here we have
C    a linear problem - fixed coefficient matrix - and are doing 
C    only inner iterations); RESMAX is the level of residual norm 
C    at which iterations are stopped. NI, NJ and NK are the numbers
C    of grid points in X, Y and Z direction, respectively. Note: here
C    FD method is used, so nodes are uniformly distributed between
C    boundaries, i.e. the spacing in X-direction is XL/ (NI-1), etc.
C     
      NIM=NI-1
      NJM=NJ-1
      NKM=NK-1
      NIJ=NI*NJ
C
C.....WORKING ARRAYS FOR CONVERSION OF INDICES
C
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
      DO K=1,NK
        LK(K)=(K-1)*NIJ
      END DO
C
C.....DEFINE THE GRID (HERE UNIFORM)
C
      DX=XL/REAL(NIM)
      DY=YL/REAL(NJM)
      DZ=ZL/REAL(NKM)
      X(1)=0.
      Y(1)=0.
      Z(1)=0.
      DO I=2,NI
        X(I)=X(I-1)+DX
      END DO  
      DO J=2,NJ
        Y(J)=Y(J-1)+DY
      END DO
      DO K=2,NK
        Z(K)=Z(K-1)+DZ
      END DO
C
C  The grid is assumed here to be uniformly spaced in each direction
C  and these are then the coefficients needed to set up the matrix:
C
      DXR2=-1./DX**2
      DYR2=-1./DY**2
      DZR2=-1./DZ**2
      APC=-2.*(DXR2+DYR2+DZR2)
C
C.....INITIALIZE FIELD VALUES 
C
      DO IJK=1,NXYZ
        T(IJK)=0.
      END DO
C
C.....SET UP BOUNDARY VALUES WHERE NON-ZERO
C
      DO K=1,NK
        DO I=1,NI
          IJK=LK(K)+LI(I)+NJ
          T(IJK)=X(I)*Y(NJ)*Z(K)
        END DO
      END DO
C
      DO J=1,NJ
        DO I=1,NI
          IJK=LK(NK)+LI(I)+J
          T(IJK)=X(I)*Y(J)*Z(NK)
        END DO
      END DO
C
      DO K=1,NK
        DO J=1,NJ
          IJK=LK(K)+LI(NI)+J
          T(IJK)=X(NI)*Y(J)*Z(K)
        END DO
      END DO
C
C.....CALCULATE ELEMENTS OF MATRIX [A]
C
      DO K=2,NKM
        DO J=2,NJM
          DO I=2,NIM
            IJK=LK(K)+LI(I)+J
            AE(IJK)=DXR2
            AW(IJK)=DXR2
            AN(IJK)=DYR2
            AS(IJK)=DYR2
            AT(IJK)=DZR2
            AB(IJK)=DZR2
            AP(IJK)=APC
            Q(IJK)=0.
          END DO
        END DO
      END DO
C
C.....IMPLEMENT BOUNDARY CONDITIONS (DIRICHLET)
C
C   Product of the coefficient and a fixed boundary value is added to
C   the source term, and the coefficient then set to zero (not
C   necessarily required by the solver, but it is cleaner this way).
C
C.....SOUTH AND NORTH BOUNDARY
C
      DO K=2,NKM
        DO I=2,NIM
          IJK=LK(K)+LI(I)+2
          Q(IJK)=Q(IJK)-AS(IJK)*T(IJK-1)
          AS(IJK)=0.
C
          IJK=LK(K)+LI(I)+NJM
          Q(IJK)=Q(IJK)-AN(IJK)*T(IJK+1)
          AN(IJK)=0.
        END DO
      END DO
C
C.....WEST AND EAST BOUNDARY
C
      DO K=2,NKM
        DO J=2,NJM
          IJK=LK(K)+LI(2)+J
          Q(IJK)=Q(IJK)-AW(IJK)*T(IJK-NJ)
          AW(IJK)=0.
C
          IJK=LK(K)+LI(NIM)+J
          Q(IJK)=Q(IJK)-AE(IJK)*T(IJK+NJ)
          AE(IJK)=0.
        END DO
      END DO
C
C.....BOTTOM AND TOP BOUNDARY
C
      DO I=2,NIM
        DO J=2,NJM
          IJK=LK(2)+LI(I)+J
          Q(IJK)=Q(IJK)-AB(IJK)*T(IJK-NIJ)
          AB(IJK)=0.
C
          IJK=LK(NKM)+LI(I)+J
          Q(IJK)=Q(IJK)-AT(IJK)*T(IJK+NIJ)
          AT(IJK)=0.
        END DO
      END DO
C
C.....CHOOSE SOLVER AND SOLVE EQUATION SYSTEM
C
      PRINT *,'  CHOOSE SOLVER: 1 - SIP3D, 2 - ICCG, 3 - CGSTAB:  '
      READ(*,*) ISOL
C
C  ALFA is an iteration parameter in the Stone's solver, which is
C  usually around 0.92 (0 to 1, but above 0.95 iterations may
C  diverge, and 0 means standard ILU (incomplete lower-upper)
C  decomposition.
C
      IF(ISOL.EQ.1) THEN
        PRINT *, ' ENTER: ALFA :  '
        READ(*,*) ALFA
        CALL SIP3D(T,MAXIT)
      ENDIF
C
      IF(ISOL.EQ.2) CALL ICCG(T,MAXIT)
      IF(ISOL.EQ.3) CALL CGSTAB(T,MAXIT)
C
C.....PRINT ERROR NORM AND SOLUTION FIELD
C
C  In this case we know the exact solution and can calculate the error
C  norm (and also check if the solver does the job properly).
C
      ERR=0.
      DO K=2,NKM
        DO J=2,NJM
          DO I=2,NIM
            IJK=LK(K)+LI(I)+J
            ERR=ERR+(T(IJK)-X(I)*Y(J)*Z(K))**2
          END DO
        END DO
      END DO
      ERR=SQRT(ERR/REAL(NI*NJ*NK))
C
C.....WRITE THE RESULT OUT
C
      WRITE(6,*) '    PROBLEM SIZE: NX = ',NX,', NY = ',NY,', NZ = ',NZ
      WRITE(6,*) '    DOMAIN DIMENSIONS: XL = ',XL,', YL = ',YL,
     *                 ', ZL = ',ZL
      WRITE(6,*) '    SOLVER USED: ',ISOL,' (1 -> SIP3D, 2 -> ICCG,',
     *                ' 3 -> CGSTAB) '
      WRITE(6,*) '    ERROR NORM:  ERR = ',ERR
      WRITE(6,*) '  '
      WRITE(6,*) '  '
C
C  Routine PRINT prints a 3D array in an easy to read form.
C
      CALL PRINT(T)
C
      STOP
      END
C
C###############################################################
      SUBROUTINE PRINT(FI)
C############################################################### 
      PARAMETER (NX=41,NY=41,NZ=41,NXYZ=NX*NY*NZ)
      COMMON /INDI/ NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,LI(NX),LK(NZ),
     *       LTEST
      DIMENSION FI(NXYZ)
C
      WRITE(6,20) 
      DO K=1,NK
        KK=LK(K)
        WRITE(6,*) '   PLANE  K  =',K
        WRITE(6,*) '   =========================='
        WRITE(6,*) '  '
        NLOOP=NIM/12+1
        DO N=1,NLOOP
          IS=1
          IE=MIN(12,NI)
          WRITE(6,21) (I,I=IS,IE)
          WRITE(6,22)
          DO J=NJ,1,-1
            WRITE(6,23) J,(FI(L),L=KK+LI(IS)+J,KK+LI(IE)+J,NJ)
          END DO
          IS=IE+1
          IE=MIN(NI,IS+11)
        END DO
        WRITE(6,*) '  '
      END DO
C   
   20 FORMAT(2X,26('*-'),7X,'  FI  ',7X,26('-*'))
   21 FORMAT(3X,'I = ',I3,11I10)
   22 FORMAT(2X,'J')
   23 FORMAT(1X,I3,1P12E10.2)
      RETURN
      END
C
C#######################################################################
      SUBROUTINE ICCG(FI,NS)
C#######################################################################
C    This routine incorporates the Incomplete Cholesky preconditioned 
C    Conjugate Gradient solver for symmetric matrices in 3D problems
C    with seven-diagonal matrix structure (see Sect. 5.3.6). Array
C    index IJK converted from 3D indices I, J, and K according to
C    Table 3.1. NS is the number of inner iterations (sweeps).
C
C    Writen by Ismet Demirdzic, Sarajevo, 1991.
C======================================================================
      PARAMETER (NX=41,NY=41,NZ=41,NXYZ=NX*NY*NZ)
      COMMON /INDI/ NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,LI(NX),LK(NZ),
     *       LTEST
      COMMON /INDR/ RESMAX,ALFA
      COMMON /COEF/ AE(NXYZ),AW(NXYZ),AN(NXYZ),AS(NXYZ),
     *       AT(NXYZ),AB(NXYZ),AP(NXYZ),Q(NXYZ)
      DIMENSION FI(NXYZ),ZK(NXYZ),RES(NXYZ),D(NXYZ),PK(NXYZ)
C
C.....INITALIZE WORKING ARRAYS
C
      DO IJK=1,NIJK
        PK(IJK)=0.
        ZK(IJK)=0.
        D(IJK)=0.
        RES(IJK)=0.
      END DO
C
C.....CALCULATE INITIAL RESIDUAL VECTOR AND THE NORM
C
      RES0=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            RES(IJK)=Q(IJK)-AE(IJK)*FI(IJK+NJ)-AW(IJK)*FI(IJK-NJ)-
     *        AN(IJK)*FI(IJK+1)-AS(IJK)*FI(IJK-1)-AT(IJK)*FI(IJK+NIJ)-
     *        AB(IJK)*FI(IJK-NIJ)-AP(IJK)*FI(IJK)
            RES0=RES0+ABS(RES(IJK))
          END DO
        END DO
      END DO
C
C.....IF LTEST=1, PRINT THE NORM 
C
      IF(LTEST.EQ.1) WRITE(6,*) 0,' SWEEP, RES0 = ',RES0
C
C.....CALCULATE ELEMENTS OF DIAGONAL PRECONDITIONING MATRIX
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            D(IJK)=1./(AP(IJK)-AW(IJK)**2*D(IJK-NJ)-AS(IJK)**2*D(IJK-1)
     *             -AB(IJK)**2*D(IJK-NIJ))
          END DO
        END DO
      END DO
C
      S0=1.E20
C
C....START INNER ITERATIONS
C
      DO L=1,NS
C
C.....SOLVE FOR ZK(IJK) -- FORWARD SUBSTITUTION
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=(RES(IJK)-AW(IJK)*ZK(IJK-NJ)-AS(IJK)*ZK(IJK-1)-
     *              AB(IJK)*ZK(IJK-NIJ))*D(IJK)
          END DO
        END DO
      END DO
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=ZK(IJK)/(D(IJK)+1.E-30)
          END DO
        END DO
      END DO
C
C..... BACKWARD SUBSTITUTION; CALCULATE SCALAR PRODUCT SK
C
      SK=0.
      DO K=NKM,2,-1
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=(ZK(IJK)-AE(IJK)*ZK(IJK+NJ)-AN(IJK)*ZK(IJK+1)-
     *               AT(IJK)*ZK(IJK+NIJ))*D(IJK)
            SK=SK+RES(IJK)*ZK(IJK)
          END DO
        END DO
      END DO
C
C.....CALCULATE BETA
C
      BET=SK/S0
C
C.....CALCULATE NEW SEARCH VECTOR PK
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            PK(IJK)=ZK(IJK)+BET*PK(IJK)
          END DO
        END DO
      END DO
C
C.... CALCULATE SCALAR PRODUCT (PK.A PK) AND ALPHA (OVERWRITE ZK)
C
      PKAPK=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=AP(IJK)*PK(IJK)+AE(IJK)*PK(IJK+NJ)+
     *        AW(IJK)*PK(IJK-NJ)+AN(IJK)*PK(IJK+1)+AS(IJK)*PK(IJK-1)+
     *        AT(IJK)*PK(IJK+NIJ)+AB(IJK)*PK(IJK-NIJ)
            PKAPK=PKAPK+PK(IJK)*ZK(IJK)
          END DO
        END DO
      END DO
C
      ALF=SK/PKAPK
C
C.....CALCULATE VARIABLE CORRECTION, NEW RESIDUAL VECTOR, AND NORM
C
      RESL=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            FI(IJK)=FI(IJK)+ALF*PK(IJK)
            RES(IJK)=RES(IJK)-ALF*ZK(IJK)
            RESL=RESL+ABS(RES(IJK))
          END DO
        END DO
      END DO
C
      S0=SK
C
C.....CHECK CONVERGENCE
C
      RSM=RESL/(RES0+1.E-30)
      IF(LTEST.EQ.1) WRITE(6,*) L,' SWEEP, RESL = ',RESL,' RSM = ',RSM
      IF(RSM.LT.RESMAX) RETURN
C
C.....END OF ITERATION LOOP
C
      END DO
C
      RETURN
      END
C
C#######################################################################
      SUBROUTINE CGSTAB(FI,NS)
C#######################################################################
C    This routine incorporates the CGSTAB solver for seven-diagonal,
C    non-symmetric coefficient matrices (suitable for convection/
C    diffusion problems). See Sect. 5.3.7 for details. Array index
C    IJK calculated from indices I, J, and K according to Table 3.1.
C
C    Writen by Samir Muzaferija, Institut fuer Schiffbau, Hamburg, 1995.
C=======================================================================
      PARAMETER (NX=41,NY=41,NZ=41,NXYZ=NX*NY*NZ)
      COMMON /INDI/ NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,LI(NX),LK(NZ),
     *       LTEST
      COMMON /INDR/ RESMAX,ALFA
      COMMON /COEF/ AE(NXYZ),AW(NXYZ),AN(NXYZ),AS(NXYZ),
     *       AT(NXYZ),AB(NXYZ),AP(NXYZ),Q(NXYZ)
      DIMENSION FI(NXYZ),RES(NXYZ),RESO(NXYZ),PK(NXYZ),UK(NXYZ),
     *          ZK(NXYZ),VK(NXYZ),D(NXYZ)
C
C.....CALCULATE INITIAL RESIDUAL VECTOR
C
      RES0=0.
      DO K=2,NKM
        DO I=2,NIM 
          DO J=2,NJM 
            IJK=LK(K)+LI(I)+J
            RES(IJK)=Q(IJK) - AP(IJK)*FI(IJK)-
     *           AE(IJK)*FI(IJK+NJ) - AW(IJK)*FI(IJK-NJ) -
     *           AN(IJK)*FI(IJK+1)  - AS(IJK)*FI(IJK-1)  -
     *           AT(IJK)*FI(IJK+NIJ)- AB(IJK)*FI(IJK-NIJ)
            RES0=RES0+ABS(RES(IJK))
          END DO
        END DO
      END DO
C
      IF(LTEST.EQ.1) WRITE(6,*) 0,' SWEEP, RES0 = ',RES0
C
C.....CALCULATE ELEMENTS OF PRECONDITIONING MATRIX DIAGONAL
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            D(IJK)=1./(AP(IJK) - AW(IJK)*D(IJK-NJ)*AE(IJK-NJ) -
     *             AS(IJK)*D(IJK-1)*AN(IJK-1) - 
     *             AB(IJK)*D(IJK-NIJ)*AT(IJK-NIJ)) 
          END DO
        END DO
      END DO
C
C.....INITIALIZE WORKING ARRAYS AND CONSTANTS
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            RESO(IJK)=RES(IJK)
            PK(IJK)=0.
            UK(IJK)=0.
            ZK(IJK)=0.
            VK(IJK)=0.
          END DO
        END DO
      END DO
      ALF=1.
      BETO=1.
      GAM=1.
C
C.....START INNER ITERATIONS
C
      DO L=1,NS
C
C..... CALCULATE BETA AND OMEGA
C
      BET=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            BET=BET+RES(IJK)*RESO(IJK)
          END DO
        END DO
      END DO
      OM=BET*GAM/(ALF*BETO+1.E-30)
      BETO=BET
C
C..... CALCULATE PK
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            PK(IJK)=RES(IJK)+OM*(PK(IJK)-ALF*UK(IJK))
          END DO
        END DO
      END DO
C
C.....SOLVE (M ZK = PK) - FORWARD SUBSTITUTION
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=(PK(IJK)-AW(IJK)*ZK(IJK-NJ) -
     *              AS(IJK)*ZK(IJK-1)-AB(IJK)*ZK(IJK-NIJ))*D(IJK)
          END DO
        END DO
      END DO
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=ZK(IJK)/(D(IJK)+1.E-30)
          END DO
        END DO
      END DO
C
C..... BACKWARD SUBSTITUTION
C
      DO K=NKM,2,-1
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=(ZK(IJK)-AE(IJK)*ZK(IJK+NJ)-
     *              AN(IJK)*ZK(IJK+1)-AT(IJK)*ZK(IJK+NIJ))*D(IJK)
          END DO
        END DO
      END DO
C
C.....CALCULATE UK = A.PK
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            UK(IJK)=AP(IJK)*ZK(IJK)+AE(IJK)*ZK(IJK+NJ) +
     *               AW(IJK)*ZK(IJK-NJ)+AN(IJK)*ZK(IJK+1)+
     *               AS(IJK)*ZK(IJK-1)+AT(IJK)*ZK(IJK+NIJ)+ 
     *               AB(IJK)*ZK(IJK-NIJ)
          END DO
        END DO
      END DO
C
C..... CALCULATE SCALAR PRODUCT UK.RESO AND GAMMA
C
      UKRESO=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            UKRESO=UKRESO+UK(IJK)*RESO(IJK)
          END DO
        END DO
      END DO
      GAM=BET/UKRESO
C
C.....UPDATE (FI) AND CALCULATE W (OVERWRITE RES - IT IS RES-UPDATE)
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            FI(IJK)=FI(IJK)+GAM*ZK(IJK)
            RES(IJK)=RES(IJK)-GAM*UK(IJK)
          END DO
        END DO
      END DO
C
C.....SOLVE (M Y = W); Y OVERWRITES ZK; FORWARD SUBSTITUTION
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=(RES(IJK)-AW(IJK)*ZK(IJK-NJ)-
     *              AS(IJK)*ZK(IJK-1)-AB(IJK)*ZK(IJK-NIJ))*D(IJK)
           END DO
         END DO
      END DO
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=ZK(IJK)/(D(IJK)+1.E-30)
          END DO
        END DO
      END DO
C
C.....BACKWARD SUBSTITUTION
C
      DO K=NKM,2,-1
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            IJK=LK(K)+LI(I)+J
            ZK(IJK)=(ZK(IJK)-AE(IJK)*ZK(IJK+NJ)-
     *              AN(IJK)*ZK(IJK+1)-AT(IJK)*ZK(IJK+NIJ))*D(IJK)
          END DO
        END DO
      END DO
C
C.....CALCULATE V = A.Y (VK = A.ZK)
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            VK(IJK)=AP(IJK)*ZK(IJK)   +AE(IJK)*ZK(IJK+NJ)+
     *              AW(IJK)*ZK(IJK-NJ)+AN(IJK)*ZK(IJK+1)+
     *              AS(IJK)*ZK(IJK-1) +AT(IJK)*ZK(IJK+NIJ)+ 
     *              AB(IJK)*ZK(IJK-NIJ)
          END DO
        END DO
      END DO
C
C..... CALCULATE ALPHA (ALF)
C
      VRES=0.
      VV=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            VRES=VRES+VK(IJK)*RES(IJK)
            VV=VV+VK(IJK)*VK(IJK)
          END DO
        END DO
      END DO
C
      ALF=VRES/(VV+1.E-30)
C
C.....UPDATE VARIABLE (FI) AND RESIDUAL (RES) VECTORS
C
      RESL=0.
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            FI(IJK)=FI(IJK)+ALF*ZK(IJK)
            RES(IJK)=RES(IJK)-ALF*VK(IJK)
            RESL=RESL+ABS(RES(IJK))
          END DO
        END DO
      END DO
C
C.....CHECK CONVERGENCE
C
      RSM=RESL/(RES0+1.E-30)
      IF(LTEST.EQ.1) WRITE(6,*) L,' SWEEP, RESL = ',RESL,' RSM = ',RSM
      IF(RSM.LT.RESMAX) RETURN
C
C.....END OF ITERATION LOOP
C
      END DO
C
      RETURN
      END
C
C##############################################################
      SUBROUTINE SIP3D(FI,NS)
C##############################################################
C
C     SIP solver for 3D problems (see Sect. 5.3.4 and paper by
C     Leister and Peric (1994)
C
C                     M. Peric, IfS, Hamburg, 1995
C##############################################################
      PARAMETER (NX=41,NY=41,NZ=41,NXYZ=NX*NY*NZ)
      COMMON /INDI/ NI,NJ,NK,NIM,NJM,NKM,NIJ,NIJK,LI(NX),LK(NZ),
     *       LTEST
      COMMON /INDR/ RESMAX,ALFA
      COMMON /COEF/ AE(NXYZ),AW(NXYZ),AN(NXYZ),AS(NXYZ),
     *       AT(NXYZ),AB(NXYZ),AP(NXYZ),Q(NXYZ)
      DIMENSION UE(NXYZ),UN(NXYZ),UT(NXYZ),RES(NXYZ),
     *          FI(NXYZ),LB(NXYZ),LS(NXYZ),LW(NXYZ),LPR(NXYZ)
      REAL LB,LS,LW,LPR
C                     
C-----CALCULATE COEFFICIENTS OF  [L]  AND  [U]  MATRICES
C
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            LB(IJK)=AB(IJK)/(1.+ALFA*(UN(IJK-NIJ)+UE(IJK-NIJ)))
            LW(IJK)=AW(IJK)/(1.+ALFA*(UN(IJK-NJ)+UT(IJK-NJ)))
            LS(IJK)=AS(IJK)/(1.+ALFA*(UE(IJK-1)+UT(IJK-1)))
            P1=ALFA*(LB(IJK)*UN(IJK-NIJ)+LW(IJK)*UN(IJK-NJ))
            P2=ALFA*(LB(IJK)*UE(IJK-NIJ)+LS(IJK)*UE(IJK-1))
            P3=ALFA*(LW(IJK)*UT(IJK-NJ)+ LS(IJK)*UT(IJK-1))
            LPR(IJK)=1./(AP(IJK)+P1+P2+P3-LB(IJK)*UT(IJK-NIJ)-
     *               LW(IJK)*UE(IJK-NJ)-LS(IJK)*UN(IJK-1)+1.E-20)
            UN(IJK)=(AN(IJK)-P1)*LPR(IJK)
            UE(IJK)=(AE(IJK)-P2)*LPR(IJK)
            UT(IJK)=(AT(IJK)-P3)*LPR(IJK)
          END DO
        END DO
      END DO
C
C-----CALCULATE RESIDUALS AND AUXILLIARY VECTOR; INNER ITERATIONS
C
      DO L=1,NS
C
      RESL=0.0
      DO K=2,NKM
        DO I=2,NIM
          DO J=2,NJM
            IJK=LK(K)+LI(I)+J
            RES(IJK)=Q(IJK)-AE(IJK)*FI(IJK+NJ)-AW(IJK)*FI(IJK-NJ)-
     *               AN(IJK)*FI(IJK+1)-AS(IJK)*FI(IJK-1)-
     *               AT(IJK)*FI(IJK+NIJ)-AB(IJK)*FI(IJK-NIJ)-
     *               AP(IJK)*FI(IJK)
            RESL=RESL+ABS(RES(IJK))
            RES(IJK)=(RES(IJK)-LB(IJK)*RES(IJK-NIJ)-
     *               LW(IJK)*RES(IJK-NJ)-LS(IJK)*RES(IJK-1))*LPR(IJK)
          END DO
        END DO
      END DO
C
      IF(L.EQ.1) RES0=RESL
      RSM=RESL/RES0
C
C-----CALCULATE CORRECTION AND UPDATE VARIABLES (BACKWARD SUBST.)
C
      DO K=NKM,2,-1
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            IJK=LK(K)+LI(I)+J
            RES(IJK)=RES(IJK)-UN(IJK)*RES(IJK+1)-UE(IJK)*RES(IJK+NJ)-
     *               UT(IJK)*RES(IJK+NIJ)
            FI(IJK)=FI(IJK)+RES(IJK)
          END DO
        END DO
      END DO
C
C.....CHECK CONVERGENCE
C
      IF(LTEST.EQ.1) PRINT *,' ',L,' SWEEP, RESL = ',RESL,' RSM = ',RSM
      IF(RSM.LT.RESMAX) RETURN
C
C.....END ITERATION LOOP
C
      END DO
C
      RETURN
      END
