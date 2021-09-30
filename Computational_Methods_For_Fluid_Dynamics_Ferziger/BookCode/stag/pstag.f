C############################################################
      PROGRAM COMET
C############################################################
C     This program incorporates the FV method for solving the
C     Navier-Stokes equations using 2D, Cartesian grids and the
C     staggered arrangement of variables. Variables are stored
C     as 2D arrays. SIMPLE method is used for pressure
C     calculation. UDS and CDS are implemented for the 
C     discretization of convective terms, CDS is used for the
C     diffusive terms. The boundary conditions are set for the
C     lid-driven cavity flow. Only steady flows are considered.
C
C                    M. Peric, IfS, Hamburg, 1996
C#############################################################
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /COEF/ AE(NX,NY),AW(NX,NY),AN(NX,NY),AS(NX,NY),
     *       AP(NX,NY),SU(NX,NY),DU(NX,NY),DV(NX,NY),PP(NX,NY)
      COMMON /VAR/ U(NX,NY),V(NX,NY),P(NX,NY),CX(NX,NY),CY(NX,NY) 
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
      CHARACTER*10 FILIN,FILOUT,FILRES,FILGR,FILPL
C 
C.....READ FILE NAMES, OPEN FILES 
C 
      PRINT *,' ENTER INPUT FILE NAME:  '
      READ(*,1) FILIN
      PRINT *,' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILOUT
      PRINT *,' ENTER GRID FILE NAME:  '
      READ(*,1) FILGR
      PRINT *,' ENTER RESULT FILE NAME:  '
      READ(*,1) FILRES
      PRINT *,' ENTER PLOT FILE NAME:  '
      READ(*,1) FILPL
    1 FORMAT(A10)
      OPEN(UNIT=5,FILE=FILIN)
      OPEN(UNIT=1,FILE=FILGR)
      OPEN(UNIT=6,FILE=FILOUT)
      OPEN(UNIT=2,FILE=FILPL,FORM='UNFORMATTED')
      OPEN(UNIT=3,FILE=FILRES,FORM='UNFORMATTED')
      REWIND 1
      REWIND 3
      REWIND 5
      REWIND 6
C
C.....INITIALIZATION, GRID DEFINITION, BOUNDARY CONDITIONS
C
      CALL INIT 
C
      ITER=0
C
C.....READ RESULTS OF PREVIOUS RUN IF CONTINUATION
C
      IF(LREAD) READ(3) ITER,TIME,NI,NJ,NIM,NJM,NIJ,(X(I),I=1,NI),
     *          (Y(J),J=1,NJ),((CX(I,J),J=1,NJ),I=1,NI),
     *          ((CY(I,J),J=1,NJ),I=1,NI),((U(I,J),J=1,NJ),I=1,NI),
     *          ((V(I,J),J=1,NJ),I=1,NI),((P(I,J),J=1,NJ),I=1,NI)  
      REWIND 3
C
C.....INITIAL PRINTOUT
C
      CALL OUT1 
      IF(LTEST) THEN 
        IF(LCAL(IU)) CALL PRINT(U,'U VEL.') 
        IF(LCAL(IV)) CALL PRINT(V,'V VEL.') 
        IF(LCAL(IPP)) CALL PRINT(P,'PRESS.')
      ENDIF
      WRITE(6,66) IMON,JMON 
C 
C.....OUTER ITERATIONS 
C 
      DO ITER=1,MAXIT
        IF(LCAL(IU)) CALL CALCU 
        IF(LCAL(IV)) CALL CALCV 
        IF(LCAL(IPP)) CALL CALCP
        WRITE(6,76) ITER,RESOR(IU),RESOR(IV),RESOR(IPP),
     *              U(IMON,JMON),V(IMON,JMON),P(IMON,JMON) 
        SOURCE=MAX(RESOR(IU),RESOR(IV),RESOR(IPP))
        IF(SOURCE.GT.SLARGE) GO TO 110
        IF(SOURCE.LT.SORMAX) GO TO 90
      END DO
C
C.....OUTER ITERATIONS CONVERGED -> SAVE AND PRINT
C
   90 CONTINUE
      NIJ=NI*NJ
      IF(LCAL(IU)) CALL PRINT(U,'U VEL.') 
      IF(LCAL(IV)) CALL PRINT(V,'V VEL.') 
      IF(LCAL(IPP)) CALL PRINT(P,'PRESS.')
C
      IF(LWRITE) WRITE(3) ITER,TIME,NI,NJ,NIM,NJM,NIJ,(X(I),I=1,NI),
     *          (Y(J),J=1,NJ),((CX(I,J),J=1,NJ),I=1,NI),
     *          ((CY(I,J),J=1,NJ),I=1,NI),((U(I,J),J=1,NJ),I=1,NI),
     *          ((V(I,J),J=1,NJ),I=1,NI),((P(I,J),J=1,NJ),I=1,NI)
      REWIND 3
C
C==============================================================
C.....PREPARE AND SAVE DATA FOR PLOT PROGRAM (U -> AE, V -> AW)
C==============================================================
C     Interpolate velocities for scalar CV centers, as needed in
C     the plot program
C
      itim=1.
      time=0.
c
      AE(1,1)=U(1,1)
      AW(1,1)=V(1,1)
      AE(NI,1)=U(NIM,1)
      AW(NI,1)=V(NI,1)
C
      DO J=2,NJM
        AE(1,J)=U(1,J)
        AW(1,J)=0.5*(V(1,J)+V(1,J-1))
        AE(NI,J)=U(NIM,J)
        AW(NI,J)=0.5*(V(NI,J)+V(NI,J-1))
      END DO
C
      AE(1,NJ)=U(1,NJ)
      AW(1,NJ)=V(1,NJM)
      AE(NI,NJ)=U(NIM,NJ)
      AW(NI,NJ)=V(NI,NJM)
C
      DO I=2,NIM
        AE(I,1)=0.5*(U(I,1)+U(I-1,1))
        AW(I,J)=V(I,J)
        AE(I,NJ)=0.5*(U(I,NJ)+U(I-1,NJ))
        AW(I,NJ)=V(I,NJM)
      END DO
C
      DO I=2,NIM
        DO J=2,NJM
          AE(I,J)=0.5*(U(I,J)+U(I-1,J))
          AW(I,J)=0.5*(V(I,J)+V(I,J-1))
        END DO
      END DO
C
      WRITE(2) ITIM,TIME,NI,NJ,NIM,NJM,NIJ,
     *        ((X(I),J=1,NJ),I=1,NI),((Y(J),J=1,NJ),I=1,NI),
     *        ((XC(I),J=1,NJ),I=1,NI),((YC(J),J=1,NJ),I=1,NI),
     *        ((CX(I,J),J=1,NJ),I=1,NI),((CY(I,J),J=1,NJ),I=1,NI),
     *        ((AE(I,J),J=1,NJ),I=1,NI),((AW(I,J),J=1,NJ),I=1,NI),
     *        ((P(I,J),J=1,NJ),I=1,NI),((P(I,J),J=1,NJ),I=1,NI)
      REWIND 4
C
      STOP
C
C==============================================================
C.....OUTER ITERATIONS DIVERGING
C==============================================================
C
  110 PRINT *,'     *** TERMINATED - OUTER ITERATIONS DIVERGING ***'
      STOP
C
C.....FORMAT SPECIFICATIONS
C
   60 FORMAT(20X,'TIME STEP NO.',I3,' ,  TIME =',E10.4,/,20X,36('*'),/) 
   66 FORMAT(2X,'ITER   I-----------ABSOLUTE RESIDUAL SOURCE SUMS----', 
     *      '-------I    I----FIELD VALUES AT MONITORING LOCATION (', 
     *      I3,',',I3,')---I',/,2X,'NO. ',6X,'UMOM',6X,'VMOM',6X, 
     *      'MASS',30X,'U',9X,'V',9X,'P',9X,'T',/)
   76 FORMAT(2X,I4,3X,1P3E10.3,24X,1P3E10.3)
      END 
C
C
C##############################################################
      SUBROUTINE CALCU
C##############################################################
C     This routine assembles and solves U-equation. Along west
C     and east boundary, there are half-CVs which are not
C     included in the integration region. CX and CY are the mass 
C     fluxes across east and north faces of mass CVs (positive
C     for flow out of CV). The geometrical and other coefficients
C     are first initialized along west and south boundary; then,
C     in a loop over CVs, east and north cell faces are treated.
C==============================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /COEF/ AE(NX,NY),AW(NX,NY),AN(NX,NY),AS(NX,NY),
     *       AP(NX,NY),SU(NX,NY),DU(NX,NY),DV(NX,NY),PP(NX,NY)
      COMMON /VAR/ U(NX,NY),V(NX,NY),P(NX,NY),CX(NX,NY),CY(NX,NY) 
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      DIMENSION DW(NY),CW(NY)
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
C============================================================== 
C
      URFUR=1./URF(IU)
C 
C.....INITIALIZE QUANTITIES ALONG WEST BOUNDARY
C 
      DXER=1./(X(2)-X(1))
      DO J=2,NJM 
        SW=0.5*(R(J)+R(J-1))*(Y(J)-Y(J-1)) 
        DW(J)=VIS*SW*DXER 
        CW(J)=0.5*(CX(2,J)+CX(1,J)) 
      END DO
C 
C.....INITIALIZE QUANTITIES ALONG SOUTH BOUNDARY 
C 
      DO I=2,NIMM
        DX=XC(I+1)-XC(I)
        DYNR=1./(YC(2)-YC(1))
        SS=DX*R(1)
        DXER=1./(X(I+1)-X(I)) 
        DS=VIS*SS*DYNR
        CS=0.5*(CY(I,1)+CY(I+1,1)) 
C
C=========================================================== 
C.....MAIN LOOP - EAST AND NORTH SIDE 
C=========================================================== 
C
        DO J=2,NJM 
C
C.....CELL FACE AREA, RECIPROCAL DISTANCE FROM P TO N
C
          SE=0.5*(R(J)+R(J-1))*(Y(J)-Y(J-1)) 
          SN=DX*R(J)
          DYNR=1./(YC(J+1)-YC(J))
C
C.....MASS FLUXES ACROSS U-VEL. CV FACES
C
          CN=0.5*(CY(I,J)+CY(I+1,J)) 
          CE=0.5*(CX(I,J)+CX(I+1,J)) 
C
C.....DIFFUSION COEFFICIENTS
C
          DN=VIS*SN*DYNR
          DE=VIS*SE*DXER 
C
C......PRESSURE SOURCE TERM
C
          SUP=SE*(P(I,J)-P(I+1,J))
C 
C.....EXPLICIT SUMM OF CONVECTIVE FLUXES USING UDS AND CDS 
C 
          AE1=-MIN(CE,0.) 
          AW1= MAX(CW(J),0.) 
          AN1=-MIN(CN,0.) 
          AS1= MAX(CS,0.) 
C 
          SUCU=AE1*U(I+1,J)+AW1*U(I-1,J)+AN1*U(I,J+1)+AS1*U(I,J-1)-
     *         (AE1+AW1+AN1+AS1)*U(I,J)
          SUCC=0.5*((U(I,J)+U(I-1,J))*CW(J)-(U(I+1,J)+U(I,J))*CE)-
     *         CN*(U(I,J+1)*FY(J)+U(I,J)*(1.-FY(J)))+
     *         CS*(U(I,J)*FY(J-1)+U(I,J-1)*(1.-FY(J-1)))
C
C.....MATRIX COEFFICIENTS (ONLY UDS)
C
          AE(I,J)=-(AE1+DE)
          AW(I,J)=-(AW1+DW(J)) 
          AN(I,J)=-(AN1+DN) 
          AS(I,J)=-(AS1+DS)
C
C.....SOURCE TERM
C 
          SU(I,J)=SUP+G(IU)*(SUCC-SUCU)
C
C.....SAVE COEFFICIENTS OF EAST AND NORTH SIDE SIDE 
C
          DW(J)=DE
          CW(J)=CE
          CS=CN
          DS=DN
C
        END DO
      END DO
C 
C=============================================================
C.....FINAL ASSEMBLY (NO BOUNDARY MODIFICATIONS NECESSARY)
C============================================================= 
C
      DO I=2,NIMM
      DO J=2,NJM 
        AP(I,J)=-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))*URFUR
        SU(I,J)=SU(I,J)+(1.-URF(IU))*AP(I,J)*U(I,J) 
        DU(I,J)=1./AP(I,J) 
      END DO 
      END DO
C 
C.....SOLUTION
C 
      CALL SIPSOL(U,IU,NIMM,NJM)
C
      RETURN
      END 
C
C
C##############################################################
      SUBROUTINE CALCV
C##############################################################
C     This routine assembles and solves V-equation. Along south
C     and north boundary, there are half-CVs which are not
C     included in the integration region. CX and CY are the mass 
C     fluxes across east and north faces of mass CVs (positive
C     for flow out of CV). The geometrical and other coefficients
C     are first initialized along west and south boundary; then,
C     in a loop over CVs, east and north cell faces are treated.
C==============================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /COEF/ AE(NX,NY),AW(NX,NY),AN(NX,NY),AS(NX,NY),
     *       AP(NX,NY),SU(NX,NY),DU(NX,NY),DV(NX,NY),PP(NX,NY)
      COMMON /VAR/ U(NX,NY),V(NX,NY),P(NX,NY),CX(NX,NY),CY(NX,NY) 
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      DIMENSION DW(NY),CW(NY)
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
C===============================================================
C
      URFVR=1./URF(IV)
C 
C.....INITIALIZE QUANTITIES ALONG WEST BOUNDARY
C 
      DXER=1./(XC(2)-XC(1))
      DO J=2,NJMM
        SW=0.5*(YC(J+1)-YC(J))*(R(J+1)+R(J-1))
        DW(J)=VIS*SW*DXER 
        CW(J)=0.5*(CX(1,J)+CX(1,J+1))
      END DO
C 
C.....INITIALIZE QUANTITIES ALONG SOUTH BOUNDARY 
C 
      DO I=2,NIM 
        DX=X(I)-X(I-1) 
        DXER=1./(XC(I+1)-XC(I))
        DYNR=1./(Y(2)-Y(1))
        SS=0.5*(R(2)+R(1))*DX
        DS=VIS*SS*DYNR
        CS=0.5*(CY(I,2)+CY(I,1))
C 
C==============================================================
C.....MAIN LOOP - EAST AND NORTH SIDES
C============================================================== 
C
       DO J=2,NJMM
C
C.....CELL FACE AREA, RECIPROCAL DISTANCE FROM P TO N
C
         DY=YC(J+1)-YC(J)
         SE=DY*0.5*(R(J+1)+R(J-1))
         SN=0.5*(R(J+1)+R(J))*DX
         DYNR=1./(Y(J+1)-Y(J)) 
C
C.....MASS FLUXES THROUGH EAST AND NORTH FACE OF V-CV
C
         CN=0.5*(CY(I,J+1)+CY(I,J)) 
         CE=0.5*(CX(I,J+1)+CX(I,J)) 
C
C......DIFFUSION COEFFICIENTS
C
         DN=VIS*SN*DYNR 
         DE=VIS*SE*DXER
C
C.....PRESSURE SOURCE TERM
C
         SUP=DX*R(J)*(P(I,J)-P(I,J+1))
C 
C.....EXPLICIT SUM OF CONVECTIVE FLUXES USING UDS AND CDS
C 
          AE1=-MIN(CE,0.) 
          AW1= MAX(CW(J),0.) 
          AN1=-MIN(CN,0.) 
          AS1= MAX(CS,0.) 
C 
          SUCU=AE1*V(I+1,J)+AW1*V(I-1,J)+AN1*V(I,J+1)+AS1*V(I,J-1)-
     *         (AE1+AW1+AN1+AS1)*V(I,J)
          SUCC=0.5*((V(I,J)+V(I,J-1))*CS-(V(I,J+1)+V(I,J))*CN)-
     *         CE*(V(I+1,J)*FX(I)+V(I,J)*(1.-FX(I)))+
     *         CW(J)*(V(I,J)*FX(I-1)+V(I-1,J)*(1.-FX(I-1)))
C
C.....MATRIX COEFFICIENTS (ONLY UDS)
C
           AE(I,J)=-(AE1+DE)
           AW(I,J)=-(AW1+DW(J)) 
           AN(I,J)=-(AN1+DN) 
           AS(I,J)=-(AS1+DS)
           AP(I,J)=0.
C 
C.....SOURCE TERMS
C 
          VOL=DX*SE
          IF(LAKSI) AP(I,J)=AP(I,J)+VIS*VOL/R(J)**2
          SU(I,J)=SUP+G(IV)*(SUCC-SUCU)
C
C......SAVE COEFFICIENTS FROM EAST AND NORTH SIDE
C
          DW(J)=DE
          CW(J)=CE
          DS=DN
          CS=CN
C
        END DO
      END DO
C 
C==============================================================
C.....FINAL ASSEMBLY (NO BOUNDARY MODIFICATIONS NECESSARY)
C============================================================== 
C
      DO J=2,NJMM
      DO I=2,NIM 
        AP(I,J)=AP(I,J)-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J)) 
        AP(I,J)=AP(I,J)*URFVR 
        SU(I,J)=SU(I,J)+(1.-URF(IV))*AP(I,J)*V(I,J) 
        DV(I,J)=1./AP(I,J) 
      END DO
      END DO
C 
C.....SOLUTION 
C 
      CALL SIPSOL(V,IV,NIM,NJMM)
C
      RETURN
      END 
C
C
C##############################################################
      SUBROUTINE CALCP
C##############################################################
C     This routine assembles and solves the pressure-correction
C     equation of the SIMPLE algorithm. Here, mass fluxes across
C     boundary cell faces are assumed known (prescribed), so
C     they need not be corrected; this is equivalent to a zero
C     gradient condition for P', which reduces to zero coefficients
C     in the matrix for the boundary nodes. Therefore, only
C     inner CV faces are treated.
C==============================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /COEF/ AE(NX,NY),AW(NX,NY),AN(NX,NY),AS(NX,NY),
     *       AP(NX,NY),SU(NX,NY),DU(NX,NY),DV(NX,NY),PP(NX,NY)
      COMMON /VAR/ U(NX,NY),V(NX,NY),P(NX,NY),CX(NX,NY),CY(NX,NY) 
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
C
      SUM=0.
C 
C.....MAIN LOOP - EAST AND NORTH SIDES
C 
      DO I=2,NIM 
        DX=X(I)-X(I-1) 
C
C.....CELL FACE AREA AND UNCORRECTED MASS FLUXES
C
        DO J=2,NJM 
          SE=0.5*(R(J)+R(J-1))*(Y(J)-Y(J-1))
          SN=DX*R(J)
          CX(I,J)=DEN*SE*U(I,J) 
          CY(I,J)=DEN*SN*V(I,J) 
C 
C.....MATRIX COEFFICIENTS
C 
          AW(I,J)= AE(I-1,J)
          AE(I,J)=-DEN*SE*SE*DU(I,J)
          AS(I,J)= AN(I,J-1)
          AN(I,J)=-DEN*SN*SN*DV(I,J)
C 
C.....SOURCE TERMS
C 
          SU(I,J)=CX(I-1,J)-CX(I,J)+CY(I,J-1)-CY(I,J) 
          SUM=SUM+SU(I,J) 
        END DO 
      END DO
C
      IF(LTEST) WRITE(6,*) '   SUM = ',SUM
C 
C===========================================================
C.....FINAL ASSEMBLY (NO BOUNDARY MODIFICATIONS NECEESSARY)
C=========================================================== 
C
      DO I=2,NIM 
      DO J=2,NJM 
        PP(I,J)=0.
        AP(I,J)=-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J)) 
      END DO 
      END DO
C 
C.....SOLUTION
C 
      CALL SIPSOL(PP,IPP,NIM,NJM) 
C 
C.....PRESSURE CORRECTION AT REFERENCE LOCATION 
C 
      PPREF=PP(IPREF,JPREF) 
C
C.....CORRECT U VELOCITY AND CX MASS FLUX
C
      DO I=2,NIMM
        DO J=2,NJM
          SE=0.5*(R(J)+R(J-1))*(Y(J)-Y(J-1))
          U(I,J)=U(I,J)-SE*DU(I,J)*(PP(I+1,J)-PP(I,J)) 
          CX(I,J)=CX(I,J)+AE(I,J)*(PP(I+1,J)-PP(I,J))  
        END DO 
      END DO  
C
C.....CORRECT V VELOCITY AND CY MASS FLUX
C
      DO I=2,NIM
        DO J=2,NJMM
          SN=(X(I)-X(I-1))*R(J)
          V(I,J)=V(I,J)-SN*DV(I,J)*(PP(I,J+1)-PP(I,J)) 
          CY(I,J)=CY(I,J)+AN(I,J)*(PP(I,J+1)-PP(I,J))  
        END DO 
      END DO  
C
C.....CORRECT PRESSURE
C
      DO J=2,NJM 
        DO I=2,NIM 
          P(I,J)=P(I,J)+URF(IPP)*(PP(I,J)-PPREF)
        END DO
      END DO
C
      RETURN
      END 
C
C
C###########################################################
      SUBROUTINE SIPSOL(FI,IFI,IEND,JEND) 
C###########################################################
C     This routine contains the SIP-solver with 2D indexing;
C     see Sect. 5.3.4 for a description of the algorithm.
C===========================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /COEF/ AE(NX,NY),AW(NX,NY),AN(NX,NY),AS(NX,NY),
     *       AP(NX,NY),SU(NX,NY),DU(NX,NY),DV(NX,NY),PP(NX,NY)
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      DIMENSION LW(NX,NY),UE(NX,NY),LS(NX,NY),UN(NX,NY),
     *          LPR(NX,NY),RES(NX,NY),FI(NX,NY)
      REAL LW,LS,LPR
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
      DATA UN,UE,RES /NXY*0.,NXY*0.,NXY*0./
C
C.....COEFFICIENTS OF [L] AND [U] MATRICES
C
      DO I=2,IEND
      DO J=2,JEND
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
C.....INNER ITERATION LOOP
C
      DO L=1,NSWP(IFI)
        RESL=0.
C
C.....RESIDUAL AND FORWARD SUBSTITUTION
C
        DO I=2,IEND
        DO J=2,JEND
          RES(I,J)=SU(I,J)-AN(I,J)*FI(I,J+1)-AS(I,J)*FI(I,J-1)-
     *             AE(I,J)*FI(I+1,J)-AW(I,J)*FI(I-1,J)-AP(I,J)*FI(I,J) 
          RESL=RESL+ABS(RES(I,J)) 
          RES(I,J)=(RES(I,J)-LS(I,J)*RES(I,J-1)-
     *             LW(I,J)*RES(I-1,J))*LPR(I,J) 
        END DO 
        END DO
C
        IF(L.EQ.1) RESOR(IFI)=RESL 
        RSM=RESL/(RESOR(IFI)+1.E-20)
C
C.....BACKWARD SUBSTITUTION AND CORRECTION
C
        DO I=IEND,2,-1 
        DO J=JEND,2,-1 
          RES(I,J)=RES(I,J)-UN(I,J)*RES(I,J+1)-UE(I,J)*RES(I+1,J) 
          FI(I,J)=FI(I,J)+RES(I,J)
        END DO
        END DO
C
C.....CHECK CONVERGENCE
C
       IF(LTEST) WRITE(6,*) '     ',L,' SWEEP, RSM =',RSM 
       IF(RSM.LT.SOR(IFI)) RETURN
C
      END DO
C
      RETURN
      END 
C
C
C###########################################################
      SUBROUTINE PRINT(FI,TITLE)
C###########################################################
C     This routine prints 2D arrays in an easy to read form.
C===========================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      DIMENSION FI(NX,NY) 
      CHARACTER*6 TITLE
C
      WRITE(6,20) TITLE 
      IS=-11
  100 IS=IS+12
      IE=IS+11
      IE=MIN(NI,IE)
      WRITE(6,21) (I,I=IS,IE) 
      WRITE(6,22) 
      DO J=NJ,1,-1
        WRITE(6,23) J,(FI(I,J),I=IS,IE) 
      END DO
      IF(IE.LT.NI) GO TO 100
   20 FORMAT(2X,26('*-'),7X,A6,7X,26('-*')) 
   21 FORMAT(3X,'I = ',I3,11I10)
   22 FORMAT(2X,'J')
   23 FORMAT(1X,I3,1P12E10.2) 
      RETURN
      END 
C
C
C##############################################################
      SUBROUTINE INIT 
C##############################################################
C     In this routine input data is read, the grid is set up,
C     and some control parameters are defined.
C==============================================================
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /VAR/ U(NX,NY),V(NX,NY),P(NX,NY),CX(NX,NY),CY(NX,NY) 
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
      COMMON /CHAR/ TITLE
      CHARACTER TITLE*50
C 
C.....READ AND SET CONTROL PARAMETERS (MEANING AS IN PCOL.F)
C 
      READ(5,6) TITLE 
    6 FORMAT(A50) 
      READ(5,*) LREAD,LWRITE,LTEST,LAKSI 
      READ(5,*) (LCAL(I),I=1,3)
      READ(5,*) MAXIT,SORMAX,SLARGE,IMON,JMON,IPREF,JPREF,ALFA
      READ(5,*) (URF(I),I=1,3) 
      READ(5,*) (SOR(I),I=1,3) 
      READ(5,*) (NSWP(I),I=1,3)
      READ(5,*) (G(I),I=1,3)
      READ(5,*) DEN,VIS,ULID
C
      SMALL=1.E-30
      GREAT=1.E30 
      IU=1
      IV=2
      IPP=3 
C 
C.....DEFINE THE GRID (GENERATED USING GRID.F)
C 
      READ(1,*) I
      READ(1,*) J
      READ(1,*) NI
      READ(1,*) NJ
      READ(1,*) IJ
      READ(1,*) (X(I),I=1,NI)
      READ(1,*) (Y(J),J=1,NJ)
      NIM=NI-1
      NJM=NJ-1
      NIMM=NIM-1
      NJMM=NJM-1
C
C.....X- COORDINATES OF CV-CENTERS
C
      DO I=2,NIM
        XC(I)=0.5*(X(I)+X(I-1))
      END DO
      XC(1)=X(1)
      XC(NI)=X(NIM)
C
C.....Y- COORDINATES OF CV-CENTERS
C
      DO J=2,NJM
        YC(J)=0.5*(Y(J)+Y(J-1))
      END DO
      YC(1)=Y(1)
      YC(NJ)=Y(NJM)
C
C.....SET RADIUS (R=1 FOR PLANE, R=Y FOR AXISYMMETRIC GEOMETRY)
C
      IF(LAKSI) THEN 
       DO J=1,NJ
         R(J)=Y(J) 
       END DO 
      ELSE
       DO J=1,NJ
         R(J)=1.
       END DO
      ENDIF 
C
C.....INTERPOLATION FACTORS (ON SCALAR CVs)
C
      FX(1)=0.
      FY(1)=0.
      FX(NI)=0. 
      FY(NJ)=0. 
      DO I=2,NIM 
        FX(I)=(X(I)-X(I-1))/(X(I+1)-X(I-1)) 
      END DO 
      DO J=2,NJM 
        FY(J)=(Y(J)-Y(J-1))/(Y(J+1)-Y(J-1)) 
      END DO 
C 
C.....SET BOUNDARY VALUES (LID-DRIVEN CAVITY)
C 
      DO I=2,NIMM
        U(I,NJ)=ULID 
      END DO 
C
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE OUT1 
C###########################################################
      PARAMETER (NX=82,NY=82,NXY=NX*NY)
      COMMON /IDAT/ NI,NJ,NIM,NJM,NIMM,NJMM,IU,IV,IPP,
     *       MAXIT,IMON,JMON,IPREF,JPREF,NSWP(3)
      COMMON /RDAT/ RESOR(3),SOR(3),URF(3),SLARGE,SORMAX,ULID,
     *       SMALL,GREAT,G(3),ALFA,VIS,DEN,X(NX),Y(NY),
     *       XC(NX),YC(NY),R(NY),FX(NX),FY(NY)
      COMMON /LOGIC/ LCAL(3),LREAD,LWRITE,LTEST,LAKSI
      LOGICAL LCAL,LREAD,LWRITE,LTEST,LAKSI
      COMMON /CHAR/ TITLE
      CHARACTER TITLE*50
C 
C.....INITIAL OUTPUT
C 
      REY=DEN*Y(NJM)*ULID/VIS 
      WRITE(6,601) TITLE,REY,DEN,VIS
  601 FORMAT('1',//,40X,A50,/,40X,50('*'),//,40X, 
     *     'REYNOLDS  NUMBER   :     REY =',1PE10.3,/,40X,
     *     'FLUID  DENSITY     :     DEN =',1PE10.3,/,40X,
     *     'DINAMIC VISCOCITY  :     VIS =',1PE10.3,/)
      IF(LCAL(IU)) WRITE(6,602) 'URF( IU) =',URF(IU) 
      IF(LCAL(IV)) WRITE(6,602) 'URF( IV) =',URF(IV) 
      IF(LCAL(IPP)) WRITE(6,602) 'URF( IP) =',URF(IPP) 
  602 FORMAT(40X,A10,F8.2)
      WRITE(6,603) NI,NJ
  603 FORMAT(/,40X,'NUMBER OF NODES IN X-DIRECTION: NI =',I4, 
     *         /,40X,'NUMBER OF NODES IN Y-DIRECTION: NJ =',I4, 
     *  //,40X,'COMPUTATIONAL GRID (SCALAR CELL BOUNDARIES)',/, 
     *     40X,43('*'),/) 
      WRITE(6,604) (X(I),I=1,NI)
      WRITE(6,605) (Y(J),J=1,NJ)
      WRITE(6,*) '  '
  604 FORMAT(2X,'X(I),I=1,NI :',1P10E12.3,/(15X,1P10E12.3)) 
  605 FORMAT(/,2X,'Y(J),J=1,NJ :',1P10E12.3,/(15X,1P10E12.3)) 
      RETURN
      END 
