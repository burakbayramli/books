C#############################################################
      PROGRAM PCOL
C#############################################################
C
C     This code solves the 2D Navier-Stokes equations using
C     Cartesian grid and colocated variable arrangement.
C     It is set for lid- and buoyancy-driven flows in closed
C     cavities (steady or unsteady; includes UDS and CDS
C     schemes for convective fluxes, Euler implicit or
C     three time levels time stepping). 
C
C                      M. Peric, IfS, Hamburg, January 1997
C-------------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /VAROLD/ TIME,DT,DTR,UO(NXY),VO(NXY),TO(NXY),
     *       UOO(NXY),VOO(NXY),TOO(NXY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
      CHARACTER*20 FILRES,FILIN,FILOUT,FILGR,FILTER
C--------------------------------------------------------------
C
C.....I/O FILE NAMES
C
      PRINT *, 'ENTER INPUT FILE NAME:  '
      READ(*,'(A20)') FILIN
      PRINT *,' ENTER OUTPUT FILE NAME:  '
      READ(*,'(A20)') FILOUT
      PRINT *, 'ENTER GRID FILE NAME:  '
      READ(*,'(A20)') FILGR
      PRINT *,' ENTER RESULTS FILE NAME:  '
      READ(*,'(A20)') FILRES
C
      OPEN (UNIT=5,FILE=FILIN)
      OPEN (UNIT=2,FILE=FILOUT)
      OPEN (UNIT=1,FILE=FILGR)
      OPEN (UNIT=3,FILE=FILRES,FORM='UNFORMATTED')
      REWIND 3
      REWIND 5
      REWIND 2
      REWIND 1
C
C.....INPUT AND BOUNDARY DATA, INITIALIZATION, OUTPUT TITLE, ETC.
C
      ITIM=0
      TIME=0.
      CALL MODINP
C
      IF(LTIME) THEN
        PRINT *,' ENTER NAME OF UNSTEADY RESULTS FILE:  '
        READ(5,'(A20)') FILTER
        OPEN (UNIT=4,FILE=FILTER,FORM='UNFORMATTED')
        REWIND 4
      ENDIF
C
C.....READ RESULTS OF PREVIOUS TIME STEP (IF CONTINUATION)
C
      IF(LREAD) THEN
        READ(3) ITIM,TIME,NI,NJ,NIM,NJM,NIJ,
     *        ((X(I),J=1,NJ),I=1,NI),((Y(J),J=1,NJ),I=1,NI),
     *        ((XC(I),J=1,NJ),I=1,NI),((YC(J),J=1,NJ),I=1,NI),
     *        (F1(IJ),IJ=1,NIJ),(F2(IJ),IJ=1,NIJ),(U(IJ),IJ=1,NIJ),
     *        (V(IJ),IJ=1,NIJ),(P(IJ),IJ=1,NIJ),(T(IJ),IJ=1,NIJ),
     *        (UO(IJ),IJ=1,NIJ),(VO(IJ),IJ=1,NIJ),(TO(IJ),IJ=1,NIJ)
        REWIND 3
      ENDIF
C
C==============================================
C.....TIME LOOP
C==============================================
C
      ITIMS=ITIM+1
      ITIME=ITIM+ITST
C
      DO ITIM=ITIMS,ITIME
      TIME=TIME+DT
C
C.....SHIFT SOLUTIONS IN TIME
C
      IF(LTIME) THEN
        DO IJ=1,NIJ
          TOO(IJ)=TO(IJ)
          UOO(IJ)=UO(IJ)
          VOO(IJ)=VO(IJ)
          TO(IJ)=T(IJ)
          UO(IJ)=U(IJ)
          VO(IJ)=V(IJ)
        END DO
      ENDIF
C
      WRITE(2,*) '     TIME = ',TIME
      WRITE(2,*) '     *****************************'
      WRITE(2,*) '  '
C
C.....PRINT INITIAL FIELDS IF DESIRED
C
      IF(LOUTS.AND.(ITIM.EQ.ITIMS)) THEN
        IF(LCAL(IU)) CALL PRINT(U,'U VEL.')
        IF(LCAL(IV)) CALL PRINT(V,'V VEL.')
        IF(LCAL(IP)) CALL PRINT(P,'PRESS.')
        IF(LCAL(IEN)) CALL PRINT(T,'TEMPER')
      ENDIF
C
C.....DEFINE MONITORING LOCATION (NODE WITH I=IMON, J=JMON)
C
      IJMON=LI(IMON)+JMON
      WRITE(2,600) IMON,JMON
C
C.....SET BOUNDARY CONDITIONS FOR THE NEW TIME STEP
C
      IF(LTIME) CALL BCTIME
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.....OUTER ITERATIONS (SIMPLE RELAXATIONS)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DO ITER=1,MAXIT
        IF(LCAL(IU)) CALL CALCUV
        IF(LCAL(IP)) CALL CALCP
        IF(LCAL(IEN)) CALL CALCT
C
C.....CHECK CONVERGENCE OF OUTER ITERATIONS
C
        WRITE(2,606) ITER,RESOR(IU),RESOR(IV),RESOR(IP),
     *       RESOR(IEN),U(IJMON),V(IJMON),P(IJMON),T(IJMON)
        SOURCE=MAX(RESOR(IU),RESOR(IV),RESOR(IP),RESOR(IEN))
        IF(SOURCE.GT.SLARGE) GO TO 510
        IF(SOURCE.LT.SORMAX) GO TO 250
      END DO
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
  250 CONTINUE
C
C.....CONVERGED: IF UNSTEADY FLOW, PRINT AND SAVE NPRTth SOLUTION
C
      IF((.NOT.LTIME).OR.(LTIME.AND.MOD(ITIM,NPRT).EQ.0)) THEN
        IF(LOUTE) THEN
          IF(LCAL(IU)) CALL PRINT(U,'U VEL.')
          IF(LCAL(IV)) CALL PRINT(V,'V VEL.')
          IF(LCAL(IP)) CALL PRINT(P,'PRESS.')
          IF(LCAL(IEN)) CALL PRINT(T,'TEMPER')
        ENDIF
        CALL OUT2
C
        IF(LWRITE) THEN
          WRITE(4) ITIM,TIME,NI,NJ,NIM,NJM,NIJ,
     *        ((X(I),J=1,NJ),I=1,NI),((Y(J),J=1,NJ),I=1,NI),
     *        ((XC(I),J=1,NJ),I=1,NI),((YC(J),J=1,NJ),I=1,NI),
     *        (F1(IJ),IJ=1,NIJ),(F2(IJ),IJ=1,NIJ),(U(IJ),IJ=1,NIJ),
     *        (V(IJ),IJ=1,NIJ),(P(IJ),IJ=1,NIJ),(T(IJ),IJ=1,NIJ)
        ENDIF
      ENDIF
C
      END DO
C
C==============================================================
C.....ALL TIME STEPS DONE; SAVE LAST SOLUTION FOR CONTINUATION
C==============================================================
C
      WRITE(3) ITIM,TIME,NI,NJ,NIM,NJM,NIJ,
     *        ((X(I),J=1,NJ),I=1,NI),((Y(J),J=1,NJ),I=1,NI),
     *        ((XC(I),J=1,NJ),I=1,NI),((YC(J),J=1,NJ),I=1,NI),
     *        (F1(IJ),IJ=1,NIJ),(F2(IJ),IJ=1,NIJ),(U(IJ),IJ=1,NIJ),
     *        (V(IJ),IJ=1,NIJ),(P(IJ),IJ=1,NIJ),(T(IJ),IJ=1,NIJ),
     *        (UO(IJ),IJ=1,NIJ),(VO(IJ),IJ=1,NIJ),(TO(IJ),IJ=1,NIJ)
      STOP
C
C==============================================================
C......MESSAGE FOR DIVERGENCE 
C==============================================================
C
  510 PRINT *,'  *** TERMINATED - OUTER ITERATIONS DIVERGING ***'
      STOP
C
C==============================================================
C......FORMAT SPECIFICATIONS
C==============================================================
C
  600 FORMAT(1X,'ITER.',3X,
     *'I---------ABSOLUTE RESIDUAL SOURCE SUMS--------I',3X,
     *'I----FIELD VALUES AT MONITORING LOCATION (',I3,',',I3,
     *')----I',/,2X,'NO.',9X,'U',11X,'V',9X,'MASS',10X,'T',
     *16X,'U',11X,'V',11X,'P',11X,'T',/)
  606 FORMAT(1X,I4,2X,1P4E12.4,5X,1P4E12.4)
C
      END
C
C
C###########################################################
      SUBROUTINE CALCUV
C###########################################################
C     This routine sets the coefficient matrix for the U and
C     V equations, and calls the linear equation solver to
C     update the velocity components. Constant fluid 
C     properties are assumed (parts of diffusive fluxes
C     cancel out, see Chap. 7, Sect. 2).
C--------------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /VAROLD/ TIME,DT,DTR,UO(NXY),VO(NXY),TO(NXY),
     *       UOO(NXY),VOO(NXY),TOO(NXY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),
     *       SU(NXY),SV(NXY),APU(NXY),APV(NXY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
C----------------------------------------------------------
C
C.....RECIPROCAL VALUES OF UNDER-RELAXATION FACTORS FOR U AND V
C
      URFU=1./URF(IU)
      URFV=1./URF(IV)
C
C.....SET BOUNDARY PRESSURE (LINEAR EXTRAPOLATION FROM INSIDE)
C
      CALL PBOUND(P)
C
C.....INITIALIZE TEMPORARILY STORED VARIABLES
C
      DO IJ=1,NIJ
        SU(IJ)=0.
        SV(IJ)=0.
        APU(IJ)=0.
        APV(IJ)=0.
      END DO
C
C==========================================================
C.....FLUXES THROUGH INTERNAL EAST CV FACES 
C==========================================================
C     F1(IJ) is the mass flux through the east face (outward
C     normal directed to E); FX(I) is the ratio of distance
C     from P to cell face, to distance from P to E; IJ 
C     denotes node P and IJE node E.
C     Contribution of convective and diffusive fluxes from
C     east face to AE(P), AW(E), and source terms at both
C     P and E are calculated; contributions to AP(P) and 
C     AP(E) come through the sum of neighbor coefficients
C     and are not explicitly calculated.
C----------------------------------------------------------
C
      DO I=2,NIM-1
C
C.....INTERPOLATION FACTORS, DISTANCE FROM P TO E (SAME FOR ALL J)
C
        FXE =FX(I)
        FXP =1.-FXE
        DXPE=XC(I+1)-XC(I)
C
        DO J=2,NJM
          IJ=LI(I)+J
          IJE=IJ+NJ
C
C.....CELL FACE AREA S = DY*RE*1
C
          S=(Y(J)-Y(J-1))*(R(J)+R(J-1))*0.5
C
C.....COEFFICIENT RESULTING FROM DIFFUSIVE FLUX (SAME FOR U AND V)
C
          D=VISC*S/DXPE
C
C.....EXPLICIT CONVECTIVE FLUXES FOR UDS AND CDS
C
          CE=MIN(F1(IJ),0.)
          CP=MAX(F1(IJ),0.)
C
          FUUDS=CP*U(IJ)+CE*U(IJE)
          FVUDS=CP*V(IJ)+CE*V(IJE)
          FUCDS=F1(IJ)*(U(IJE)*FXE+U(IJ)*FXP)
          FVCDS=F1(IJ)*(V(IJE)*FXE+V(IJ)*FXP)
C
C.....COEFFICIENTS AE(P) AND AW(E) DUE TO UDS
C
          AE(IJ) = CE-D
          AW(IJE)=-CP-D
C
C.....SOURCE TERM CONTRIBUTIONS AT P AND E DUE TO DEFERRED CORRECTION
C
          SU(IJ) =SU(IJ) +GDS(IU)*(FUUDS-FUCDS)
          SU(IJE)=SU(IJE)-GDS(IU)*(FUUDS-FUCDS)
          SV(IJ) =SV(IJ) +GDS(IU)*(FVUDS-FVCDS)
          SV(IJE)=SV(IJE)-GDS(IU)*(FVUDS-FVCDS)
        END DO
      END DO
C
C=========================================================
C.....FLUXES THROUGH INTERNAL NORTH CV FACES 
C=========================================================
C     F2(IJ) is the mass flux through the north face (outward
C     normal directed to N); FY(J) is the ratio of distance
C     from P to cell face, to distance from P to N; IJ 
C     denotes node P and IJN node N.
C     Contribution of convective and diffusive fluxes from
C     north face to AN(P), AS(N), and source terms at both
C     P and N are calculated; contributions to AP(P) and 
C     AP(N) come through the sum of neighbor coefficients
C     and are not explicitly calculated.
C----------------------------------------------------------
C
      DO J=2,NJM-1
C
C.....INTERPOLATION FACTORS, DISTANCE FROM P TO N (SAME FOR ALL J)
C
        FYN =FY(J)
        FYP =1.-FYN
        DYPN=YC(J+1)-YC(J)
C
        DO I=2,NIM
          IJ =LI(I)+J
          IJN=IJ+1
C
C.....CELL FACE AREA S = DX*RN*1
C
          S=(X(I)-X(I-1))*R(J)
C
C.....COEFFICIENT RESULTING FROM DIFFUSIVE FLUX (SAME FOR U AND V)
C
          D=VISC*S/DYPN
C
C.....EXPLICIT CONVECTIVE FLUXES FOR UDS AND CDS
C
          CN=MIN(F2(IJ),0.)
          CP=MAX(F2(IJ),0.)
C
          FUUDS=CP*U(IJ)+CN*U(IJN)
          FVUDS=CP*V(IJ)+CN*V(IJN)
          FUCDS=F2(IJ)*(U(IJN)*FYN+U(IJ)*FYP)
          FVCDS=F2(IJ)*(V(IJN)*FYN+V(IJ)*FYP)
C
C.....COEFFICIENTS AN(P) AND AS(N) DUE TO UDS
C
          AN(IJ) = CN-D
          AS(IJN)=-CP-D
C
C.....SOURCE TERM CONTRIBUTIONS AT P AND N DUE TO DEFERRED CORRECTION
C
          SU(IJ) =SU(IJ) +GDS(IU)*(FUUDS-FUCDS)
          SU(IJN)=SU(IJN)-GDS(IU)*(FUUDS-FUCDS)
          SV(IJ) =SV(IJ) +GDS(IU)*(FVUDS-FVCDS)
          SV(IJN)=SV(IJN)-GDS(IU)*(FVUDS-FVCDS)
        END DO
      END DO
C
C=============================================================
C.....VOLUME INTEGRALS (SOURCE TERMS)
C=============================================================
C     Cell-face pressure calculated using linear interpolation;
C     cell volume is VOL, RP is the radius at node P; DX and DY
C     are the width and height of the cell. Contribution to AP
C     coefficient from volume integrals is stored temporarily
C     in arrays APU and APV for U and V, respectively; these
C     arrays are later used to store 1/AP, which is needed in 
C     the pressure-correction equation.
C--------------------------------------------------------------
C
      DO I=2,NIM
        DX=X(I)-X(I-1)
C
        DO J=2,NJM
          DY=Y(J)-Y(J-1)
          RP=0.5*(R(J)+R(J-1))
          VOL=DX*DY*RP
          IJ=LI(I)+J
C
C...... CELL-FACE PRESSURE, CELL-CENTER GRADIENT, SOURCE 
C
          PE=P(IJ+NJ)*FX(I)+P(IJ)*(1.-FX(I))
          PW=P(IJ)*FX(I-1)+P(IJ-NJ)*(1.-FX(I-1))
          PN=P(IJ+1)*FY(J)+P(IJ)*(1.-FY(J))
          PS=P(IJ)*FY(J-1)+P(IJ-1)*(1.-FY(J-1))
          DPX(IJ)=(PE-PW)/DX
          DPY(IJ)=(PN-PS)/DY
          SU(IJ)=SU(IJ)-DPX(IJ)*VOL
          SV(IJ)=SV(IJ)-DPY(IJ)*VOL
C
C..... BUOYANCY SOURCE CONTRIBUTION
C
          IF(LCAL(IEN)) THEN
            SB=-BETA*DENSIT*VOL*(T(IJ)-TREF)
            SU(IJ)=SU(IJ)+GRAVX*SB
            SV(IJ)=SV(IJ)+GRAVY*SB
          ENDIF
C
C..... AXISYMMETRIC CONTRIBUTION
C
          IF(LAXIS) THEN
            APV(IJ)=APV(IJ)+VISC*VOL/RP**2
          ENDIF
C
C..... UNSTEADY TERM CONTRIBUTION TO AP AND SU
C
          IF(LTIME) THEN
            APT=DENSIT*VOL*DTR
            SU(IJ)=SU(IJ)+(1.+GAMT)*APT*UO(IJ)-0.5*GAMT*APT*UOO(IJ)
            SV(IJ)=SV(IJ)+(1.+GAMT)*APT*VO(IJ)-0.5*GAMT*APT*VOO(IJ)
            APV(IJ)=APV(IJ)+(1.+0.5*GAMT)*APT
            APU(IJ)=APU(IJ)+(1.+0.5*GAMT)*APT
          ENDIF
C
        END DO
      END DO
C
C=============================================================
C.....PROBLEM MODIFICATIONS - BOUNDARY CONDITIONS
C=============================================================
C
      CALL BCUV
C
C=============================================================
C.....UNDER-RELAXATION, SOLVING EQUATION SYSTEM FOR U-VELOCITY
C=============================================================
C
      DO I=2,NIM
      DO IJ=LI(I)+2,LI(I)+NJM
        AP(IJ)=(-AE(IJ)-AW(IJ)-AN(IJ)-AS(IJ)+APU(IJ))*URFU
        SU(IJ)=SU(IJ)+(1.-URF(IU))*AP(IJ)*U(IJ)
        APU(IJ)=1./AP(IJ)
      END DO
      END DO
C
      CALL SIPSOL(U,IU)
C
C=============================================================
C.....UNDER-RELAXATION, SOLVING EQUATION SYSTEM FOR V-VELOCITY
C=============================================================
C
      DO I=2,NIM
      DO IJ=LI(I)+2,LI(I)+NJM
        AP(IJ)=(-AE(IJ)-AW(IJ)-AN(IJ)-AS(IJ)+APV(IJ))*URFV
        SU(IJ)=SV(IJ)+(1.-URF(IV))*AP(IJ)*V(IJ)
        APV(IJ)=1./AP(IJ)
      END DO
      END DO
C
      CALL SIPSOL(V,IV)
C
      RETURN
      END 
C
C
C############################################################## 
      SUBROUTINE CALCP 
C##############################################################
C     This routine assembles and solves the pressure-correction
C     equation. Cell-face values of velocity components, used
C     to calculate the mass fluxes, are obtained by linear
C     interpolation and then corrected by adding a term 
C     proportional to the third derivative of pressure and
C     squared grid spacing, as described in Chap. 7, Sect. 7.5.3.
C-------------------------------------------------------------- 
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /VAROLD/ TIME,DT,DTR,UO(NXY),VO(NXY),TO(NXY),
     *       UOO(NXY),VOO(NXY),TOO(NXY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),
     *       SU(NXY),SV(NXY),APU(NXY),APV(NXY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
C-------------------------------------------------------------- 
C
C============================================================
C.....EAST CV FACES (S - AREA, VOLE - VOLUME BETWEEN P AND E)
C============================================================
C
      DO I=2,NIM-1
        DXPE=XC(I+1)-XC(I)
        FXE=FX(I)
        FXP=1.-FXE
C
        DO J=2,NJM
          IJ=LI(I)+J
          IJE=IJ+NJ
C
          S=(Y(J)-Y(J-1))*(R(J)+R(J-1))*0.5
          VOLE=DXPE*S
          D=DENSIT*S
C
C.....INTERPOLATED CELL FACE QUANTITIES (PRESSURE GRAD., U AND 1/AP)
C     Note: pressure gradient is interpolated midway between P and E,
C     since the gradient calculated at cell face is second order
C     accurate at that location; the velocity is interpolated linearly,
C     to achieve second order accuracy at cell face center.
C
          DPXEL=0.5*(DPX(IJE)+DPX(IJ))
          UEL=U(IJE)*FXE+U(IJ)*FXP
          APUE=APU(IJE)*FXE+APU(IJ)*FXP
C
C.....CELL FACE GRADIENT, VELOCITY AND MASS FLUX
C
          DPXE=(P(IJE)-P(IJ))/DXPE
          UE=UEL-APUE*VOLE*(DPXE-DPXEL)
          F1(IJ)=D*UE
C
C.....COEFFICIENTS OF P' EQUATION, AE(P) AND AW(E)
C
          AE(IJ)=-D*APUE*S
          AW(IJE)=AE(IJ)
C
        END DO
      END DO
C
C=============================================================
C.....NORTH CV FACES (S - AREA, VOLN - VOLUME BETWEEN P AND N)
C=============================================================
C
      DO J=2,NJM-1
        DYPN=YC(J+1)-YC(J)
        FYN=FY(J)
        FYP=1.-FYN
C
        DO I=2,NIM
          IJ=LI(I)+J
          IJN=IJ+1
C
          S=(X(I)-X(I-1))*R(J)
          VOLN=S*DYPN
          D=DENSIT*S
C
C.....INTERPOLATED CELL-FACE QUANTITIES (PRESSURE GRAD., U AND 1/AP)
C
          DPYNL=0.5*(DPY(IJN)+DPY(IJ))
          VNL=V(IJN)*FYN+V(IJ)*FYP
          APVN=APV(IJN)*FYN+APV(IJ)*FYP
C
C.....CELL-FACE GRADIENT, VELOCITY AND MASS FLUX
C
          DPYN=(P(IJN)-P(IJ))/DYPN
          VN=VNL-APVN*VOLN*(DPYN-DPYNL)
          F2(IJ)=D*VN
C
C.....COEFFICIENTS OF P' EQUATION, AN(P) AND AS(N)
C
          AN(IJ)=-D*APVN*S
          AS(IJN)=AN(IJ)
C
        END DO
      END DO
C
C===============================================================
C.....BOUNDARY CONDITIONS: PRESCRIBED MASS FLUXES, ZERO CORRECTION
C.....(EQUIVALENT TO ZERO NORMAL GRADIENT FOR P'; COEFFICIENT FOR
C.....THE BOUNDARY NODE IS ZERO, NO SPECIAL TREATMENT REQUIRED)
C===============================================================
C
C===============================================================
C..... SORCE TERM AND COEFFICIENT OF NODE P
C===============================================================
C
      SUM=0.
      DO I=2,NIM
      DO IJ=LI(I)+2,LI(I)+NJM
        SU(IJ)=F1(IJ-NJ)-F1(IJ)+F2(IJ-1)-F2(IJ)
        AP(IJ)=-(AE(IJ)+AW(IJ)+AN(IJ)+AS(IJ))
        SUM=SUM+SU(IJ)
        PP(IJ)=0.
      END DO
      END DO
C
C.....SUM MUST BE ZERO IF GLOBAL MASS CONSERVATION IS ASSURED!
C
      IF(LTEST) WRITE(2,*) '       SUM = ',SUM
C
C===============================================================
C.....SOLVE EQUATIONS SYSTEM FOR P' AND APPLY CORRECTIONS
C===============================================================
C
      CALL SIPSOL(PP,IP)
C
C.....CALCULATE PRESSURE CORRECTION AT BOUNDARIES
C
      CALL PBOUND(PP)
C
C.....VALUE OF P' AT REFERENCE LOCATION TO BE SUBTRACTED FROM ALL P'
C
      IJPREF=LI(IPR)+JPR
      PPO=PP(IJPREF)
C
C.....CORRECT EAST MASS FLUXES 
C
      DO I=2,NIM-1
      DO IJ=LI(I)+2,LI(I)+NJM
        F1(IJ)=F1(IJ)+AE(IJ)*(PP(IJ+NJ)-PP(IJ))
      END DO
      END DO
C
C.....CORRECT NORTH MASS FLUXES 
C
      DO I=2,NIM
      DO IJ=LI(I)+2,LI(I)+NJM-1
        F2(IJ)=F2(IJ)+AN(IJ)*(PP(IJ+1)-PP(IJ))
      END DO
      END DO
C
C.....CORRECT PRESSURE AND VELOCITIES AT CELL CENTER
C
      DO I=2,NIM
        DX=X(I)-X(I-1)
C
        DO J=2,NJM
          IJ=LI(I)+J
          RP=0.5*(R(J)+R(J-1))
          DY=Y(J)-Y(J-1)
C
          PPE=PP(IJ+NJ)*FX(I)+PP(IJ)*(1.-FX(I))
          PPW=PP(IJ)*FX(I-1)+PP(IJ-NJ)*(1.-FX(I-1))
          PPN=PP(IJ+1)*FY(J)+PP(IJ)*(1.-FY(J))
          PPS=PP(IJ)*FY(J-1)+PP(IJ-1)*(1.-FY(J-1))
C
          U(IJ)=U(IJ)-(PPE-PPW)*DY*RP*APU(IJ)
          V(IJ)=V(IJ)-(PPN-PPS)*DX*RP*APV(IJ)
          P(IJ)=P(IJ)+URF(IP)*(PP(IJ)-PPO)
        END DO
      END DO
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE CALCT
C############################################################
C     This routine solves the temperature equation. Constant
C     viscosity, density and Prandtl number are assumed - only
C     the density variation due to buoyancy is considered 
C     using Boussinesq approximation (valid for temperature
C     differences less than 5 deg. in water and 20 deg. in air).
C     PRR is the reciprocal value of the Prandtl number, 1/Pr.
C-------------------------------------------------------------- 
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /VAROLD/ TIME,DT,DTR,UO(NXY),VO(NXY),TO(NXY),
     *       UOO(NXY),VOO(NXY),TOO(NXY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),
     *       SU(NXY),SV(NXY),APU(NXY),APV(NXY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
C-------------------------------------------------------------
C
C.....INITIALIZATION OF TEMPORARILY STORED VARIABLES
C
      DO IJ=1,NIJ
        SU(IJ)=0.
        AP(IJ)=0.
      END DO
C      
      URFI=1./URF(IEN)
C
C==========================================================
C.....FLUXES THROUGH INTERNAL EAST CV-FACES 
C==========================================================
C
      DO I=2,NIM-1
C
C.....INTERPOLATION FACTORS, DISTANCE FROM P TO E (SAME FOR ALL J)
C
        FXE =FX(I)
        FXP =1.-FXE
        DXPE=XC(I+1)-XC(I)
C
        DO J=2,NJM
          IJ=LI(I)+J
          IJE=IJ+NJ
C
C.....CELL FACE AREA S = DY*RE*1
C
          S=(Y(J)-Y(J-1))*(R(J)+R(J-1))*0.5
C
C.....COEFFICIENT RESULTING FROM DIFFUSIVE FLUX
C
          D=VISC*PRR*S/DXPE
C
C.....EXPLICIT CONVECTIVE FLUX FOR UDS AND CDS
C
          CE=MIN(F1(IJ),0.)
          CP=MAX(F1(IJ),0.)
C
          FUDS=CP*T(IJ)+CE*T(IJE)
          FCDS=F1(IJ)*(T(IJE)*FXE+T(IJ)*FXP)
C
C.....COEFFICIENTS AE(P) AND AW(E) DUE TO UDS
C
          AE(IJ) = CE-D
          AW(IJE)=-CP-D
C
C.....SOURCE TERM CONTRIBUTIONS AT P AND E DUE TO DEFERRED CORRECTION
C
          SU(IJ) =SU(IJ) +GDS(IEN)*(FUDS-FCDS)
          SU(IJE)=SU(IJE)-GDS(IEN)*(FUDS-FCDS)
        END DO
      END DO
C
C=========================================================
C.....FLUXES THROUGH INTERNAL NORTH CV FACES 
C=========================================================
C
      DO J=2,NJM-1
C
C.....INTERPOLATION FACTORS, DISTANCE FROM P TO N (SAME FOR ALL J)
C
        FYN =FY(J)
        FYP =1.-FYN
        DYPN=YC(J+1)-YC(J)
C
        DO I=2,NIM
          IJ =LI(I)+J
          IJN=IJ+1
C
C.....CELL FACE AREA S = DX*RN*1
C
          S=(X(I)-X(I-1))*R(J)
C
C.....COEFFICIENT RESULTING FROM DIFFUSIVE FLUX (SAME FOR U AND V)
C
          D=VISC*PRR*S/DYPN
C
C.....EXPLICIT CONVECTIVE FLUXES FOR UDS AND CDS
C
          CN=MIN(F2(IJ),0.)
          CP=MAX(F2(IJ),0.)
C
          FUDS=CP*T(IJ)+CN*T(IJN)
          FCDS=F2(IJ)*(T(IJN)*FYN+T(IJ)*FYP)
C
C.....COEFFICIENTS AE(P) AND AW(E) DUE TO UDS
C
          AN(IJ) = CN-D
          AS(IJN)=-CP-D
C
C.....SOURCE TERM CONTRIBUTIONS AT P AND E DUE TO DEFERRED CORRECTION
C
          SU(IJ) =SU(IJ) +GDS(IEN)*(FUDS-FCDS)
          SU(IJN)=SU(IJN)-GDS(IEN)*(FUDS-FCDS)
C
       END DO
      END DO
C
C=============================================================
C.....VOLUME INTEGRALS (SOURCE TERMS)
C=============================================================
C
      DO I=2,NIM
        DX=X(I)-X(I-1)
C
        DO J=2,NJM
          IJ=LI(I)+J
          DY=Y(J)-Y(J-1)
          RP=0.5*(R(J)+R(J-1))
          VOL=DX*DY*RP
C
C..... UNSTEADY TERM CONTRIBUTION TO AP AND SU
C
          IF(LTIME) THEN
            APT=DENSIT*VOL*DTR
            SU(IJ)=SU(IJ)+(1.+GAMT)*APT*TO(IJ)-0.5*GAMT*APT*TOO(IJ)
            AP(IJ)=AP(IJ)+(1.+0.5*GAMT)*APT
          ENDIF
C
        END DO
      END DO
C
C=============================================================
C.....PROBLEM MODIFICATIONS - BOUNDARY CONDITIONS
C=============================================================
C
      CALL BCT
C
C==============================================================
C.....UNDER-RELAXATION, SOLVING EQUATION SYSTEM FOR TEMPERATURE
C==============================================================
C
      DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          AP(IJ)=(AP(IJ)-AW(IJ)-AE(IJ)-AN(IJ)-AS(IJ))*URFI
          SU(IJ)=SU(IJ)+(1.-URF(IEN))*AP(IJ)*T(IJ)
        END DO
      END DO
C
      CALL SIPSOL(T,IEN)
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE PRINT(PHI,HEDPHI)
C#############################################################
C    This routine prints 2D array in an easy to read format.
C-------------------------------------------------------------- 
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      DIMENSION PHI(NXY)
      CHARACTER*6 HEDPHI
C--------------------------------------------------------
C
      WRITE(2,20) HEDPHI
      NL=(NI-1)/12+1
C
      DO L=1,NL
        IS=(L-1)*12+1
        IE=MIN(NI,L*12)
        WRITE(2,21) (I,I=IS,IE)
        WRITE(2,22)
C
        DO J=NJ,1,-1
          WRITE(2,23) J,(PHI(LI(I)+J),I=IS,IE)
        END DO
      END DO
C
   20 FORMAT(2X,26('*-'),5X,A6,5X,26('-*'))
   21 FORMAT(3X,'I = ',I3,11I10)
   22 FORMAT(2X,'J')
   23 FORMAT(1X,I3,1P12E10.2)
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE SIPSOL(FI,IFI)
C#############################################################
C     This routine incorporates the Stone's SIP-solver, see
C     Chap. 5, Sect. 5.3.4.
C-------------------------------------------------------------- 
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),
     *       SU(NXY),SV(NXY),APU(NXY),APV(NXY)
      DIMENSION FI(NXY),UE(NXY),UN(NXY),RES(NXY)
      REAL LW(NXY),LS(NXY),LPR(NXY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
C-------------------------------------------------------------
      DATA UE,UN,RES /NXY*0.,NXY*0.,NXY*0./
C
C.....COEFFICIENTS OF UPPER AND LOWER TRIANGULAR MATRICES
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
C==============================================================
C.....INNER ITERATIONS LOOP
C==============================================================
C
      DO L=1,NSW(IFI)
        RESL=0.
C      
C.....CALCULATE RESIDUAL AND OVERWRITE IT BY INTERMEDIATE VECTOR
C
        DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          RES(IJ)=SU(IJ)-AN(IJ)*FI(IJ+1)-AS(IJ)*FI(IJ-1)-
     *            AE(IJ)*FI(IJ+NJ)-AW(IJ)*FI(IJ-NJ)-AP(IJ)*FI(IJ)
          RESL=RESL+ABS(RES(IJ))
          RES(IJ)=(RES(IJ)-LS(IJ)*RES(IJ-1)-LW(IJ)*RES(IJ-NJ))*LPR(IJ)
        END DO
        END DO
C
C.....STORE INITIAL RESIDUAL SUM FOR CHECKING CONV. OF OUTER ITER.
C
        IF(L.EQ.1) RESOR(IFI)=RESL
        RSM=RESL/(RESOR(IFI)+1.E-20)
C
C.....BACK SUBSTITUTION AND CORRECTION
C
        DO I=NIM,2,-1
        DO IJ=LI(I)+NJM,LI(I)+2,-1
          RES(IJ)=RES(IJ)-UN(IJ)*RES(IJ+1)-UE(IJ)*RES(IJ+NJ)
          FI(IJ)=FI(IJ)+RES(IJ)
        END DO
        END DO
C
C.....CHECK CONVERGENCE OF INNER ITERATIONS
C
        IF(LTEST) WRITE(2,*) '  ',L,'INNER ITER, RESL = ',RESL
        IF(RSM.LT.SOR(IFI)) RETURN
C
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE PBOUND(FI)
C#############################################################
C     This routine calculates boundary values of pressure or
C     pressure-correction by extrapolating (linearly) from
C     inside.
C-------------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      DIMENSION FI(NXY)
C--------------------------------------------------------------
C
C.....SOUTH AND NORTH BOUNDARIES
C
      DO I=2,NIM
        IJ=LI(I)+1
        FI(IJ)=FI(IJ+1)+(FI(IJ+1)-FI(IJ+2))*FY(2)
        IJ=LI(I)+NJ
        FI(IJ)=FI(IJ-1)+(FI(IJ-1)-FI(IJ-2))*(1.-FY(NJM-1)) 
      END DO 
C
C..... WEST AND EAST BOUNDARIES
C
      NJ2=2*NJ
      DO J=2,NJM
        IJ=LI(1)+J
        FI(IJ)=FI(IJ+NJ)+(FI(IJ+NJ)-FI(IJ+NJ2))*FX(2) 
        IJ=LI(NI)+J
        FI(IJ)=FI(IJ-NJ)+(FI(IJ-NJ)-FI(IJ-NJ2))*(1.-FX(NIM-1))
      END DO
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE BCUV
C#############################################################
C     In this routine, boundary conditions for U and V equations
C     are implemented, i.e. fluxes through boundary cell faces
C     are approximated. Here, the boundaries encountered in 
C     cavity flows are considered; inlet and outlet boundaries
C     require different treatment, see Sect. 7.7.
C-------------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),
     *       SU(NXY),SV(NXY),APU(NXY),APV(NXY)
C---------------------------------------------------------------
C
C.....SOUTH BOUNDARY (WALL; SHEAR FORCE IN X-DIR, DV/DY=0)
C
      DO I=2,NIM
        IJ=LI(I)+2
        D=VISC*(X(I)-X(I-1))*R(1)/(YC(2)-YC(1))
        APU(IJ)=APU(IJ)+D
        SU(IJ) =SU(IJ) +D*U(IJ-1)
      END DO
C
C.....NORTH BOUNDARY (WALL, SHEAR FORCE IN X-DIR, DV/DY=0)
C
      DO I=2,NIM
        IJ=LI(I)+NJM
        D=VISC*(X(I)-X(I-1))*R(NJM)/(YC(NJ)-YC(NJM))
        APU(IJ)=APU(IJ)+D
        SU(IJ) =SU(IJ) +D*U(IJ+1)
      END DO
C
C.....WEST BOUNDARY (WALL, SHEAR FORCE IN Y-DIR, DU/DX=0)
C
      DO J=2,NJM
        IJ=LI(2)+J
        D=0.5*VISC*(Y(J)-Y(J-1))*(R(J)+R(J-1))/(XC(2)-XC(1))
        APV(IJ)=APV(IJ)+D
        SV(IJ) =SV(IJ) +D*V(IJ-NJ)
      END DO 
C
C.....EAST BOUNDARY (WALL, SHEAR FORCE IN Y-DIR, DU/DX=0)
C
      DO J=2,NJM
        IJ=LI(NIM)+J
        D=0.5*VISC*(Y(J)-Y(J-1))*(R(J)+R(J-1))/(XC(NI)-XC(NIM))
        APV(IJ)=APV(IJ)+D
        SV(IJ) =SV(IJ) +D*V(IJ+NJ)
      END DO
C
      RETURN
      END
C
C#############################################################
      SUBROUTINE BCT
C#############################################################
C     In this routine, boundary conditions for the temperature 
C     equation are implemented, i.e. heat fluxes through the
C     boundary cell faces are calculated. Here, specified wall 
C     temperature and adiabatic wall (zero heat flux) are considered;
C     treatment at symmetry planes is the same as for an adiabatic
C     wall, but inlet and outlet require different treatment,
C     see Sect. 7.7.
C-------------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /COEF/ AE(NXY),AW(NXY),AN(NXY),AS(NXY),AP(NXY),
     *       SU(NXY),SV(NXY),APU(NXY),APV(NXY)
C--------------------------------------------------------------
C
C.....SOUTH BOUNDARY (ADIABATIC WALL, DT/DY=0, ZERO FLUX)
C
      DO I=2,NIM
        IJ=LI(I)+1
        T(IJ)=T(IJ+1)
      END DO
C
C.....NORTH BOUNDARY (ADIABATIC WALL, DT/DY=0, ZERO FLUX)
C
      DO I=2,NIM
        IJ=LI(I)+NJ
        T(IJ)=T(IJ-1)
      END DO
C
C.....WEST BOUNDARY (ISOTHERMAL WALL, NON-ZERO DIFFUSIVE FLUX)
C
      DO J=2,NJM
        IJ=LI(2)+J
        D=0.5*VISC*PRR*(Y(J)-Y(J-1))*(R(J)+R(J-1))/(XC(2)-XC(1))
        AP(IJ)=AP(IJ)+D
        SU(IJ)=SU(IJ)+D*T(IJ-NJ)
      END DO
C
C..... EAST BOUNDARY (ISOTHERMAL WALL)
C
      DO J=2,NJM
        IJ=LI(NIM)+J
        D=0.5*VISC*PRR*(Y(J)-Y(J-1))*(R(J)+R(J-1))/(XC(NI)-XC(NIM))
        AP(IJ)=AP(IJ)+D
        SU(IJ)=SU(IJ)+D*T(IJ+NJ)
      END DO
C
      RETURN
      END
C
C############################################################
      SUBROUTINE MODINP
C############################################################
C     In this routine, input data is read, boundary conditions
C     are defined, the grid is set up, and the variable values
C     are initialized.
C-------------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      COMMON /VAROLD/ TIME,DT,DTR,UO(NXY),VO(NXY),TO(NXY),
     *       UOO(NXY),VOO(NXY),TOO(NXY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
      CHARACTER TITLE*80
C-----------------------------------------------------------
C
C-----------------------------------------------------------
C.....READ INPUT DATA FROM UNIT 5
C-----------------------------------------------------------
C
      READ(5,6) TITLE
    6 FORMAT(A80)
C
C     If LREAD is set true, results from previous run are read
C     before starting computation; if LWRITE is set true, the
C     results of calculation are written onto a file for post-
C     processing or continuation in a later run; if LTEST is set
C     true, additional output is printed (global mass conservation
C     check, convergence of inner iterations); if LOUTS is set
C     true, the initial field values of ll variables are printed 
C     out; if LOUTE is set true, the final values of all variables
C     are printed out; if LTIME is set true, unsteady calculation
C     is performed. 
C
      READ(5,*) LREAD,LWRITE,LTEST,LOUTS,LOUTE,LTIME
C
C     MAXIT is the maximum number of outer iterations to be performed
C     (e.g. one can run 10 outer iterations with LTEST=TRUE to check
C     if everything is OK, then resume calculation with LTEST=FALSE);
C     IMON and JMON are the I and J index of the monitoring location
C     (variable values at this location are printed after every outer
C     iteration); IPR and JPR are the I and J index of the node at
C     which the pressure is kept fixed (usually zero; reference
C     location); SORMAX is the level of the residual norm at which
C     outer iterations are stoped (converged solution); SLARGE is the 
C     level of the residual norm at which iterations are stoped because
C     of divergence; ALFA is the parameter in the SIP solver (see
C     example input file PCOL.INP for typical values).
C
      READ(5,*) MAXIT,IMON,JMON,IPR,JPR,SORMAX,SLARGE,ALFA
C
C     DENSIT is the fluid density (here assumed constant); VISC is
C     the fluid dynamic viscosity (here assumed constant); PRM is
C     the fluid Prandtl number; GRAVX and GRAVY are the X and Y
C     component of the gravity vector; BETA is the volumetric
C     expansion factor for the fluid; TH, TC, and TREF are the hot
C     wall, cold wall, and reference temperature, respectively.
C
      READ(5,*) DENSIT,VISC,PRM,GRAVX,GRAVY,BETA,TH,TC,TREF
C
C     UIN, VIN, PIN, and TIN are the values of U, V, P, and T used
C     to initialize fields (usually zero, or some mean values);
C     ULID is the lid velocity for the lid-driven flow; TPER is the
C     oscillation period in the case of unsteady flow with oscillating
C     lid.
C
      READ(5,*) UIN,VIN,PIN,TIN,ULID,TPER
C
C     ITST is the number of time steps to be performed (1 if steady 
C     flow is considered); results are saved on file for every NPRTth
C     time step; DT is the time step size; GAMT is the blending factor
C     for time differencing schemes (GAMT=1 -> three time levels scheme,
C     GAMT=0 -> Euler implicit scheme).
C
      READ(5,*) ITST,NPRT,DT,GAMT
C
C     LCAL(I) defines which equations are to be solved (I defines 
C     variable as follows: 1 -> U, 2 -> V, 3 -> P', 4 -> T).
C
      READ(5,*) (LCAL(I),I=1,NPHI)
C
C     URF(I) is the under-relaxation factor for the Ith variable.
C
      READ(5,*) (URF(I),I=1,NPHI)
C
C     SOR(I) is the required ratio of reduction of the residual norm
C     during inner iterations for Ith variable before they are stoped 
C     (e.g. value 0.2 means the residual norm should be reduced by a
C     factor of 5; this is usually sufficient for inner iterations
C     before updating the matrix).
C
      READ(5,*) (SOR(I),I=1,NPHI)
C
C     NSW(I) is the maximum allowed number of inner iterations for the
C     Ith variable (for U, V, and T, one inner iteration by SIP is
C     sufficiant; for P', 5 to 10 may be required to satisfy the 
C     convergencee criterion).
C
      READ(5,*) (NSW(I),I=1,NPHI)
C
C     GDS(I) is the blending factor for UDS and CDS in the equation for
C     Ith variable (convective terms; value 1.0 means CDS (second order), 
C     0.0 means UDS (first order), any value between 0.0 and 1.0 can
C     be used). The value 1.0 is recomended, except for coarse grids,
C     in case convergence problems are encountered.
C
      READ(5,*) (GDS(I),I=1,NPHI)
C
C.....SET SOME CONTROL VARIABLES
C
      IU=1
      IV=2
      IP=3
      IEN=4
      SMALL=1.E-15
      GREAT=1.E15
      DTR=1./DT
      OM=8.*ATAN(1.)/TPER
      PRR=1./PRM
C
C-----------------------------------------------------------
C.....READ GRID DATA (GENERATED USING GRID GENERATOR FOR MG.)
C-----------------------------------------------------------
C
C     Note: array dimensions NX and NY should be equal to or larger
C     than NI and NJ (which are equal to the number of CVs in X and
C     Y direction plus two boundary nodes, respectively). LI(I) is
C     used for conversion of 2D into 1D indices, see Table 3.1).
C
      READ(1,*) I
      READ(1,*) J
      READ(1,*) NI
      READ(1,*) NJ
      READ(1,*) IJ
      READ(1,*) (X(I),I=1,NI)
      READ(1,*) (Y(J),J=1,NJ)
      DO I=1,NI
        LI(I)=(I-1)*NJ
      END DO
      NIM=NI-1
      NJM=NJ-1
      NIJ=NI*NJ
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
C.....INTERPOLATION FACTORS (see Sect. 4.4.2, Eq. (4.14))
C
      DO I=1,NIM
        FX(I)=(X(I)-XC(I))/(XC(I+1)-XC(I))
      END DO
C
      DO J=1,NJM
        FY(J)=(Y(J)-YC(J))/(YC(J+1)-YC(J))
      END DO
C
C.....SET RADIUS (1. FOR PLANE, Y(J) FOR AXI-SYMMETRIC GEOMETRY)
C
      IF(LAXIS) THEN
        DO J=1,NJ
          R(J)=Y(J)
        END DO
      ELSE
        DO J=1,NJ
          R(J)=1.
        END DO
      ENDIF
C
C---------------------------------------------------
C.....BOUNDARY AND INITIAL CONDITIONS
C---------------------------------------------------
C
C.....WEST AND EAST ISOTHERMAL BOUNDARIES
C
      DO J=1,NJ
        T(J)=TH
      END DO
C
      DO J=1,NJ
        T(LI(NI)+J)=TC
      END DO
C
C.....NORTH WALL VELOCITY (FOR LID-DRIVEN CAVITY)
C
      IF(LTIME) THEN
        CALL BCTIME
      ELSE
        DO I=2,NIM
          U(LI(I)+NJ)=ULID
        END DO
      ENDIF
C
C.....INITIAL VARIBLE VALUES (INITIAL CONDITIONS)
C
      DO I=2,NIM
        DO IJ=LI(I)+2,LI(I)+NJM
          U(IJ)=UIN
          V(IJ)=VIN
          T(IJ)=TIN
          P(IJ)=PIN
          UO(IJ)=UIN
          VO(IJ)=VIN
          TO(IJ)=TIN
        END DO
      END DO
C
C------------------------------------------------------
C.....INITIAL OUTPUT - PRINTOUT OF FLOW PARAMETERS
C------------------------------------------------------
C
      WRITE(2,601) TITLE,DENSIT,VISC
  601 FORMAT(1H1,//,10X,A80,/,10X,50('*'),/,10X,
     *        ' FLUID DENSITY    :  ',1P1E10.4,/,10X,
     *        ' DYNAMIC VISCOSITY:  ',1P1E10.4)
      IF(ULID.NE.0.) THEN
        WRITE(2,*) '          MAX. LID VELOCITY:  ',ULID
      ENDIF
      IF(LCAL(IEN)) THEN
        WRITE(2,*) '          GRAVITY IN X-DIR.:  ',GRAVX
        WRITE(2,*) '          GRAVITY IN Y-DIR.:  ',GRAVY
        WRITE(2,*) '          HOT  WALL TEMPER.:  ',TH
        WRITE(2,*) '          COLD WALL TEMPER.:  ',TC
        WRITE(2,*) '          PRANDTL NUMBER   :  ',PRM
      ENDIF
      WRITE(2,*) '  '
      WRITE(2,*) '          ALFA  PARAMETER  :  ',ALFA
      WRITE(2,*) '  '
      WRITE(2,*) '          UNDERRELAXATION  FACTORS'
      WRITE(2,*) '          ========================'
      WRITE(2,*) '          U-VELOCITY  :  ',URF(IU)
      WRITE(2,*) '          V-VELOCITY  :  ',URF(IV)
      WRITE(2,*) '          PRESSURE    :  ',URF(IP)
      WRITE(2,*) '          TEMPERATURE :  ',URF(IEN)
      WRITE(2,*) '  '
      WRITE(2,*) '          SPATIAL BLENDING FACTORS (CDS-UDS)'
      WRITE(2,*) '          =================================='
      WRITE(2,*) '          U-VELOCITY  :  ',GDS(IU)
      WRITE(2,*) '          V-VELOCITY  :  ',GDS(IV)
      WRITE(2,*) '          TEMPERATURE :  ',GDS(IEN)
      WRITE(2,*) '  '
      IF(LTIME) THEN
        WRITE(2,*) '          UNSTEADY FLOW SIMULATION'
        WRITE(2,*) '          ================================='
        WRITE(2,*) '          TIME STEP SIZE       : ',DT
        WRITE(2,*) '          BLEND. FACTOR (3L-IE): ',GAMT
        WRITE(2,*) '          OSCILLATION PERIOD   : ',TPER
      ENDIF
      WRITE(2,*) '  '
      WRITE(2,*) '  '
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE BCTIME
C###########################################################
C     Setting boundary values for each time step
C-----------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /RCONT/ SOR(NPHI),RESOR(NPHI),URF(NPHI),GAMT,
     *       BETA,GRAVX,GRAVY,GDS(NPHI),SORMAX,SLARGE,ALFA,
     *       GREAT,SMALL,ULID,OM,TPER
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /VAROLD/ TIME,DT,DTR,UO(NXY),VO(NXY),TO(NXY),
     *       UOO(NXY),VOO(NXY),TOO(NXY)
C-----------------------------------------------------------
C
C.....LID VELOCITY AS A FUNCTION OF TIME
C
      DO I=2,NIM
        IJ=LI(I)+NJ
        U(IJ)=ULID*SIN(OM*TIME)
      END DO
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE OUT2
C##########################################################
C     Output of some integral quantities for checking
C     convergence towards grid-independent solution and
C     estimation of discretization errors
C----------------------------------------------------------
      PARAMETER (NX=82,NY=82,NXY=NX*NY,NPHI=4)
      COMMON /ICONT/ NI,NJ,NIM,NJM,NIJ,NITP,LI(NX),IMON,
     *       JMON,IJMON,MAXIT,IU,IV,IP,IEN,IPR,JPR,NSW(NPHI),
     *       ITIM,ITST,NPRT 
      COMMON /LOGIC/ LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,
     *       LCAL(NPHI),LTIME
      COMMON /VAR/ DENSIT,VISC,PRM,PRR,FLOMAS,FLOMOM,TREF,
     *       U(NXY),V(NXY),P(NXY),PP(NXY),T(NXY),F1(NXY),
     *       F2(NXY),DPX(NXY),DPY(NXY)
      COMMON /GEO/ X(NX),Y(NY),XC(NX),YC(NY),FX(NX),FY(NY),R(NY)
      LOGICAL LWRITE,LREAD,LTEST,LAXIS,LOUTS,LOUTE,LCAL,LTIME
C--------------------------------------------------------------
C
C.....HEAT FLUXES AT WEST AND EAST ISOTHERMAL WALLS
C
      WRITE(2,*) '  '
C
      IF(LCAL(IEN)) THEN
        QWALL=0.
        DO J=2,NJM
          IJ=LI(1)+J
          S=0.5*(R(J)+R(J-1))*(Y(J)-Y(J-1))
          D=VISC*PRR*S/(XC(2)-XC(1))
          QWALL=QWALL+D*(T(IJ+NJ)-T(IJ))
        END DO
        WRITE(2,*) '  HEAT FLUX THROUGH WEST WALL: ',QWALL
C
        QWALL=0.
        DO J=2,NJM
          IJ=LI(NI)+J
          S=0.5*(R(J)+R(J-1))*(Y(J)-Y(J-1))
          D=VISC*PRR*S/(XC(NI)-XC(NIM))
          QWALL=QWALL+D*(T(IJ)-T(IJ-NJ))
        END DO
        WRITE(2,*) '  HEAT FLUX THROUGH EAST WALL: ',QWALL
      ENDIF
C
C--------------------------------------------------------------------
C.....STREAMFUNCTION VALUES AT CV-VERTICES (ZERO AT SOUTH-WEST CORNER)
C--------------------------------------------------------------------
      PP(LI(1)+1)=0.
C
C.....WEST BOUNDARY (APPLICABLE FOR INLET OR OUTLET)
C
      DO J=2,NJM
        IJ=LI(1)+J
        PP(IJ)=PP(IJ-1)+F1(IJ)
      END DO
C
C.....SOUTH BOUNDARY (APPLICABLE FOR INLET OR OUTLET)
C
      DO I=2,NIM
        IJ=LI(I)+1
        PP(IJ)=PP(IJ)-F2(IJ)
C
C.....INNER REGION
C
        DO J=2,NJM
          IJ=LI(I)+J
          PP(IJ)=PP(IJ-1)+F1(IJ)
        END DO
      END DO
C       
C.....STRENGTH OF PRIMARY AND SECONDARY EDDY (MIN and MAX values)
C
      PSIMIN= 1.E20
      PSIMAX=-1.E20
C
      DO I=1,NIM
        DO J=1,NJM
          IJ=LI(I)+J
          PSIMIN=MIN(PSIMIN,PP(IJ))
          PSIMAX=MAX(PSIMAX,PP(IJ))
        END DO
      END DO
C
      WRITE(2,*) '  '
      WRITE(2,*) '  MAXIMUM STREAMFUNCTION VALUE:  ',PSIMAX 
      WRITE(2,*) '  MINIMUM STREAMFUNCTION VALUE:  ',PSIMIN
C 
      RETURN
      END
