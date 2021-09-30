C##########################################################
      PROGRAM CAFFA
C##########################################################
C     This code incorporates the Finite Volume Method using
C     SIMPLE algorithm on colocated body-fitted grids. For
C     a description of the solution method, see the book by
C     Ferziger and Peric (1996) or paper by Demirdzic,
C     Muzaferija and Peric (1996). Description of code 
C     features is provided in the acompanying README-file.
C
C     This is Version 1.2 of the code, October 1996.
C
C     The user may modify the code and give it to third
C     parties, provided that an acknowledgement to the
C     source of the original version is retained.
C
C                M. Peric, Hamburg, 1996
C                Milovan.Peric@t-online.de
C===========================================================
C NB: CAFFA stands for "Computer Aided Fluid Flow Analysis". 
C===========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'charac.inc'
C
C.....SET SOME CONSTANTS
C
      CALL SETDAT
C
C.....READ PROBLEM NAME AND OPEN FILES
C
      PRINT *, ' ENTER PROBLEM NAME (SIX CHARACTERS):  '
      READ(*,'(A6)') NAME
      WRITE( FILIN,'(A6,4H.cin)') NAME
      WRITE(FILOUT,'(A6,4H.out)') NAME
      WRITE(FILGRD,'(A6,4H.grd)') NAME
C
      OPEN (UNIT=5,FILE=FILIN)
      OPEN (UNIT=2,FILE=FILOUT)
      OPEN (UNIT=4,FILE=FILGRD,FORM='UNFORMATTED')
      REWIND 2
      REWIND 5
      REWIND 4
C
C.....INPUT DATA AND INITIALIZATION
C
      CALL INIT
C
C.....INITIAL OUTPUT
C
      CALL OUTIN
      ITIM=0
      TIME=0.
      KGST=0
      KGRD=1
C
C======================================================
C.....READ RESULTS OF PREVIOUS RUN IF REQUIRED
C======================================================
C
      IF(LREAD) THEN
        DO K=1,KIN
          WRITE(FILRES,'(A6,3H.re,I1)') NAME,K
          OPEN (UNIT=3,FILE=FILRES,FORM='UNFORMATTED')
          READ(3) KGRD,IJST,IJEN,ITIM,TIME,(F1(IJ),IJ=IJST,IJEN),
     *            (F2(IJ),IJ=IJST,IJEN),(U(IJ),IJ=IJST,IJEN),
     *            (V(IJ),IJ=IJST,IJEN),(P(IJ),IJ=IJST,IJEN),
     *            (T(IJ),IJ=IJST,IJEN),
     *            (FMOC(I),I=IOCS(KGRD)+1,IOCS(KGRD)+NOC(KGRD))
          IF(LTIME) READ(3) (UO(IJ),IJ=IJST,IJEN),(VO(IJ),IJ=IJST,IJEN),
     *                      (TO(IJ),IJ=IJST,IJEN)
          REWIND 3
          CLOSE(UNIT=3)
        END DO
C
        KGST=KGRD-1
        ITIM=ITIM-1
      ENDIF
      ICONT=0
C
C======================================================
C.....START GRID LEVELS LOOP
C======================================================
C
      DO 500 KGR=KGST+1,NGR
C
C.....EXTRAPOLATE RESULTS FROM COARSE TO FINE GRID
C
      IF(KGR.GT.KGRD) THEN
        IF(LCAL(IU)) CALL VINT(KGR-1,U)
        IF(LCAL(IV)) CALL VINT(KGR-1,V)
        IF(LCAL(IP)) CALL VINT(KGR-1,P)
        CALL PRESB(KGR,P)        
        IF(LCAL(IP)) CALL CALCP(KGR,0)
        IF(LCAL(IEN)) CALL VINT(KGR-1,T)
C
        IF(LTIME) THEN
          IF(LCAL(IU)) CALL VINT(KGR-1,UO)
          IF(LCAL(IV)) CALL VINT(KGR-1,VO)
          IF(LCAL(IEN)) CALL VINT(KGR-1,TO)
        ENDIF
      ENDIF
C
      IF(LTIME) THEN
        WRITE(FILTO,'(A6,3H.to,I1)') NAME,KGR
        OPEN(UNIT=10,FILE=FILTO)
        REWIND 10
      ENDIF
C
C======================================================
C.....START TIME LOOP
C======================================================
C
      INIBC=.TRUE.
      IF(LTIME) ICONT=0
      ITIMS=ITIM+1
      ITIME=ITIM+ITSTEP
      IF(LSG(KGR).EQ.0) ITIME=0
C
      DO 400 ITIM=ITIMS,ITIME
      TIME=TIME+DT
C
C.....SHIFT SOLUTIONS IN TIME (OOLD = OLD, OLD = CURRENT)
C
      IF(LTIME) THEN
        IJST=IJGR(KGR)+1
        IJEN=IJGR(KGR)+NIGR(KGR)*NJGR(KGR)
        DO IJ=IJST,IJEN
          UOO(IJ)=UO(IJ)
          VOO(IJ)=VO(IJ)
          TOO(IJ)=TO(IJ)
          UO(IJ)=U(IJ)
          VO(IJ)=V(IJ)
          TO(IJ)=T(IJ)
        END DO
C
        WRITE(2,*) '  '
        WRITE(2,*) '  TIME = ',TIME
        WRITE(2,*) '  *****************************'
      ENDIF
C
C.....SET REFERENCE PRESSURE LOCATION & INLET BOUNDARY CONDITIONS 
C
      IST=IGR(KGR)
      IIM=2**(KGR-1)*(IPR-1)+1
      JJM=2**(KGR-1)*(JPR-1)+1
      IJPR=LI(IIM+IST)+JJM
C
      IF(INIBC) CALL BCIN(KGR)
C
C.....PRINT INITAL FIELDS
C
      IF(LOUTS.AND.(ITIM.EQ.ITIMS)) CALL OUTRES(KGR)
C
C.....SET MONITORING LOCATION 
C
      IIM=2**(KGR-1)*(IMON-1)+1
      JJM=2**(KGR-1)*(JMON-1)+1
      IJMON=LI(IIM+IST)+JJM
C
      WRITE(2,600) KGR,IIM,JJM
C
C======================================================
C.....START SIMPLE RELAXATIONS (OUTER ITERATIONS)
C======================================================
C
      DO LS=1,LSG(KGR)
        IF(LCAL(IU))   CALL CALCUV(KGR)
        IF(LCAL(IP))   CALL CALCP(KGR,1)
        IF(LCAL(IEN))  CALL CALCSC(KGR,IEN,T,TO,TOO)
C
C.....NORMALIZE RESIDUALS, PRINT RES. LEVELS AND MONITORING VALUES
C
        DO L=1,NFI
          RESOR(L)=RESOR(L)*RNOR(L)
        END DO
        WRITE(2,606) KGR,LS,LS,(RESOR(I),I=1,4),U(IJMON),V(IJMON),
     *          P(IJMON),T(IJMON)
        IF (.NOT.LTIME) WRITE(*,606) KGR,LS,LS,(RESOR(I),I=1,4),
     *          U(IJMON),V(IJMON),P(IJMON),T(IJMON)
        SOURCE=MAX(RESOR(IU),RESOR(IV),RESOR(IP),RESOR(IEN))
        IF(SOURCE.GT.SLARGE) GO TO 510
        IF(SOURCE.LT.SORMAX) GO TO 250
      END DO
C
  250 CONTINUE
C
C==========================================================
C.....SAVE SOLUTIONS FOR RE-START OR POSTPROCESSING; OUTPUT
C==========================================================
C
C.....UNSTEADY FLOW - INTERMEDIATE SOLUTIONS:
C
      IF(LTIME) THEN
        WRITE(*,606) KGR,LS,LS,(RESOR(I),I=1,4),
     *          U(IJMON),V(IJMON),P(IJMON),T(IJMON)
        CALL TOUT(KGR)
C
        IF(MOD(ITIM,NOTT).EQ.0.AND.LWRITE) THEN
          ICONT=ICONT+1
          CALL POST(KGR,ICONT)
          IF(LOUTE) CALL OUTRES(KGR)
          CALL SOUT(KGR)
        ENDIF
      ENDIF
C
  400 CONTINUE
C
C.....STEADY FLOW, OR UNSTEADY FLOW - LAST TIME STEP: PRINT AND
C     SAVE RESULTS IF COMPUTATION WAS PERFORMED ON THIS GRID LEVEL
C
      IF(LSG(KGR).GT.0) THEN
        ICONT=ICONT+1
        CALL POST(KGR,ICONT)
        IF(LOUTE) CALL OUTRES(KGR)
        CALL SOUT(KGR)
        CALL SRES(KGR)
        IF(LTIME) CLOSE(UNIT=10)
        ITIM=0
        TIME=0.
      ENDIF
C
  500 CONTINUE
C
C==========================================================
C.....CLOSE FILES, FORMATS
C==========================================================
C
      CLOSE(UNIT=8)
      CLOSE(UNIT=3)
      CLOSE(UNIT=4)
      CLOSE(UNIT=2)
      CLOSE(UNIT=5)
C
      PRINT *,'     *** CALCULATION FINISHED - SEE RESULTS ***'
      STOP
C
C.....MESSAGE FOR DIVERGENCE 
C
  510 PRINT *,'     *** TERMINATED - OUTER ITERATIONS DIVERGING ***'
C
C.....FORMAT SPECIFICATIONS
C
  600 FORMAT(1X,'GRID',1X,'CYCLE',1X,'ITER',2X,
     * 'I------ABSOLUTE RESIDUAL SOURCE SUMS--------I',1X,
     * 'I------FIELD VALUES AT MONITORING LOCATION(',1X,I3,',',I3,
     * ',',I3,',)--I',/,1X,3('NO',4X),4X,'UMOM',6X,'VMOM',6X,'MASS',
     * 6X,'ENER',16X,'U',9X,'V',9X,'P',
     * 9X,'T',/)
  606 FORMAT(1X,I2,3X,I3,2X,I4,6X,1P4E10.3,8X,1P4E10.3)
      END
C
C
C##########################################################
      SUBROUTINE VINT(K,FI)
C##########################################################
C     This routine interpolates coarse grid solution to the
C     fine grid, using gradient vector at coarse grid nodes
C     and position vectors of fine grid nodes relative to
C     coarse grid nodes (CV centers).
C==========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'grad.inc'
      DIMENSION FI(NXYA)
C
C.....CALCULATE GRADIENT VECTOR COMPONENTS ON COARSE GRID
C
      CALL SETIND(K)
      CALL GRADFI(K,FI,DUX,DUY)
C
C.....INTERPOLATE FOR FINE GRID CV-CENTERS USING GRAD. VECTOR
C
      NJF=NJGR(K+1)
      ISTF=IGR(K+1)
C
      DO IG=2,NIM
        IF=2*IG-2
        DO JG=2,NJM
          IJG=LI(IG+IST)+JG
          JF=2*JG-2
          IJF=LI(IF+ISTF)+JF
          FI(IJF)=FI(IJG)+FICF(IJG,IJF)
          FI(IJF+1)=FI(IJG)+FICF(IJG,IJF+1)
          FI(IJF+NJF)=FI(IJG)+FICF(IJG,IJF+NJF)
          FI(IJF+NJF+1)=FI(IJG)+FICF(IJG,IJF+NJF+1)
        END DO
      END DO
C
      RETURN
      END
C
C
C#########################################################
      FUNCTION FICF(IJG,IJF)
C#########################################################
C     This function calculates dot product of the gradient
C     vector at a coarse grid node with position vector of
C     a fine grid node relative to coarse grid node.
C=========================================================
      INCLUDE 'param.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'grad.inc'
C
      FICF=DUX(IJG)*(XC(IJF)-XC(IJG))+DUY(IJG)*(YC(IJF)-YC(IJG))
C
      RETURN
      END
C
C
C#########################################################
      SUBROUTINE CALCUV(K)
C#########################################################
C     This routine discretizes and solves the linearized
C     equations for X and Y momentum componentS (U and V
C     Cartesian velocity components). 
C=========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'coef.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'grad.inc'
      INCLUDE 'bound.inc'
C
C.....CALCULATE GRADIENT VECTOR COMPONENTS AT CV-CENTER FOR U, V & P
C
      CALL SETIND(K)
      CALL GRADFI(K,U,DUX,DUY)
      CALL GRADFI(K,V,DVX,DVY)
      CALL GRADFI(K,P,DPX,DPY)
C
C.....INITIALIZE ARRAYS, SET BLENDING FACTOR
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIJ
C
      DO IJ=IJST,IJEN
        SU(IJ)=0.
        SV(IJ)=0.
        AP(IJ)=0.
        APR(IJ)=0.
      END DO
C
      GU=GDS(1)
C
C.....CALCULATE FLUXES THROUGH INNER CV-FACES: EAST
C
      DO I=2,NIM-1
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        CALL FLUXUV(IJ,IJ+NJ,IJ,IJ-1,F1(IJ),AW(IJ+NJ),AE(IJ),FX(IJ),GU)
      END DO
      END DO
C
C.....CALCULATE FLUXES THROUGH INNER CV-FACES: NORTH
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM-1
        CALL FLUXUV(IJ,IJ+1,IJ-NJ,IJ,F2(IJ),AS(IJ+1),AN(IJ),FY(IJ),GU)
      END DO
      END DO
C
C.....BUOYANCY SOURCE CONTRIBUTION
C
      IF(LCAL(IEN)) THEN
        DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          SB=-BETA*DEN(IJ)*VOL(IJ)*(T(IJ)-TREF)
          SU(IJ)=SU(IJ)+GRAVX*SB
          SV(IJ)=SV(IJ)+GRAVY*SB
        END DO
        END DO
      ENDIF
C
C.....AXISYMMETRIC CONTRIBUTION
C
      IF(LAXIS) THEN
        DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          RCR=4./(R(IJ)+R(IJ-1)+R(IJ-NJ)+R(IJ-NJ-1))
          APR(IJ)=APR(IJ)+2.*VIS(IJ)*VOL(IJ)*RCR**2
        END DO
        END DO
      ENDIF
C
C.....PRESSURE SOURCE CONTRIBUTION
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        SU(IJ)=SU(IJ)-DPX(IJ)*VOL(IJ)
        SV(IJ)=SV(IJ)-DPY(IJ)*VOL(IJ)
      END DO
      END DO
C
C.....UNSTEADY TERM CONTRIBUTION (GAMT = 0 -> IMPLICIT EULER;
C.....GAMT = 1 -> THREE TIME LEVELS; BLENDING POSSIBLE)
C
      IF(LTIME) THEN
        DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          APT=DEN(IJ)*VOL(IJ)*DTR
          SU(IJ)=SU(IJ)+APT*((1.+GAMT)*UO(IJ)-0.5*GAMT*UOO(IJ))
          SV(IJ)=SV(IJ)+APT*((1.+GAMT)*VO(IJ)-0.5*GAMT*VOO(IJ))
          APR(IJ)=APR(IJ)+APT*(1.+0.5*GAMT)
          AP(IJ)= AP(IJ) +APT*(1.+0.5*GAMT)
        END DO
        END DO
      ENDIF
C
C.....INLET BOUNDARIES (CONSTANT GRADIENT BETWEEN BOUNDARY & CV-CENTER ASSUMED)
C
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJP=IJPI(II)
        IJB=IJI(II)
        DUX(IJB)=DUX(IJP)
        DUY(IJB)=DUY(IJP)
        DVX(IJB)=DVX(IJP)
        DVY(IJB)=DVY(IJP)
        CALL FLUXUV(IJP,IJB,IJI1(II),IJI2(II),FMI(II),CP,CB,ONE,ZERO)
        AP(IJP) =AP(IJP) -CB
        APR(IJP)=APR(IJP)-CB
        SU(IJP)=SU(IJP)-CB*U(IJB)
        SV(IJP)=SV(IJP)-CB*V(IJB)
      END DO
C
C.....OUTLET BOUNDARIES (CONSTANT GRADIENT BETWEEN BOUNDARY & CV-CENTER ASSUMED)
C
      DO IO=IOS(K)+1,IOS(K)+NOUT(K)
        IJP=IJPO(IO)
        IJB=IJO(IO)
        DUX(IJB)=DUX(IJP)
        DUY(IJB)=DUY(IJP)
        DVX(IJB)=DVX(IJP)
        DVY(IJB)=DVY(IJP)
        CALL FLUXUV(IJP,IJB,IJO1(IO),IJO2(IO),FMO(IO),CP,CB,ONE,ZERO)
        AP(IJP)= AP(IJP) -CB
        APR(IJP)=APR(IJP)-CB
        SU(IJP)=SU(IJP)-CB*U(IJB)
        SV(IJP)=SV(IJP)-CB*V(IJB)
      END DO
C
C.....WALL BOUNDARIES
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IJP=IJPW(IW)
        IJB=IJW(IW)
        COEF=VIS(IJB)*SRDW(IW)
        AP(IJP) =AP(IJP) +COEF*XTW(IW)**2
        APR(IJP)=APR(IJP)+COEF*YTW(IW)**2
        SU(IJP)=SU(IJP)+COEF*(U(IJB)*XTW(IW)**2-
     *         (V(IJP)-V(IJB))*XTW(IW)*YTW(IW))
        SV(IJP)=SV(IJP)+COEF*(V(IJB)*YTW(IW)**2-
     *         (U(IJP)-U(IJB))*XTW(IW)*YTW(IW))
      END DO
C
C.....SYMMETRY BOUNDARIES
C
      DO IS=ISS(K)+1,ISS(K)+NSYM(K)
        IJP=IJPS(IS)
        IJB=IJS(IS)
        C1=2.*XNS(IS)*YNS(IS)
        COEF=VIS(IJB)*SRDS(IS)
        AP(IJP) =AP(IJP) +COEF*XNS(IS)**2
        APR(IJP)=APR(IJP)+COEF*YNS(IS)**2
        SU(IJP)=SU(IJP)-COEF*(C1*V(IJP)+U(IJP)*XNS(IS)**2)
        SV(IJP)=SV(IJP)-COEF*(C1*U(IJP)+V(IJP)*YNS(IS)**2)
      END DO
C
C.....O- AND C-GRID CUTS (THESE ARE NOT BOUNDARIES!)
C
      DO I=IOCS(K)+1,IOCS(K)+NOC(K)
        IJP=IJL(I)
        IJN=IJR(I)
        CALL FLUXUV(IJP,IJN,IJOC1(I),IJOC2(I),FMOC(I),
     *              AL(I),AR(I),FOC(I),GU)
        AP(IJP) =AP(IJP) -AR(I)
        APR(IJP)=APR(IJP)-AR(I)
        AP(IJN) =AP(IJN) -AL(I)
        APR(IJN)=APR(IJN)-AL(I)
      END DO
C
C.....FINAL COEFFICIENT AND SOURCES MATRIX FOR U-EQUATION
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        AP(IJ)=(AP(IJ)-AE(IJ)-AW(IJ)-AN(IJ)-AS(IJ))*URFU
        SU(IJ)=SU(IJ)+(1.-URF(IU))*AP(IJ)*U(IJ)
      END DO
      END DO
C
C.....SOLVING EQUATION SYSTEM FOR U-VELOCITY USING SIP-SOLVER
C
      CALL SIPSOL(U,IU,K)
C 
C.....FINAL COEFFICIENT AND SOURCES MATRIX FOR V-EQUATION
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        AP(IJ)=(APR(IJ)-AE(IJ)-AW(IJ)-AN(IJ)-AS(IJ))*URFV
        SU(IJ)=SV(IJ)+(1.-URF(IV))*AP(IJ)*V(IJ)
        APR(IJ)=1./(AP(IJ)+SMALL)
      END DO
      END DO
C
C.....SOLVING EQUATION SYSTEM FOR V-VELOCITY USING SIP-SOLVER
C
      CALL SIPSOL(V,IV,K)
C
      RETURN
      END 
C
C
C################################################################
      SUBROUTINE FLUXUV(IJP,IJN,IJ1,IJ2,FM,CAP,CAN,FAC,G)
C################################################################
C     This routine calculates momentum fluxes (convective and
C     diffusive) through the cell face between nodes IJP and IJN. 
C     IJ1 and IJ2 are the indices of CV corners defining the cell 
C     face. FM is the mass flux through the face, and FAC is the 
C     interpolation factor (distance from node IJP to cell face 
C     center over the sum of this distance and the distance from 
C     cell face center to node IJN). CAP and CAN are the 
C     contributions to matrix coefficients in the momentum
C     equations at nodes IJP and IJN. Diffusive fluxes are
C     discretized using central differences; for convective
C     fluxes, linear interpolation can be blended with upwind
C     approximation; see Sect. 8.6 for details. Note: cell
C     face surface vector is directed from P to N.
C==============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'grad.inc'
      INCLUDE 'coef.inc'
      INCLUDE 'rcont.inc'
C
C.....INTERPOLATED CELL FACE VALUES
C
      FACP=1.-FAC
      UI=U(IJN)*FAC+U(IJP)*FACP
      VI=V(IJN)*FAC+V(IJP)*FACP
      VISI=VIS(IJN)*FAC+VIS(IJP)*FACP
      DUXI=DUX(IJN)*FAC+DUX(IJP)*FACP
      DVXI=DVX(IJN)*FAC+DVX(IJP)*FACP
      DUYI=DUY(IJN)*FAC+DUY(IJP)*FACP
      DVYI=DVY(IJN)*FAC+DVY(IJP)*FACP
C
C.....SURFACE AND DISTANCE VECTOR COMPONENTS, DIFFUSION COEFFICIENT
C
      RC=0.5*(R(IJ1)+R(IJ2))
      SX=(Y(IJ1)-Y(IJ2))*RC
      SY=(X(IJ2)-X(IJ1))*RC
      XPN=XC(IJN)-XC(IJP)
      YPN=YC(IJN)-YC(IJP)
      VSOL=VISI*SQRT((SX**2+SY**2)/(XPN**2+YPN**2))
C
C.....EXPLICIT CONVECTIVE AND DIFFUSIVE FLUXES
C
      FCUE=FM*UI
      FCVE=FM*VI
      FDUE=VISI*(2.*DUXI*SX+(DUYI+DVXI)*SY)
      FDVE=VISI*((DUYI+DVXI)*SX+2.*DVYI*SY)
C
C.....IMPLICIT CONVECTIVE AND DIFFUSIVE FLUXES
C
      FCUI=MIN(FM,ZERO)*U(IJN)+MAX(FM,ZERO)*U(IJP)
      FCVI=MIN(FM,ZERO)*V(IJN)+MAX(FM,ZERO)*V(IJP)
      FDUI=VSOL*(DUXI*XPN+DUYI*YPN)
      FDVI=VSOL*(DVXI*XPN+DVYI*YPN)
C
C.....COEFFICIENTS, DEFERRED CORRECTION, SOURCE TERMS
C
      CAN=-VSOL+MIN(FM,ZERO)
      CAP=-VSOL-MAX(FM,ZERO)
      FUC=G*(FCUE-FCUI)
      FVC=G*(FCVE-FCVI)
C
      SU(IJP)=SU(IJP)-FUC+FDUE-FDUI
      SU(IJN)=SU(IJN)+FUC-FDUE+FDUI
      SV(IJP)=SV(IJP)-FVC+FDVE-FDVI
      SV(IJN)=SV(IJN)+FVC-FDVE+FDVI
C
      RETURN
      END
C
C
C############################################################## 
      SUBROUTINE CALCP(K,INTMF) 
C############################################################## 
C     This routine assembles and solves the pressure-correction
C     equation using colocated grid. SIMPLE algorithm with one
C     corrector step (non-orthogonality effects neglected) is
C     applied.
C==============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'coef.inc'
      INCLUDE 'grad.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'varold.inc'
C
C.....SET INDICES, INITIALIZE FIELDS
C
      CALL SETIND(K)
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIJ
      SUM=0.
C
      DO IJ=IJST,IJEN
        SU(IJ)=0.
        AP(IJ)=0.
      END DO
C
C.....CALCULATE FLUXES THROUGH INNER CV-FACES (EAST & NORTH)
C
      DO I=2,NIM-1
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        CALL FLUXM(IJ,IJ+NJ,IJ,IJ-1,F1(IJ),AW(IJ+NJ),AE(IJ),FX(IJ))
      END DO
      END DO
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM-1
        CALL FLUXM(IJ,IJ+1,IJ-NJ,IJ,F2(IJ),AS(IJ+1),AN(IJ),FY(IJ))
      END DO
      END DO
C
C.....O- AND C-GRID CUTS
C
      DO I=IOCS(K)+1,IOCS(K)+NOC(K)
        IJP=IJL(I)
        IJN=IJR(I)
        CALL FLUXM(IJP,IJN,IJOC1(I),IJOC2(I),FMOC(I),
     *              AL(I),AR(I),FOC(I))
        AP(IJP)=AP(IJP)-AR(I)
        AP(IJN)=AP(IJN)-AL(I)
        SU(IJP)=SU(IJP)-FMOC(I)
        SU(IJN)=SU(IJN)+FMOC(I)
      END DO
C
C.....RETURN IF ONLY MASS FLUXES UPDATED !!!
C
      IF(INTMF.EQ.0) RETURN
C
C.....INLET BOUNDARIES (MASS FLUXES PRESCRIBED IN ROUTINE 'BCIN')
C
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        SU(IJPI(II))=SU(IJPI(II))-FMI(II)
      END DO
C
C.....EXTRAPOLATED VELOCITY AT OUTLET BOUNDARY, OUTLET MASS FLUXES
C
      FLOWO=0.
      DO IO=IOS(K)+1,IOS(K)+NOUT(K)
        IJB=IJO(IO)
        IJP=IJPO(IO)
        U(IJB)=U(IJP)
        V(IJB)=V(IJP)
        RS=0.5*(R(IJO1(IO))+R(IJO2(IO)))
        SX=(Y(IJO1(IO))-Y(IJO2(IO)))
        SY=(X(IJO2(IO))-X(IJO1(IO)))
        FMO(IO)=DEN(IJP)*(U(IJB)*SX+V(IJB)*SY)*RS
        FLOWO=FLOWO+FMO(IO)
      END DO
C
C.....CORRECT MASS FLUX TO SATISFY GLOBAL MASS CONSERVATION & ADD TO SOURCE
C
      FAC=FLOMAS/(FLOWO+SMALL)
      DO IO=IOS(K)+1,IOS(K)+NOUT(K)
        IB=IJO(IO)
        FMO(IO)=FMO(IO)*FAC
        U(IB)=U(IB)*FAC
        V(IB)=V(IB)*FAC
        SU(IJPO(IO))=SU(IJPO(IO))-FMO(IO)
      END DO
C
C.....SOURCE TERM AND CENTRAL COEFFICIENT (F1 and F2 are ZERO at boundaries!)
C
      DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          PP(IJ)=0.
          SU(IJ)=SU(IJ)+F1(IJ-NJ)-F1(IJ)+F2(IJ-1)-F2(IJ)
          AP(IJ)=AP(IJ)-AE(IJ)-AW(IJ)-AN(IJ)-AS(IJ)
          SUM=SUM+SU(IJ)
        END DO
      END DO
C    
C.....TEST GLOBAL MASS CONSERVATION & SOLVE EQUATIONS SYSTEM FOR P'
C
      IF(LTEST) WRITE(2,*) '         SUM = ',SUM
      CALL SIPSOL(PP,IP,K)
C
C.....UPDATE PRESSURE CORECTION AT BOUNDARIES
C
      CALL PRESB(K,PP)
C
C.....CALCULATE PRESSURE-CORRECTION GRADIENTS, REFERENCE P'
C
      CALL GRADFI(K,PP,DPX,DPY)
      PPO=PP(IJPR)
C
C.....CORRECT MASS FLUXES AT INNER CV-FACES
C
      DO I=2,NIM-1
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          F1(IJ)=F1(IJ)+AE(IJ)*(PP(IJ+NJ)-PP(IJ))
        END DO
      END DO
C
      DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM-1
          F2(IJ)=F2(IJ)+AN(IJ)*(PP(IJ+1)-PP(IJ))
        END DO
      END DO
C
      DO I=IOCS(K)+1,IOCS(K)+NOC(K)
        FMOC(I)=FMOC(I)+AR(I)*(PP(IJR(I))-PP(IJL(I)))
      END DO
C
C.....CORRECT PRESSURE AND VELOCITIES
C
      DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          U(IJ)=U(IJ)-DPX(IJ)*VOL(IJ)*APR(IJ)
          V(IJ)=V(IJ)-DPY(IJ)*VOL(IJ)*APR(IJ)
          P(IJ)=P(IJ)+URF(IP)*(PP(IJ)-PPO)
        END DO
      END DO
C
C.....UPDATE PRESSURE AT BOUNDARIES
C
      CALL PRESB(K,P)
C
C.....UPDATE VELOCITY COMPONENTS ALONG SYMMETRY BOUNDARIES
C
      DO IS=ISS(K)+1,ISS(K)+NSYM(K)
        IJP=IJPS(IS)
        IJB=IJS(IS)
        UN=U(IJP)*XNS(IS)+V(IJP)*YNS(IS)
        U(IJB)=U(IJP)-UN*XNS(IS)
        V(IJB)=V(IJP)-UN*YNS(IS)
      END DO
C
      RETURN
      END
C
C
C##############################################################
      SUBROUTINE FLUXM(IJP,IJN,IJ1,IJ2,FM,CAP,CAN,FAC)
C##############################################################
C     This routine calculates mass flux through the cell face 
C     between nodes IJP and IJN. IJ1 and IJ2 are the indices of 
C     CV corners defining the cell face. FM is the mass flux 
C     through the face, and FAC is the interpolation
C     factor (distance from node IJP to cell face center over
C     the sum of this distance and the distance from cell face 
C     center to node IJN). CAP and CAN are the contributions to
C     matrix coefficients in the pressure-correction equation
C     at nodes IJP and IJN. Surface vector directed from P to N.
C==============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'grad.inc'
      INCLUDE 'coef.inc'
C
C.....INTERPOLATED CELL FACE VALUES
C
      FACP=1.-FAC
      UI=U(IJN)*FAC+U(IJP)*FACP
      VI=V(IJN)*FAC+V(IJP)*FACP
      DENI=DEN(IJN)*FAC+DEN(IJP)*FACP
C
C.....SURFACE AND DISTANCE VECTOR COMPONENTS
C
      RC=0.5*(R(IJ1)+R(IJ2))
      SX=(Y(IJ1)-Y(IJ2))*RC
      SY=(X(IJ2)-X(IJ1))*RC
      XPN=XC(IJN)-XC(IJP)
      YPN=YC(IJN)-YC(IJP)
      SMDPN=(SX**2+SY**2)/(SX*XPN+SY*YPN)   
C
C.....MASS FLUX, COEFFICIENTS FOR THE P'-EQUATION
C
      CAP=-0.5*(VOL(IJP)*APR(IJP)+VOL(IJN)*APR(IJN))*DENI*SMDPN
      CAN=CAP
      DPXI=0.5*(DPX(IJN)+DPX(IJP))*XPN
      DPYI=0.5*(DPY(IJN)+DPY(IJP))*YPN
      FM=DENI*(UI*SX+VI*SY)+CAP*(P(IJN)-P(IJP)-DPXI-DPYI)
C
      RETURN
      END
C
C
C###############################################################
      SUBROUTINE PRESB(K,FI)
C###############################################################
C     This routine extrapolates the pressure or pressure
C     correction from interior to the boundary. Linear 
C     extrapolation is used, but one can also linearly
C     extrapolate the gradient...
C==============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'geo.inc'
      DIMENSION FI(NXYA)
C
C.....SET INDICES
C
      CALL SETIND(K)
C
C.....EXTRAPOLATE TO SOUTH AN NORTH BOUNDARIES
C
      DO I=2,NIM
        IJ=LI(I+IST)+1
        FI(IJ)=FI(IJ+1)+(FI(IJ+1)-FI(IJ+2))*FY(IJ+1)
        IJ=LI(I+IST)+NJ
        FI(IJ)=FI(IJ-1)+(FI(IJ-1)-FI(IJ-2))*(1.-FY(IJ-2)) 
      END DO 
C
C.....EXTRAPOLATE TO WEST AND EAST BOUNDARIES
C
      DO J=2,NJM
        IJ=LI(1+IST)+J
        FI(IJ)=FI(IJ+NJ)+(FI(IJ+NJ)-FI(IJ+NJ+NJ))*FX(IJ+NJ) 
        IJ=LI(NI+IST)+J
        FI(IJ)=FI(IJ-NJ)+(FI(IJ-NJ)-FI(IJ-NJ-NJ))*(1.-FX(IJ-NJ-NJ))
      END DO
C
      RETURN
      END
C
C
C###############################################################
      SUBROUTINE GRADFI(K,FI,DFX,DFY)
C###############################################################
C     This routine calculates the components of the gradient
C     vector of a scalar FI at the CV center, using conservative
C     scheme based on the Gauss theorem; see Sect. 8.6 for 
C     details. FIE are values at east side, FIN at north side.
C     Contributions from boundary faces are calculated in a
C     separate loops...
C===============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'var.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      DIMENSION FI(NXYA),DFX(NXYA),DFY(NXYA)
C
C.....SET INDICES, INITIALIZE FIELDS
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIJ
C
      DO IJ=IJST,IJEN
        DFX(IJ)=0.
        DFY(IJ)=0.
      END DO
C
C.....CONTRIBUTION FROM INNER EAST SIDES
C
      DO I=2,NIM-1
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          FIE=FI(IJ+NJ)*FX(IJ)+FI(IJ)*(1.-FX(IJ))
          RE=0.5*(R(IJ)+R(IJ-1))
          SX=(Y(IJ)-Y(IJ-1))*RE
          SY=(X(IJ-1)-X(IJ))*RE
          DFXE=FIE*SX
          DFYE=FIE*SY
C
          DFX(IJ)=DFX(IJ)+DFXE
          DFY(IJ)=DFY(IJ)+DFYE
          DFX(IJ+NJ)=DFX(IJ+NJ)-DFXE
          DFY(IJ+NJ)=DFY(IJ+NJ)-DFYE
        END DO
      END DO
C
C.....CONTRIBUTION FROM INNER NORTH SIDES
C
      DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM-1
          FIN=FI(IJ+1)*FY(IJ)+FI(IJ)*(1.-FY(IJ))
          RN =0.5*(R(IJ)+R(IJ-NJ))
          SX =(Y(IJ-NJ)-Y(IJ))*RN
          SY =(X(IJ)-X(IJ-NJ))*RN
          DFXN=FIN*SX
          DFYN=FIN*SY
C
          DFX(IJ)=DFX(IJ)+DFXN
          DFY(IJ)=DFY(IJ)+DFYN
          DFX(IJ+1)=DFX(IJ+1)-DFXN
          DFY(IJ+1)=DFY(IJ+1)-DFYN
        END DO
      END DO
C
C.....CONTRIBUTION FROM O- AND C-GRID CUTS
C
      DO I=IOCS(K)+1,IOCS(K)+NOC(K)
        IJP=IJL(I)
        IJN=IJR(I)
        FIE=FI(IJN)*FOC(I)+FI(IJP)*(1.-FOC(I))
        RE=0.5*(R(IJOC1(I))+R(IJOC2(I)))
        SX=(Y(IJOC1(I))-Y(IJOC2(I)))*RE
        SY=(X(IJOC2(I))-X(IJOC1(I)))*RE
        DFXE=FIE*SX
        DFYE=FIE*SY
C
        DFX(IJP)=DFX(IJP)+DFXE
        DFY(IJP)=DFY(IJP)+DFYE
        DFX(IJN)=DFX(IJN)-DFXE
        DFY(IJN)=DFY(IJN)-DFYE
      END DO
C
C.....CONTRIBUTION FROM INLET BOUNDARIES
C
      DO I=IIS(K)+1,IIS(K)+NINL(K)
        RB=0.5*(R(IJI1(I))+R(IJI2(I)))
        SX=(Y(IJI1(I))-Y(IJI2(I)))*RB
        SY=(X(IJI2(I))-X(IJI1(I)))*RB
        DFX(IJPI(I))=DFX(IJPI(I))+FI(IJI(I))*SX
        DFY(IJPI(I))=DFY(IJPI(I))+FI(IJI(I))*SY
      END DO
C
C.....CONTRIBUTION FROM OUTLET BOUNDARIES
C
      DO I=IOS(K)+1,IOS(K)+NOUT(K)
        RB=0.5*(R(IJO1(I))+R(IJO2(I)))
        SX=(Y(IJO1(I))-Y(IJO2(I)))*RB
        SY=(X(IJO2(I))-X(IJO1(I)))*RB
        DFX(IJPO(I))=DFX(IJPO(I))+FI(IJO(I))*SX
        DFY(IJPO(I))=DFY(IJPO(I))+FI(IJO(I))*SY
      END DO
C
C.....CONTRIBUTION FROM SYMMETRY BOUNDARIES
C
      DO I=ISS(K)+1,ISS(K)+NSYM(K)
        RB=0.5*(R(IJS1(I))+R(IJS2(I)))
        SX=(Y(IJS1(I))-Y(IJS2(I)))*RB
        SY=(X(IJS2(I))-X(IJS1(I)))*RB
        DFX(IJPS(I))=DFX(IJPS(I))+FI(IJS(I))*SX
        DFY(IJPS(I))=DFY(IJPS(I))+FI(IJS(I))*SY
      END DO
C
C.....CONTRIBUTION FROM WALL BOUNDARIES
C
      DO I=IWS(K)+1,IWS(K)+NWAL(K)
        RB=0.5*(R(IJW1(I))+R(IJW2(I)))
        SX=(Y(IJW1(I))-Y(IJW2(I)))*RB
        SY=(X(IJW2(I))-X(IJW1(I)))*RB
        DFX(IJPW(I))=DFX(IJPW(I))+FI(IJW(I))*SX
        DFY(IJPW(I))=DFY(IJPW(I))+FI(IJW(I))*SY
      END DO
C
C.....AXISYMMETRIC CONTRIBUTION (FRONT AND REAR CELL FACE)
C
      IF(LAXIS) THEN
        DO I=2,NIM
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
            AREA=0.5*((X(IJ)-X(IJ-NJ-1))*(Y(IJ-NJ)-Y(IJ-1))-
     *                (Y(IJ)-Y(IJ-NJ-1))*(X(IJ-NJ)-X(IJ-1)))
            DFY(IJ)=DFY(IJ)-FI(IJ)*AREA
          END DO
        END DO
      ENDIF
C
C.....CALCULATE GRADIENT COMPONENTS AT CV-CENTERS
C
      DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          DFX(IJ)=DFX(IJ)/VOL(IJ)
          DFY(IJ)=DFY(IJ)/VOL(IJ)
        END DO
      END DO
C
      RETURN
      END
C
C
C#############################################################
      SUBROUTINE CALCSC(K,IFI,FI,FIO,FIOO)
C#############################################################
C     This routine discretizes and solves the scalar transport
C     equations (temperature, turbulent kinetic energy, diss.).
C     Here, only temperature is solved for, but the routine is
C     prepared for possible extensions...
C=============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'coef.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'grad.inc'
      INCLUDE 'bound.inc'
      DIMENSION FI(NXYA),FIO(NXYA),FIOO(NXYA)
C
C.....CALCULATE GRADIENTS OF FI
C
      CALL SETIND(K)
      CALL GRADFI(K,FI,DPX,DPY)
C
C.....INITIALIZE ARRAYS, SET BLENDING AND UNDER_RELAXATION COEFF.
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIJ
C
      DO IJ=IJST,IJEN
        SU(IJ)=0.
        AP(IJ)=0.
      END DO
C
      GFI=GDS(IFI)
      URFFI=1./URF(IFI)
C
C.....CALCULATE FLUXES THROUGH INNER CV-FACES (EAST & NORTH)
C
      DO I=2,NIM-1
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        CALL FLUXSC(IFI,IJ,IJ+NJ,IJ,IJ-1,F1(IJ),AW(IJ+NJ),AE(IJ),
     *              FX(IJ),GFI,FI)
      END DO
      END DO
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM-1
        CALL FLUXSC(IFI,IJ,IJ+1,IJ-NJ,IJ,F2(IJ),AS(IJ+1),AN(IJ),
     *              FY(IJ),GFI,FI)
      END DO
      END DO
C
C.....CONTRIBUTION FROM O- AND C-GRID CUTS
C
      DO I=IOCS(K)+1,IOCS(K)+NOC(K)
        IJP=IJL(I)
        IJN=IJR(I)
        CALL FLUXSC(IFI,IJP,IJN,IJOC1(I),IJOC2(I),FMOC(I),
     *              AL(I),AR(I),FOC(I),GFI,FI)
        AP(IJP)=AP(IJP)-AR(I)
        AP(IJN)=AP(IJN)-AL(I)
      END DO
C
C.....UNSTEADY TERM CONTRIBUTION
C
      IF(LTIME) THEN
        DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          APT=DEN(IJ)*VOL(IJ)*DTR
          SU(IJ)=SU(IJ)+APT*((1.+GAMT)*FIO(IJ)-0.5*GAMT*FIOO(IJ))
          AP(IJ)= AP(IJ) +APT*(1.+0.5*GAMT)
        END DO
        END DO
      ENDIF
C
C.....INLET BOUNDARIES
C
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJP=IJPI(II)
        IJB=IJI(II)
        DPX(IJB)=DPX(IJP)
        DPY(IJB)=DPY(IJP)
        CALL FLUXSC(IFI,IJP,IJB,IJI1(II),IJI2(II),FMI(II),CP,CB,
     *              ONE,ZERO,FI)
        AP(IJP)=AP(IJP)-CB
        SU(IJP)=SU(IJP)-CB*FI(IJB)
      END DO
C
C.....OUTLET BOUNDARIES
C
      DO IO=IOS(K)+1,IOS(K)+NOUT(K)
        IJP=IJPO(IO)
        IJB=IJO(IO)
        FI(IJB)=FI(IJP)
        DPX(IJB)=DPX(IJP)
        DPY(IJB)=DPY(IJP)
        CALL FLUXSC(IFI,IJP,IJB,IJO1(IO),IJO2(IO),FMO(IO),C,C,
     *              ONE,ZERO,FI)
      END DO
C
C..... SYMMETRY BOUNDARIES
C
      DO IS=ISS(K)+1,ISS(K)+NSYM(K)
        FI(IJS(IS))=FI(IJPS(IS))
      END DO
C
C.....WALL BOUNDARY CONDITIONS AND SOURCES FOR TEMPERATURE
C
      IF(IFI.EQ.IEN) CALL TEMP(K)
C
C.....FINAL COEFFICIENT AND SOURCE MATRIX FOR FI-EQUATION
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        AP(IJ)=(AP(IJ)-AE(IJ)-AW(IJ)-AN(IJ)-AS(IJ))*URFFI
        SU(IJ)=SU(IJ)+(1.-URF(IFI))*AP(IJ)*FI(IJ)
      END DO
      END DO
C
C.....SOLVING EQUATION SYSTEM FOR FI-EQUATION
C
      CALL SIPSOL(FI,IFI,K)
C
      RETURN
      END 
C
C
C################################################################
      SUBROUTINE FLUXSC(IFI,IJP,IJN,IJ1,IJ2,FM,CAP,CAN,FAC,G,FI)
C################################################################
C     This routine calculates scalar fluxes (convective and
C     diffusive) through the cell face between nodes IJP and IJN.
C     It is analogous to the routine FLUXUV, see above. 
C================================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'grad.inc'
      INCLUDE 'coef.inc'
      DIMENSION FI(NXYA)
C
C.....INTERPOLATED CELL FACE VALUES
C
      FACP=1.-FAC
      FII=FI(IJN)*FAC+FI(IJP)*FACP
      VISI=VIS(IJN)*FAC+VIS(IJP)*FACP
      DFXI=DPX(IJN)*FAC+DPX(IJP)*FACP
      DFYI=DPY(IJN)*FAC+DPY(IJP)*FACP
C
C.....SURFACE AND DISTANCE VECTOR COMPONENTS, DIFFUSION COEFF.
C
      RC=0.5*(R(IJ1)+R(IJ2))
      SX=(Y(IJ1)-Y(IJ2))*RC
      SY=(X(IJ2)-X(IJ1))*RC
      XPN=XC(IJN)-XC(IJP)
      YPN=YC(IJN)-YC(IJP)
      VSOL=VISI*RPR(IFI)*SQRT((SX**2+SY**2)/(XPN**2+YPN**2))
C
C.....EXPLICIT CONVECTIVE AND DIFFUSIVE FLUXES
C
      FCFIE=FM*FII
      FDFIE=VISI*RPR(IFI)*(DFXI*SX+DFYI*SY)
C
C.....IMPLICIT CONVECTIVE AND DIFFUSIVE FLUXES
C
      FCFII=MIN(FM,ZERO)*FI(IJN)+MAX(FM,ZERO)*FI(IJP)
      FDFII=VSOL*(DFXI*XPN+DFYI*YPN)
C
C.....COEFFICIENTS, DEFERRED CORRECTION, SOURCE TERMS
C
      CAN=-VSOL+MIN(FM,ZERO)
      CAP=-VSOL-MAX(FM,ZERO)
      FFIC=G*(FCFIE-FCFII)
      SU(IJP)=SU(IJP)-FFIC+FDFIE-FDFII
      SU(IJN)=SU(IJN)+FFIC-FDFIE+FDFII
C
      RETURN
      END
C
C
C############################################################### 
      SUBROUTINE TEMP(K) 
C###############################################################
C     This routine assembles the source terms (volume integrals)
C     and applies wall boundary conditions for the temperature
C     (energy) equation.
C===============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'coef.inc'
      INCLUDE 'bound.inc'
C
C.....NO VOLUMETRIC SOURCES OF THERMAL ENERGY 
C
C.....ISOTHERMAL WALL BOUNDARIES
C
      DO IW=IWS(K)+1,IWS(K)+NWALI(K)
        IJP=IJPW(IW)
        IJB=IJW(IW)
        COEF=VIS(IJB)*RPR(IEN)*SRDW(IW)
        AP(IJP)=AP(IJP)+COEF
        SU(IJP)=SU(IJP)+COEF*T(IJB)
      END DO
C
C.....ADIABATIC WALL BOUNDARIES
C
      DO IW=IWAS(K)+1,IWAS(K)+NWALA(K)
        T(IJW(IW))=T(IJPW(IW))
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SETIND(K)
C########################################################
C     This routine sets the indices for the current grid.
C========================================================     
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
C
      NI=NIGR(K)
      NJ=NJGR(K)
      IST=IGR(K)
      JST=JGR(K)
      NJM=NJ-1
      NIM=NI-1
      NIJ=NI*NJ
C
      RETURN
      END
C
C
C##########################################################
      SUBROUTINE PRINT(K,FI,HEDFI)
C##########################################################
C     This routine prints the field variables in an easy to
C     read form.
C==========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      DIMENSION FI(NXYA)
      CHARACTER*6 HEDFI
C
C.....SET INDICES, PRINT HEADER, FIND HOW MANY 12-COLUMN BLOCKS
C
      CALL SETIND(K)
      WRITE(2,20) HEDFI
C
      IEND=0
      NL=NI/12+1
      IF(MOD(NI,12).EQ.0) NL=NL-1
C
C.....PRINT THE ARRAY FI
C
      DO L=1,NL
        IBEG=IEND+1
        IEND=MIN(IBEG+11,NI)
        WRITE(2,'(3X,4HI = ,I3,11I10)') (I,I=IBEG,IEND)
        WRITE(2,*) '  J'
        DO J=NJ,1,-1
          WRITE(2,'(1X,I3,1P12E10.2)') J,(FI(LI(I+IST)+J),I=IBEG,IEND)
        END DO
      END DO
C
   20 FORMAT(2X,26('*-'),6X,A6,6X,26('-*'))
C
      RETURN
      END
C
C
C############################################################
      SUBROUTINE SIPSOL(FI,IFI,K)
C############################################################
C     This routine incorporates the Stone's SIP solver, based
C     on ILU-decomposition. See Sect. 5.3.4 for details.
C============================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'coef.inc'
      INCLUDE 'bound.inc'
      DIMENSION FI(NXYA)
      REAL LW(NXYA),LS(NXYA),LPR(NXYA),UE(NXYA),UN(NXYA),RES(NXYA)
      DATA UE,UN,RES /NXYA*0.,NXYA*0.,NXYA*0./
C
C.....COEFFICIENTS OF UPPER AND LOWER TRIANG. MATRICES [L] & [U]
C
      DO I=2,NIM
      DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
        LW(IJ)=AW(IJ)/(1.+ALFA*UN(IJ-NJ))
        LS(IJ)=AS(IJ)/(1.+ALFA*UE(IJ-1))
        P1=ALFA*LW(IJ)*UN(IJ-NJ)
        P2=ALFA*LS(IJ)*UE(IJ-1)
        LPR(IJ)=1./(AP(IJ)+P1+P2-LW(IJ)*UE(IJ-NJ)-LS(IJ)*UN(IJ-1)+SMALL)
        UN(IJ)=(AN(IJ)-P1)*LPR(IJ)
        UE(IJ)=(AE(IJ)-P2)*LPR(IJ)
      END DO
      END DO
C
C.....INNER ITERATIONS LOOP
C
      DO L=1,NSW(IFI)
        RESAB=0.
C      
C.....CALCULATE RESIDUAL VECTOR AND THE SUM OF ABSOLUTE VALUES
C
        DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          RES(IJ)=SU(IJ)-AP(IJ)*FI(IJ)-AN(IJ)*FI(IJ+1)-
     *            AS(IJ)*FI(IJ-1)-AE(IJ)*FI(IJ+NJ)-AW(IJ)*FI(IJ-NJ)
        END DO
        END DO
C
        DO I=IOCS(K)+1,IOCS(K)+NOC(K)
          RES(IJL(I))=RES(IJL(I))-AR(I)*FI(IJR(I))
          RES(IJR(I))=RES(IJR(I))-AL(I)*FI(IJL(I))
        END DO
C
C.....FORWARD SUBSTITUTION
C
        DO I=2,NIM
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
          RESAB=RESAB+ABS(RES(IJ))
          RES(IJ)=(RES(IJ)-LS(IJ)*RES(IJ-1)-LW(IJ)*RES(IJ-NJ))*LPR(IJ)
        END DO
        END DO
C   
        IF(L.EQ.1) RESOR(IFI)=RESAB
        RSM=RESAB/(RESOR(IFI)+SMALL)
C
C.....BACKWARD SUBSTITUTION AND CORRECTION OF VARIABLE
C
        DO I=NIM,2,-1
        DO IJ=LI(I+IST)+NJM,LI(I+IST)+2,-1
          RES(IJ)=RES(IJ)-UN(IJ)*RES(IJ+1)-UE(IJ)*RES(IJ+NJ)
          FI(IJ)=FI(IJ)+RES(IJ)
        END DO
        END DO
C
C.....CHECK CONVERGENCE OF INNER ITERATIONS
C
        IF(LTEST) WRITE(2,*)L,' INNER ITER, RESAB= ',RESAB
        IF(RSM.LT.SOR(IFI)) RETURN
C
      END DO
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE INIT
C########################################################
C     This routine reads input parameters, grid data etc.
C========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'varold.inc'
C
C.....READ INPUT DATA IN THE FOLLOWING ORDER OF RECORDS:
C
C   1.  TITLE FOR THE PROBLEM SOLVED
C   2.  LOGICAL CONTROL PARAMETERS, GRID LEVEL FROM WHICH TO CONTINUE
C   3.  INDICES OF MONITORING LOCATION AND PRESSURE REFERENCE POINT
C   4.  CONVERGENCE AND DIVERGENCE CRITERION, SIP-PARAMETER
C   5.  DENSITY, DYNAMIC VISCOSITY AND PRANDTL NUMBER
C   6.  GRAVITY COMP., EXPANSION COEF., HOT, COLD AND REFERENCE TEMP.
C   7.  VALUES FOR FIELD INITIALIZATION (UIN,VIN,PIN,TIN) AND LID VELOCITY
C   8.  NO. OF TIME STEPS, OUTPUT CONTROL, TIME STEP, BLENDING FACTOR
C   9.  LOGICAL CONTROL VARIABLES (EQUATIONS TO BE SOLVED: U,V,PP,T)
C  10.  UNDER-RELAXATION FACTORS
C  11.  CONVERGENCE CRITERION FOR INNER ITERATIONS
C  12.  MAXIMUM ALLOWED NUMBER OF INNER ITERATIONS
C  13.  BLENDING FACTOR FOR CONVECTIVE FLUXES
C  14.  NUMBER OF OUTER ITERATIONS ON EACH GRID (FINEST LEVEL)
C
      READ(5,'(A50)') TITLE
      READ(5,*) LREAD,LWRITE,LTEST,LOUTS,LOUTE,LTIME,KIN
      READ(5,*) IMON,JMON,IPR,JPR
      READ(5,*) SORMAX,SLARGE,ALFA
      READ(5,*) DENS,VISC,PRANL
      READ(5,*) GRAVX,GRAVY,BETA,TH,TC,TREF
      READ(5,*) UIN,VIN,PIN,TIN,ULID
      READ(5,*) ITSTEP,NOTT,DT,GAMT
      READ(5,*) (LCAL(I),I=1,NFI)
      READ(5,*) (URF(I),I=1,NFI)
      READ(5,*) (SOR(I),I=1,NFI)
      READ(5,*) (NSW(I),I=1,NFI)
      READ(5,*) (GDS(I),I=1,NFI)
      READ(5,*) (LSG(IK),IK=1,NGR)
C
C.....READ GRID DATA GEOMETRY 
C
      READ(4) IA,(ITB(1,I),I=1,NXA),(ITB(2,I),I=1,NXA),
     *      (JTB(1,J),J=1,NYA),(JTB(2,J),J=1,NYA),
     *      (LI(I),I=1,NXA),(NIGR(K),K=1,NGR),(NJGR(K),K=1,NGR),
     *      (IGR(K),K=1,NGR),(JGR(K),K=1,NGR),(IJGR(K),K=1,NGR),
     *      NINA,(NINL(K),K=1,NGR),(IIS(K),K=1,NGR),(IJI(I),I=1,NINA),
     *      (IJPI(I),I=1,NINA),(IJI1(I),I=1,NINA),(IJI2(I),I=1,NINA),
     *      NOT,(NOUT(K),K=1,NGR),(IOS(K),K=1,NGR),(IJO(I),I=1,NOT),
     *      (IJPO(I),I=1,NOT),(IJO1(I),I=1,NOT),(IJO2(I),I=1,NOT),
     *      NWT,(NWAL(K),K=1,NGR),(IWS(K),K=1,NGR),(IJW(I),I=1,NWT),
     *      (IJPW(I),I=1,NWT),(IJW1(I),I=1,NWT),(IJW2(I),I=1,NWT),
     *      (NWALI(K),K=1,NGR),(IWAS(K),K=1,NGR),(NWALA(K),K=1,NGR),
     *      NST,(NSYM(K),K=1,NGR),(ISS(K),K=1,NGR),(IJS(I),I=1,NST),
     *      (IJPS(I),I=1,NST),(IJS1(I),I=1,NST),(IJS2(I),I=1,NST),
     *      NOCT,(NOC(K),K=1,NGR),(IOCS(K),K=1,NGR),(IJL(I),I=1,NOCT),
     *      (IJR(I),I=1,NOCT),(IJOC1(I),I=1,NOCT),(IJOC2(I),I=1,NOCT)
      READ(4) (X(I),I=1,NXYA),(Y(I),I=1,NXYA),(XC(I),I=1,NXYA),
     *        (YC(I),I=1,NXYA),(FX(I),I=1,NXYA),(FY(I),I=1,NXYA),
     *        (VOL(I),I=1,NXYA),(SRDW(I),I=1,NWT),(XTW(I),I=1,NWT),
     *        (YTW(I),I=1,NWT),(SRDS(I),I=1,NST),(XNS(I),I=1,NST),
     *        (YNS(I),I=1,NST),(FOC(I),I=1,NOCT)
      REWIND 4
      LAXIS=.FALSE.
      IF(IA.EQ.1) LAXIS=.TRUE.
C
C.....CHECK IF MONITORING POINT IS OK.
C
      IF(IMON.GT.NIGR(1)-1) IMON=NIGR(1)/2
      IF(JMON.GT.NJGR(1)-1) JMON=NJGR(1)/2
C
C.....RECIPROCAL VALUES OF URF & TIME STEP
C
      URFU=1./(URF(IU)+SMALL)
      URFV=1./(URF(IV)+SMALL)
      URFT=1./(URF(IEN)+SMALL)
      RPR(IEN)=1./(PRANL+SMALL)
      DTR=1./DT
C
C.....INITIALIZE VISCOSITY AND DENSITY AT ALL NODES
C
      DO IJ=1,NXYA
        VIS(IJ)=VISC
        DEN(IJ)=DENS
      END DO
C
C.....INITIALIZE VARIABLES AT INNER NODES OF ALL GRIDS
C
      DO IK=1,NGR
        CALL SETIND(IK)
C
        DO I=2,NIM
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
             U(IJ)=UIN
             V(IJ)=VIN
             P(IJ)=PIN
             T(IJ)=TIN
          END DO
        END DO
      END DO
C
C.....SET RADIUS EQUAL TO Y IF AXISYMMETRIC, ELSE R=1 
C
      IF(LAXIS) THEN
        DO IJ=1,NXYA
          R(IJ)=Y(IJ)
        END DO
      ELSE
        DO IJ=1,NXYA
          R(IJ)=1.
        END DO
      ENDIF
C
      RETURN
      END
C
C
C###########################################################
      SUBROUTINE OUTRES(K)
C###########################################################
C     This routine prints out the variable fields.
C===========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
C
C.....PRINT VELOCITIES, PRESSURE AND TEMPERATURE
C
      IF(LCAL(IU))  CALL PRINT(K,U,'U VEL.')
      IF(LCAL(IV))  CALL PRINT(K,V,'V VEL.')
      IF(LCAL(IP))  CALL PRINT(K,P,'PRESS.')
      IF(LCAL(IEN)) CALL PRINT(K,T,'TEMPER')
C
C.....PRINT MASS FLUXES IF TESTING
C
      IF(LTEST.AND.LCAL(IU)) CALL PRINT(K,F1,'MASS_E')
      IF(LTEST.AND.LCAL(IU)) CALL PRINT(K,F2,'MASS_N')
C
      RETURN

      END
C
C
C###########################################################
      SUBROUTINE OUTIN
C###########################################################
C     This routine prints title and parameters used in
C     computation.
C===========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'varold.inc'
C
C.....PRINT TITLE, DENSITY, VISCOSITY, CONV. CRITERION, SIP-PARAMETER
C    
      WRITE(2,'(A50)') TITLE
      WRITE(2,*) '====================================================='
      WRITE(2,*) '  '
      WRITE(2,*) '     FLUID DENSITY     : ',DENS
      WRITE(2,*) '     DYNAMIC VISCOSITY : ',VISC
      WRITE(2,*) '     CONVERGENCE CRIT. : ',SORMAX
      WRITE(2,*) '     SIP-PARAMETER     : ',ALFA
      WRITE(2,*) '    '
      IF(LAXIS) WRITE(2,*) '     AXISYMMETRIC FLOW CALCULATION '
      WRITE(2,*) '    '
C
C.....PRINT PARAMETERS FOR ENERGY EQUATION
C
      IF(LCAL(IEN)) THEN
        WRITE(2,*) '     PRANDTL NUMBER        : ',PRANL
        WRITE(2,*) '     HOT WALL TEMPERATURE  : ',TH
        WRITE(2,*) '     COLD WALL TEMPERATURE : ',TC
        WRITE(2,*) '     REFERENCE TEMPERATURE : ',TREF
        WRITE(2,*) '     '
      ENDIF
C
C.....PRINT UNDER-RELAXATION FACTORS
C
      IF(LCAL(IU))  WRITE(2,*) '     UNDER-RELAXATION FOR U: ',URF(IU)
      IF(LCAL(IV))  WRITE(2,*) '     UNDER-RELAXATION FOR V: ',URF(IV)
      IF(LCAL(IP))  WRITE(2,*) '     UNDER-RELAXATION FOR P: ',URF(IP)
      IF(LCAL(IEN)) WRITE(2,*) '     UNDER-RELAXATION FOR T: ',URF(IEN)
      WRITE(2,*) '     '
C
C.....PRINT BLENDING FACTORS FOR CONVECTIVE FLUXES (CDS-PART)
C
      WRITE(2,*) '     DIFFUSIVE FLUXES DISCRETIZED USING CDS '
      WRITE(2,*) '     CONTRIBUTION OF CDS VS. UDS IN CONV. FLUXES: '
      IF(LCAL(IU))  WRITE(2,*) '     BLENDING FACTOR FOR U: ',GDS(IU)
      IF(LCAL(IV))  WRITE(2,*) '     BLENDING FACTOR FOR V: ',GDS(IV)
      IF(LCAL(IEN)) WRITE(2,*) '     BLENDING FACTOR FOR T: ',GDS(IEN)
      WRITE(2,*) '     '
C
C.....PRINT CONVERGENCE CRITERION FOR INNER ITERATIONS
C
      IF(LCAL(IU))  WRITE(2,*) '     CONV. CRIT. FOR U: ',SOR(IU)
      IF(LCAL(IV))  WRITE(2,*) '     CONV. CRIT. FOR V: ',SOR(IV)
      IF(LCAL(IP))  WRITE(2,*) '     CONV. CRIT. FOR P: ',SOR(IP)
      IF(LCAL(IEN)) WRITE(2,*) '     CONV. CRIT. FOR T: ',SOR(IEN)
      WRITE(2,*) '     '
C
C.....PRINT MAXIMUM NUMBER OF INNER ITERATIONS
C
      IF(LCAL(IU))  WRITE(2,*) '     MAX. INNER ITER. FOR U: ',NSW(IU)
      IF(LCAL(IV))  WRITE(2,*) '     MAX. INNER ITER. FOR V: ',NSW(IV)
      IF(LCAL(IP))  WRITE(2,*) '     MAX. INNER ITER. FOR P: ',NSW(IP)
      IF(LCAL(IEN)) WRITE(2,*) '     MAX. INNER ITER. FOR T: ',NSW(IEN)
      WRITE(2,*) '     '
C
C.....PRINT TIME STEP SIZE, OUTPUT CONTROL, BLENDING FACTOR
C
      IF(LTIME) THEN
        OMGT=1.-GAMT
        WRITE(2,*) '     TIME STEP SIZE:                  ',DT
        WRITE(2,*) '     NUMBER OF TIME STEPS TO PERFORM: ',ITSTEP
        WRITE(2,*) '     SAVE RESULTS EVERY   ',NOTT,' TIME STEPS'
        WRITE(2,*) '     BLENDING ',GAMT,' OF THREE TIME LEVEL SCHEME'
        WRITE(2,*) '         WITH ',OMGT,' OF IMPLICIT EULER SCHEME '
        WRITE(2,*) '     '
      ENDIF
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SETDAT
C########################################################
C     In this routine some constants are assigned values.
C========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
C
      IU=1
      IV=2
      IP=3
      IEN=4
C
      SMALL=1.E-20
      GREAT=1.E+20
      ONE=1.0
      ZERO=0.
C     
      RETURN
      END
C
C
C###################################################################
      SUBROUTINE POST(K,ICOUNT)
C###################################################################
C     This routine prepares and saves data for post-processing, in
C     the format required by the post-processor. Since the post-
C     processor knows nothing about the grid type, variable values
C     at boundary nodes along C- and O-grid cuts must be calculated
C     (they are never used in the calculation). Also, block corner
C     values are never used in the computation but are needed in the
C     post-processor (they are simply set equal to the values at
C     the node next to corner). Also, mass fluxes through boundary 
C     faces need to be calculated and stored in arrays F1
C     and F2, for consistency with post-processor.
C===================================================================
      INCLUDE 'param.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'charac.inc'
      CHARACTER DSN*3
C
      CALL SETIND(K)
C
C.....SET 'BOUNDARY VALUES' AT O- AND C-GRID CUTS
C
      DO I=2,NIM
        IF(ITB(1,I+IST).EQ.10) THEN
          IJB=LI(I+IST)+1
          CALL FINDOC(IJB,IJB+1,K)
        ENDIF
C
        IF(ITB(2,I+IST).EQ.10) THEN
          IJB=LI(I+IST)+NJ
          CALL FINDOC(IJB,IJB-1,K)
        ENDIF
      END DO
C
      DO J=2,NJM
        IF(JTB(1,J+JST).EQ.10) THEN
          IJB=LI(1+IST)+J
          CALL FINDOC(IJB,IJB+NJ,K)
        ENDIF
C
        IF(JTB(2,J+JST).EQ.10) THEN
          IJB=LI(NI+IST)+J
          CALL FINDOC(IJB,IJB-NJ,K)
        ENDIF
      END DO         
C
C.....CALCULATE WEST AND EAST BOUNDARY MASS FLUXES
C
      DO J=2,NJM
        IJ=LI(1+IST)+J
        RB=0.5*(R(IJ)+R(IJ-1))
        SX=(Y(IJ)-Y(IJ-1))*RB
        SY=(X(IJ-1)-X(IJ))*RB
        F1(IJ)=DEN(IJ)*(U(IJ)*SX+V(IJ)*SY)
C
        IJ=LI(NIM+IST)+J
        RB=0.5*(R(IJ)+R(IJ-1))
        SX=(Y(IJ)-Y(IJ-1))*RB
        SY=(X(IJ-1)-X(IJ))*RB
        F1(IJ)=DEN(IJ+NJ)*(U(IJ+NJ)*SX+V(IJ+NJ)*SY)
      END DO
C
C.....CALCULATE SOUTH AND NORTH BOUNDARY MASS FLUXES
C
      DO I=2,NIM
        IJ=LI(I+IST)+1
        RB=0.5*(R(IJ)+R(IJ-NJ))
        SX=(Y(IJ-NJ)-Y(IJ))*RB
        SY=(X(IJ)-X(IJ-NJ))*RB
        F2(IJ)=DEN(IJ)*(U(IJ)*SX+V(IJ)*SY)
C    
        IJ=LI(I+IST)+NJM
        RB=0.5*(R(IJ)+R(IJ-NJ))
        SX=(Y(IJ-NJ)-Y(IJ))*RB
        SY=(X(IJ)-X(IJ-NJ))*RB
        F2(IJ)=DEN(IJ+1)*(U(IJ+1)*SX+V(IJ+1)*SY)
      END DO
C
C.....SET CORNER VALUES
C
      IJ=LI(1+IST)+1
      U(IJ)=U(IJ+1)
      V(IJ)=V(IJ+1)
      T(IJ)=T(IJ+1)
      P(IJ)=P(IJ+1)
C
      IJ=LI(1+IST)+NJ
      U(IJ)=U(IJ-1)
      V(IJ)=V(IJ-1)
      T(IJ)=T(IJ-1)
      P(IJ)=P(IJ-1)
C
      IJ=LI(NI+IST)+1
      U(IJ)=U(IJ+1)
      V(IJ)=V(IJ+1)
      T(IJ)=T(IJ+1)
      P(IJ)=P(IJ+1)
C
      IJ=LI(NI+IST)+NJ
      U(IJ)=U(IJ-1)
      V(IJ)=V(IJ-1)
      T(IJ)=T(IJ-1)
      P(IJ)=P(IJ-1)
C
C.....WRITE SOLUTION ON A FILE FOR POST-PROCESSING
C
      IF(ICOUNT.LT.10) WRITE(DSN,'(I1,2H  )') ICOUNT
      IF(ICOUNT.GE.10.AND.ICOUNT.LT.100) WRITE(DSN,'(I2,1H )') ICOUNT
      IF(ICOUNT.GE.100) WRITE(DSN,'(I3)') ICOUNT
      WRITE(FILPOS,'(A6,1H.,A3)') NAME,DSN
      OPEN (UNIT=8,FILE=FILPOS,FORM='UNFORMATTED')
      REWIND 8
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIJ
      WRITE(8) ITIM,TIME,NI,NJ,NIM,NJM,NIJ,
     *         (X(IJ), IJ=IJST,IJEN),(Y(IJ), IJ=IJST,IJEN),
     *         (XC(IJ),IJ=IJST,IJEN),(YC(IJ),IJ=IJST,IJEN),
     *         (F1(IJ),IJ=IJST,IJEN),(F2(IJ),IJ=IJST,IJEN),
     *         (U(IJ), IJ=IJST,IJEN),(V(IJ), IJ=IJST,IJEN),
     *         (P(IJ), IJ=IJST,IJEN),(T(IJ), IJ=IJST,IJEN)
      CLOSE(UNIT=8)
C
C.....RESET WEST AND EAST BOUNDARY MASS FLUXES TO ZERO
C
      DO J=2,NJM
        F1(LI(1+IST)+J)=0.
        F1(LI(NIM+IST)+J)=0.
      END DO
C
C.....RESET SOUTH AND NORTH BOUNDARY MASS FLUXES TO ZERO
C
      DO I=2,NIM
        F2(LI(I+IST)+1)=0.
        F2(LI(I+IST)+NJM)=0.
      END DO
C
      RETURN
      END
C 
C
C###################################################################
      SUBROUTINE SRES(K)
C###################################################################
C     This routine writes out the results onto a file
C     so that re-start is possible at a later stage.
C===================================================================
      INCLUDE 'param.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'var.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'charac.inc'
C
      WRITE(FILRES,'(A6,3H.re,I1)') NAME,K
      OPEN (UNIT=3,FILE=FILRES,FORM='UNFORMATTED')
      REWIND 3
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIGR(K)*NJGR(K)
      WRITE(3) K,IJST,IJEN,ITIM,TIME,(F1(IJ),IJ=IJST,IJEN),
     *         (F2(IJ),IJ=IJST,IJEN),(U(IJ),IJ=IJST,IJEN),
     *         (V(IJ),IJ=IJST,IJEN),(P(IJ),IJ=IJST,IJEN),
     *         (T(IJ),IJ=IJST,IJEN),
     *         (FMOC(I),I=IOCS(K)+1,IOCS(K)+NOC(K))
      IF(LTIME) WRITE(3) (UO(IJ),IJ=IJST,IJEN),(VO(IJ),IJ=IJST,IJEN),
     *         (TO(IJ),IJ=IJST,IJEN)
C
      CLOSE(UNIT=3)
C
      RETURN
      END
C
C
C###################################################################
      SUBROUTINE FINDOC(IJB,IJN,K)
C###################################################################
C     This routine searches along O- and C-interfaces to find the
C     face corresponding to the 'boundary' node IJB, whose neighbor
C     next-to-boundary is IJN. IJN must correspond to either IJL or
C     IJR node, and if so, the variable value at the 'boundary' node
C     (which is used only in the plot-program) is calculated by
C     interpolation.
C===================================================================
      INCLUDE 'param.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'var.inc'
C
      DO IOC=IOCS(K)+1,IOCS(K)+NOC(K)
        IF(IJN.EQ.IJL(IOC).OR.IJN.EQ.IJR(IOC)) THEN
          U(IJB)=U(IJR(IOC))*FOC(IOC)+U(IJL(IOC))*(1.-FOC(IOC))
          V(IJB)=V(IJR(IOC))*FOC(IOC)+V(IJL(IOC))*(1.-FOC(IOC))
          P(IJB)=P(IJR(IOC))*FOC(IOC)+P(IJL(IOC))*(1.-FOC(IOC))
          T(IJB)=T(IJR(IOC))*FOC(IOC)+T(IJL(IOC))*(1.-FOC(IOC))
        ENDIF
      END DO
C
      RETURN
      END
C
C---------------------------------------------------------------
C     Here come the routines with user-programmed input data and
C     user-programmed interpretation of results.
C     For each case, create separate user-files,
C     and copy them prior to compilation to the file 'user.f'
C     (routine BCIN provides boundary conditions, routine SOUT
C     prints data out for steady flows and at selected time steps
C     in unsteady flows; routine TOUT writes
C     data out at every time step). 
C---------------------------------------------------------------
      INCLUDE 'user.f'
C---------------------------------------------------------------
C
