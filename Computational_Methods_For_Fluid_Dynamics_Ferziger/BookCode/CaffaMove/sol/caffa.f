C#######################################################################
      PROGRAM CAFFA 
C####################################################################### 
C     This code incorporates the Finite Volume Method using SIMPLE  
C     algorithm on colocated body-fitted grids. For a description of the  
C     solution method, see the book by Ferziger and Peric (1996-2001).
C     Description of code features is provided in the accompanying
C     README-file.
C                                                              
C-----------------------------------------------------------------------
C     NEW FEATURES
C-----------------------------------------------------------------------  
C 
C     This version of the code can additionally predict flows with a
C     moving grid. The method is described in Chapter 12 of the book
C     by Ferziger and Peric (1996-2001).
C     Pressure boundary conditions are also implemented.
C                                                              
C                  Hidajet Hadzic, TU Hamburg-Harburg, November 2001
C----------------------------------------------------------------------
C
C     The user may modify the code and give it to third parties,
C     provided that an acknowledgement to the source of the original
C     version is retained.
C=======================================================================
C NB: CAFFA stands for "Computer Aided Fluid Flow Analysis". ! 
C======================================================================= 
      INCLUDE 'float.inc' 
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
      WRITE(*,*) 
      WRITE(*,*) ' ENTER PROBLEM NAME (UP TO ',MNM,' CHARACTERS):' 
      READ(*,'(A)') NAME 
C 
      CALL STRLEN(NAME,NAME,NNAME) 
      FILIN =NAME(1:NNAME)//'.cin' 
      FILOUT=NAME(1:NNAME)//'.out' 
      FILGRD=NAME(1:NNAME)//'.grd' 
      FILHIS=NAME(1:NNAME)//'.his' 
C 
      OPEN (UNIT=15,FILE=FILIN,STATUS='OLD',ERR=1000) 
      OPEN (UNIT=2,FILE=FILOUT) 
      OPEN (UNIT=4,FILE=FILGRD,FORM='UNFORMATTED') 
      OPEN (UNIT=16,FILE=FILHIS) 
      REWIND 2 
      REWIND 15 
      REWIND 4 
      REWIND 16 
C 
C.....INPUT DATA AND INITIALIZATION 
C 
      CALL INIT 
C 
C.....OPEN HISTORY FILE OF INNER ITERATION CONVERGENCE IF REQUIRED 
C 
      IF(LTEST) THEN 
        FILHIN=NAME(1:NNAME)//'.hin' 
        OPEN(UNIT=17,FILE=FILHIN) 
        REWIND 17 
      END IF 
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
        write(*,'(a,$)') '  read data for restart -' 
        DO K=1,KIN 
          write(filres,'(a,i1)') name(1:nname)//'.re',k 
          OPEN (UNIT=13,FILE=FILRES,FORM='UNFORMATTED', 
     &          STATUS='OLD',ERR=1002) 
C 
          READ(13) KGRD,IJST,IJEN,ITIM,TIME,(F1(IJ),IJ=IJST,IJEN), 
     &            (F2(IJ),IJ=IJST,IJEN),(U(IJ),IJ=IJST,IJEN), 
     &            (V(IJ),IJ=IJST,IJEN),(P(IJ),IJ=IJST,IJEN), 
     &            (T(IJ),IJ=IJST,IJEN), 
     &            (FMOC(I),I=IOCS(KGRD)+1,IOCS(KGRD)+NOC(KGRD)) 
          IF(LTIME) READ(13) (UO(IJ),IJ=IJST,IJEN), 
     &              (VO(IJ),IJ=IJST,IJEN),(TO(IJ),IJ=IJST,IJEN) 
          IF(MOVGR) READ(13) (X(IJ),IJ=IJST,IJEN), 
     &         (Y(IJ) ,IJ=IJST,IJEN),(VOL(IJ),IJ=IJST,IJEN) 
C 
          REWIND 13 
          CLOSE(UNIT=13) 
        END DO 
C 
        KGST=KGRD-1 
        ITIM=ITIM-1 
        write(*,*) ' successfull' 
      ENDIF 
C 
C..SET THE COUNTER FOR POSTPROICESSING RESULTS 
C 
      IF(LTIME.AND.LREAD) THEN 
        ICONT=ICONS-1 
      ELSE 
        ICONT=0 
      END IF 
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
        IF(LCAL(IU))  CALL VINT(KGR-1,U) 
        IF(LCAL(IV))  CALL VINT(KGR-1,V) 
        IF(LCAL(IP))  CALL VINT(kgr-1,P) 
        IF(LCAL(IP))  CALL CALCP(KGR,0) 
        IF(LCAL(IEN)) CALL VINT(KGR-1,T) 
C 
        IF(LTIME) THEN 
          IF(LCAL(IU))  CALL VINT(KGR-1,UO) 
          IF(LCAL(IV))  CALL VINT(KGR-1,VO) 
          IF(LCAL(IEN)) CALL VINT(KGR-1,TO) 
        ENDIF 
      ENDIF 
C 
      IF(LTIME) THEN 
        write(filto,'(a,i1)') name(1:nname)//'.to',kgr 
        OPEN(UNIT=10,FILE=FILTO) 
        REWIND 10 
      ENDIF 
C 
C====================================================== 
C.....START TIME LOOP 
C====================================================== 
C 
      INIBC=.TRUE. 
      IF(LTIME) ICONT=ICONS-1 
      IF(.NOT.LTIME) ITSTEP=1 
      ITIMS=ITIM+1 
      ITIME=ITIM+ITSTEP 
      IF(LSG(KGR).EQ.0) ITIME=0 
C************************************************* 
      DO 400 ITIM=ITIMS,ITIME 
C************************************************* 
C 
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
          VOLO(IJ)=VOL(IJ) 
        END DO 
C 
        WRITE(16,601) ITIM,TIME 
        WRITE( *,601) ITIM,TIME 
      ENDIF 
C 
C.....SET INLET BOUNDARY CONDITIONS 
C 
      IF(INIBC.OR.MOVGR) CALL BCIN(KGR) 
C 
C.....Grid movement - current coordinates become old coordinates...
C 
      IF(MOVGR) THEN 
         CALL SETIND(KGR) 
         DO I=IST+1,IST+NI 
           DO IJ=LI(I)+1,LI(I)+NJ 
              XO(IJ)=X(IJ) 
              YO(IJ)=Y(IJ) 
           END DO 
         END DO 
C 
C.....Define new coordinates of boundary vertices & generate new grid 
C 
         CALL NEWCOR(KGR) 
         CALL CALXY(KGR,IDIR) 
C 
C.....Calculate volume fluxes swept by cell faces & corect
C     boundary velocities
C 
         CALL GRFLUX(KGR) 
         CALL VCOR(KGR) 
C 
C.....Calculate geometry data for the new grid
C 
         CALL GEOM(KGR) 
      END IF 
C 
C.....PRINT INITAL FIELDS 
C 
      IF(LOUTS.AND.(ITIM.EQ.ITIMS)) CALL OUTRES(KGR) 
C 
C.....SET MONITORING LOCATION & PRESSURE REFERENCE LOCATION 
C 
      IST=IGR(KGR) 
      IIM=2**(KGR-1)*(IMON-1)+1 
      JJM=2**(KGR-1)*(JMON-1)+1 
      IJMON=LI(IIM+IST)+JJM 
C 
      IIM=2**(KGR-1)*(IPR-1)+1 
      JJM=2**(KGR-1)*(JPR-1)+1 
      IJPR=LI(IIM+IST)+JJM 
C 
      WRITE(16,600) KGR,IIM,JJM 
      IF(.NOT.LTIME) WRITE(*,600) KGR,IIM,JJM 
C 
C====================================================== 
C.....START SIMPLE RELAXATIONS (OUTER ITERATIONS) 
C====================================================== 
C 
      DO LS=1,LSG(KGR) 
C 
        IF(LCAL(IU))   CALL CALCUV(KGR) 
        IF(LCAL(IP))   CALL CALCP(KGR,1) 
        IF(LCAL(IEN))  CALL CALCSC(KGR,IEN,T,TO,TOO) 
C 
C.....NORMALIZE RESIDUALS, PRINT RES. LEVELS AND MONITORING VALUES 
C 
        DO L=1,NFI 
          RESOR(L)=RESOR(L)*RNOR(L) 
        END DO 
        WRITE(16,606) KGR,LS,LS,(RESOR(I),I=1,4), 
     *                U(IJMON),V(IJMON),P(IJMON),T(IJMON) 
        WRITE(*,606) KGR,LS,LS,(RESOR(I),I=1,4), 
     *               U(IJMON),V(IJMON),P(IJMON),T(IJMON) 
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
        CALL TOUT(KGR) 
C 
        IF(MOD(ITIM,NOTT).EQ.0.AND.LWRITE) THEN 
          ICONT=ICONT+1 
          WRITE(2,601) ITIM,TIME 
          CALL POST(KGR,ICONT) 
          IF(LOUTE) CALL OUTRES(KGR) 
          CALL SOUT(KGR) 
          CALL SRES(KGR) 
        ENDIF 
      ENDIF 
C----------------------------- 
C  Close time loop 
C***************************** 
  400 CONTINUE 
C***************************** 
C 
  509 CONTINUE      
C 
C.....STEADY FLOW, OR UNSTEADY FLOW - LAST TIME STEP: PRINT AND 
C     SAVE RESULTS IF COMPUTATION WAS PERFORMED ON THIS GRID LEVEL 
C 
      IF(LSG(KGR).GT.0) THEN 
        IF(.NOT.LTIME.OR.MOD(ITIM,NOTT).NE.0) THEN 
          ICONT=ICONT+1 
          CALL POST(KGR,ICONT) 
          IF(LOUTE) CALL OUTRES(KGR) 
          CALL SOUT(KGR) 
          CALL SRES(KGR) 
          IF(LTIME) CLOSE(UNIT=10) 
          ITIM=0 
          TIME=0. 
        END IF 
      ENDIF 
      write(*,*) ' save results' 
C 
  500 CONTINUE 
C 
C========================================================== 
C                 CLOSE FILES, FORMATS 
C========================================================== 
C 
      CLOSE(UNIT=8) 
      CLOSE(UNIT=13) 
      CLOSE(UNIT=4) 
      CLOSE(UNIT=2) 
      CLOSE(UNIT=15) 
      CLOSE(UNIT=16) 
      IF(LTEST) CLOSE (UNIT=17) 
C 
      WRITE(*,*) ' *** CALCULATION FINISHED - SEE RESULTS ***' 
      STOP
C 
C.....MESSAGE FOR DIVERGENCE & OPENING FILE ERRORS 
C 
  510 WRITE(*,*) ' *** TERMINATED - OUTER ITERATIONS DIVERGING ***' 
      STOP 
 1000 WRITE(*,999) FILIN 
      STOP 
 1001 WRITE(*,999) FILGEOM 
      STOP 
 1002 WRITE(*,999) FILRES 
      STOP 
C 
C.....FORMAT SPECIFICATIONS 
C 
  999 FORMAT('    ERROR CANNOT OPEN FILE ',a) 
  601 FORMAT('  ** TIME STEP=',I6,'   TIME = ',1pe13.5,' **') 
  600 FORMAT(1X,'GRID',1X,'CYCLE',1X,'ITER',2X, 
     * 'I------ABSOLUTE RESIDUAL SOURCE SUMS--------I',1X, 
     * 'I-FIELD VALUES AT MONITORING LOCATION(',1X,I3,',',I3, 
     * ',',I3,',)--I',/,1X,3('NO',4X),4X,'UMOM',6X,'VMOM',6X,'MASS', 
     * 6X,'ENER',16X,'U',9X,'V',9X,'P', 
     * 9X,'T',/) 
  606 FORMAT(1X,I2,3X,I3,2X,I4,6X,1P4E10.3,8X,1P4E10.3) 
C=========================================================== 
C                END OF PROGRAM 
C=========================================================== 
      END 
C 
C####################################################################### 
      SUBROUTINE VINT(K,FI) 
C####################################################################### 
C     This routine interpolates coarse grid solution to the fine grid,  
C     using gradient vector at coarse grid nodes and position vectors of  
C     fine grid nodes relative to coarse grid nodes (CV centers). 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C####################################################################### 
      FUNCTION FICF(IJG,IJF) 
C####################################################################### 
C     This function calculates dot product of the gradient vector at a  
C     coarse grid node with position vector of a fine grid node relative 
C      to coarse grid node. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'grad.inc' 
C 
      FICF=DUX(IJG)*(XC(IJF)-XC(IJG))+DUY(IJG)*(YC(IJF)-YC(IJG)) 
C 
      RETURN 
      END
C
C####################################################################### 
      SUBROUTINE CALCUV(K) 
C####################################################################### 
C     This routine discretizes and solves the linearized equations for X  
C     and Y momentum componentS (U and V Cartesian velocity components).  
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
     &         (V(IJP)-V(IJB))*XTW(IW)*YTW(IW)) 
        SV(IJP)=SV(IJP)+COEF*(V(IJB)*YTW(IW)**2- 
     &         (U(IJP)-U(IJB))*XTW(IW)*YTW(IW)) 
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
C.....Pressure boundary conditions 
C 
      do ib=ips(k)+1,ips(k)+npre(k)
        ijp=ijpp(ib)
        ijb=ijpb(ib)
        dux(ijb)=dux(ijp)
        duy(ijb)=duy(ijp)
        dvx(ijb)=dvx(ijp)
        dvy(ijb)=dvy(ijp)
        call fluxuv(ijp,ijb,ijp1(ib),ijp2(ib),fmp(ib),cp,cb,one,zero)
        ap(ijp) =ap(ijp) -cb
        apr(ijp)=apr(ijp)-cb
        su(ijp) =su(ijp) -cb*u(ijb)
        sv(ijp) =sv(ijp) -cb*v(ijb)
      end do
C 
C.....O- AND C-GRID CUTS (THESE ARE NOT BOUNDARIES!) 
C 
      DO I=IOCS(K)+1,IOCS(K)+NOC(K) 
        IJP=IJL(I) 
        IJN=IJR(I) 
        CALL FLUXUV(IJP,IJN,IJOC1(I),IJOC2(I),FMOC(I), 
     &              AL(I),AR(I),FOC(I),GU) 
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
C####################################################################### 
      SUBROUTINE FLUXUV(IJP,IJN,IJ1,IJ2,FM,CAP,CAN,FAC,G) 
C####################################################################### 
C     This routine calculates momentum fluxes (convective and diffusive) 
C     through the cell face between nodes IJP and IJN. IJ1 and IJ2 are
C     the indices of CV corners defining the cell face. FM is the mass  
C     flux through the face, and FAC is the interpolation factor  
C     (distance from node IJP to cell face center over the sum of this  
C     distance and the distance from cell face center to node IJN). CAP  
C     and CAN are the contributions to matrix coefficients in the  
C     momentum equations at nodes IJP and IJN. Diffusive fluxes are 
C     discretized using central differences; for convective fluxes,  
C     linear interpolation can be blended with upwind approximation;  
C     see Sect. 8.6 for details. Note: cell face surface vector is  
C     directed from P to N. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'grad.inc' 
      INCLUDE 'coef.inc' 
      INCLUDE 'rcont.inc' 
C 
C.....INTERPOLATED CELL FACE VALUES (Note: simple linear interpolation 
C     gives the value at e', see Fig. 8.6; gradient correction of the 
C     form used in Eq. (8.35) is used to obtain velocity values at e.) 
C 
      FACP=1.-FAC 
      DUXI=DUX(IJN)*FAC+DUX(IJP)*FACP 
      DVXI=DVX(IJN)*FAC+DVX(IJP)*FACP 
      DUYI=DUY(IJN)*FAC+DUY(IJP)*FACP 
      DVYI=DVY(IJN)*FAC+DVY(IJP)*FACP 
C 
      XI=XC(IJN)*FAC+XC(IJP)*FACP 
      YI=YC(IJN)*FAC+YC(IJP)*FACP 
      XF=0.5*(X(IJ2)+X(IJ1)) 
      YF=0.5*(Y(IJ2)+Y(IJ1)) 
C 
      UI=U(IJN)*FAC+U(IJP)*FACP+DUXI*(XF-XI)+DUYI*(YF-YI) 
      VI=V(IJN)*FAC+V(IJP)*FACP+DVXI*(XF-XI)+DVYI*(YF-YI) 
      VISI=VIS(IJN)*FAC+VIS(IJP)*FACP 
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
C#######################################################################  
      SUBROUTINE CALCP(K,INTMF)  
C#######################################################################  
C     This routine assembles and solves the pressure-correction equation  
C     using colocated grid. SIMPLE algorithm with one corrector step  
C     (non-orthogonality effects neglected) is applied. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
        F1(IJ)=0. 
        F2(IJ)=0. 
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
     &              AL(I),AR(I),FOC(I)) 
        AP(IJP)=AP(IJP)-AR(I)
        AP(IJN)=AP(IJN)-AL(I) 
        SU(IJP)=SU(IJP)-FMOC(I) 
        SU(IJN)=SU(IJN)+FMOC(I) 
      END DO 
C 
C.....Add contribution from moving grid 
C 
      DELTAM=0.          
      IF(MOVGR) THEN 
C 
C.....Add mass fluxes due to moving grid 
C 
        DO I=1,NIM 
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
            F1(IJ)=F1(IJ)-F1G(IJ) 
          END DO 
        END DO 
C 
        DO I=2,NIM 
          DO IJ=LI(I+IST)+1,LI(I+IST)+NJM 
            F2(IJ)=F2(IJ)-F2G(IJ) 
          END DO 
        END DO 
C 
C.....Corect source term due to moving grid 
C 
        DELTAM=0. 
        DO I=2,NIM 
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
            SU(IJ)=SU(IJ)+DEN(IJ)*(VOLO(IJ)-VOL(IJ))*DTR 
            DELTAM=DELTAM+DEN(IJ)*(VOLO(IJ)-VOL(IJ))*DTR 
          END DO 
        END DO 
C 
C.....Reset mass fluxes through wall boundaries to zero 
C 
        DO IW=IWS(K)+1,IWS(K)+NWAL(K) 
          IJP=IJPW(IW) 
          IJB=IJW(IW) 
          IF(IJP.GT.IJB) THEN 
            IJ=IJB 
          ELSE 
            IJ=IJP 
          END IF 
          IF(ABS(IJP-IJB).EQ.1) THEN 
            F2(IJ)=0. 
          ELSE 
            F1(IJ)=0. 
          END IF 
        END DO 
      END IF 
C 
C.....INLET BOUNDARIES (MASS FLUXES PRESCRIBED IN ROUTINE 'BCIN') 
C 
      DO II=IIS(K)+1,IIS(K)+NINL(K) 
        SU(IJPI(II))=SU(IJPI(II))-FMI(II)
      END DO 
c 
c.....Pressure boundaries
c 
      broj1=0.
      do ib=ips(k)+1,ips(k)+npre(k)
        ijp=ijpp(ib)
        ijb=ijpb(ib)
        call fluxp(ijp,ijb,ijp1(ib),ijp2(ib),cap,fmp(ib))
        ap(ijp)=ap(ijp)-cap
        su(ijp)=su(ijp)-fmp(ib)
        pp(ijb)=0.
        broj1=broj1+fmp(ib)
      end do
C 
C.....OUTLET BOUNDARIES 
C 
      IF(.NOT.LTIME) THEN 
        DO IO=IOS(K)+1,IOS(K)+NOUT(K) 
           U(IJO(IO))=U(IJPO(IO)) 
           V(IJO(IO))=V(IJPO(IO)) 
        END DO 
      ELSE 
        OUTARE=SMALL 
        DO IO=IOS(K)+1,IOS(K)+NOUT(K) 
          RS=0.5*(R(IJO1(IO))+R(IJO2(IO))) 
          SX=(Y(IJO1(IO))-Y(IJO2(IO)))*RS 
          SY=(X(IJO2(IO))-X(IJO1(IO)))*RS 
          OUTARE=OUTARE+SQRT(SX**2+SY**2) 
        END DO                                                                
        UAVE=FLOMAS/OUTARE 
        DO IO=IOS(K)+1,IOS(K)+NOUT(K) 
          DPBR=1./SQRT((XC(IJO(IO))-XC(IJPO(IO)))**2+ 
     &                 (YC(IJO(IO))-YC(IJPO(IO)))**2) 
          U(IJO(IO))=(UO(IJO(IO))*DTR+U(IJPO(IO))*UAVE*DPBR)/ 
     &               (            DTR+            UAVE*DPBR) 
          V(IJO(IO))=(VO(IJO(IO))*DTR+V(IJPO(IO))*UAVE*DPBR)/ 
     &               (            DTR+            UAVE*DPBR) 
        END DO                                                                         
      END IF 
C 
C.....CORECT OUTFLOW FLUXES TO SATISFY GLOBAL MASS CONSERVATION & ADD TO SOURCE 
C 
      FLOWO=0. 
      DO IO=IOS(K)+1,IOS(K)+NOUT(K) 
        IJB=IJO(IO) 
        IJP=IJPO(IO) 
        RS=0.5*(R(IJO1(IO))+R(IJO2(IO))) 
        SX=(Y(IJO1(IO))-Y(IJO2(IO))) 
        SY=(X(IJO2(IO))-X(IJO1(IO))) 
        FMO(IO)=DEN(IJP)*(U(IJB)*SX+V(IJB)*SY)*RS 
        FLOWO=FLOWO+FMO(IO) 
      END DO 
C 
      FAC=(FLOMAS+DELTAM)/(FLOWO+SMALL) 
c
      DO IO=IOS(K)+1,IOS(K)+NOUT(K) 
        IJB=IJO(IO) 
        FMO(IO)=FMO(IO)*FAC 
        U(IJB)=U(IJB)*FAC 
        V(IJB)=V(IJB)*FAC 
        SU(IJPO(IO))=SU(IJPO(IO))-FMO(IO) 
      END DO 
C 
C.....RETURN IF ONLY MASS FLUXES UPDATED !!! 
C 
      IF(INTMF.EQ.0) RETURN 
C 
C.....SOURCE TERM AND CENTRAL COEFFICIENT  
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
      IF(LTEST) WRITE(17,'(4(a,1pe13.5,2x))') 
     & 'SUM = ',SUM,'FLOMAS=',FLOMAS,'DELTAM=',DELTAM,'FLOWO=',FLOWO
      CALL SIPSOL(PP,IP,K) 
C 
C.....UPDATE PRESSURE CORECTION AT BOUNDARIES 
C 
      CALL PRESB(K,PP) 
C 
C.....CALCULATE PRESSURE-CORRECTION GRADIENTS
C 
      CALL GRADFI(K,PP,DPX,DPY) 
c
c.....Set reference value for p' (if there is any pressure boundary
C     reset it to zero)
c
      PPO=PP(IJPR) 
      if(npre(1).gt.0) ppo=0.
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
c 
c....Correct mass fluxes at pressure boundaries
c 
      do ib=ips(k)+1,ips(k)+npre(k)
        call flcorp(ijpp(ib),ijpb(ib),ijp1(ib),ijp2(ib),fmp(ib))
      end do
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
C#######################################################################
      SUBROUTINE FLUXM(IJP,IJN,IJ1,IJ2,FM,CAP,CAN,FAC) 
C####################################################################### 
C     This routine calculates mass flux through the cell face between  
C     nodes IJP and IJN. IJ1 and IJ2 are the indices of CV corners  
C     defining the cell face. FM is the mass flux through the face, and  
C     FAC is the interpolation factor (distance from node IJP to cell  
C     face center over the sum of this distance and the distance from  
C     cell face center to node IJN). CAP and CAN are the contributions  
C     to matrix coefficients in the pressure-correction equation at  
C     nodes IJP and IJN. Surface vector directed from P to N. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'grad.inc' 
      INCLUDE 'coef.inc' 
C 
C.....INTERPOLATED CELL FACE VALUES (Note: correction from e' 
C     to e is applied, see FLUXUV) 
C 
      FACP=1.-FAC 
C 
      XI=XC(IJN)*FAC+XC(IJP)*FACP 
      YI=YC(IJN)*FAC+YC(IJP)*FACP 
      XF=0.5*(X(IJ2)+X(IJ1)) 
      YF=0.5*(Y(IJ2)+Y(IJ1)) 
C 
      DUXI=DUX(IJN)*FAC+DUX(IJP)*FACP 
      DVXI=DVX(IJN)*FAC+DVX(IJP)*FACP 
      DUYI=DUY(IJN)*FAC+DUY(IJP)*FACP 
      DVYI=DVY(IJN)*FAC+DVY(IJP)*FACP 
      UI=U(IJN)*FAC+U(IJP)*FACP+DUXI*(XF-XI)+DUYI*(YF-YI) 
      VI=V(IJN)*FAC+V(IJP)*FACP+DVXI*(XF-XI)+DVYI*(YF-YI) 
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
C***********************************************************************
      SUBROUTINE FLUXP(IJP,IJB,IJ1,IJ2,CAP,FM)
C***********************************************************************
C     This routine calculates mass fluxes through pressure boundaries.
C-----------------------------------------------------------------------
      include 'float.inc' 
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
c
      rc=0.5*(r(ij1)+r(ij2))
      sx=(y(ij1)-y(ij2))*rc
      sy=(x(ij2)-x(ij1))*rc
      xpn=xc(ijb)-xc(ijp)
      ypn=yc(ijb)-yc(ijp)
c
      capp=vol(ijp)*apr(ijp)/sqrt((sx**2+sy**2)*(xpn**2+ypn**2))
      dpcor=p(ijb)-p(ijp)-(dpx(ijp)*xpn+dpy(ijp)*ypn)
      u(ijb)=u(ijp)-sx*capp*dpcor
      v(ijb)=v(ijp)-sy*capp*dpcor
      fm=den(ijp)*(u(ijb)*sx+v(ijb)*sy)
      cap=-(sx**2+sy**2)*capp*den(ijp)
c
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE FLCORP(IJP,IJB,IJ1,IJ2,FM)
C***********************************************************************
C     This routine corrects mass fluxes through pressure boundaries.
C-----------------------------------------------------------------------
      include 'float.inc' 
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
c
      rc=0.5*(r(ij1)+r(ij2))
      sx=(y(ij1)-y(ij2))*rc
      sy=(x(ij2)-x(ij1))*rc
      xpn=xc(ijb)-xc(ijp)
      ypn=yc(ijb)-yc(ijp)
c
      cap=vol(ijp)*apr(ijp)/sqrt((sx**2+sy**2)*(xpn**2+ypn**2))
      dpcor=pp(ijb)-pp(ijp)
      u(ijb)=u(ijb)-sx*cap*dpcor
      v(ijb)=v(ijb)-sy*cap*dpcor
      fm=fm-(sx**2+sy**2)*cap*dpcor*den(ijp)
c
      RETURN
      END
C 
C#######################################################################
      SUBROUTINE PRESB(K,FI)
C#######################################################################
C  This routine extrapolates the pressure or pressure correction from 
C  interior to the boundary. Linear extrapolation is used, but one can 
C  also linearly extrapolate the gradient...
C  Extrapolation is not performed at pressure boundaries.
C-----------------------------------------------------------------------
      include 'float.inc' 
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
      DIMENSION FI(NXYA)
c
      efac(i1,i2,i3)=sqrt((xc(i1)-xc(i2))**2+(yc(i1)-yc(i2))**2)/
     &     (sqrt((xc(i2)-xc(i3))**2+(yc(i2)-yc(i3))**2)+small)
c
c.....Extrapolate pressure or its correction to boundaries
c
*     efact=0.5
      CALL SETIND(K)
c
c.....inlet
c
      do ii=iis(k)+1,iis(k)+ninl(k)
        ijp=ijpi(ii)
        ijb=iji(ii)
        ijn=2*ijp-ijb
        efact=efac(ijb,ijp,ijn)
        fi(ijb)=fi(ijp)+(fi(ijp)-fi(ijn))*efact
      end do
c
c.....outlet
c
      do io=ios(k)+1,ios(k)+nout(k)
        ijp=ijpo(io)
        ijb=ijo(io)
        ijn=2*ijp-ijb
        efact=efac(ijb,ijp,ijn)
        fi(ijb)=fi(ijp)+(fi(ijp)-fi(ijn))*efact
      end do
c
c.....symmetry
c
      do is=iss(k)+1,iss(k)+nsym(k)
        ijp=ijps(is)
        ijb=ijs(is)
        ijn=2*ijp-ijb
        efact=efac(ijb,ijp,ijn)
        fi(ijb)=fi(ijp)+(fi(ijp)-fi(ijn))*efact
      end do
c
c.....wall
c
      do iw=iws(k)+1,iws(k)+nwal(k)
        ijp=ijpw(iw)
        ijb=ijw(iw)
        ijn=2*ijp-ijb
        efact=efac(ijb,ijp,ijn)
        fi(ijb)=fi(ijp)+(fi(ijp)-fi(ijn))*efact
      end do
c
      RETURN
      END
C
C#######################################################################
      SUBROUTINE GRADFI(K,FI,DFX,DFY)
C#######################################################################
C     This routine calculates the components of the gradient vector of a 
C     scalar FI at the CV center, using conservative scheme based on the
C     Gauss theorem; see Sect. 8.6 for details. FIE are values at east 
C     side, FIN at north side. Contributions from boundary faces are
C     calculated in a separate loops...
C-----------------------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'var.inc'
      INCLUDE 'logic.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      DIMENSION FI(NXYA),DFX(NXYA),DFY(NXYA)
      COMMON /GROLD/ DFXO(NXYA),DFYO(NXYA)
C
C.....SET INDICES, INITIALIZE OLD GRADIENT
C
      IJST=IJGR(K)+1
      IJEN=IJGR(K)+NIJ
C
      DO IJ=IJST,IJEN
        DFXO(IJ)=0.
        DFYO(IJ)=0.
      END DO
C
C.....START ITERATIVE CALCULATION OF GRADIENTS
C
      DO LC=1,NIGRAD
C
C.......INITIALIZE NEW GRADIENT
C
        DO IJ=IJST,IJEN
          DFX(IJ)=0.
          DFY(IJ)=0.
        END DO
C
C.......CONTRIBUTION FROM INNER EAST SIDES
C
        DO I=2,NIM-1
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
            CALL GRADCO(FI,DFX,DFY,FX(IJ),IJ,IJ+NJ,IJ,IJ-1)
          END DO
        END DO
C
C.......CONTRIBUTION FROM INNER NORTH SIDES
C
        DO I=2,NIM
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM-1
            CALL GRADCO(FI,DFX,DFY,FY(IJ),IJ,IJ+1,IJ-NJ,IJ)
          END DO
        END DO
C
C.......CONTRIBUTION FROM O- AND C-GRID CUTS
C
        DO I=IOCS(K)+1,IOCS(K)+NOC(K)
         CALL GRADCO(FI,DFX,DFY,FOC(I),IJL(I),IJR(I),IJOC1(I),IJOC2(I))
        END DO
C
C.......CONTRIBUTION FROM INLET BOUNDARIES
C
        DO I=IIS(K)+1,IIS(K)+NINL(K)
          CALL GRADBC(IJPI(I),IJI(I),IJI1(I),IJI2(I),DFX,DFY,FI)
        END DO
C
C.......Contribution from pressure boundaries
C
        do i=ips(k)+1,ips(k)+npre(k)
          call gradbc(ijpp(i),ijpb(i),ijp1(i),ijp2(i),dfx,dfy,fi)
        end do
C
C.......CONTRIBUTION FROM OUTLET BOUNDARIES
C
        DO I=IOS(K)+1,IOS(K)+NOUT(K)
          CALL GRADBC(IJPO(I),IJO(I),IJO1(I),IJO2(I),DFX,DFY,FI)
        END DO
C
C.......CONTRIBUTION FROM SYMMETRY BOUNDARIES
C
        DO I=ISS(K)+1,ISS(K)+NSYM(K)
          CALL GRADBC(IJPS(I),IJS(I),IJS1(I),IJS2(I),DFX,DFY,FI)
        END DO
C
C.......CONTRIBUTION FROM WALL BOUNDARIES
C
        DO I=IWS(K)+1,IWS(K)+NWAL(K)
          CALL GRADBC(IJPW(I),IJW(I),IJW1(I),IJW2(I),DFX,DFY,FI)
        END DO
C
C.......AXISYMMETRIC CONTRIBUTION (FRONT AND REAR CELL FACE)
C
        IF(LAXIS) THEN
          DO I=2,NIM
            DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
              AREA=0.5*((X(IJ)-X(IJ-NJ-1))*(Y(IJ-NJ)-Y(IJ-1))-
     *                  (Y(IJ)-Y(IJ-NJ-1))*(X(IJ-NJ)-X(IJ-1)))
              DFY(IJ)=DFY(IJ)-FI(IJ)*AREA
            END DO
          END DO
        ENDIF
C
C.......CALCULATE GRADIENT COMPONENTS AT CV-CENTERS
C
        DO I=2,NIM
          DO IJ=LI(I+IST)+2,LI(I+IST)+NJM
            DFX(IJ)=DFX(IJ)/VOL(IJ)
            DFY(IJ)=DFY(IJ)/VOL(IJ)
          END DO
        END DO
C
C.......SET OLD GRADIENT = NEW GRADIENT FOR THE NEXT ITERATION
C
        IF(LC.NE.NIGRAD) THEN
          DO IJ=IJST,IJEN
            DFXO(IJ)=DFX(IJ)
            DFYO(IJ)=DFY(IJ)
          END DO
        ENDIF
C
      END DO
C
      RETURN
      END
C
C#######################################################################
      SUBROUTINE GRADCO(FI,DFX,DFY,FAC,IJP,IJN,IJ1,IJ2)
C#######################################################################
C     This routine calculates contribution to the gradient vector of a 
C     scalar FI at the CV center, arising from an inner cell face 
C     (cell-face value of FI times the corresponding component of the 
C     surface vector).
C-----------------------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      DIMENSION FI(NXYA),DFX(NXYA),DFY(NXYA)
      COMMON /GROLD/ DFXO(NXYA),DFYO(NXYA)
C
C.....COORDINATES OF POINT ON THE LINE CONNECTING CENTER AND NEIGHBOR,
C     OLD GRADIENT VECTOR COMPONENTS INTERPOLATED FOR THIS LOCATION
C
      FACP=1.-FAC
      XI=XC(IJN)*FAC+XC(IJP)*FACP
      YI=YC(IJN)*FAC+YC(IJP)*FACP
      DFXI=DFXO(IJN)*FAC+DFXO(IJP)*FACP
      DFYI=DFYO(IJN)*FAC+DFYO(IJP)*FACP
C
C.....COORDINATES OF THE CELL-FACE CENTER, VARIABLE VALUE THERE 
C
      XF=0.5*(X(IJ1)+X(IJ2))
      YF=0.5*(Y(IJ1)+Y(IJ2))
      FIE=FI(IJN)*FAC+FI(IJP)*FACP+DFXI*(XF-XI)+DFYI*(YF-YI)
C
C.....SURFACE VECTOR COMPONENTS, GRADIENT CONTRIBUTION FROM CELL FACE
C
      RE=(R(IJ1)+R(IJ2))*0.5
      SX=(Y(IJ1)-Y(IJ2))*RE
      SY=(X(IJ2)-X(IJ1))*RE
      DFXE=FIE*SX
      DFYE=FIE*SY
C
C.....ACCUMULATE CONTRIBUTION AT CELL CENTER AND NEIGHBOR
C
      DFX(IJP)=DFX(IJP)+DFXE
      DFY(IJP)=DFY(IJP)+DFYE
      DFX(IJN)=DFX(IJN)-DFXE
      DFY(IJN)=DFY(IJN)-DFYE
C
      RETURN
      END
C
C#######################################################################
      SUBROUTINE GRADBC(IJP,IJB,IJ1,IJ2,DFX,DFY,FI)
C#######################################################################
C     This routine calculates the contribution of a boundary cell face 
C     to the gradient at CV-center.
C-----------------------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'geo.inc'
      DIMENSION FI(NXYA),DFX(NXYA),DFY(NXYA)
C
      RB=0.5*(R(IJ1)+R(IJ2))
      SX=(Y(IJ1)-Y(IJ2))*RB
      SY=(X(IJ2)-X(IJ1))*RB
      DFX(IJP)=DFX(IJP)+FI(IJB)*SX
      DFY(IJP)=DFY(IJP)+FI(IJB)*SY
C
      RETURN
      END
C
C####################################################################### 
      SUBROUTINE CALCSC(K,IFI,FI,FIO,FIOO) 
C####################################################################### 
C     This routine discretizes and solves the scalar transport equations  
C     (temperature, turbulent kinetic energy, diss.). Here, only  
C     temperature is solved for, but the routine is prepared for  
C     possible extensions... 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
        DPX(IJB)=DPX(IJP) 
        DPY(IJB)=DPY(IJP) 
        CALL FLUXSC(IFI,IJP,IJB,IJO1(IO),IJO2(IO),FMO(IO),C,C, 
     *              ONE,ZERO,FI) 
        FI(IJB)=FI(IJP) 
      END DO 
C 
C.....Pressure boundaries (zero gradient of dependent variables is assumed)
C 
      do ib=ips(k)+1,ips(k)+npre(k)
        ijp=ijpp(ib)
        ijb=ijpb(ib)
        dpx(ijb)=dpx(ijp)
        dpy(ijb)=dpy(ijp)
        call fluxsc(ifi,ijp,ijb,ijp1(ib),ijp2(ib),fmp(ib),cb,cb,
     &              one,zero,fi)
        fi(ijb)=fi(ijp)
      end do
C 
C.....SYMMETRY BOUNDARIES 
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
C####################################################################### 
      SUBROUTINE FLUXSC(IFI,IJP,IJN,IJ1,IJ2,FM,CAP,CAN,FAC,G,FI) 
C####################################################################### 
C     This routine calculates scalar fluxes (convective and diffusive)  
C     through the cell face between nodes IJP and IJN. It is analogous  
C     to the routine FLUXUV, see above.  
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
      DFXI=DPX(IJN)*FAC+DPX(IJP)*FACP 
      DFYI=DPY(IJN)*FAC+DPY(IJP)*FACP 
      XI=XC(IJN)*FAC+XC(IJP)*FACP 
      YI=YC(IJN)*FAC+YC(IJP)*FACP 
      XF=0.5*(X(IJ2)+X(IJ1)) 
      YF=0.5*(Y(IJ2)+Y(IJ1)) 
C 
      FII=FI(IJN)*FAC+FI(IJP)*FACP+DFXI*(XF-XI)+DFYI*(YF-YI) 
      VISI=VIS(IJN)*FAC+VIS(IJP)*FACP 
c 
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
C####################################################################### 
      SUBROUTINE TEMP(K)  
C####################################################################### 
C This routine assembles the source terms (volume integrals) and applies  
C wall boundary conditions for the temperature (energy) equation. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C####################################################################### 
      SUBROUTINE SETIND(K) 
C####################################################################### 
C     This routine sets the indices for the current grid. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C####################################################################### 
      SUBROUTINE PRINT(K,FI,HEDFI) 
C####################################################################### 
C     This routine prints the field variables in an easy to read form. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C####################################################################### 
      SUBROUTINE SIPSOL(FI,IFI,K) 
C####################################################################### 
C     This routine incorporates the Stone's SIP solver, based on  
C     ILU-decomposition. See Sect. 5.3.4 for details. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
        IF(LTEST) WRITE(17,'(10x,a,i2,5x,a,i5,5x,a,1pe15.9)') 
     &    'EQ=',ifi,' INNER ITER=',l,'  RSM= ',rsm 
        IF(RSM.LT.SOR(IFI)) RETURN 
C 
      END DO 
C 
      RETURN 
      END 
C 
C####################################################################### 
      SUBROUTINE INIT 
C####################################################################### 
C     This routine reads input parameters, grid data etc. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C   2.  LOGICAL CONTROL PARAMETERS, GRID LEVEL FROM WHICH TO CONTINUE, 
C       STARTING DATA SET FOR POSTPROCESSING 
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
      write(*,*) ' READ: TITLE ' 
      READ(15,'(A50)') TITLE 
      write(*,*)  
     &' READ: LREAD,LWRITE,LTEST,LOUTS,LOUTE,LTIME,MOVGR,KIN' 
      READ(15,*)   LREAD,LWRITE,LTEST,LOUTS,LOUTE,LTIME,MOVGR,KIN 
      write(*,*) ' READ: IMON,JMON,IPR,JPR,NIGRAD,ICONS' 
      READ(15,*)   IMON,JMON,IPR,JPR,NIGRAD,ICONS
      write(*,*) ' READ: SORMAX,SLARGE,ALFA' 
      READ(15,*)   SORMAX,SLARGE,ALFA 
      write(*,*) ' READ: DENS,VISC,PRANL' 
      READ(15,*)   DENS,VISC,PRANL 
      write(*,*) ' READ: GRAVX,GRAVY,BETA,TH,TC,TREF' 
      READ(15,*)   GRAVX,GRAVY,BETA,TH,TC,TREF 
      write(*,*) ' READ: UIN,VIN,PIN,TIN,ULID' 
      READ(15,*)   UIN,VIN,PIN,TIN,ULID 
      write(*,*) ' READ: ITSTEP,NOTT,DT,GAMT' 
      READ(15,*)   ITSTEP,NOTT,DT,GAMT 
      write(*,*) ' READ: (LCAL(I),I=1,NFI)' 
      READ(15,*)   (LCAL(I),I=1,NFI) 
      write(*,*) ' READ: (URF(I),I=1,NFI)' 
      READ(15,*)   (URF(I),I=1,NFI) 
      write(*,*) ' READ: (SOR(I),I=1,NFI)' 
      READ(15,*)   (SOR(I),I=1,NFI) 
      write(*,*) ' READ: (NSW(I),I=1,NFI)' 
      READ(15,*)   (NSW(I),I=1,NFI) 
      write(*,*) ' READ: (GDS(I),I=1,NFI)' 
      READ(15,*)   (GDS(I),I=1,NFI) 
      write(*,*) ' READ: (LSG(IK),IK=1,NGR)' 
      READ(15,*)   (LSG(IK),IK=1,NGR) 
C 
C.....READ GRID DATA GEOMETRY  
C 
      write(*,'(a,$)') ' reading of geometry data - ' 
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
     &      npt,(npre(k),k=1,ngr),(ips(k),k=1,ngr),(ijpb(i),i=1,npt),
     &      (ijpp(i),i=1,npt),(ijp1(i),i=1,npt),(ijp2(i),i=1,npt),
     *      NOCT,(NOC(K),K=1,NGR),(IOCS(K),K=1,NGR),(IJL(I),I=1,NOCT), 
     *      (IJR(I),I=1,NOCT),(IJOC1(I),I=1,NOCT),(IJOC2(I),I=1,NOCT)

      READ(4) (X(I),I=1,NXYA),(Y(I),I=1,NXYA),(XC(I),I=1,NXYA), 
     *        (YC(I),I=1,NXYA),(FX(I),I=1,NXYA),(FY(I),I=1,NXYA), 
     *        (VOL(I),I=1,NXYA),(SRDW(I),I=1,NWT),(XTW(I),I=1,NWT), 
     *        (YTW(I),I=1,NWT),(SRDS(I),I=1,NST),(XNS(I),I=1,NST), 
     *        (YNS(I),I=1,NST),(FOC(I),I=1,NOCT) 
      READ(4) IDIR,ISMOG
      REWIND 4 
      write(*,*) 'successfull' 
C 
      LAXIS=.FALSE. 
      IF(IA.EQ.1) LAXIS=.TRUE. 
      LSMOG=.FALSE. 
      IF(ISMOG.EQ.1) LSMOG=.TRUE. 
C 
      IF(.NOT.LTIME) MOVGR=.FALSE. 
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
C.....SET LTEST TO .FALSE. IF NUMBER OF TIME STEPS .GT. 100 
C 
      IF(LTIME.AND.ITSTEP.GT.100) LTEST=.FALSE. 
C 
      RETURN 
      END 
C 
C####################################################################### 
      SUBROUTINE OUTRES(K) 
C####################################################################### 
C     This routine prints out the variable fields. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C####################################################################### 
      SUBROUTINE OUTIN 
C####################################################################### 
C     This routine prints title and parameters used in computation. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'rcont.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'varold.inc'
C 
C.....PRINT TITLE, DENSITY, VISCOSITY, CONV. CRITERION, SIP-PARAMETER 
C     
      WRITE(2,'(A50)') TITLE 
      WRITE(2,*) '====================================================' 
      WRITE(2,*) '  ' 
      WRITE(2,*) '     FLUID DENSITY     : ',DENS 
      WRITE(2,*) '     DYNAMIC VISCOSITY : ',VISC 
      WRITE(2,*) '     CONVERGENCE CRIT. : ',SORMAX 
      WRITE(2,*) '     SIP-PARAMETER     : ',ALFA 
      WRITE(2,*) '    ' 
      IF(LAXIS) WRITE(2,*) '     AXISYMMETRIC FLOW CALCULATION ' 
      IF(MOVGR) WRITE(2,*) '     MOVING GRID ACTIVATED' 
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
C####################################################################### 
      SUBROUTINE SETDAT 
C####################################################################### 
C     In this routine some constants are assigned values. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
      HALF=0.5 
      QUAR=0.25 
C      
      RETURN 
      END
C
C####################################################################### 
      SUBROUTINE POST(K,ICOUNT) 
C####################################################################### 
C     This routine prepares and saves data for post-processing, in the  
C     format required by the post-processor. Since the post-processor  
C     knows nothing about the grid type, variable values at boundary  
C     nodes along C- and O-grid cuts must be calculated (they are never  
C     used in the calculation). Also, block corner values are never used  
C     in the computation but are needed in the post-processor (they are  
C     simply set equal to the values at the node next to corner). Also,  
C     mass fluxes through boundary faces need to be calculated and  
C     stored in arrays F1 and F2, for consistency with post-processor. 
C-----------------------------------------------------------------------
      include 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'bound.inc' 
      INCLUDE 'charac.inc' 
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
      call strlen(name,name,nname) 
      write(filpos,'(a,i4.4,a4)') name(1:nname),icount,'.pos' 
C 
      OPEN (UNIT=8,FILE=FILPOS,FORM='UNFORMATTED') 
      REWIND 8 
C 
      ktim=0 
      if(ltime) ktim=1 
c 
      IJST=IJGR(K)+1 
      IJEN=IJGR(K)+NIJ 
      WRITE(8) KTIM,TIME,NI,NJ,NIM,NJM,NIJ, 
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
C####################################################################### 
      SUBROUTINE SRES(K) 
C####################################################################### 
C     This routine writes out the results onto a file so that re-start  
C     is possible at a later stage. All relevant data are stored  
C     including old values of all variables and grid coordinates even if 
C     the calculation is steady or moving grid is not activated. This 
C     reastart of calculation in unsteady mod or with moving grid after  
C     a steady calculation. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc'
      INCLUDE 'bound.inc' 
      INCLUDE 'charac.inc' 
C 
      CALL STRLEN(NAME,NAME,NNAME) 
      write(filres,'(a,i1)') name(1:nname)//'.re',k 
C 
      OPEN (UNIT=13,FILE=FILRES,FORM='UNFORMATTED') 
      REWIND 13 
C 
      IJST=IJGR(K)+1 
      IJEN=IJGR(K)+NIGR(K)*NJGR(K) 
      WRITE(13) K,IJST,IJEN,ITIM,TIME,(F1(IJ),IJ=IJST,IJEN), 
     &         (F2(IJ),IJ=IJST,IJEN),(U(IJ),IJ=IJST,IJEN), 
     &         (V(IJ),IJ=IJST,IJEN),(P(IJ),IJ=IJST,IJEN), 
     &         (T(IJ),IJ=IJST,IJEN), 
     &         (FMOC(I),I=IOCS(K)+1,IOCS(K)+NOC(K)) 
      WRITE(13) (UO(IJ),IJ=IJST,IJEN),(VO(IJ),IJ=IJST,IJEN), 
     &          (TO(IJ),IJ=IJST,IJEN) 
      WRITE(13) (X(IJ),IJ=IJST,IJEN), 
     *          (Y(IJ) ,IJ=IJST,IJEN),(VOL(IJ),IJ=IJST,IJEN) 
C 
      CLOSE(UNIT=13) 
C 
      RETURN 
      END 
C 
C####################################################################### 
      SUBROUTINE FINDOC(IJB,IJN,K) 
C####################################################################### 
C     This routine searches along O- and C-interfaces to find the face  
C     corresponding to the 'boundary' node IJB, whose neighbor next-to- 
C     boundary is IJN. IJN must correspond to either IJL or IJR node,  
C     and if so, the variable value at the 'boundary' node (which is  
C     used only in the plot-program) is calculated by interpolation. 
C----------------------------------------------------------------------- 
      include 'float.inc' 
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
C####################################################################### 
      SUBROUTINE STRLEN(INPUT,OUTPUT,LOUT) 
C#######################################################################
C     This routine finds string length. 
C----------------------------------------------------------------------- 
      CHARACTER*(*) INPUT,OUTPUT 
C&&& 
      LIN=LEN(INPUT) 
      DO 10 I=LIN,1,-1 
        LOUT=I 
        IF(INPUT(LOUT:LOUT).NE.' ') GO TO 11 
   10 CONTINUE 
      LOUT=0 
      OUTPUT=' ' 
      RETURN 
   11 OUTPUT(1:LOUT)=INPUT(1:LOUT) 
      RETURN 
      END
C
C#######################################################################
C     Here come the routines ncessary for remeshing when moving grid 
C     option is activated.
C####################################################################### 
      SUBROUTINE GRFLUX(K) 
C####################################################################### 
C     This routine calculates mass fluxes through grid cell faces and 
C     change of volume of cells due to moving grid.  
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
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
      CALL SETIND(K) 
C 
C.... CALCULATE CELL-FACE FLUXES DUE TO MOVING GRID 
C 
      DO I=1,NIM 
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
          IJM=IJ-1 
          F1G(IJ)=((X(IJM)-XO(IJ))*(Y(IJ)-YO(IJM))- 
     *             (X(IJ)-XO(IJM))*(Y(IJM)-YO(IJ)))*HALF 
        END DO 
      END DO 
C 
      DO I=2,NIM 
        DO IJ=LI(I+IST)+1,LI(I+IST)+NJM 
          IMJ=IJ-NJ 
          F2G(IJ)=((X(IJ)-XO(IMJ))*(Y(IMJ)-YO(IJ))- 
     *             (X(IMJ)-XO(IJ))*(Y(IJ)-YO(IMJ)))*HALF 
        END DO 
      END DO 
C 
C.....CALCULATE CHANGE OF VOLUME DUE TO MOVING GRID 
C 
      DO I=2,NIM 
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
          VOL(IJ)=VOLO(IJ)+F1G(IJ)+F2G(IJ)-F1G(IJ-NJ)-F2G(IJ-1) 
        END DO 
      END DO 
C 
C.....MASS FLUXES THROUGH CELL-FACES DUE TO MOVING GRID 
C 
      DO I=1,NIM 
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
          F1G(IJ)=F1G(IJ)*DEN(IJ)*DTR 
        END DO 
      END DO 
C 
      DO I=2,NIM 
        DO IJ=LI(I+IST)+1,LI(I+IST)+NJM 
          F2G(IJ)=F2G(IJ)*DEN(IJ)*DTR 
        END DO 
      END DO 
C 
      RETURN 
      END 
C 
C####################################################################### 
      SUBROUTINE GEOM(K) 
C####################################################################### 
C     This routine calculates geometry data for new grid. 
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'bound.inc'
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'charac.inc' 
      INCLUDE 'coef.inc' 
      INCLUDE 'grad.inc' 
C 
      CALL SETIND(K) 
C 
C.....CALCULATION OF CELL VOLUMES FOR AXI-SYMMETRIC GEOMETRY 
C 
      IF(TIME.LT.SMALL.OR..NOT.MOVGR) THEN 
        IF (LAXIS) THEN 
          SIXR=1./6. 
          DO I=2,NIM 
            DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
              IMJ=IJ-NJ 
              IMJM=IMJ-1 
              IJM=IJ-1 
              RIJ=Y(IJ)**2 
              RIMJ=Y(IMJ)**2 
              RIMJM=Y(IMJM)**2 
              RIJM=Y(IJM)**2 
              VOL(IJ)=SIXR*((X(IJ)-X(IMJ))*(RIJ+RIMJ+Y(IJ)*Y(IMJ))+ 
     *                (X(IMJ)-X(IMJM))*(RIMJ+RIMJM+Y(IMJ)*Y(IMJM))+ 
     *                (X(IMJM)-X(IJM))*(RIMJM+RIJM+Y(IMJM)*Y(IJM))+ 
     *                (X(IJM)-X(IJ))*(RIJM+RIJ+Y(IJM)*Y(IJ))) 
            END DO 
          END DO 
C 
          ELSE 
C 
C.....CALCULATION OF CELL VOLUMES FOR PLANE GEOMETRY 
C 
          DO I=2,NIM 
            DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
              DXNESW=X(IJ)-X(IJ-NJ-1) 
              DYNESW=Y(IJ)-Y(IJ-NJ-1) 
              DXNWSE=X(IJ-NJ)-X(IJ-1) 
              DYNWSE=Y(IJ-NJ)-Y(IJ-1) 
              VOL(IJ)=0.5*ABS(DXNESW*DYNWSE-DXNWSE*DYNESW) 
            END DO 
          END DO 
C 
        END IF 
      END IF 
C 
C.....CALCULATION OF NODE COORDINATES: CORNER (DUMMY) NODES 
C 
        IJ=LI(1+IST)+1 
        XC(IJ)=X(IJ) 
        YC(IJ)=Y(IJ) 
C 
        IJ=LI(NIM+IST)+1 
        XC(IJ+NJ)=X(IJ)
        YC(IJ+NJ)=Y(IJ) 
C 
        IJ=LI(1+IST)+NJM 
        XC(IJ+1)=X(IJ) 
        YC(IJ+1)=Y(IJ) 
C 
        IJ=LI(NIM+IST)+NJM 
        XC(IJ+NJ+1)=X(IJ) 
        YC(IJ+NJ+1)=Y(IJ) 
C 
C.....CALCULATION OF NODE COORDINATES: BOUNDARY NODES 
C 
        DO J=2,NJM 
          IJ=LI(IST+NIM)+J 
           XC(IJ+NJ)=(X(IJ)+X(IJ-1))*half 
           YC(IJ+NJ)=(Y(IJ)+Y(IJ-1))*half 
          IJ=LI(IST+1)+J 
           XC(IJ)=(X(IJ)+X(IJ-1))*half 
           YC(IJ)=(Y(IJ)+Y(IJ-1))*half 
        END DO 
C 
        DO I=2,NIM 
          IJ=LI(I+IST)+1 
           XC(IJ)=(X(IJ)+X(IJ-NJ))*half 
           YC(IJ)=(Y(IJ)+Y(IJ-NJ))*half 
          IJ=LI(I+IST)+NJM 
           XC(IJ+1)=(X(IJ)+X(IJ-NJ))*half 
           YC(IJ+1)=(Y(IJ)+Y(IJ-NJ))*half 
        END DO 
C 
C.....CALCULATION OF NODE COORDINATES: CELL CENTERS 
C 
        do i=2,nim 
          do ij=li(i+ist)+2,li(i+ist)+njm 
            XC(IJ)=(X(IJ)+X(IJ-1)+X(IJ-NJ)+X(IJ-NJ-1))*quar 
            YC(IJ)=(Y(IJ)+Y(IJ-1)+Y(IJ-NJ)+Y(IJ-NJ-1))*quar 
          end do 
        end do 
C 
C.....SET RADIUS 
C 
      IF(LAXIS) THEN 
        DO I=1,NI  
          DO IJ=LI(I+IST)+1,LI(I+IST)+NJ 
            R(IJ)=Y(IJ) 
          END DO 
        END DO 
      ELSE 
        DO I=1,NI  
          DO IJ=LI(I+IST)+1,LI(I+IST)+NJ 
            R(IJ)=1. 
          END DO 
        END DO 
      ENDIF 
C 
C==========================================================
C..... CALCULATION OF INTERPOLATION FACTORS 
C========================================================== 
C 
      DO I=2,NIM 
        DO IJ=LI(I+IST)+2,LI(I+IST)+NJM 
C 
C.....INTERPOLATION IN I-DIRECTION: FX = Pe/PE 
C 
          XE=0.5*(X(IJ)+X(IJ-1)) 
          YE=0.5*(Y(IJ)+Y(IJ-1)) 
          DLPE=SQRT((XE-XC(IJ))**2+(YE-YC(IJ))**2) 
          DLEE=SQRT((XC(IJ+NJ)-XE)**2+(YC(IJ+NJ)-YE)**2) 
          FX(IJ)=DLPE/(DLPE+DLEE+1.E-20) 
C 
C.....INTERPOLATION IN J-DIRECTION: FY = Pn/PN 
C 
          XN=0.5*(X(IJ)+X(IJ-NJ)) 
          YN=0.5*(Y(IJ)+Y(IJ-NJ)) 
          DLPN=SQRT((XN-XC(IJ))**2+(YN-YC(IJ))**2) 
          DLNN=SQRT((XC(IJ+1)-XN)**2+(YC(IJ+1)-YN)**2) 
          FY(IJ)=DLPN/(DLPN+DLNN+1.E-20) 
        END DO 
      END DO 
C 
      CALL SODW(K) 
      CALL SODS(K) 
C 
      RETURN 
      END
C
C####################################################################### 
      SUBROUTINE SODW(K) 
C####################################################################### 
C     This routine calculates components of the unit vector normal to 
C     wall boundary faces and area of that face divided by the distance 
C     of cell center to the wall. 
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'bound.inc' 
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'charac.inc' 
      INCLUDE 'coef.inc' 
      INCLUDE 'grad.inc' 
C 
C.....CALCULATE COMPONENTS OF THE UNIT VECTOR TANGENTIAL TO CELL FACE 
C 
      DO IW=IWS(K)+1,IWS(K)+NWAL(K) 
        IJB=IJW(IW) 
        IJP=IJPW(IW) 
        IJ1=IJW1(IW) 
        IJ2=IJW2(IW)
C 
        DX=X(IJ1)-X(IJ2) 
        DY=Y(IJ1)-Y(IJ2) 
        RR=SQRT(DX**2+DY**2) 
        XTW(IW)=DX/(RR+SMALL) 
        YTW(IW)=DY/(RR+SMALL) 
        XN=YTW(IW) 
        YN=-XTW(IW) 
C 
C.....NORMAL DISTANBE FROM CELL FACE CENTER TO CELL CENTER 
C 
        DN=(XC(IJB)-XC(IJP))*XN+(YC(IJB)-YC(IJP))*YN 
C 
C.....CELL FACE AREA DIVIDED BY DISTANCE TO THE CELL CENTER 
C 
        SRDW(IW)=RR/(DN+SMALL)  
        IF(LAXIS) SRDW(IW)=SRDW(IW)*YC(IJB) 
C 
      END DO 
C 
      RETURN 
      END 
C 
C####################################################################### 
      SUBROUTINE SODS(K) 
C####################################################################### 
C     This routine calculates components of the unit vector parallel to 
C     symmetry boundary faces and area of that face divided by the 
C     distance of cell center to boundary.
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'bound.inc' 
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'charac.inc' 
      INCLUDE 'coef.inc' 
      INCLUDE 'grad.inc' 
C 
C.....CALCULATE COMPONENTS OF THE UNIT VECTOR TANGENTIAL TO CELL FACE 
C 
      DO IS=ISS(K)+1,ISS(K)+NSYM(K) 
        IJB=IJS(IS) 
        IJP=IJPS(IS) 
        IJ1=IJS1(IS) 
        IJ2=IJS2(IS) 
C 
        DX=X(IJ1)-X(IJ2) 
        DY=Y(IJ1)-Y(IJ2) 
        RR=SQRT(DX**2+DY**2) 
        XT=DX/(RR+SMALL)  
        YT=DY/(RR+SMALL)  
        XNS(IS)=YT 
        YNS(IS)=-XT 
C 
C.....NORMAL DISTANCE FROM CELL FACE CENTER TO CELL CENTER 
C 
        DN=(XC(IJB)-XC(IJP))*XNS(IS)+(YC(IJB)-YC(IJP))*YNS(IS) 
C 
C.....CELL FACE AREA DIVIDED BY DISTANCE TO THE CELL CENTER 
C 
        SRDS(IS)=RR/(DN+SMALL)  
        IF(LAXIS) SRDS(IS)=SRDS(IS)*YC(IJB) 
C 
      END DO 
C 
      RETURN 
      END 
C 
C####################################################################### 
      SUBROUTINE CALXY(K,IDD) 
C####################################################################### 
C     This routine calculates the coordinates of grid points inside of 
C     the solution domain for new grid. Coordinates of grid points along
C     boundaries are defined by user in routine NEWCOR. 
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'bound.inc'
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'charac.inc' 
      INCLUDE 'coef.inc' 
      INCLUDE 'grad.inc' 
      DIMENSION FL(NXA),FR(NXA) 
C 
      CALL SETIND(K) 
C 
C.....OFFSETS AT WEST AND EAST BOUNDARY 
C 
      IIE=LI(IST+NIM) 
      IIW=LI(IST+1) 
c 
C================================================================== 
C.....CALCULATE COORDINATES OF INTERIOR POINTS (STRAIGHT LINES N-S) 
C================================================================== 
C 
      IF(IDD.EQ.0) THEN 
C 
C.....DISTANCE BETWEEN CORNER POINTS (NORTH - SOUTH) 
C 
        DLR=1./SQRT((X(IIE+NJM)-X(IIE+1))**2+(Y(IIE+NJM)-Y(IIE+1))**2) 
        DLL=1./SQRT((X(IIW+NJM)-X(IIW+1))**2+(Y(IIW+NJM)-Y(IIW+1))**2) 
C 
C.....DISTRIBUTION FUNCTION ON WEST AND EAST SIDE 
C 
        DO J=1,NJM 
          FL(J)=SQRT((X(IIW+J)-X(IIW+1))**2+(Y(IIW+J)-Y(IIW+1))**2)*DLL 
          FR(J)=SQRT((X(IIE+J)-X(IIE+1))**2+(Y(IIE+J)-Y(IIE+1))**2)*DLR 
        END DO 
C 
C.....DISTANCE BETWEEN OPOSITE POINTS ON SOUTH AND NORTH SIDE 
C 
        RNIM=1./REAL(NIM-1) 
        DO I=1,NIM   
          II=LI(I) 
          DX=X(II+NJM)-X(II+1) 
          DY=Y(II+NJM)-Y(II+1) 
C 
C.....DISTRIBUTE POINTS USING INTERPOLATED WEST AND EAST DISTRIBUTIONS 
C 
          DO J=2,NJM-1 
            FAC=(REAL(I-1)*FR(J)+REAL(NIM-I)*FL(J))*RNIM 
            X(II+J)=X(II+1)+FAC*DX 
            Y(II+J)=Y(II+1)+FAC*DY 
          END DO 
        END DO 
C 
C================================================================== 
C.....CALCULATE COORDINATES OF INTERIOR POINTS (STRAIGHT LINES E-W) 
C================================================================== 
C 
      ELSEIF(IDD.EQ.1) THEN
C 
C.....DISTANCE BETWEEN CORNERS (EAST - WEST) 
C 
        DLR=1./SQRT((X(IIE+NJM)-X(IIW+NJM))**2+ 
     *              (Y(IIE+NJM)-Y(IIW+NJM))**2) 
        DLL=1./SQRT((X(IIE+1)-X(IIW+1))**2+(Y(IIE+1)-Y(IIW+1))**2) 
C 
C.....DISTRIBUTION FUNCTION ALONG SOUTH AND NORTH BOUNDARY 
C 
        DO I=1,NIM 
          II=LI(I+IST) 
          FL(I)=SQRT((X(II+1)-X(IIW+1))**2+(Y(II+1)-Y(IIW+1))**2)*DLL 
          FR(I)=SQRT((X(II+NJM)-X(IIW+NJM))**2+ 
     *            (Y(II+NJM)-Y(IIW+NJM))**2)*DLR 
        END DO 
C 
C.....DISTANCE BETWEEN OPOSITE POINTS ON EAST AND WEST BOUNDARY 
C 
        RNJM=1./REAL(NJM-1) 
        DO J=1,NJM 
          DX=X(IIE+J)-X(IIW+J) 
          DY=Y(IIE+J)-Y(IIW+J) 
C 
C.....DISTRIBUTE POINTS USING INTERPOLATED NORTH AND SOUTH DISTRIBUTIONS 
C 
          DO I=2,NIM-1 
            IJ=LI(I+IST)+J 
            FAC=(REAL(J-1)*FR(I)+REAL(NJM-J)*FL(I))*RNJM 
            X(IJ)=X(IIW+J)+FAC*DX 
            Y(IJ)=Y(IIW+J)+FAC*DY 
          END DO 
        END DO 
C 
      ENDIF 
C 
C.....SMOOTH NEW GRID 
C 
      IF(LSMOG) CALL SMOG(K) 
C 
      RETURN 
      END
C
C####################################################################### 
      SUBROUTINE SMOG(K) 
C####################################################################### 
C     This routine smooths the new grid in the interior. 
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'bound.inc' 
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'charac.inc'
      INCLUDE 'coef.inc' 
      INCLUDe 'grad.inc' 
C 
      CALL SETIND(K) 
C 
C.....SMOOTH WEST-EAST LINES 
C 
      CALL SMOGP(NJ) 
  
C.....SMOOTH SOUTH-NORTH LINES 
C 
      IND=1 
      CALL SMOGP(IND) 
C 
      RETURN 
      END
C
C####################################################################### 
      SUBROUTINE SMOGP(IND) 
C####################################################################### 
C     This routine adjusts coordinates of interior grid points to make 
C     the grid smoother. Two neighbor grid points (L and R) of point P 
C     are connected and an auxilliary point is set on this line at a 
C     location corresponding to the position of P between L and R. 
C     The new grid point is put midway between the old location P and 
C     the auxilliary point. The points are moving inwards towards center 
C     of curvature.                                    
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
      INCLUDE 'param.inc' 
      INCLUDE 'indexc.inc' 
      INCLUDE 'logic.inc' 
      INCLUDE 'bound.inc' 
      INCLUDE 'rcont.inc' 
      INCLUDE 'var.inc' 
      INCLUDE 'geo.inc' 
      INCLUDE 'varold.inc' 
      INCLUDE 'charac.inc' 
      INCLUDE 'coef.inc' 
      INCLUDE 'grad.inc' 
      DIMENSION XN(NXYA),YN(NXYA)   
C 
C.....COPY OLD COORDINATES 
C 
      DO I=2,NIM-1 
        DO J=2,NJM-1 
          IJ=LI(I+IST)+J 
          XN(IJ)=X(IJ) 
          YN(IJ)=Y(IJ) 
        END DO 
      END DO                                              
C 
C.....SMOOTH LINES 
C 
       DO I=2,NIM-1 
        DO J=2,NJM-1 
          IJ=LI(I+IST)+J 
          ikl=IJ-IND 
          ikr=IJ+IND 
C 
C.....CONNECTIONS IJ <--> ikl AND IJ <--> ikr 
C 
          TL=SQRT((X(IJ)-X(ikl))**2+(Y(IJ)-Y(ikl))**2) 
          TR=SQRT((X(IJ)-X(ikr))**2+(Y(IJ)-Y(ikr))**2) 
C                                                                                       
C.....AUXILLIARY POINT ON LINE IKL - IKR, MODIFIED GRID NODE 
C 
          FL=TL/(TL+TR) 
          XP=X(ikl)+FL*(X(ikr)-X(ikl)) 
          YP=Y(ikl)+FL*(Y(ikr)-Y(ikl)) 
          XN(IJ)=0.5*(X(IJ)+XP) 
          YN(IJ)=0.5*(Y(IJ)+YP) 
        END DO
      END DO 
C 
C.....RESET COORDINATES OF GRID NODES 
C 
      DO I=2,NIM-1 
        DO J=2,NJM-1 
          IJ=LI(I+IST)+J 
          X(IJ)=XN(IJ) 
          Y(IJ)=YN(IJ) 
        END DO 
      END DO 
C 
      RETURN                                                                      
      END    
C 
C####################################################################### 
      SUBROUTINE VCOR(K) 
C####################################################################### 
C     This routine corects velocity of wall boundary due to moving grid. 
C-----------------------------------------------------------------------
      INCLUDE 'float.inc' 
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
      CALL SETIND(K) 
C 
      DO IW=IWS(K)+1,IWS(K)+NWAL(K) 
         IJ=IJW(IW) 
         IJ1=IJW1(IW) 
         IJ2=IJW2(IW) 
         XCN=(X(IJ1)+X(IJ2))*HALF 
         YCN=(Y(IJ1)+Y(IJ2))*HALF 
         XCO=(XO(IJ1)+XO(IJ2))*HALF 
         YCO=(YO(IJ1)+YO(IJ2))*HALF 
         U(IJ)=U(IJ)+(XCN-XCO)*DTR 
         V(IJ)=V(IJ)+(YCN-YCO)*DTR 
      END DO 
C 
      RETURN 
      END 
C 
C----------------------------------------------------------------------- 
C     Here come the routines with user-programmed input data and  
C     user-programmed interpretation of results.For each case, create  
C     separate user-files, and copy them prior to compilation to the  
C     file 'user.f'(routine BCIN provides boundary conditions, routine  
C     NEWCOR calculates new coordinates of grid points along boundaries,  
C     routine SOUT prints data out for steady flows and at selected
C     time steps in unsteady flows; routine TOUT writes data out at every  
C     time step).  
C----------------------------------------------------------------------- 
C 
      INCLUDE 'user.f' 
C 
C####################################################################### 
