C#########################################################
      SUBROUTINE BCIN(K)
C#########################################################
C     This routine sets inlet boundary conditions (which 
C     may also change in time in an unsteady problem; in
C     that case, set the logical variable INIBC to 'true'.)
C     It is called once at the begining of calculations on 
C     each grid level, unles INIBC=.TRUE. forces calls in
C     each time step.
C     This routine includes several options which are most
C     often required and serves the test cases provided with
C     the code; it may require adaptation to the problem
C     being solved, especially regarding velocity or
C     temperature distribution at boundaries.
C=========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'var.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
C.....SET INDICES, INITIALIZE MOMENTUM AND MASS FLOW RATE
C
      INIBC=.FALSE.
      CALL SETIND(K)     
      FLOMOM=0.
      FLOMAS=0.
C
C.....READ or SET INLET BOUNDARY VALUES, CALCULATE INLET MASS FLUXES
C.....NOTE: FIRST COME VALUES AT WEST, THEN EAST, THEN SOUTH, THEN NORTH
C.....SIDE, IN THE ORDER OF INCREASING INDEX I OR J !!!
C
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJ=IJI(II)
        READ(5,*) U(IJ),V(IJ),T(IJ)
        RC=0.5*(R(IJI1(II))+R(IJI2(II)))
        SX=(Y(IJI1(II))-Y(IJI2(II)))*RC
        SY=(X(IJI2(II))-X(IJI1(II)))*RC
        FMI(II)=DEN(IJ)*(U(IJ)*SX+V(IJ)*SY)
        IF(FMI(II).LT.ZERO) THEN
          FLOMOM=FLOMOM+ABS(FMI(II))*SQRT(U(IJ)**2+V(IJ)**2)
          FLOMAS=FLOMAS-FMI(II)
        ENDIF
      END DO
C
C.....SET HOT AND COLD WALL TEMPERATURE (WEST COLD, EAST HOT)
C
      IF(LCAL(IEN)) THEN
        DO IW=IWS(K)+1,IWS(K)+NWALI(K)
          IJ=IJW(IW)
          IF((IJW1(IW)-IJW2(IW)).LT.0) THEN
            T(IJ)=TC
          ELSE
            T(IJ)=TH
          ENDIF
        END DO
      ENDIF
C
C.....SET ADIABATIC WALL TEMPERATURE TO REFERENCE TEMPERATURE
C
      IF(LCAL(IEN).AND.K.EQ.ONE) THEN
        DO IW=IWAS(K)+1,IWAS(K)+NWALA(K)
          IJ=IJW(IW)
          T(IJ)=TREF
        END DO
      ENDIF
C
C.....SET LID VELOCITY (FOR LID-DRIVEN CAVITIES ONLY; SET ULID=0. FOR
C     BUOYANCY_DRIVEN CAVITY FLOWS IN THE *.CIN FILE)
C
      DO I=2,NIM
        IJ=LI(I+IST)+NJ
        U(IJ)=ULID
      END DO
C
C.....SET RESIDUAL NORMALISATION FACTORS
C
      DO L=1,NFI
        RNOR(L)=1.
        RESOR(L)=0.0
      END DO
C
      IF(FLOMAS.LT.SMALL) FLOMAS=1.
      IF(FLOMOM.LT.SMALL) FLOMOM=1.
      WRITE(2,*) '   NORMALISATION OF MOMENTUM, MASS: ',FLOMOM,FLOMAS
      WRITE(2,*) '  '
C
      RNOR(IU)=1./(FLOMOM+SMALL)
      RNOR(IV)=RNOR(IU)
      RNOR(IP)=1./(FLOMAS+SMALL)
      RNOR(IEN)=1./((FLOMAS*TREF)+SMALL)
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE SOUT(K)
C########################################################
C     This routine prints some additional quantities, as
C     programmed by the user for the problem considered. 
C     Note that profiles can be obtained in post-processor.
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
      CALL SETIND(K)
C
C.....Calculate total shear force on a part of wall boundary,
C.....and the distribution of the shear stress. Here: bottom wall.
C
      FTAUX=0.
      FTAUY=0.
      WRITE(2,*) '  '
      WRITE(2,*) '      XC            YC           TAUX          TAUY '
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IF(YC(IJW(IW)).LT.1.E-5) THEN
          IJP=IJPW(IW)
          IJB=IJW(IW)
          COEF=VIS(IJB)*SRDW(IW)
          TAUX=COEF*((U(IJP)-U(IJB))*XTW(IW)**2+
     *               (V(IJP)-V(IJB))*XTW(IW)*YTW(IW))
          TAUY=COEF*((U(IJP)-U(IJB))*XTW(IW)*YTW(IW)+
     *               (V(IJP)-V(IJB))*YTW(IW)**2)
          S=SQRT((X(IJW1(IW))-X(IJW2(IW)))**2+
     *           (Y(IJW1(IW))-Y(IJW2(IW)))**2)
          IF(LAXIS) S=S*YC(IJB)
          FTAUX=FTAUX+TAUX
          FTAUY=FTAUY+TAUY
          WRITE(2,'(1P4E14.4)') XC(IJB),YC(IJB),TAUX/S,TAUY/S
        ENDIF
      END DO
      WRITE(2,*) '  '
      WRITE(2,*) '   TOTAL SHEAR FORCE IN X-DIRECTION: ',FTAUX
      WRITE(2,*) '   TOTAL SHEAR FORCE IN Y-DIRECTION: ',FTAUY
      WRITE(2,*) '  '
C
C.....Calculate total pressure force on a part of wall boundary,
C.....and the distribution of the pressure. Here: bottom wall.
C.....Note: this is the force exerted by fluid onto wall.
C
      FPRX=0.
      FPRY=0.
      WRITE(2,*) '  '
      WRITE(2,*) '      XC            YC         PRESSURE '
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IF(YC(IJW(IW)).LT.1.E-5) THEN
          IJB=IJW(IW)
          SX=Y(IJW1(IW))-Y(IJW2(IW))
          SY=X(IJW1(IW))-X(IJW2(IW))
          IF(LAXIS) SX=SX*YC(IJB)
          IF(LAXIS) SY=SY*YC(IJB)
          FPRX=FPRX+P(IJB)*SX
          FPRY=FPRY-P(IJB)*SY
          WRITE(2,'(1P3E14.4)') XC(IJB),YC(IJB),P(IJB)
        ENDIF
      END DO
      WRITE(2,*) '  '
      WRITE(2,*) '   TOTAL PRESSURE FORCE IN X-DIRECTION: ',FPRX
      WRITE(2,*) '   TOTAL PRESSURE FORCE IN Y-DIRECTION: ',FPRY
      WRITE(2,*) '  '
C
C.....CALCULATE HEAT FLUX THROUGH AN ISOTHERMAL WALL (here HOT)
C.....HEAT(IW) is the specific heat flux (per unit area); HEATS is 
C.....the total flux through the wall surface.
C
      IF(LCAL(IEN)) THEN
        WRITE(2,*) '  '
        WRITE(2,*) '      XC            YC         HEAT FLUX '
        HEATS=0.
        DO IW=IWS(K)+1,IWS(K)+NWALI(K)
          IF(T(IJW(IW)).GT.TREF) THEN
            HEAT=(VISC/PRANL)*SRDW(IW)*(T(IJW(IW))-T(IJPW(IW)))
            HEATS=HEATS+HEAT
            S=SQRT((X(IJW1(IW))-X(IJW2(IW)))**2+
     *             (Y(IJW1(IW))-Y(IJW2(IW)))**2)
            IF(LAXIS) S=S*YC(IJW(IW))
            WRITE(2,'(1P3E14.4)') XC(IJW(IW)),YC(IJW(IW)),HEAT/S
          ENDIF
        END DO
        WRITE(2,*) '  '
        WRITE(2,*) '   TOTAL HEAT FLUX THROUGH THE HOT WALL: ',HEATS
        WRITE(2,*) '  '
      ENDIF
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE TOUT(K)
C########################################################
C     This routine prints some additional quantities, as
C     programmed by the user for the problem considered.
C     Note that profiles can be obtained in post-processor.
C========================================================
      INCLUDE 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
C
      RETURN
      END
