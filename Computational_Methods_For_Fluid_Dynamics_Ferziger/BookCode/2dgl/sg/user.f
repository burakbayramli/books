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
        FLOMOM=FLOMOM+ABS(FMI(II))*SQRT(U(IJ)**2+V(IJ)**2)
        FLOMAS=FLOMAS-FMI(II)
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
C.....Calculate total shear force on the cylinder wall boundary,
C.....and the distribution of the shear stress. 
C
      FTAUX=0.
      FTAUY=0.
      WRITE(2,*) '  '
      WRITE(2,*) '      XC            YC           TAUX          TAUY '
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IJP=IJPW(IW)
        IJB=IJW(IW)
        COEF=VIS(IJB)*SRDW(IW)
        TAUX=COEF*((U(IJP)-U(IJB))*XTW(IW)**2+
     *             (V(IJP)-V(IJB))*XTW(IW)*YTW(IW))
        TAUY=COEF*((U(IJP)-U(IJB))*XTW(IW)*YTW(IW)+
     *               (V(IJP)-V(IJB))*YTW(IW)**2)
        S=SQRT((X(IJW1(IW))-X(IJW2(IW)))**2+
     *         (Y(IJW1(IW))-Y(IJW2(IW)))**2)
        IF(LAXIS) S=S*YC(IJB)
        FTAUX=FTAUX+TAUX
        FTAUY=FTAUY+TAUY
        WRITE(2,'(1P4E14.4)') XC(IJB),YC(IJB),TAUX/S,TAUY/S
      END DO
      WRITE(2,*) '  '
      WRITE(2,*) '   TOTAL SHEAR FORCE IN X-DIRECTION: ',FTAUX
      WRITE(2,*) '   TOTAL SHEAR FORCE IN Y-DIRECTION: ',FTAUY
      WRITE(2,*) '  '
C
C.....Calculate total pressure force on the cylinder wall boundary,
C.....and the distribution of the pressure. 
C.....Note: this is the force exerted by fluid onto wall.
C
      FPRX=0.
      FPRY=0.
      WRITE(2,*) '  '
      WRITE(2,*) '      XC            YC         PRESSURE '
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IJB=IJW(IW)
        SX=Y(IJW1(IW))-Y(IJW2(IW))
        SY=X(IJW1(IW))-X(IJW2(IW))
        IF(LAXIS) SX=SX*YC(IJB)
        IF(LAXIS) SY=SY*YC(IJB)
        FPRX=FPRX+P(IJB)*SX
        FPRY=FPRY-P(IJB)*SY
        WRITE(2,'(1P3E14.4)') XC(IJB),YC(IJB),P(IJB)
      END DO
      WRITE(2,*) '  '
      WRITE(2,*) '   TOTAL PRESSURE FORCE IN X-DIRECTION: ',FPRX
      WRITE(2,*) '   TOTAL PRESSURE FORCE IN Y-DIRECTION: ',FPRY
      WRITE(2,*) '  '
C
      RETURN
      END
C
C
C########################################################
      SUBROUTINE TOUT(K)
C########################################################
C     This routine prints some additional quantities in each
C     time step, as programmed by the user for the problem 
C     considered. For example, time variation of the forces
C     acting on a body may be of interest...     
C========================================================
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
      CALL SETIND(K)
C
C.....Calculate total shear force on the cylinder wall boundary.
C
      FTAUX=0.
      FTAUY=0.
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IJP=IJPW(IW)
        IJB=IJW(IW)
        COEF=VIS(IJB)*SRDW(IW)
        TAUX=COEF*((U(IJP)-U(IJB))*XTW(IW)**2+
     *             (V(IJP)-V(IJB))*XTW(IW)*YTW(IW))
        TAUY=COEF*((U(IJP)-U(IJB))*XTW(IW)*YTW(IW)+
     *               (V(IJP)-V(IJB))*YTW(IW)**2)
        S=SQRT((X(IJW1(IW))-X(IJW2(IW)))**2+
     *         (Y(IJW1(IW))-Y(IJW2(IW)))**2)
        IF(LAXIS) S=S*YC(IJB)
        FTAUX=FTAUX+TAUX
        FTAUY=FTAUY+TAUY
      END DO
C
C.....Calculate total pressure force on the cylinder wall boundary.
C
      FPRX=0.
      FPRY=0.
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
        IJB=IJW(IW)
        SX=Y(IJW1(IW))-Y(IJW2(IW))
        SY=X(IJW1(IW))-X(IJW2(IW))
        IF(LAXIS) SX=SX*YC(IJB)
        IF(LAXIS) SY=SY*YC(IJB)
        FPRX=FPRX+P(IJB)*SX
        FPRY=FPRY-P(IJB)*SY
      END DO
      CD=2.*(FPRX+FTAUX)
      CL=2.*(FPRY+FTAUY)
C
      WRITE(10,'(1P7E11.3)') TIME,CD,CL,FPRX,FTAUX,FPRY,FTAUY
C
      RETURN
      END
