C#######################################################################
      SUBROUTINE BCIN(K)
C#######################################################################
C     This routine sets inlet boundary conditions (which may also change 
C     in time in an unsteady problem; in that case, set the logical 
C     variable INIBC to 'true'. In the case of moving grid logical 
C     variable INIBC must be set to 'true'!)
C     It is called once at the begining of calculations on each grid 
C     level, unles INIBC=.TRUE. forces calls in each time step. This 
C     routine includes several options which are most often required and 
C     serves the test cases provided with the code; it may require 
C     adaptation to the problem being solved, especially regarding 
C     velocity or temperature distribution at boundaries.
C-----------------------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'var.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
C.....Set logical variable "inibc" and indices
C
      INIBC=.FALSE.
      IF(MOVGR) INIBC=.TRUE.
      CALL SETIND(K)     
C
C.....Inlet boundary conditions (Specified U,V and T)
C
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJ=IJI(II)
        U(IJ)=0.0 
        V(IJ)=ulid
        T(IJ)=0.0
      END DO
C
C.....Pressure boundary conditions (Specified pressure)
C
      DO IB=IPS(K)+1,IPS(K)+NPRE(K)
        IJP=IJPP(IB)
        IJB=IJPB(IB)
        IF(YC(IJB).LT.1.) THEN
          P(IJB)=6.
        ELSE
          P(IJB)=0.
        END IF
      END DO
C
C.....Wall boundary conditions (momentum equation)
C
      DO IW=IWS(K)+1,IWS(K)+NWAL(K)
         IJ=IJW(IW)
         IF((IJW(IW)-IJPW(IW)).EQ.1) THEN
           U(IJ)=0.
         ELSE
           U(IJ)=0.
         END IF
         V(IJ)=0.
      END DO
C
C.....Wall boundary conditions (energy equation: isothermal wall)
C
      IF(LCAL(IEN)) THEN
        DO IW=IWS(K)+1,IWS(K)+NWALI(K)
          IJ=IJW(IW)
          IF((IJW1(IW)-IJW2(IW)).LT.0) THEN
            T(IJ)=TH
          ELSE
            T(IJ)=TC
          ENDIF
        END DO
      ENDIF
C
C.....Wall boundary conditions (energy equation: adiabatic wall)
C
      IF(LCAL(IEN)) THEN
        DO IW=IWAS(K)+1,IWAS(K)+NWALA(K)
          IJ=IJW(IW)
          T(IJ)=TREF
        END DO
      ENDIF
C
C.....Calculate inlet mass fluxes, momentum and mass flow rate
C
      FLOMOM=0.
      FLOMAS=0.
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJ=IJI(II)
        RC=0.5*(R(IJI1(II))+R(IJI2(II)))
        SX=(Y(IJI1(II))-Y(IJI2(II)))*RC
        SY=(X(IJI2(II))-X(IJI1(II)))*RC
        FMI(II)=DEN(IJ)*(U(IJ)*SX+V(IJ)*SY)
        FLOMOM=FLOMOM+ABS(FMI(II))*SQRT(U(IJ)**2+V(IJ)**2)
        FLOMAS=FLOMAS-FMI(II)
      END DO
C
C.....Set residual normalisation factors
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
C#######################################################################
      SUBROUTINE NEWCOR(K)
C#######################################################################
C  This routine calculates new coordinates of boundary vertices acording 
C  to movement of the domain boundary specified by user.
C-----------------------------------------------------------------------
      include 'float.inc'
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
C-----------------------------------------------------------
C.....Calculate new coordinates of boundary vertices
C-----------------------------------------------------------
C
C.....North boundary
C
      DO I=1,NIM
         IJ=LI(I+IST)+NJM
         X(IJ)=X(IJ)
         Y(IJ)=Y(IJ)
         FIRAD=FIRAD+DFI
      END DO
C
C.....South boundary
C
      DO I=1,NIM
         IJ=LI(I+IST)+1
         X(IJ)=X(IJ)
         Y(IJ)=Y(IJ)
      END DO
C
C.....West boundary
C
      DO IJ=LI(1+IST)+2,LI(1+IST)+NJM-1
         X(IJ)=X(IJ)
         Y(IJ)=Y(IJ)
      END DO
C
C.....East boundary
C
      DO IJ=LI(NIM+IST)+2,LI(NIM+IST)+NJM-1
         X(IJ)=X(IJ)
         Y(IJ)=Y(IJ)
      END DO
C&&&&
C
      RETURN
      END
C
C#######################################################################
      SUBROUTINE SOUT(K)
C#######################################################################
C     This routine prints some additional quantities, as programmed by 
C     the user for the problem considered.Note that profiles can be 
C     obtained in post-processor.
C-----------------------------------------------------------------------
      include 'float.inc'
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
c
*     call shforc(k)
*     call prforc(k)
*     call hflux(k)
C
      RETURN
      END
C
C#######################################################################
      SUBROUTINE TOUT(K)
C#######################################################################
C     This routine prints some additional quantities, as programmed by 
C     the user for the problem considered. Note that profiles can be 
C     obtained in post-processor.
C-----------------------------------------------------------------------
      include 'float.inc'
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
      RETURN
      END
C
C-----------------------------------------------------------------------
C Some examples of routines which are used to print some additional 
C informations about flow, such as pressure or shear forces on wall
C boundaries, or total heat flux through the wall or a part of the 
C wall boundaries. 
C Note: These routines are not called by the main program but by the
C       routine SOUT. The same code could be programmed directly in that
C       routine.
C#######################################################################
      SUBROUTINE SHFORC(K)
C#######################################################################
C     This routine calculates total shear force on a part of wall 
C     and the distribution of the shear stress.
C-----------------------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
C.....Shear stress and shear force for bottom wall.
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
      RETURN
      END
C
C#######################################################################
      SUBROUTINE PRFORC(K)
C#######################################################################
C     This routine calculates total pressure force on a part of wall 
C     boundary and the distribution of the pressure.
C     Note: this is the force exerted by fluid onto wall.
C-----------------------------------------------------------------------

      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
C.....Pressure force and distribution on bottom wall.
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
      RETURN
      END
C
C#######################################################################
      SUBROUTINE HFLUX(K)
C#######################################################################
C     This routine calculates total heat flux through a part of wall 
C     boundary.
C-----------------------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
C
C.....Calculate heat flux through an isothermal wall (here HOT)
C
      WRITE(2,*) '  '
      WRITE(2,*) '      XC            YC         HEAT FLUX '
      HEATS=0.
      DO IW=IWS(K)+1,IWS(K)+NWALI(K)
        IF(T(IJW(IW)).GT.TREF) THEN
          HEAT=(VISC/PRANL)*SRDW(IW)*(T(IJW(IW))-T(IJPW(IW)))
          HEATS=HEATS+HEAT
          S=SQRT((X(IJW1(IW))-X(IJW2(IW)))**2+
     &           (Y(IJW1(IW))-Y(IJW2(IW)))**2)
          IF(LAXIS) S=S*YC(IJW(IW))
          WRITE(2,'(1P3E14.4)') XC(IJW(IW)),YC(IJW(IW)),HEAT/S
        ENDIF
      END DO
      WRITE(2,*) '  '
      WRITE(2,*) '   TOTAL HEAT FLUX THROUGH THE HOT WALL: ',HEATS
      WRITE(2,*) '  '
C
      RETURN
      END
