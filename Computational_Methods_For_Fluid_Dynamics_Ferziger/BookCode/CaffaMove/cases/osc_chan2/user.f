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
C.....Set indices 
C
      INIBC=.FALSE.
      CALL SETIND(K)     
C
C.....Inlet boundary conditions (Specified U,V and T)
C
      b=0.01
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJ=IJI(II)
        coefb=yc(ij)/b
        u(ij)=ulid*6*coefb*(1.-coefb)
        V(IJ)=0.0
        T(IJ)=0.0
      END DO
C
C.....Pressure boundary conditions (Specified pressure)
C
      DO IB=IPS(K)+1,IPS(K)+NPRE(K)
        IJP=IJPP(IB)
        IJB=IJPB(IB)
        IF(XC(IJB).LT.1.) THEN
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
         U(IJ)=0.
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
      pi=acos(-1.)
      period=0.27
      t0=period
c
      h=0.
      a=4.14
      b=0.01
      hmax=0.38*b
      x1=4.0*b
      x3=6.5*b
      x2=0.5*(x1+x3)
C&&
      if(time.gt.t0) then
C
C.....South boundary
C
        tnor=(time-t0)/period
        h=0.5*hmax*(1.-cos(2.*pi*tnor))
        write(10,'(1p3e13.5)') time,tnor,h
        DO I=1,NIM
          IJ=LI(I+IST)+1
*         X(IJ)=X(IJ)
          if(abs(x(ij)).lt.x1) then
            y(ij)=h
          else if(abs(x(ij)).ge.x1.and.abs(x(ij)).le.x3) then
            y(ij)=0.5*h*(1.-tanh(a*(abs(x(ij))-x2)/b))
          end if
        END DO
      end if
      return
C&&&
C
C.....North boundary
C
        DO I=1,NIM
          IJ=LI(I+IST)+NJM
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
      character myfil*20
C&&
      CALL SETIND(K)
c
      period=0.27
      t0=period
      tnor=(time-t0)/period
c
      if(tnor.ge.0.0.and.tnor.le.1.0) then
        write(myfil,'(a,i4.4,a)') 'shsts',itim,'.dat'
        open(unit=21,file=myfil)
        write(myfil,'(a,i4.4,a)') 'shstn',itim,'.dat'
        open(unit=22,file=myfil)
        rewind 21
        rewind 22
c
        write(21,31) '#  SHEAR STERESS AND PRESSURE ON BOTTOM WALL',tnor
        write(22,31) '#  SHEAR STERESS AND PRESSURE ON UPPER WALL',tnor
        do iw=iws(k)+1,iws(k)+nwal(k)
          ijb=ijw(iw)
          ijp=ijpw(iw)
          if((ijp-ijb).eq.1) then
c
c..Botom wall
c
            call shstre(iw,ssx,ssy)
            write(21,'(1p4e13.5)') xc(ijb)*100.,ssx,ssy,p(ijb)
          else if((ijp-ijb).eq.-1) then
c
c..Upper wall
c
            call shstre(iw,ssx,ssy)
            write(22,'(1p4e13.5)') xc(ijb)*100.,ssx,ssy,p(ijb)
          end if
        end do
        close (21)
      end if
c
   31 format(a,/,'#  TNORM=',1pe13.5,/,'#     X','SSX','SSY','P')
C&&
      RETURN
      END
C
C#######################################################################
      SUBROUTINE TOUT(K)
C#######################################################################
C     This routine prints some additional quantities, as programmed by 
C     the user for the problem considered. Note that profiles can be 
C     obtained in post-processor.
C     Note: write data to UNIT=10
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
C######################################################
      SUBROUTINE SHSTRE(IW,SSX,SSY)
C######################################################
C     Calculate wall shear stress.
C------------------------------------------------------
      include 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
c
      ijp=ijpw(iw)
      ijb=ijw(iw)
      coef=vis(ijb)*srdw(iw)
      s=sqrt((x(ijw1(iw))-x(ijw2(iw)))**2+
     &       (y(ijw1(iw))-y(ijw2(iw)))**2)
      if(laxis) s=s*yc(ijb)
      ssx=coef*((u(ijp)-u(ijb))*xtw(iw)*xtw(iw)
     &        + (v(ijp)-v(ijb))*xtw(iw)*ytw(iw))/(s+small)
      ssy=coef*((u(ijp)-u(ijb))*xtw(iw)*ytw(iw)
     &        + (v(ijp)-v(ijb))*ytw(iw)*ytw(iw))/(s+small)
c
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
