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
      DO II=IIS(K)+1,IIS(K)+NINL(K)
        IJ=IJI(II)
        U(IJ)=ULID
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
          T(IJ)=TH
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
      pi    = acos(-1.)
      coef  = pi/180.
      fi0   = 15.*coef
      period= 1.2
      omega = 2.*pi/period
      tref  = 0.6
      fi    = 0.
      write(51,*)  'DT=',dt,' T=',period,' OMEGA=',omega
C
C.....Grid motion begins after time TREF is elapsed (input parameter
C     in the file CaseName.cin)
C
      IF(TIME.GT.TREF) THEN
        dtime=time-tref
        fiold=fi0*sin(omega*(dtime-dt))
        fi=fi0*sin(omega*dtime)
C
C.....SOUTH BOUNDARY
C
        DO I=1,NIM
          IJ=LI(I+IST)+1
          X(IJ)=X(IJ)
          Y(IJ)=Y(IJ)
        END DO
C
C.....NORTH BOUNDARY
C
        ii1=  1
        ii2= 71
        ii3=171
        ii4=241
c
        do i=ii2,ii3
          IJ=LI(I+IST)+NJM
          x(ij)=x(ij)*cos(fi-fiold)-y(ij)*sin(fi-fiold)
          y(ij)=x(ij)*sin(fi-fiold)+y(ij)*cos(fi-fiold)
        end do
c
        DO I=1,NIM
          IJ=LI(I+IST)+NJM
c
          if(i.ge.ii1.and.i.le.ii2.or.i.ge.ii3.and.i.le.ii4) then
            ij1=li(ii1+ist)+njm
            ij2=li(ii2+ist)+njm
            elx=x(ij2)-x(ij1)
            ely=y(ij2)-y(ij1)
            disl=sqrt(elx**2+ely**2)
            dislo=sqrt((xo(ij2)-xo(ij1))**2+(yo(ij2)-yo(ij1))**2)
c
            disx=sqrt((xo(ij)-xo(ij1))**2+(yo(ij)-yo(ij1))**2)
            coefl=disx/(dislo+small)
            x(ij)=x(ij1)+coefl*elx
            y(ij)=y(ij1)+coefl*ely
          end if
        END DO
C
C.....WEST BOUNDARY
C
        DO IJ=LI(1+IST)+2,LI(1+IST)+NJM-1
           X(IJ)=X(IJ)
           Y(IJ)=Y(IJ)
        END DO
C
C.....EAST BOUNDARY
C
        DO IJ=LI(NIM+IST)+2,LI(NIM+IST)+NJM-1
           X(IJ)=X(IJ)
           Y(IJ)=Y(IJ)
        END DO
C&&&&
      END IF
C
      RETURN
      END
C
C#######################################################################
      SUBROUTINE SOUT(K)
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
C&&&&
      CALL SETIND(K)
      if(itim.gt.600.and.mod(itim,100).eq.0) call presdis(k)
C&&&&
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
      if(ltime.and.itim.eq.1) write(10,95)
c
      if(time.ge.0.4) then
        fshx=0.
        fshy=0.
        fprx=0.
        fpry=0.
C
        do iw=iws(k)+1,iws(k)+nwal(k)
          ijp=ijpw(iw)
          ijb=ijw(iw)
c
c..Pressure force
           sx=y(ijw1(iw))-y(ijw2(iw))
           sy=x(ijw1(iw))-x(ijw2(iw))
           if(laxis) sx=sx*yc(ijb)
           if(laxis) sy=sy*yc(ijb)
           fprx=fprx+p(ijb)*sx
           fpry=fpry-p(ijb)*sy
           fptot=sqrt(fprx**2+fpry**2)
c
c..Shear force
           coef=vis(ijb)*srdw(iw)
           taux=coef*((u(ijp)-u(ijb))*xtw(iw)*xtw(iw)
     &              + (v(ijp)-v(ijb))*xtw(iw)*ytw(iw))
           tauy=coef*((u(ijp)-u(ijb))*xtw(iw)*ytw(iw)
     &              + (v(ijp)-v(ijb))*ytw(iw)*ytw(iw))
           fshx=fshx+taux
           fshy=fshy+tauy
           fstot=sqrt(fshx**2+fshy**2)
        end do
        write(10,'(1p10e13.5)') time,fprx,fpry,fptot,fshx,fshy,fstot
      end if
C
   95 FORMAT('#',26x,'TOTAL PRESSURE & SHEAR FORCE',/,
     &       '#',4x,'TIME',9x,'FPRX',9X,'FPRY',9x,'FPTOT',8x,
     &              'FSHX',9x,'FSHY',9x,'FSTOT')
C
      RETURN
      END
C########################################################
      SUBROUTINE PRESDIS(K)
C########################################################
      INCLUDE 'float.inc'
      INCLUDE 'param.inc'
      INCLUDE 'indexc.inc'
      INCLUDE 'rcont.inc'
      INCLUDE 'var.inc'
      INCLUDE 'varold.inc'
      INCLUDE 'geo.inc'
      INCLUDE 'bound.inc'
      INCLUDE 'logic.inc'
      character filpres*13
C
C.....Write pressure distribution around the airfoil
C
      ind=itim
      if(.not.ltime) ind=0
C
      write(filpres,'(a,i6.6,a)') 'bpr',ind,'.dat'
c
      open(unit=90,file=filpres)
      rewind 90
      write(90,96) time
C
      do iw=iws(k)+1,iws(k)+nwal(k)
         ijb=ijw(iw)
         write(90,'(i7,1p5e13.5)') ijb,xc(ijb),yc(ijb),p(ijb)
      end do
C
   96 format('#     PRESSURE DISTRIBUTION ALONG THE PROFIL AT T='
     &       ,1pe15.5,' S',/,
     &       '#     IJB       XB      YB         P ')
C
      RETURN
      END
C
