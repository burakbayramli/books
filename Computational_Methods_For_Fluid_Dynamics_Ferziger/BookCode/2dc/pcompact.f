C#########################################################
      PROGRAM FV
C#########################################################
C     FINITE VOLUME METHOD FOR SOLVING CONSERVATION EQUATION
C     FOR SCALAR TRANSPORT USING CARTESIAN GRIDS AND KNOWN
C     VELOCITY FIELD (HERE: STAGNATION POINT FLOW, U=X AND
C     V=-Y; AT X=0, SCALAR VARIES FROM 0.0 AT Y=Ymax TO 1.0 AT
C     Y=0; AT X=Xmax, OUTFLOW - ZERO GRADIENT EXTRAPOLATION
C     FROM INSIDE; AT Y=Ymax, INFLOW, SCALAR = 0.0; AT Y=0,
C     NEUMANN BOUNDARY CONDITION, ZERO GRADIENT IN 
C     Y-DIRECTION).
C                               M. PERIC, IfS, 1996
C#########################################################
      PARAMETER (NX=162,NY=194)
      implicit double precision (a-h,o-z)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY),dfdx(nx,ny),
     *              dfdy(nx,ny)
      DIMENSION X(NX),Y(NY),XC(NX),YC(NY),CU(NX,NY),CV(NX,NY),
     *       FX(NX),FY(NY),DW(NY),fiw(ny),fisol(ny),fiex(ny)
      CHARACTER FILEO*20
C
C.....READ INPUT DATA
C
      PRINT *, ' ENTER OUTPUT FILE NAME:  '
      READ(*,1) FILEO
    1 FORMAT(A20)
      OPEN (UNIT=8,FILE=FILEO)
C
      PRINT *,' ENTER: DEN(sity), DIF(fusion coeff.)  '
      READ(*,*) DEN,DIF
      PRINT *, ' CHOOSE SCHEME: 1 - UDS; 2 - CDS; 3 - HOS  '
      READ(*,*) ID
      PRINT *, '  ENTER: ALFA  '
      READ(*,*) ALFA
      print *, ' enter under-relaxation factor) '
      read(*,*) urf
C
C.....DEFINE X-GRID (EXX - Exp. factor; NICV - Number of CV)
C
      PRINT *,' ENTER: XMIN, XMAX, EXX, NICV  '
      READ(*,*) XMIN,XMAX,EXX,N
C
      NI=N+2
      NIM=NI-1
      IF(EXX.EQ.1.) THEN
        DX=(XMAX-XMIN)/REAL(N)
      ELSE
        DX=(XMAX-XMIN)*(1.-EXX)/(1.-EXX**N)
      ENDIF
C
      X(1)=XMIN
      DO I=2,NIM
        X(I)=X(I-1)+DX
        DX=DX*EXX
      END DO
      X(NI)=X(NIM)
C
C.....COORDINATES OF CELL CENTERS
C
      XC(1)=X(1)
      DO I=2,NIM
        XC(I)=0.5*(X(I)+X(I-1))
      END DO
      XC(NI)=X(NIM)
C
C.....DEFINE Y-GRID (EXY - Exp. factor; NJCV - Number of CV)
C
      PRINT *,' ENTER: YMIN, YMAX, EXY, NJCV  '
      READ(*,*) YMIN,YMAX,EXY,M
C
      NJ=M+2
      NJM=NJ-1
      IF(EXY.EQ.1.) THEN
        DY=(YMAX-YMIN)/REAL(M)
      ELSE
        DY=(YMAX-YMIN)*(1.-EXY)/(1.-EXY**M)
      ENDIF
C
      Y(1)=YMIN
      DO J=2,NJM
        Y(J)=Y(J-1)+DY
        DY=DY*EXY
      END DO
      Y(NJ)=Y(NJM)
C
C.....COORDINATES OF CELL CENTERS
C
      YC(1)=Y(1)
      DO J=2,NJM
        YC(J)=0.5*(Y(J)+Y(J-1))
      END DO
      YC(NJ)=Y(NJM)
C
C.....CALCULATE INTERPOLATION FACTORS (FX = (X_e - X_P)/(X_E - X_P))
C
      FX(1)=0.
      DO I=2,NIM
        FX(I)=(X(I)-XC(I))/(XC(I+1)-XC(I))
      END DO
C
      FY(1)=0.
      DO J=2,NJM
        FY(J)=(Y(J)-YC(J))/(YC(J+1)-YC(J))
      END DO
C
C.....INITIALIZE VARIABLE VALUES
C
      DO I=1,NI
        DO J=1,NJ
          FI(I,J)=0.
        END DO
      END DO
C
      pi=4.*atan(1.0)
      DO I=2,NIM
	if(xc(i).ge.0.2.and.xc(i).le.0.6) then
	  fi(i,nj)=sin(2.5*(xc(i)-0.2)*pi)
	else
	  fi(i,nj)=0.
	endif
      END DO
C
C.....CALCULATE MASS FLUXES (CU = DEN*U*DY; CV = DEN*V*DX;
C     U = X(I) at cell face 'e'; V = Y(J) at cell face 'n')
C
      DO I=1,NIM
        DO J=2,NJM
          CU(I,J)=DEN*X(I)*(Y(J)-Y(J-1))
        END DO
      END DO
C
      DO J=1,NJM
        DO I=2,NIM
          CV(I,J)=-DEN*Y(J)*(X(I)-X(I-1))
        END DO
      END DO
c
c.....start solver loop due to deferred correction
c
      do k=1,1000
C
C.....INITIALIZE DIFFUSION COEFFICIENT AT WEST AND SOUTH FACE
C
      DO J=2,NJM
        DW(J)=-DIF*(Y(J)-Y(J-1))/(XC(2)-XC(1))
	fiw(j)=fi(1,j)
      END DO
C
      DO I=2,NIM
        ASD=-DIF*(X(I)-X(I-1))/(YC(2)-YC(1))
	fis=fi(i,1)
C
C.....LOOP OVER EAST AND NORTH FACE OF EACH CV; DIFFUSION COEFFICIENTS
C
        DO J=2,NJM
          AWD=DW(J)
          AED=-DIF*(Y(J)-Y(J-1))/(XC(I+1)-XC(I))
          AND=-DIF*(X(I)-X(I-1))/(YC(J+1)-YC(J))
C
C.....DISCRETIZE CONVECTION TERM (UDS coefficients)
C
          AEC= MIN(CU(I,J),0.)
          AWC=-MAX(CU(I-1,J),0.)
          ANC= MIN(CV(I,J),0.)
          ASC=-MAX(CV(I,J-1),0.)
c
c.....cell-face values (CDS & HOS)
c
          fie=fx(i)*fi(i+1,j)+(1.-fx(i))*fi(i,j)
	  fin=fy(j)*fi(i,j+1)+(1.-fy(j))*fi(i,j)
	  if (id.eq.3) then
	    fie=fie+0.125*(dfdx(i,j)-dfdx(i+1,j))
	    fin=fin+0.125*(dfdy(i,j)-dfdy(i,j+1))
	    dfdx(i,j)=(fie-fiw(j))/(x(i)-x(i-1))
	    dfdy(i,j)=(fin-fis)/(y(j)-y(j-1))
	  endif
c
          if((x(i)-1.0).lt.0.00001) then
	    fisol(j)=fie
          endif
C
C.....SET COEFFICIENTS MATRIX
C
          AE(I,J)=AED+AEC
          AW(I,J)=AWD+AWC
          AN(I,J)=AND+ANC
          AS(I,J)=ASD+ASC
          AP(I,J)=-(AE(I,J)+AW(I,J)+AN(I,J)+AS(I,J))
          Q(I,J)=0.
c
c....deferred correction
c
          if(id.gt.1) then
	    q(i,j)=-(fie*cu(i,j)-fiw(j)*cu(i-1,j)+fin*cv(i,j)-
     *               fis*cv(i,j-1))+awc*fi(i-1,j)+aec*fi(i+1,j)+
     *               asc*fi(i,j-1)+anc*fi(i,j+1)-
     *               (aec+awc+anc+asc)*fi(i,j)
          endif
C
          DW(J)=AED
	  fiw(j)=fie
          ASD=AND
	  fis=fin
        END DO
      END DO
C
C.....WEST BOUNDARY - DIRICHLET B.C.
C
      DO J=2,NJM
        Q(2,J)=Q(2,J)-AW(2,J)*FI(1,J)
        AW(2,J)=0.
C
C.....EAST BOUNDARY - OTFLOW B.C. (ZERO-GRAD. EXTRAPOLATION)
C
        AP(NIM,J)=AP(NIM,J)+AE(NIM,J)
        AE(NIM,J)=0.
      END DO
C
C.....SOUTH BOUNDARY - SYMMETRY B.C.
C
      DO I=2,NIM
        AP(I,2)=AP(I,2)+AS(I,2)
        AS(I,2)=0.
C
C.....NORTH BOUNDARY - INLET B.C.
C
        Q(I,NJM)=Q(I,NJM)-AN(I,NJM)*FI(I,NJ)
        AN(I,NJM)=0.
      END DO
C
C.....CALCULATE RESIDUALS
C
      RESL=0.
      DO I=2,NIM
        DO J=2,NJM
          RES=Q(I,J)-AP(I,J)*FI(I,J)-AN(I,J)*FI(I,J+1)-
     *      AS(I,J)*FI(I,J-1)-AE(I,J)*FI(I+1,J)-AW(I,J)*FI(I-1,J)
          RESL=RESL+ABS(RES)
	  ap(i,j)=ap(i,j)/urf
	  q(i,j)=q(i,j)+(1.-urf)*ap(i,j)*fi(i,j)
        END DO
      END DO
c
      write(8,*) '#    Outer iter. ',k,', Res = ',resl
      print *, ' Outer iter. ',k,', Res = ',resl
c
      if(resl.lt.1.e-10) go to 100
C
C.....SOLVE EQUATION SYSTEM
C
        CALL SIPSOL(ALFA)
c
      end do
  100 continue
c
c.....check the error
c
      err=0.
      do j=2,njm
        if(yc(j).ge.0.2.and.yc(j).le.0.6) then
	  fiex(j)=sin(2.5*(yc(j)-0.2)*pi)
        else
	  fiex(j)=0.
        endif
	err=err+(fisol(j)-fiex(j))**2
      end do
      err=sqrt(err)/real(nj-2)
C
C.....PRINT THE RESULT
C
      DO I=2,NIM
        FI(I,1)=FI(I,2)
      END DO
      DO J=1,NJ
        FI(NI,J)=FI(NIM,J)
      END DO
C
c      CALL PRINT(FI,' TEMP ')
C
      WRITE(8,*) '#  '
      IF(ID.EQ.1) WRITE(8,*) '#      UDS USED FOR CONVECTION '
      IF(ID.EQ.2) WRITE(8,*) '#      CDS USED FOR CONVECTION '
      IF(ID.EQ.3) WRITE(8,*) '#      HOS USED FOR CONVECTION '
      IF(IS.EQ.1) WRITE(8,*) '#      TDMA SOLVER IN X-DIRECTION '
      IF(IS.EQ.2) WRITE(8,*) '#      TDMA SOLVER IN Y-DIRECTION '
      IF(IS.EQ.3) WRITE(8,*) '#      SIP - SOLVER '
      WRITE(8,*) '#  yc(j),fiex(j),fisol(j)'
      do j=2,njm
        write(8,*) yc(j),fiex(j),fisol(j)
      end do
c
      STOP
      END
C
C############################################################
      SUBROUTINE SIPSOL(ALFA)
C############################################################
C     SIP solver of Stone (1968)
C============================================================
      PARAMETER (NX=162,NY=194,NXY=NX*NY)
      implicit double precision (a-h,o-z)
      COMMON /IDAT/ NI,NJ,NIM,NJM
      COMMON /RDAT/ FI(NX,NY),AE(NX,NY),AW(NX,NY),AN(NX,NY),
     *              AS(NX,NY),AP(NX,NY),Q(NX,NY),dfdx(nx,ny),
     *              dfdy(nx,ny)
      REAL LW,LS,LPR
      DIMENSION LW(NX,NY),LS(NX,NY),LPR(NX,NY),UN(NX,NY),
     *          UE(NX,NY),RES(NX,NY)
      DATA UN,UE /NXY*0.,NXY*0./
C
C.....CALCULATE COEFFICIENTS OF [L] AND [U] MATRICES
C
      DO I=2,NIM
        DO J=2,NJM
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
      DO L=1,1000
C
C.....CALCULATE RESIDUALS AND  {R}=[L^-1]{RES}
C
        RESL=0.
        DO I=2,NIM
          DO J=2,NJM
            RES(I,J)=Q(I,J)-AP(I,J)*FI(I,J)-AN(I,J)*FI(I,J+1)-
     *           AS(I,J)*FI(I,J-1)-AE(I,J)*FI(I+1,J)-AW(I,J)*FI(I-1,J)
            RESL=RESL+ABS(RES(I,J))
            RES(I,J)=(RES(I,J)-LS(I,J)*RES(I,J-1)-
     *      LW(I,J)*RES(I-1,J))*LPR(I,J)
          END DO
        END DO
C
        IF(L.EQ.1) RESNOR=RESL
        RSM=RESL/RESNOR
C
C.....CALCULATE INCREMENT {DEL}=[U^-1]{R}
C
        DO I=NIM,2,-1
          DO J=NJM,2,-1
            RES(I,J)=RES(I,J)-UN(I,J)*RES(I,J+1)-UE(I,J)*RES(I+1,J)
            FI(I,J)=FI(I,J)+RES(I,J)
          END DO
        END DO
C
C.....CHECK CONVERGENCE
C
        WRITE(8,*)  '#     ',L,' ITER., RSM =',RSM
        IF(RSM.LT.1.E-4) RETURN
C
      END DO
C
      RETURN
      END
C
