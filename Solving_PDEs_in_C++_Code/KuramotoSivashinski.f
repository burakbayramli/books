c       KURAMOTO-SIVASHINSKI FORTRAN PROGRAM
c       ----------------------------
c       This Fortran program is presented here for the
c       sake of comparison. Indeed, although working well,
c       it is far less elegant and transparent than the
c       corresponding code in Chapter 6.
c       The program computes the numerical solution
c       of the Kuramoto-Sivashinski equation, with
c       initial contiditions u(0)=u''(0)=0 and u'(0)
c       chosen in such a way that the numerical solution
c       remains reasonably bounded for as long as possible.
c       The error in the Taylor scheme is estimated
c       by computational error estimates, and is shown
c       to be smaller than 0.001 in every spatial direction
c       (including the unstable one) for as long as r=15.
c       This proves the existence of asymptotic solution
c       for some initial conditions.

      PROGRAM PROV(OUTPUT)

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ASUP(2),AINF(2),A(2),C(2),AINT(2) 
     &,Z1(2),Z2(2),Z3(2),Z1INT(2),Z2INT(2),Z3INT(2),
     &Z1INF(2),Z2INF(2),Z3INF(2),
     &Z1SUP(2),Z2SUP(2),Z3SUP(2)
      C(1)=.85D0
      C(2)=.85D0
      PRINT*,'C=',C(1)
      H=1.D0/DBLE(2**4)
      PRINT*,'H=',H 
      A(1)=-1.72551753142265433937814595D0
      A(2)=A(1)
      ASUP(1)=A(1)+3.D-6
      ASUP(2)=ASUP(1)
      AINF(1)=A(1)-3.D-6
      AINF(2)=AINF(1)
      PRINT*,'A=',A 
      CALL RAD(C,A,R0)
      CALL TAILOR(R0,C,A,H,RINT,Z1,Z2,Z3)
      PRINT*,'AINF=',AINF
      CALL RAD(C,AINF,R0)
      CALL TAILOR(R0,C,AINF,H,RINT,Z1INF,Z2INF,Z3INF)
      PRINT*,'ASUP=',ASUP
      CALL RAD(C,ASUP,R0)
      CALL TAILOR(R0,C,ASUP,H,RINT,Z1SUP,Z2SUP,Z3SUP)
      PRINT*,'TRANSFORMED OPT-BOX'
      PRINT*,'R=',RINT
      PRINT*,'Z1=',Z1
      PRINT*,'Z2=',Z2
      PRINT*,'Z3=',Z3
C     PRINT*,'TRANSFORMED INT-BOX'
C     PRINT*,'Z1=',Z1INT
C     PRINT*,'Z2=',Z2INT
C     PRINT*,'Z3=',Z3INT
      PRINT*,'TRANSFORMED INF-BOX'
      PRINT*,'Z1=',Z1INF
      PRINT*,'Z2=',Z2INF
      PRINT*,'Z3=',Z3INF
      PRINT*,'TRANSFORMED SUP-BOX'
      PRINT*,'Z1=',Z1SUP
      PRINT*,'Z2=',Z2SUP
      PRINT*,'Z3=',Z3SUP
      END 
  
      SUBROUTINE PROD(INT1,INT2,PRO)
      DOUBLE PRECISION INT1(2),INT2(2),PRO(2),PRO1,PRO2
      EQUIVALENCE(PRO1,J1)
      EQUIVALENCE(PRO2,N1)
      IF (INT1(2).GE.0.D0)THEN
       IF (INT1(1).GE.0.D0)THEN
        IF (INT2(2).GE.0.D0)THEN
         IF(INT2(1).GE.0.D0)THEN
          PRO1=INT1(1)*INT2(1)
          PRO2=INT1(2)*INT2(2)
         ELSE
          PRO1=INT1(2)*INT2(1)
          PRO2=INT1(2)*INT2(2)
         ENDIF
        ELSE
         PRO1=INT1(2)*INT2(1) 
         PRO2=INT1(1)*INT2(2) 
        ENDIF
       ELSE
        IF(INT2(2).GE.0.D0)THEN
         IF(INT2(1).GE.0.D0)THEN
          PRO1=INT1(1)*INT2(2)
          PRO2=INT1(2)*INT2(2)
         ELSE
          PRO1=DMIN1(INT1(1)*INT2(2),INT1(2)*INT2(1))
          PRO2=DMAX1(INT1(1)*INT2(1),INT1(2)*INT2(2))
         ENDIF
        ELSE
         PRO1=INT1(2)*INT2(1) 
         PRO2=INT1(1)*INT2(1) 
        ENDIF
       ENDIF
      ELSE
       IF(INT2(2).GE.0.D0)THEN
        IF(INT2(1).GE.0.D0)THEN
         PRO1=INT1(1)*INT2(2) 
         PRO2=INT1(2)*INT2(1) 
        ELSE
         PRO1=INT1(1)*INT2(2) 
         PRO2=INT1(1)*INT2(1) 
        ENDIF
       ELSE
        PRO1=INT1(2)*INT2(2)
        PRO2=INT1(1)*INT2(1)
       ENDIF
      ENDIF
      IF(PRO1.EQ.0.D0)PRO1=-2.D0**(-96) 
      IF(PRO1.NE.0.D0)THEN
        I1=SHIFT(J1,-48)
        I1=MOD(I1,2**12)
        PRO1=PRO1-1.D0/(2.D0**(975+96-IABS(I1)))
      ENDIF
      IF(PRO2.EQ.0.D0)PRO2=2.D0**(-96)
      IF(PRO2.NE.0.D0)THEN
        M1=SHIFT(N1,-48)
        M1=MOD(M1,2**12)
        PRO2=PRO2+1.D0/(2.D0**(975+96-IABS(M1)))
      ENDIF
      PRO(1)=PRO1
      PRO(2)=PRO2
C     PRINT*,'INT1=',INT1
C     PRINT*,'INT2=',INT2
      IF(PRO(1).GT.PRO(2))THEN
       PRINT *,'ERROR'
       PRINT *,INT1(1),INT1(2)
       PRINT *,INT2(1),INT2(2)
       PRINT*,'PRO=',PRO(1),PRO(2)
       STOP
      ENDIF
      RETURN
      END 
  
      SUBROUTINE SUM(INT1,INT2,SUMAT)
      DOUBLE PRECISION INT1(2),INT2(2),SUMAT(2),SUM1,SUM2
      EQUIVALENCE(SUM1,J2)
      EQUIVALENCE(SUM2,N2)
      SUM1=INT1(1)+INT2(1)
      SUM2=INT1(2)+INT2(2)
      IF(SUM1.EQ.0.D0)SUM1=-2.D0**(-96) 
      IF(SUM1.NE.0.D0)THEN
        I2=SHIFT(J2,-48)
        I2=MOD(I2,2**12)
        SUM1=SUM1-1.D0/(2.D0**(975+96-IABS(I2)))
      ENDIF
      IF(SUM2.EQ.0.D0)SUM2=2.D0**(-96)
      IF(SUM2.NE.0.D0)THEN
        M2=SHIFT(N2,-48)
        M2=MOD(M2,2**12)
        SUM2=SUM2+1.D0/(2.D0**(975+96-IABS(M2)))
      ENDIF
      SUMAT(1)=SUM1 
      SUMAT(2)=SUM2 
      IF(SUMAT(1).GT.SUMAT(2))THEN
       PRINT *,'ERROR'
       PRINT *,INT1(1),INT1(2)
       PRINT *,INT2(1),INT2(2)
       PRINT*,'SUM=',SUMAT(1),SUMAT(2)
       STOP
      ENDIF
      RETURN
      END 
  
      SUBROUTINE DIV(SCAL,INT,DI)
      DOUBLE PRECISION SCAL,INT(2),DI(2),DI1,DI2
      EQUIVALENCE(DI1,J3)
      EQUIVALENCE(DI2,N3)
C     PRINT*,SCAL,INT
      IF(SCAL.GE.0.D0)THEN
       DI1=INT(1)/SCAL
       DI2=INT(2)/SCAL
      ELSE
       DI1=INT(2)/SCAL
       DI2=INT(1)/SCAL
      ENDIF
      IF(DI1.EQ.0.D0)DI1=-2.D0**(-96)
      IF(DI1.NE.0.D0)THEN
        I3=SHIFT(J3,-48)
        I3=MOD(I3,2**12)
        DI1=DI1-1.D0/(2.D0**(975+96-IABS(I3)))
      ENDIF
      IF(DI2.EQ.0.D0)DI2=2.D0**(-96)
      IF(DI2.NE.0.D0)THEN
        M3=SHIFT(N3,-48)
        M3=MOD(M3,2**12)
        DI2=DI2+1.D0/(2.D0**(975+96-IABS(M3)))
      ENDIF
      DI(1)=DI1
      DI(2)=DI2
      IF(DI(1).GT.DI(2))THEN
       PRINT *,'ERROR'
       PRINT *,'INT=',INT(1),INT(2)
       PRINT*,'SCAL=',SCAL
       PRINT*,'DI=',DI(1),DI(2)
       STOP
      ENDIF
      RETURN
      END 
  
      SUBROUTINE MULT(SCAL,INT,MUL)
      DOUBLE PRECISION SCAL,INT(2),MUL(2),MUL1,MUL2
      EQUIVALENCE(MUL1,J4)
      EQUIVALENCE(MUL2,N4)
C     PRINT*,SCAL,INT
      IF(SCAL.GT.0.D0)THEN
       MUL1=SCAL*INT(1)
       MUL2=SCAL*INT(2)
      ELSE
       MUL1=SCAL*INT(2)
       MUL2=SCAL*INT(1)
      ENDIF
      IF(MUL1.EQ.0.D0)MUL1=-2.D0**(-96) 
      IF(MUL1.NE.0.D0)THEN
        I4=SHIFT(J4,-48)
        I4=MOD(I4,2**12)
        MUL1=MUL1-1.D0/(2.D0**(975+96-IABS(I4)))
      ENDIF
      IF(MUL2.EQ.0.D0)MUL2=2.D0**(-96)
      IF(MUL2.NE.0.D0)THEN
        M4=SHIFT(N4,-48)
        M4=MOD(M4,2**12)
        MUL2=MUL2+1.D0/(2.D0**(975+96-IABS(M4)))
      ENDIF
      MUL(1)=MUL1
      MUL(2)=MUL2
C     PRINT*,'MUL=',MUL
      IF(MUL(1).GT.MUL(2))THEN
       PRINT *,'ERROR'
       PRINT *,'INT=',INT(1),INT(2)
       PRINT*,'SCAL=',SCAL
       PRINT *,'MUL=',MUL(1),MUL(2)
       STOP
      ENDIF
      RETURN
      END 
  
      SUBROUTINE ABSINT(INT,ABSIN)
      DOUBLE PRECISION INT(2),ABSIN(2)
      ABSIN(2)=DMAX1(DABS(INT(1)),DABS(INT(2)))
      IF((INT(1).LE.0.D0).AND.(INT(2).GE.0.D0))THEN
        ABSIN(1)=0.D0
      ELSE
        ABSIN(1)=DMIN1(DABS(INT(1)),DABS(INT(2))) 
      ENDIF
      RETURN
      END 
  
      SUBROUTINE SQINT(INT,SQ)
      DOUBLE PRECISION INT(2),SQ(2),SQ1,SQ2
      EQUIVALENCE(SQ1,J5)
      EQUIVALENCE(SQ2,N5)
      SQ1=DSQRT(INT(1))
      SQ2=DSQRT(INT(2))
      I5=SHIFT(J5,-48)
      I5=MOD(I5,2**12)
      SQ1=SQ1-1.D0/(2.D0**(975+96-I5))
      M5=SHIFT(N5,-48)
      M5=MOD(M5,2**12)
      SQ2=SQ2+1.D0/(2.D0**(975+96-M5))
      SQ(1)=SQ1
      SQ(2)=SQ2
      IF(SQ(1).GT.SQ(2))THEN
       PRINT *,'ERROR'
       PRINT *,INT(1),INT(2)
       PRINT*,'SQ=',SQ(1),SQ(2)
       STOP
      ENDIF
      RETURN
      END 
  
      SUBROUTINE BOUND(C,A1)
      IMPLICIT DOUBLE PRECISION(A-H,P-Z)
      DIMENSION C(2),A1(2),F1(2),F2(2),C2(2),A12(2)
     &,RR1P(2),UNIT(2),RR112(2),RR1121(2),SQA(2)
     &,DENOM(2),CONST(2),DELTA(2),RR0(2)
     &,RR1OP(2),RR0P(2)
      DOUBLE PRECISION NOM(2),KEEPSUM(2)
      COMMON/SS/SUP/RR/RR1(2) 
      DELTA(1)=.1D0 
      DELTA(2)=.1D0 
      CALL ABSINT(A1,RR1)
      CALL PROD(C,C,C2)
      CALL MULT(-2.D0,A1,A12) 
      CALL SUM(C2,A12,KEEPSUM)
      CALL ABSINT(KEEPSUM,CONST)
      UNIT(1)=1.D0
      UNIT(2)=1.D0
1     CALL DIV(RR1(2),UNIT,RR1P)
      RR1OP(1)=RR1P(1)
      CALL DIV(RR1(1),UNIT,RR1P)
      RR1OP(2)=RR1P(2)
      CALL MULT(12.D0,RR1,RR112)
      CALL SUM(UNIT,RR112,RR1121)
      CALL SQINT(RR1121,SQA)
      CALL SUM(UNIT,SQA,NOM)
      CALL DIV(48.D0,NOM,F1)
      CALL DIV(16.D0,RR1OP,DENOM)
      CALL PROD(CONST,DENOM,F2)
      IF(F1(1).GE.F2(2))THEN
        RR0(1)=F1(1)
        RR0(2)=F1(2)
      ELSE
        CALL SUM(RR1,DELTA,RR1)
        GO TO 1
      ENDIF
      CALL DIV(RR0(2),UNIT,RR0P)
      SUP=DSQRT(.5D0*RR0P(1)) 
      PRINT*,'SUP=',SUP
      RETURN
      END 
  
      SUBROUTINE RAD(C,A1,RADS)
      IMPLICIT DOUBLE PRECISION(A-C,E-H,K,O-Z)
      DIMENSION SQUARES(2),C(2),A1(2),RADIUS(2),
     &KEEPCO(2),KEEPSUM(2),
     &C2(2),KEEPA1(2),RADI(2) 
      COMMON /KK/CO(2,0:96)/SS/SUP
      INTEGER D
      CO(1,0)=A1(1) 
      CO(2,0)=A1(2) 
      CALL PROD(C,C,C2)
      CALL MULT(-2.D0,A1,KEEPA1)
      CALL SUM(C2,KEEPA1,KEEPSUM)
      CALL DIV(16.D0,KEEPSUM,CO(1,1))
      CALL PROD(A1,A1,SQUARES)
      DO 10 D=1,95
      M1=2*D+2
      CALL MULT(-0.5D0,SQUARES,KEEPSQ)
      CALL MULT(-DBLE(M1),CO(1,D),KEEPCO)
      CALL SUM(KEEPSQ,KEEPCO,KEEPSUM)
      CALL DIV(DBLE(M1*M1*(M1+2)),KEEPSUM,CO(1,D+1))
      SQUARES(1)=0.D0
      SQUARES(2)=0.D0
      DO 10 J=0,D
      CALL PROD(CO(1,J),CO(1,D-J),KEEPCO)
      CALL SUM(SQUARES,KEEPCO,SQUARES)
10    CONTINUE
      CALL BOUND(C,A1)
      RADS=0.D0
77    RADS=RADS+0.0625D0
      IF(RADS+0.0625D0.LT.SUP)GO TO 77
      PRINT*,'RAD=',RADS
      RETURN
      END 
  
      SUBROUTINE Y1(R0,YY)
      IMPLICIT DOUBLE PRECISION(A-C,E-H,K,O-Z)
      DIMENSION F(2),YY(2),REZID(2)
      COMMON /KK/CO(2,0:96)/ZZ/REZY(2)/RR/RR1(2)
      INTEGER D
      R02=R0*R0
      F(1)=CO(1,96) 
      F(2)=CO(2,96) 
      DO 40 D=95,0,-1
      CALL MULT(R02,F,F)
40    CALL SUM(F,CO(1,D),F)
      CALL MULT(R0,F,F)
      REZID(2)=1.D0/DBLE(2**30)/DBLE(2**30)/DBLE(2**36)
      REZID(1)=-REZID(2)
      CALL PROD(RR1,REZID,REZY)
      CALL MULT(R0,REZY,REZY) 
      CALL SUM(F,REZY,YY)
      RETURN
      END 
  
      SUBROUTINE U1(R0,UU)
      IMPLICIT DOUBLE PRECISION(A-C,E-H,K,O-Z)
      DIMENSION F(2),UU(2),KEEPCOD(2)
      COMMON /KK/CO(2,0:96)/ZZ/REZY(2)
      INTEGER D
      R02=R0*R0
      CALL MULT(193.D0,CO(1,96),F)
      DO 50 D=95,0,-1
      CALL MULT(DBLE(2*D+1),CO(1,D),KEEPCOD)
      CALL MULT(R02,F,F)
50    CALL SUM(F,KEEPCOD,F)
      CALL SUM(F,REZY,UU)
      RETURN
      END 
  
      SUBROUTINE V1(R0,VV)
      IMPLICIT DOUBLE PRECISION(A-C,E-H,K,O-Z)
      DIMENSION F(2),KEEPCOD(2),VV(2)
      COMMON /KK/CO(2,0:96)/ZZ/REZY(2)
      INTEGER D
      R02=R0*R0
      CALL MULT(192.D0*193.D0,CO(1,96),F)
      DO 60 D=95,1,-1
      M2=2*D
      CALL MULT(DBLE(M2*(M2+1)),CO(1,D),KEEPCOD)
      CALL MULT(R02,F,F)
60    CALL SUM(F,KEEPCOD,F)
      CALL MULT(R0,F,F)
      CALL SUM(F,REZY,VV)
      RETURN
      END 
  
      SUBROUTINE TAILOR(R0,C,A1,H,RO,Z,DZ,DDZ)
      IMPLICIT  DOUBLE PRECISION(A-H,K-L,O-Z)
      DIMENSION Y(2,2,0:18),MB(0:16,0:16),
     &C(2),A1(2),CSQ2(2),KEEPYR(2),KEEPR(2),KEEPCY(2),
     &YY(2),DDYR(2),DYR2(2),YR3(2),YO(2,0:2),KEEPCR(2),
     &YN(2,0:2),KEEPY(2),KEEPDY(2),KEEPDDY(2),KEEPSUM(2),
     &RPPROD(2),KEEPU(2),DIVRO(2),
     &UNIT(2),RP(2,0:18),P(0:16),STEP(2),
     &DIVRO2(2),DIVRO3(2),Z(2),DZ(2),DDZ(2),SQ2(2),TWO(2)
      INTEGER P,K,M,MB
      CALL SQRS(C(1))
      TWO(1)=2.D0
      TWO(2)=2.D0
      CALL SQINT(TWO,SQ2)
      CALL PROD(SQ2,C,CSQ2)
      L=4.D0
      RO=R0
      CALL Y1(R0,YO(1,0))
      CALL U1(R0,YO(1,1))
      CALL V1(R0,YO(1,2))
      UNIT(1)=1.D0
      UNIT(2)=1.D0
      CALL DIV(RO,UNIT,DIVRO) 
      CALL DIV(-(RO**2),UNIT,DIVRO2)
      CALL DIV(RO**3,UNIT,DIVRO3)
      CALL MULT(2.D0,DIVRO3,DIVRO3)
      CALL SUM(YO(1,0),DIVRO,YO(1,0))
      CALL SUM(YO(1,0),CSQ2,YO(1,0))
      CALL SUM(YO(1,1),DIVRO2,YO(1,1))
      CALL SUM(YO(1,2),DIVRO3,YO(1,2))
      PRINT *,'R0=',RO
      PRINT *,'Y0=',YO(1,0),YO(2,0)
      P(0)=1
      DO 35 J=1,16
35    P(J)=P(J-1)*J 
      DO 36 J=0,16
      MB(J,0)=1
36    MB(J,1)=J
      DO 15 J=2,16
      DO 14 N=2,J/2 
14    MB(J,N)=MB(J,N-1)*(J-N+1)/N
      DO 15 N=J/2+1,J
15    MB(J,N)=MB(J,J-N)
      STEP(1)=0.D0
      STEP(2)=H
65    IF(RO.GE.15.)GO TO 70
      J=1 
      J1=1
      IORD=14
      CALL DIV(RO,UNIT,RP(1,0))
      DO 13 I=1,17
      CALL MULT(-DBLE(I),RP(1,0),RPPROD)
13    CALL PROD(RPPROD,RP(1,I-1),RP(1,I))
      DO 701 I=0,2
      Y(1,J,I)=YO(1,I)
701   Y(2,J,I)=YO(2,I)
777   DO 16 I=0,IORD
      YY(1)=0.D0
      YY(2)=0.D0
      DDYR(1)=0.D0
      DDYR(2)=0.D0
      DYR2(1)=0.D0
      DYR2(2)=0.D0
      YR3(1)=0.D0
      YR3(2)=0.D0
      DO 17 N=0,I
      CALL PROD(Y(1,J,N),Y(1,J,I-N),KEEPY)
      CALL MULT(DBLE(MB(I,N)),KEEPY,KEEPY)
      CALL SUM(YY,KEEPY,YY)
      CALL PROD(RP(1,I-N),Y(1,J,N+2),KEEPY)
      CALL MULT(DBLE(MB(I,N)),KEEPY,KEEPY)
      CALL SUM(DDYR,KEEPY,DDYR)
      CALL PROD(RP(1,I-N+1),Y(1,J,N+1),KEEPDY)
      CALL MULT(DBLE(MB(I,N)),KEEPDY,KEEPDY)
      CALL SUM(DYR2,KEEPDY,DYR2)
      CALL PROD(RP(1,I-N+2),Y(1,J,N),KEEPYR)
      CALL MULT(DBLE(MB(I,N)),KEEPYR,KEEPYR)
17    CALL SUM(YR3,KEEPYR,YR3)
      CALL MULT(-.5D0,YY,KEEPSUM)
      CALL MULT(.5D0,RP(1,I+1),KEEPR)
      CALL SUM(KEEPSUM,KEEPR,KEEPSUM)
      CALL PROD(CSQ2,Y(1,J,I),KEEPCY)
      CALL SUM(KEEPSUM,KEEPCY,KEEPSUM)
      CALL MULT(-2.D0,DDYR,KEEPYR)
      CALL SUM(KEEPSUM,KEEPYR,KEEPSUM)
      CALL MULT(-1.D0,DYR2,KEEPYR)
      CALL SUM(KEEPSUM,KEEPYR,KEEPSUM)
      CALL MULT(-1.D0,Y(1,J,I+1),KEEPY) 
      CALL SUM(KEEPSUM,KEEPY,KEEPSUM)
      CALL MULT(-.5D0,YR3,KEEPYR)
      CALL SUM(KEEPSUM,KEEPYR,KEEPSUM)
      CALL PROD(CSQ2,RP(1,I+2),KEEPCR)
      CALL MULT(.5D0,KEEPCR,KEEPCR)
16    CALL SUM(KEEPSUM,KEEPCR,Y(1,J,I+3))
      IF(J.EQ.1)THEN
        DO 22 I=0,2 
        YN(1,I)=Y(1,J,15+I)
        YN(2,I)=Y(2,J,15+I)
        DO 22 M=14,0,-1
        CALL MULT(H,YN(1,I),KEEPY)
        CALL DIV(DBLE(M+1),KEEPY,YN(1,I))
22      CALL SUM(YN(1,I),Y(1,J,I+M),YN(1,I))
        CALL DIV(RO,UNIT,KEEPU)
        DIVRO(2)=KEEPU(2)
        CALL DIV(RO+H,UNIT,KEEPU)
        DIVRO(1)=KEEPU(1)
        RP(1,0)=DIVRO(1)
        RP(2,0)=DIVRO(2)
        DO 23 I=1,18
        CALL MULT(-DBLE(I),DIVRO,RPPROD)
23      CALL PROD(RPPROD,RP(1,I-1),RP(1,I))
        DO 700 I=0,2
        Y(1,2,I)=-L 
700     Y(2,2,I)=L
        J=2
        GO TO 777
      ENDIF
      IF((J.EQ.2).AND.(J1.EQ.1))THEN
        DO 72 I=0,2 
        Y(1,J,I)=Y(1,J,15+I)
        Y(2,J,I)=Y(2,J,15+I)
        DO 73 M=14,0,-1
        CALL PROD(STEP,Y(1,J,I),KEEPY)
        CALL DIV(DBLE(M+1),KEEPY,Y(1,J,I))
73      CALL SUM(Y(1,J,I),Y(1,1,I+M),Y(1,J,I))
        IF((Y(1,J,I).LT.-L).OR.(Y(2,J,I).GT.L))THEN
          PRINT*,'ERROR'
          PRINT*,'LIMD(',I,')Y=',Y(1,2,I),Y(2,2,I)
          STOP
        ENDIF
72      J1=2
        IORD=15
        GO TO 777
      ENDIF
      DO 710 I=0,2
      CALL MULT(H**8,Y(1,2,I+16),Y(1,2,I+16))
      CALL DIV(DBLE(P(16)),Y(1,2,I+16),Y(1,2,I+16))
      CALL MULT(H**8,Y(1,2,I+16),Y(1,2,I+16))
710   CALL SUM(YN(1,I),Y(1,2,I+16),YO(1,I))
      RO=RO+H
      CALL TRANSYZ(YO(1,0),YO(1,1),YO(1,2),Z,DZ,DDZ)
      PRINT*,'R=',RO,'ERRZ=',Z(2)-Z(1)
      PRINT*,'ERRDZ=',DZ(2)-DZ(1)
C     PRINT*,'ERRDDZ=',DDZ(2)-DDZ(1)
      PRINT*,'Z1=',Z
      PRINT*,'Z2=',DZ
      PRINT*,'Z3=',DDZ
      GO TO 65
70    CONTINUE
      RETURN
      END 
  
      SUBROUTINE SQRS(C)
      IMPLICIT DOUBLE PRECISION(A-H,K-M,O-Z)
      DIMENSION SQRIN(2),UNIT(2),L12(2) 
      COMMON/LL/L1(2),L2RE(2),L2IM(2)
      X0=C
      E=1.D-28
3     X02=X0*X0
      F=X02*X0-C*DSQRT(2.D0)+X0
      DF=3*X02+1.D0 
      IF(DABS(F).GT.E)THEN
        X0=X0-F/DF
        GO TO 3
      ENDIF
      UNIT(1)=1.D0
      UNIT(2)=1.D0
      L1(1)=X0-1.D-28
      L1(2)=X0+1.D-28
      CALL MULT(-.5D0,L1,L2RE)
      CALL PROD(L1,L1,L12)
      CALL MULT(.75D0,L12,SQRIN)
      CALL SUM(UNIT,SQRIN,SQRIN)
      CALL SQINT(SQRIN,L2IM)
      RETURN
      END 
  
      SUBROUTINE TRANSYZ(Y,DY,DDY,Z1,Z2,Z3)
      IMPLICIT DOUBLE PRECISION(A-H,K-M,O-Z)
      DIMENSION Y(2),DY(2),DDY(2),Z1(2),Z2(2),Z3(2),
     &L2REDY(2),L2REDY2(2),L2RE2(2),L2IM2(2),
     &KEEPSUM(2),MAGNY(2),ZNOM(2),L1L2RE(2),L1L2DY(2),L1L2DYM(2),
     &L1L2REY(2),Z2RE(2),L2IMDY(2),L1L2IM(2),L1L2Y(2),L1L2YM(2),
     &Z2IM(2),KEEPA(2),KEEPB(2),KEEPC(2),L1M(2),L2REL1(2),
     &L2REL12(2),MAGNL1(2),DENOM(2),MAGNK(2),DENOMK(2),
     &L2IM2M(2),UNIT(2)
      COMMON/GG/MAGN(2),MAGNL1P(2),DENOMP(2),DNBRE(2),DNBIM(2)
     &/LL/L1(2),L2RE(2),L2IM(2)
      UNIT(1)=1.D0
      UNIT(2)=1.D0
      CALL PROD(L2RE,L2RE,L2RE2)
      CALL PROD(L2IM,L2IM,L2IM2)
      CALL SUM(L2RE2,L2IM2,MAGN)
      CALL MULT(-1.D0,L1,L1M) 
      CALL SUM(L2RE,L1M,L2REL1)
      CALL PROD(L2REL1,L2REL1,L2REL12)
      CALL SUM(L2REL12,L2IM2,MAGNL1)
      CALL PROD(L2IM2,MAGNL1,DENOM)
      CALL MULT(4.D0,DENOM,DENOM)
      CALL DIV(MAGNL1(1),UNIT,MAGNK)
      MAGNL1P(2)=MAGNK(2)
      CALL DIV(MAGNL1(2),UNIT,MAGNK)
      MAGNL1P(1)=MAGNK(1)
      CALL DIV(DENOM(1),UNIT,DENOMK)
      DENOMP(2)=DENOMK(2)
      CALL DIV(DENOM(2),UNIT,DENOMK)
      DENOMP(1)=DENOMK(1)
      CALL MULT(-2.D0,L2IM,L2IM2M)
      CALL PROD(L2IM2M,L2IM,DNBRE)
      CALL PROD(L2IM2M,L2REL1,DNBIM)
      CALL PROD(L2RE,DY,L2REDY)
      CALL MULT(-2.D0,L2REDY,L2REDY2)
      CALL SUM(DDY,L2REDY2,KEEPSUM)
      CALL PROD(MAGN,Y,MAGNY) 
      CALL SUM(KEEPSUM,MAGNY,ZNOM)
      CALL PROD(ZNOM,MAGNL1P,Z1)
      CALL SUM(L1,L2RE,L1L2RE)
      CALL PROD(L1L2RE,DY,L1L2DY)
      CALL MULT(-1.D0,L1L2DY,L1L2DYM)
      CALL SUM(DDY,L1L2DYM,KEEPSUM)
      CALL PROD(L1,L2RE,L1L2RE)
      CALL PROD(L1L2RE,Y,L1L2REY)
      CALL SUM(KEEPSUM,L1L2REY,Z2RE)
      CALL PROD(L2IM,DY,L2IMDY)
      CALL PROD(L1,L2IM,L1L2IM)
      CALL PROD(L1L2IM,Y,L1L2Y)
      CALL MULT(-1.D0,L1L2Y,L1L2YM)
      CALL SUM(L2IMDY,L1L2YM,Z2IM)
      CALL PROD(Z2RE,DNBRE,KEEPA)
      CALL PROD(Z2IM,DNBIM,KEEPB)
      CALL MULT(-1.D0,KEEPB,KEEPC)
      CALL SUM(KEEPA,KEEPC,ZNOM)
      CALL PROD(ZNOM,DENOMP,Z2)
      CALL PROD(Z2RE,DNBIM,KEEPA)
      CALL PROD(Z2IM,DNBRE,KEEPB)
      CALL SUM(KEEPA,KEEPB,ZNOM)
      CALL PROD(ZNOM,DENOMP,Z3)
      RETURN
      END 
