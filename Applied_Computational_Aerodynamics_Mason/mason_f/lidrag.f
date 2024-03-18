C     MAIN PROGRAM FOR LIDRAG
C
      DIMENSION YSPAN(51),CCLCA(51)
      character*20 filenm
C
      write(6,90)
      read(5,95) filenm
      open(unit = 2, file = filenm, status = 'old')

      write(6,140)
      READ(2,100) FNSPAN
      NSPAN = FNSPAN
      DO 50 N = 1,NSPAN
      READ(2,100) YSPAN(N),CCLCA(N)
   50 WRITE(6,150)N, YSPAN(N),CCLCA(N)

      CALL LIDRAG(NSPAN,YSPAN,CCLCA,E,CL)

      WRITE(6,200) E,CL

      STOP

   90 format(/2x,'Program LIDRAG'//2x,'enter name of input data file')
   95 format(a20)
  100 FORMAT(5F10.5)
  140 FORMAT(//5X'LIDRAG - LIFT INDUCED DRAG ANALYSIS'//6X,
     1 'INPUT SPANLOAD'//9X,'N',5X,'Y/(B/2)',6X,'CCLCA')
  150 FORMAT(5X,I5,3F12.5)
  200 FORMAT(/5X,'Span e = ',F10.5,5x,'CL = ',F8.3/)

      END
   
      SUBROUTINE LIDRAG(NSPAN,YSPAN,CCLCA,E,CL)
C     WRITTEN BY DAVE IVES, RESEARCH, X2221, 4-5-72
C
      DIMENSION X(129),F(129),YSPAN(1),CCLCA(1)
      DOUBLE PRECISION PI,PIHALF,PI2,RADDEG
      COMMON/PIS/ PI,PIHALF,PI2,RADDEG
      PI     = 3.14159265358979323D0
      PIHALF = 1.57079632679489661D0
      PI2    = 6.28318530717958646D0
      RADDEG = 180.0D0/PI

      DO 10 J= 1,NSPAN
      X(J)   = YSPAN(J)
      F(J)   = CCLCA(J)
   10 CONTINUE

      M=32
      CALL MAST(NSPAN,M,X,F,SUM,A1)

      E  = A1**2 / SUM
      CL = PI*A1/4.

      RETURN
      END

      SUBROUTINE RFAST(X,Y,N,Z,W)
C     FAST FOURIER TRANSFORM OF REAL DATA BY DAVID IVES
C     X.....N REAL INPUT VALUES
C     Y.....N/2+1 COMPLEX OUTPUT VALUES
C     N.....EVEN NUMBER OF INPUT VALUES
C     Z.....DUMMY STORAGE OF LENGTH 2N (REAL)
C     W.....DUMMY STORAGE OF LENGTH N (REAL)
C     N MUST BE 4 OR MORE
C     THIS PRODUCES  OUTPUT Y  FROM  INPUT X , WHERE
C     ******************************************************************
C          K=N
C     Y(J)=SUM X(K)*EXP(-I*2*PI*(J-1)*(K-1)/N)
C          K=1
C     ******************************************************************
C     FOR J=1 TO J=N
C     WITH I=SQRT(-1) AND PI=3.14159........
C     NOTE THAT Y(N-J+2)=CONJ(Y(J)) FOR J=1 TO J=N/2+1
C     THUS ONLY Y(1) TO Y(N/2+1) ARE CALCULATED
C     COMPLEX NUMBERS ARE HANDLED IN FORTRAN 4 CONVENTION, NAMELY THE
C     REAL AND IMAGINARY PARTS ARE STORED IN ALTERNATE CELLS, STARTING
C     WITH THE REAL PART OF Y(1) IN THE FIRST LOCATION, ETC.
      DOUBLE PRECISION PI,PIHALF,PI2,RADDEG
      COMMON/PIS/ PI,PIHALF,PI2,RADDEG
      DIMENSION X(1),Y(1),W(1),Z(1)
      N2=N/2
      Y(N+1)=0.
      DO 2 J=2,N,2
    2 Y(N+1)=Y(N+1)+X(J-1)-X(J)
      Y(N+2)=0.
      CALL FAST(X,Y,N2,Z,W)
      DO 1 J=2,N2,2
      SJ=SIN((PI*(J/2))/N2)
      CJ=COS((PI*(J/2))/N2)
      JR=N-J
      Y1=Y(J+1)
      Y2=Y(J+2)
      Y3=Y(JR+1)
      Y4=Y(JR+2)
      Y(J+1)=.5*(Y1+Y3-SJ*(Y1-Y3)+CJ*(Y2+Y4))
      Y(J+2)=.5*(Y2-Y4-CJ*(Y1-Y3)-SJ*(Y2+Y4))
      Y(JR+1)=.5*(Y1+Y3+SJ*(Y1-Y3)-CJ*(Y2+Y4))
    1 Y(JR+2)=.5*(Y4-Y2-CJ*(Y1-Y3)-SJ*(Y2+Y4))
      Y(1)=Y(1)+Y(2)
      Y(2)=0.
      RETURN
      END

      SUBROUTINE MAST (N,M,X,F,SUM,A1)
      DIMENSION X(1),F(1),X2(512),F2(129)
      DIMENSION A(255),B(255),XX(512)
      DIMENSION FX(129),FXX(129),FXXX(129)
      DIMENSION YY(514),Z(1024),W(512)
      EQUIVALENCE (W(1),X2(1)),(Z(1),F2(1)),(Z(130),FX(1)),
     .            (Z(259),FXX(1)),(Z(388),FXXX(1))
C     N MUST NOT EXCEED 129
C     THE LARGEST PRIME FACTOR OF M SHOULD NOT EXCEED ELEVEN
C     M=4*N (OR THEREABOUTS) IS RECOMMENDED, WHERE POSSIBLE
C     IF M EXCEEDS 128 IT IS RESET TO 128
C     GAMMA AT THE TIP OF THE WING IS AUTOMATICALLY ZEROED
      IF(M.GT.128) M=128
      IF(N.GT.129) RETURN
      DO 1 I=1,N
    1 XX(I)=-ACOS(X(I))
      F(N)=0.
      CALL FIT2(N,XX,F,FX,FXX,FXXX,1,2,0.,0.)
      DX=(XX(N)-XX(1))/M
      X2(1)=XX(1)
      MP=M+1
      DO 10 I=2,MP
   10 X2(I)=X2(I-1)+DX
      CALL DELTA1(N,XX,MP,X2)
      CALL INTRP(MP,F,FX,FXX,FXXX,F2)
      F2(MP)=0.
      DO 11 I=1,M
      XX(I)=F2(MP-I+1)
      XX(I+M)=F2(I)
      XX(I+2*M)=-F2(MP-I+1)
   11 XX(I+3*M)=-F2(I)
      CALL SERIES(XX,M*4,AZ,A,B,AN,YY,Z,W)
      SUM=0.
      M2M=2*M-1
      DO 12 I=1,M2M
   12 SUM=SUM+I*B(I)**2
      A1=B(1)
      RETURN
      END

      SUBROUTINE FIT2 (N,X,F,FX,FXX,FXXX,K1,KN,END1,ENDN)
      DIMENSION X(1),F(1),FX(1),FXX(1),FXXX(1)
      NM1=N-1
      IF (N.LT.3) X(N+1)=X(N)+1.0
      IF (N.LT.3) F(N+1)=F(N)
      DX2=X(2)-X(1)
      GO TO (1,2,3),K1
    1 FX(1)=0.5
      FXXX(1)=3.*((F(2)-F(1))/DX2)/DX2
      GO TO 4
    2 FX(1)=0.0
      FXXX(1)=END1
      GO TO 4
    3 FX(1)=-1.0
      FXXX(1)=-DX2*END1
    4 DO 5 I=2,NM1
      DX1=X(I)-X(I-1)
      DX2=X(I+1)-X(I)
      FX(I)=.5*DX2/(DX1+DX2-.5*DX1*FX(I-1))
    5 FXXX(I)=(6.*(F(I+1)-F(I))/DX2-6.*(F(I)-F(I-1))/DX1-DX1*FXXX(I-1))*
     .        FX(I)/DX2
      DX1=X(N)-X(NM1)
      FX(N)=0.0
      GO TO (6,7,8),KN
    6 FXXX(N)=(6.*(ENDN-(F(N)-F(NM1))/DX1)/DX1-FXXX(NM1))/(2.-FX(NM1))
      GO TO 9
    7 FXXX(N)=ENDN
      GO TO 9
    8 FXXX(N)=(ENDN*DX1+FXXX(NM1))/(1.+FX(NM1))
    9 FXX(N)=FXXX(N)
      DO 10 II=1,NM1
      I=N-II
      DX2=X(I+1)-X(I)
      IF(II.EQ.1) GO TO 11
      FXX(I)=FXXX(I)-FX(I)*FXX(I+1)
      FXXX(I)=(FXX(I+1)-FXX(I))/DX2
      FX(I)=(F(I+1)-F(I))/DX2-DX2*(FXX(I+1)+2.*FXX(I))/6.
      GO TO 10
   11 CONTINUE
      FXX(I) = FXXX(I)
      FXXX(I) = -FXX(I)/DX2
      FX(I)=(F(I+1)-F(I))/DX2-DX2*(2.*FXX(I))/6.
   10 CONTINUE
      FXXX(N)=FXXX(NM1)
      DX1=X(N)-X(NM1)
      IF(N.EQ.19) GO TO 12
      FX(N)=(F(N)-F(NM1))/DX1+DX1*(FXX(NM1)+2.*FXX(N))/6.
      GO TO 13
   12 CONTINUE
      FX(N)=(F(N)-F(NM1))/DX1+DX1*(FXX(NM1))/6.
   13 CONTINUE
      RETURN
      END

      SUBROUTINE SERIES (Y,N2,AZ,A,B,AN,YY,Z,W)
C     DETERMINATION OF COEFFICIENTS IN SERIES BY DAVID IVES, GRUMMAN AER
C     Y(K)..INPUT VALUE OF FUNCTION AT AN ANGLE OF TK=2.*PI*(K-1)/N2
C           FOR K=1 TO K=N2
C     PI....3.14159265358979324.....
C     N2....EVEN NUMBER OF INPUT FUNCTION VALUES
C     N.....N2/2
C     AZ....CONSTANT TERM IN SERIES
C     A.....(N-1) REAL OUTPUT VALUES OF COEFFICIENTS IN COSINE SERIES
C     B.....(N-1) REAL OUTPUT VALUES OF COEFFICIENTS IN SINE SERIES
C     AN....COSINE(N*TK) TERM IN SERIES
C     YY....DUMMY STORAGE OF LENGTH 2*N+2 (REAL)
C     Z.....DUMMY STORAGE OF LENGTH 4*N (REAL)
C     W.....DUMMY STORAGE OF LENGTH 2*N (REAL)
C     ******************************************************************
C             L=N-1
C     Y(K)=AZ+ SUM (A(L)*DCOS(L*TK)+B(L)*DSIN(L*TK))+AN*DCOS(N*TK)
C              L=1
C     ******************************************************************
      DIMENSION Y(1),A(1),B(1),YY(1),Z(1),W(1)
      N=N2/2
      CALL RFAST(Y,YY,N2,Z,W)
      AZ=YY(1)/N2
      DO 1 I=2,N
      A(I-1)=YY(2*I-1)/N
    1 B(I-1)=-YY(2*I)/N
      AN=YY(2*N+1)/N2
      RETURN
      END

      SUBROUTINE DELTA1 (MI1,X1,MI2,X2)
      DIMENSION X1(1),X2(1)
      COMMON/INT/ D(129),L(129)
      MI1M1=MI1-1
      MI2M1=MI2-1
      L(1)=1
      D(1)=0.0
      IS=1
      DO 3 I2=2,MI2M1
      DO 1 I1=IS,MI1M1
      IF (X1(I1+1).GT.X2(I2)) GO TO 2
    1 CONTINUE
    2 IS=I1
      L(I2)=IS
    3 D(I2)=X2(I2)-X1(IS)
      L(MI2)=MI1
      D(MI2)=0.0
      RETURN
      END

      SUBROUTINE INTRP (MI2,F,FX,FXX,FXXX,FO)
      DIMENSION F(1),FX(1),FXX(1),FXXX(1),FO(1)
      COMMON/INT/ D(129),L(129)
      DO 1 I=1,MI2
      IS=L(I)
      IF(IS.NE.19) GO TO 2
      FO(I)=F(IS)+D(I)*(FX(IS)+.5*D(I)*(D(I)*FXXX(IS)/3.))
      GO TO 1
    2 CONTINUE
      FO(I)=F(IS)+D(I)*(FX(IS)+.5*D(I)*(FXX(IS)+D(I)*FXXX(IS)/3.))
    1 CONTINUE
      RETURN
      END

      SUBROUTINE FAST(X,Y,N,Z,W)
C     FAST FOURIER TRANSFORM OF COMPLEX DATA BY DAVID IVES, GRUMMAN AERO
C     X.....N INPUT VALUES (COMPLEX)
C     Y.....N OUTPUT VALUES (COMPLEX)
C     N.....NUMBER OF VALUES
C     Z.....DUMMY STORAGE OF LENGTH 2N (COMPLEX)
C     W.....DUMMY STORAGE OF LENGTH N (COMPLEX)
C     THIS PRODUCES  OUTPUT Y  FROM  INPUT X , WHERE
C     ******************************************************************
C          K=N
C     Y(J)=SUM X(K)*EXP(-I*2*PI*(J-1)*(K-1)/N)
C          K=1
C     ******************************************************************
C     WITH I=SQRT(-1) AND PI=3.14159........
C     COMPLEX NUMBERS ARE HANDLED IN FORTRAN 4 CONVENTION, NAMELY THE
C     REAL AND IMAGINARY PARTS ARE STORED IN ALTERNATE CELLS, STARTING
C     WITH THE REAL PART OF X(1) IN THE FIRST LOCATION, ETC.
C     TO OBTAIN X FROM Y, TAKE THE CONJUGATE OF Y AS THE  INPUT X ,THEN
C     DIVIDE THE CONJUGATE OF THE  OUTPUT Y  BY  N  TO OBTAIN X
      DOUBLE PRECISION PI,PIHALF,PI2,RADDEG
      COMMON/PIS/ PI,PIHALF,PI2,RADDEG
      DIMENSION X(1),Z(1),W(1),Y(1)
      DO 1 I=1,N
      W(2*I-1)=COS((PI2/N)*(I-1))
      W(2*I)= -SIN((PI2/N)*(I-1))
      Z(2*I-1)=X(2*I-1)
    1 Z(2*I)=X(2*I)
      ID=N
      IS=1
      J=0
    2 DO 3 IX=2,ID
      IF(MOD(ID,IX)) 4,4,3
    3 CONTINUE
    4 ID=ID/IX
      IS=IS*IX
      J=J+1
      DO 5 L1=1,IS
      DO 5 L=1,ID
      JMK=(MOD(L+(L1-1)*ID*IX,N)+MOD(J+1,2)*N)*2
      JPI=(L+(L1-1)*ID+MOD(J,2)*N)*2
      Z(JPI-1)=Z(JMK-1)
      Z(JPI)=Z(JMK)
      DO 5 IH=2,IX
      IG=(MOD((L1-1)*ID*(IH-1),N)+1)*2
      IU=JMK+(IH-1)*ID*2
      Z(JPI-1)=Z(JPI-1)+Z(IU-1)*W(IG-1)-Z(IU)*W(IG)
    5 Z(JPI)=Z(JPI)+Z(IU)*W(IG-1)+Z(IU-1)*W(IG)
      IF(ID-1) 6,6,2
    6 DO 7 I=1,N
      JPI=MOD(J,2)*N+I
      Y(2*I-1)=Z(2*JPI-1)
    7 Y(2*I)=Z(2*JPI)
      RETURN
      END