C   llt.f
c
c   lifting line theory by solving the monoplane equation
c
c   W.H. Mason, circa 1971
c   now: Department of Aerospace and Ocean Engineering
c   Virginia Tech, Blacksburg, VA 24061
c   contact: mason@aoe.vt.edu
c
c   on Mac - April 1991
c
c   induced alpha and induced drag distributions added Oct. 1991
c   root and tip incidence assuming straight line wrap, April 1992
c
C$NOCHECK
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ETA(32),    THETAT(32),  CCLCA(32),
     1          ALPHAI(32), CCDCA(32)
      PI        = 3.1415926536

      write(6,210)
  210 format(3x,'Lifting line theory program'//
     1 3x,'by W.H. Mason'/,
     2 3x,'Department of Aerospace and Ocean Engineering'/
     3 3x,'Virginia Tech, Blacksburg, VA 24061'/
     4 3x,'http://www.aoe.vt.edu/'//
     1 3x,'Note: angle of attack is relative to zero lift angle'//
     1 3x,'Input angle of attack, aspect ratio,',
     2 ' and taper ratio:'/)
      read(5,*) alpha,ar,taper

      write(6,230)
  230 format(/3x,'Input root and tip incidence (deg):'/)
      read(5,*) thetrt,thettp

      write(6,240) ar,taper,alpha,thetrt,thettp
  240 format(/5x,'Lifting Line Theory Solution'/
     1       /5x,'Aspect Ratio   =',f6.3
     2       /5x,'Taper ratio    =',f6.3
     3       /5x,'Alpha          =',f6.3,' deg.'
     4       /5x,'Root incidence =',f6.3,' deg.'
     5       /5x,'Tip incidence  =',f6.3,' deg.')
C
      ALPH0 = PI/180.0*ALPHA
C
      CALL LLT(ALPH0,AR,TAPER,thetrt,thettp,ETA,THETAT,CL,CDI,E,
     1         CCLCA,ALPHAI,CCDCA,NN)
C
      WRITE(6,200) CL,CDI,E
  200 FORMAT(/5X,'CL =',F9.5,4x,'Induced Drag = ',F7.5,4x,'e = ',f8.5,
     1//4x,'Spanload Distribution'//6x,'i',3x,'y/(b/2)',4x,'twist',4x,
     2  'ccl/ca',2X,'induced angle',2x,'ccd/ca')
C
      DO 100 I = 1,NN
      ALPHID = 180./PI*ALPHAI(I)
      thetad = 180./pi*thetat(i)
  100 WRITE(6,220) I,ETA(I),thetad,CCLCA(I),ALPHID,CCDCA(I)

  220 FORMAT(2X,I5,4F10.5,3X,F10.5)

      STOP
      END

      SUBROUTINE LLT (ALPH0,AR,TAPER,thetrt,thettp,ETA,THETAT,
     1                CL,CDI,E,CCLCA,ALPHAI,CCDCA,NN)
C
C      SIMPLE LIFTING LINE THEORY
C
C      FOR UNSWEPT TRAPEZOIDAL WINGS
C
C      INPUT (ARGUMENT LIST):
C
C        ETA      - INPUT LOCATION OF TWIST AND ALSO THE OUTPUT
C                   LOCATION OF SPANLOAD (ETA=COS(THETA);
C                                         THETA=.....
C        ALPH0    - INPUT BASIC ANGLE OF ATTACK (RADIANS)
C        AR       - PLANFORM ASPECT RATIO
C        TAPER    - THE PLANFORM TAPER RATIO, CTIP/CROOT
C        THETRT   - ROOT INCIDENCE (IN DEGREES)
C        THETTP   - TIP INCIDENCE (IN DEGREES)
C
C       THE OUTPUT IS
C
C         THETAT   - TWIST DISTRIBUTION (RADIANS)
C         CL       - THE PLANFORM LIFT COEFFICIENT
C         CDI      - THE INDUCED DRAG
C         E        - THE PLANFORM EFFICIENCY FACTOR
C         CCLCA    - THE SPANLOAD DISTRIBUTION
C         ALPHAI   - THE DISTRIBUTION OF INDUCED ALPHA
C         CCDCA    - THE DISTRIBUTION OF INDUCED DRAG
C
C      THIS CODE ASSUMES THAT 1) A0,THE BASIC LIFT CURVE SLOPE IS 2*PI
C                             2) THERE ARE 15 SPAN STATIONS, I.E. NN=15
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ETA(32), THETAT(32), CCLCA(32), A(15,15),
     1          X(32),   ALPHAI(32), CCDCA(32)
C
      PI        = 3.1415926536
      A0        = 2.0*PI
      NN        = 15
      TANRT     = TAN(PI/180.0*THETRT)
      TANTIP    = TAN(PI/180.0*THETTP)
C
      N2N       = 2*NN
      XNN       = N2N
      DO 10 N   = 1,NN
      N2X       = 2*N - 1
      XN        = N2X
      THETA     = (XN+1)*PI/2.0/XNN
      ETA(N)    = DCOS(THETA)
      C         = 1. - (1. - TAPER)*ETA(N)
      THETAT(N) = ATAN((TANRT - (TANRT - TAPER*TANTIP)*ETA(N))/C)
      THETAR    = THETA*180.0/PI
      S         = DSIN(THETA)
      XMU       = A0/2.0/AR/(1.0+TAPER)*(1.0-(1.0-TAPER)*ETA(N))
      X(N)      = XMU*(ALPH0+THETAT(N))*S
      DO 10 J   = 1,NN
      J2X       = 2*J - 1
      XJ        = J2X
      AA1       = DSIN(XJ*THETA)
      AA2       = XJ*XMU+S
      A(N,J)    = AA1*AA2
   10 continue
C
      EPS       = 2.0D-9
      MM        = 1
      CALL DGELG(X,A,NN,MM,EPS,IER)
      IF(IER.NE.0) THEN
                   WRITE(6,110) IER
                   STOP
                   END IF
C
      WRITE(6,120)
      DO 30 J   = 1,NN
      J2X       = 2*J - 1
      XJ        = J2X
      THETA     = (XJ+1)*PI/2.0/XNN
      WRITE(6,130) J,X(J)

      SUM1      = 0.0
      SUM2      = 0.0
      DO 20 N   = 1,NN
      XN        = 2*N - 1
      SUM1      = SUM1 +    X(N)*DSIN(XN*THETA)
      SUM2      = SUM2 + XN*X(N)*DSIN(XN*THETA)
   20 CONTINUE

      CCLCA(J)  = 4.0*AR*SUM1
      ALPHAI(J) = SUM2/DSIN(THETA)
      CCDCA(J)  = ALPHAI(J)*CCLCA(J)
   30 CONTINUE
C
      CL        = PI*AR*X(1)
C
      IF(X(1) .EQ. 0.0) THEN
                        WRITE(6,140)
                        CDI = 0.0
                        E   = 0.0
                        RETURN
                        END IF
      SUM1      = 0.0
      DO 40 J   = 2,NN
      XJ        = J
   40 SUM1      = SUM1+(X(J)/X(1))**2*(2.0*XJ-1.0)
C
      DELTA     = SUM1
      CDI       = CL**2/(PI*AR)*(1.0+DELTA)
      E         = 1.0/(1.0 + DELTA)
C
      RETURN
C
  110 FORMAT(/5x,'error in DGELG, IER =',I10,8F10.5)
  120 FORMAT(/5X,'FOURIER COEFFICIENTS IN SOLUTION SERIES'//
     1        6X,'I',9X,'A(I)')
  130 FORMAT(5X,I2,3X,2E14.5)
  140 FORMAT(/5X,'CL = 0.0, E COMPUTATION IS NOT VALID'/)
      END

      SUBROUTINE DGELG(R,A,M,N,EPS,IER)
C
C     SOLVE A GENERAL SYSTEM OF LINEAR ALGEBRAIC EQUATIONS
C
C     THIS IS THE STANDARD SSP DIRECT METHOD  PROGRAM
C
C     SEE THE IBM SSP MANUAL, PP. 121
C
C
      DIMENSION A(1),R(1)
      DOUBLE PRECISION R,A,PIV,TB,TOL,PIVI,DABS,EPS
      IF(M)23,23,1
C
C     SEARCH FOR GREATEST ELEMENT IN MATRIX A
    1 IER=0
      PIV=0.D0
      MM=M*M
      NM=N*M
      DO 3 L=1,MM
      TB=DABS(A(L))
      IF(TB-PIV)3,3,2
    2 PIV=TB
      I=L
    3 CONTINUE
      TOL=EPS*PIV
C     A(I) IS PIVOT ELEMENT. PIV CONTAINS THE ABSOLUTE VALUE OF A(I).
C
C
C     START ELIMINATION LOOP
      LST=1
      DO 17 K=1,M
C
C     TEST ON SINGULARITY
      IF(PIV)23,23,4
    4 IF(IER)7,5,7
    5 IF(PIV-TOL)6,6,7
    6 IER=K-1
    7 PIVI=1.D0/A(I)
      J=(I-1)/M
      I=I-J*M-K
      J=J+1-K
C     I+K IS ROW-INDEX, J+K COLUMN-INDEX OF PIVOT ELEMENT
C
C     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R
      DO 8 L=K,NM,M
      LL=L+I
      TB=PIVI*R(LL)
      R(LL)=R(L)
    8 R(L)=TB
C
C     IS ELIMINATION TERMINATED
      IF(K-M)9,18,18
C
C     COLUMN INTERCHANGE IN MATRIX A
    9 LEND=LST+M-K
      IF(J)12,12,10
   10 II=J*M
      DO 11 L=LST,LEND
      TB=A(L)
      LL=L+II
      A(L)=A(LL)
   11 A(LL)=TB
C
C     ROW INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A
   12 DO 13 L=LST,MM,M
      LL=L+I
      TB=PIVI*A(LL)
      A(LL)=A(L)
   13 A(L)=TB
C
C     SAVE COLUMN INTERCHANGE INFORMATION
      A(LST)=J
C
C     ELEMENT REDUCTION AND NEXT PIVOT SEARCH
      PIV=0.D0
      LST=LST+1
      J=0
      DO 16 II=LST,LEND
      PIVI=-A(II)
      IST=II+M
      J=J+1
      DO 15 L=IST,MM,M
      LL=L-J
      A(L)=A(L)+PIVI*A(LL)
      TB=DABS(A(L))
      IF(TB-PIV)15,15,14
   14 PIV=TB
      I=L
   15 CONTINUE
      DO 16 L=K,NM,M
      LL=L+J
   16 R(LL)=R(LL)+PIVI*R(L)
   17 LST=LST+M
C     END OF ELIMINATION LOOP
C
C
C     BACK SUBSTITUTION AND BACK INTERCHANGE
   18 IF(M-1)23,22,19
   19 IST=MM+M
      LST=M+1
      DO 21 I=2,M
      II=LST-I
      IST=IST-LST
      L=IST-M
      L=A(L)+.5D0
      DO 21 J=II,NM,M
      TB=R(J)
      LL=J
      DO 20 K=IST,MM,M
      LL=LL+1
   20 TB=TB-A(K)*R(LL)
      K=J+L
      R(J)=R(K)
   21 R(K)=TB
   22 RETURN
C
C
C     ERROR RETURN
   23 IER=-1
      RETURN
      END
