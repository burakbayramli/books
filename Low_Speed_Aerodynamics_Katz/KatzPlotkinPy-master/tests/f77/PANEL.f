C     PROGRAM No. 14: 3D PANEL METHOD, DIRICHLET B.C. (SOURCE + DOUBLET)
C     -----------------------------------------------------------------
C     3D-PANEL CODE FOR SIMPLE WING PLANFORMS. NO TIP PATCH!!!
      DIMENSION QF(22,14,3),QC(20,13,3),DS(20,13,10),SIGMA(20,13)
      DIMENSION DUB(20,13),DL(20,13),DD(20,13),CP(20,13),DDUBJ(13)
      DIMENSION CR(21,13,12)
      DIMENSION A(260,260),DUB1(260),RHS(260),IP(260)
      COMMON/NO1/ DS,CROOT,CTIP,XTIP,ZTIP,B,S,AR,IB,JB,PAY
      COMMON/NO2/ QF,QC,CR,SIGMA,DXW,UT,WT

C     ==========
C     INPUT DATA
C     ==========
      ALPHA1=5.0
      CROOT=1.0
      CTIP=1.0
      XTIP=0.0
      ZTIP=0.0
      B=10.
      VT=1.0
      JB=3
C     CROOT, CTIP - ROOT AND TIP CHORD, XTIP - AFT SWEEP OF TIP
C     B - WING SPAN, VT - FREE STREAM SPEED, IB,JP - CHORD, SPANWISE COUNTERS
C     SYMMETRY IS ASSUMED (ONLY THE SEMISPAN IS MODELED)
C     CONSTANTS
      DXW=100.0*B
      RO=1.0
      PAY=3.141592654
      UT=VT*COS(ALPHA1*PAY/180.0)
      WT=VT*SIN(ALPHA1*PAY/180.0)

C     =============
C     WING GEOMETRY
C     =============
      CALL GRID
      IB1=IB+1
      IB2=IB+2
      JB1=JB+1
      WRITE(6,101)
      WRITE(6,102) ALPHA1,B,CROOT,S,AR,VT,IB,JB
      WRITE(6,111)
      WRITE(6,113)
      DO 8 J=1,JB1
      DO 8 I=1,IB2
 8    WRITE(6,112) I,J,QF(I,J,1),QF(I,J,2),QF(I,J,3)

 111  FORMAT(1H ,' I ',' J ','QF(,I,J,1) QF(I,J,2) QF(I,J,3)')
 112  FORMAT(1H ,I3,3X,I3,3F12.4)
 113  FORMAT(1H ,46('='))

C     ========================
C     AERODYNAMIC CALCULATIONS
C     ========================

C     INFLUENCE COEFFICIENTS CALCULATION
C     COLLOCATION POINT COUNTER
      K=0
      DO 14 I=1,IB
      DO 14 J=1,JB
      K=K+1
      L=0
      RH=0
C     INFLUENCING PANEL COUNTER
      DO 10 I1=1,IB
      DO 10 J1=1,JB
      L=L+1
      IF(I1.EQ.1) THEN
C     CALCULATE WAKE CONTRIBUTION
C     FIRST CONVERT COLLOCATION POINT TO PANEL COORDINATES,
C     AND THEN CALCULATE INFLUENCE COEFFICIENTS
      CALL CONVERT(QC(IB1,J1,1),QC(IB1,J1,2),QC(IB1,J1,3),
     1 QC(I,J,1),QC(I,J,2),QC(I,J,3),
     2 DS(IB1,J1,1),DS(IB1,J1,2),DS(IB1,J1,3),
     3 DS(IB1,J1,4),DS(IB1,J1,5),DS(IB1,J1,6),
     4 DS(IB1,J1,7),DS(IB1,J1,8),DS(IB1,J1,9),
     5 XC,YC,ZC )
      CALL INFLUENCE(WDUB,DSIG,XC,YC,ZC,
     1 CR(IB1,J1,1),CR(IB1,J1,2),CR(IB1,J1,3),
     2 CR(IB1,J1,4),CR(IB1,J1,5),CR(IB1,J1,6),
     3 CR(IB1,J1,7),CR(IB1,J1,8),CR(IB1,J1,9),
     4 CR(IB1,J1,10),CR(IB1,J1,11),CR(IB1,J1,12) )
C     ADD WING'S IMAGE (SYMMETRY IS ASSUMED)
      CALL CONVERT(QC(IB1,J1,1),QC(IB1,J1,2),QC(IB1,J1,3),
     1 QC(I,J,1),-QC(I,J,2),QC(I,J,3),
     2 DS(IB1,J1,1),DS(IB1,J1,2),DS(IB1,J1,3),
     3 DS(IB1,J1,4),DS(IB1,J1,5),DS(IB1,J1,6),
     4 DS(IB1,J1,7),DS(IB1,J1,8),DS(IB1,J1,9),
     5 XC,YC,ZC )
      CALL INFLUENCE(WDUB1,DSIG,XC,YC,ZC,
     1 CR(IB1,J1,1),CR(IB1,J1,2),CR(IB1,J1,3),
     2 CR(IB1,J1,4),CR(IB1,J1,5),CR(IB1,J1,6),
     3 CR(IB1,J1,7),CR(IB1,J1,8),CR(IB1,J1,9),
     4 CR(IB1,J1,10),CR(IB1,J1,11),CR(IB1,J1,12) )
      DDUBJ(J1)=WDUB+WDUB1
      DMU2=DDUBJ(J1)
      ELSE
      DMU2=0.0
      ENDIF
      IF(I1.EQ.IB) DMU2=-DDUBJ(J1)
C     END OF WAKE INFLUENCE CALCULATION
C     CONVERT COLLOCATION POINT TO PANEL COORDINATES
      CALL CONVERT(QC(I1,J1,1),QC(I1,J1,2),QC(I1,J1,3),
     1 QC(I,J,1),QC(I,J,2),QC(I,J,3),
     2 DS(I1,J1,1),DS(I1,J1,2),DS(I1,J1,3),
     3 DS(I1,J1,4),DS(I1,J1,5),DS(I1,J1,6),
     4 DS(I1,J1,7),DS(I1,J1,8),DS(I1,J1,9),
     5 XC,YC,ZC )
      CALL INFLUENCE(DMU,DSIG,XC,YC,ZC,
     1 CR(I1,J1,1),CR(I1,J1,2),CR(I1,J1,3),
     2 CR(I1,J1,4),CR(I1,J1,5),CR(I1,J1,6),
     3 CR(I1,J1,7),CR(I1,J1,8),CR(I1,J1,9),
     4 CR(I1,J1,10),CR(I1,J1,11),CR(I1,J1,12) )
      IF((I1.EQ.I).AND.(J1.EQ.J)) DMU=-0.5
C     A PANEL INFLUENCE ON ITSELF IS DMU=1/2

C     ADD INFLUENCE OF WING'S IMAGE (OTHER HALF)
      CALL CONVERT(QC(I1,J1,1),QC(I1,J1,2),QC(I1,J1,3),
     1 QC(I,J,1),-QC(I,J,2),QC(I,J,3),
     2 DS(I1,J1,1),DS(I1,J1,2),DS(I1,J1,3),
     3 DS(I1,J1,4),DS(I1,J1,5),DS(I1,J1,6),
     4 DS(I1,J1,7),DS(I1,J1,8),DS(I1,J1,9),
     5 XC,YC,ZC )
      CALL INFLUENCE(DMU1,DSIG1,XC,YC,ZC,
     1 CR(I1,J1,1),CR(I1,J1,2),CR(I1,J1,3),
     2 CR(I1,J1,4),CR(I1,J1,5),CR(I1,J1,6),
     3 CR(I1,J1,7),CR(I1,J1,8),CR(I1,J1,9),
     4 CR(I1,J1,10),CR(I1,J1,11),CR(I1,J1,12) )

C     A(K,L) - IS THE INFLUENCE MATRIX COEFFICIENT
      A(K,L)=DMU+DMU1-DMU2
      RH=RH+(DSIG+DSIG1)*SIGMA(I1,J1)
 10   CONTINUE

C     CALCULATE RHS
      RHS(K)=RH
 14   CONTINUE

C     SOLUTION OF THE PROBLEM: A(K,L)*DUB(K)=RHS(K)
      K1=IB*JB
      DO 15 K=1,K1
 15   DUB1(K)=RHS(K)
      CALL DECOMP(K1,260,A,IP)
 16   CONTINUE
      CALL SOLVER(K1,260,A,DUB1,IP)
C     HERE * THE SAME ARRAY SIZE IS REQUIRED,
C     AS SPECIFIED IN THE BEGINNING OF THE CODE

C     WING DOUBLET LATTICE LISTING
      K=0
      DO 17 I=1,IB
      DO 17 J=1,JB
      K=K+1
 17   DUB(I,J)=DUB1(K)
      DO 18 J=1,JB
 18   DUB(IB1,J)=DUB(1,J)-DUB(IB,J)

C     ==================
C     FORCES CALCULATION
C     ==================
      FL=0.
      FD=0.
      FM=0.
      QUE=0.5*RO*VT*VT
      DO 20 J=1,JB
      DO 20 I=1,IB

      I1=I-1
      I2=I+1
      J1=J-1
      J2=J+1
      IF(I.EQ.1) I1=1
      IF(I.EQ.IB) I2=IB
      IF(J.EQ.1) J1=1
      IF(J.EQ.JB) J2=JB

C     CHORDWISE VELOCITY
      XF=0.5*(QF(I+1,J,1)+QF(I+1,J+1,1))
      YF=0.5*(QF(I+1,J,2)+QF(I+1,J+1,2))
      ZF=0.5*(QF(I+1,J,3)+QF(I+1,J+1,3))
      XR=0.5*(QF(I,J,1)+QF(I,J+1,1))
      YR=0.5*(QF(I,J,2)+QF(I,J+1,2))
      ZR=0.5*(QF(I,J,3)+QF(I,J+1,3))
      DX2=QC(I2,J,1)-XF
      DY2=QC(I2,J,2)-YF
      DZ2=QC(I2,J,3)-ZF
      DX3=QC(I1,J,1)-XR
      DY3=QC(I1,J,2)-YR
      DZ3=QC(I1,J,3)-ZR
      DL1=SQRT((XF-XR)**2+(YF-YR)**2+(ZF-ZR)**2)
      DL2=SQRT(DX2**2+DY2**2+DZ2**2)
      DL3=SQRT(DX3**2+DY3**2+DZ3**2)
      DLL=DL1+DL2+DL3
      IF(I.EQ.1) DLL=DL1/2.0+DL2
      IF(I.EQ.IB) DLL=DL1/2.0+DL3
      QL=-(DUB(I2,J)-DUB(I1,J))/DLL

C     SPANWISE VELOCITY
      DX=QC(I,J2,1)-QC(I,J1,1)
      DY=QC(I,J2,2)-QC(I,J1,2)
      DZ=QC(I,J2,3)-QC(I,J1,3)
      DR=SQRT(DX**2+DY**2+DZ**2)
      QM=-(DUB(I,J2)-DUB(I,J1))/DR

C     FIRST ORDER CORRECTION FOR PANEL SWEEP
      QL=QL+QM*(DX**2+DZ**2)/DR
      QM=QM*(DY**2+DZ**2)/DR
      QINF=UT*DS(I,J,9)-WT*DS(I,J,7)
      CP(I,J)=1.0-((QINF+QL)**2+QM**2)/(VT**2)
      DL(I,J)=-CP(I,J)*DS(I,J,10)*DS(I,J,9)
      DD(I,J)=CP(I,J)*DS(I,J,10)*DS(I,J,7)
      FL=FL+DL(I,J)
      FD=FD+DD(I,J)
      FM=FM+DL(I,J)*QC(I,J,1)
 20   CONTINUE
      CL=FL/(QUE*S)
      CD=FD/(QUE*S)
      CM=FM/(QUE*S*CROOT)

C     OUTPUT
      WRITE(6,110)
      DO 21 J=1,JB
      DO 21 I=1,IB1
 21   WRITE(6,105)I,J,QC(I,J,1),CP(I,J),DL(I,J),DD(I,J),DUB(I,J),
 1    SIGMA(I,J)
      WRITE(6,104) CL,FL,CM,CD

C     END OF PROGRAM
 100  CONTINUE

C     FORMATS
 101  FORMAT(1H ,/,20X,'INTERNAL POTENTIAL BASED PANEL CODE',
     1 /,20X,36('-'))
 102  FORMAT(1H ,/,10X,'ALFA:',F10.2,8X,'B :',
      1F10.2,8X,'C :',F13.2,/,10X,
     2'S :',F10.2,8X,'AR :',F10.2,8X,'V(INF) :',F10.2,/,10X,
     3'IB :',I10,8X,'JB :',I10,8X,/)
 103  FORMAT(1H ,I3,' I ',F9.3,' II ',4(F9.3,' I '),' I ',4(F9.3,' I '))
 104  FORMAT(/,1H ,'CL=',F10.4,2X,'L=',F10.4,4X,'CM=',F10.4,3X,
     1'CD=',F10.4)
 105  FORMAT(2I4,6F10.4)
 110  FORMAT(/,1H ,2X,'I J',7X,'X',8X,'CP',8X,'DL',8X,'DD',7X,
     1'DUB',6X,'SIGMA',/,68('='))

      STOP
      END

      SUBROUTINE GRID
      DIMENSION QF(22,14,3),QC(20,13,3),DS(20,13,10),SIGMA(20,13)
      DIMENSION CR(21,13,12)
      COMMON/NO1/ DS,CROOT,CTIP,XTIP,ZTIP,B,S,AR,IB,JB,PAY
      COMMON/NO2/ QF,QC,CR,SIGMA,DXW,UT,WT

      WRITE(6,9)
9     FORMAT( 1X,'AIRFOIL COORDINATES',/,1X,19('='),/,8X,'X Z')
      READ(5,11) IB1
      DO 10 I=1, IB1
      READ(5,12) QF(I,1,1),QF(I,1,3)
10    WRITE(6,12) QF(I,1,1),QF(I,1,3)
11    FORMAT(I3)
12    FORMAT(3F10.4)
C     IB: NO. OF CHORDWISE PANELS, JB: NO. OF SPANWISE PANELS
      IB=IB1-1
      IB2=IB1+1
      JB1=JB+1

C     CALCULATE PANEL CORNERPOINTS; QF(I,J,(X,Y,Z))
      DO 3 J=1,JB1
      Y=B/2.0/JB*(J-1)
      DXLE=XTIP*2.0*Y/B
      DZLE=ZTIP*2.0*Y/B
      CHORD=CROOT-(CROOT-CTIP)*2.0*Y/B
C     B - FULL SPAN, DXLE - LOCAL SWEEP
      DO 1 I=1,IB1
      QF(I,J,1)=QF(I,1,1)*CHORD+DXLE
      QF(I,J,2)=Y
      QF(I,J,3)=QF(I,1,3)*CHORD+DZLE
 1    CONTINUE
C     WAKE FAR FIELD POINTS (QF - IS IN BODY FRAME OF REFERENCE)
      QF(IB2,J,1)=QF(IB1,J,1)+DXW
      QF(IB2,J,2)=QF(IB1,J,2)
      QF(IB2,J,3)=QF(IB1,J,3)
 3    CONTINUE

C     WING COLLOCATION POINTS

      592 Appendix D / Sample Computer Programs
      DO 4 J=1,JB
      DO 4 I=1,IB1
      QC(I,J,1)=(QF(I,J,1)+QF(I,J+1,1)+QF(I+1,J+1,1)+QF(I+1,J,1))/4
      QC(I,J,2)=(QF(I,J,2)+QF(I,J+1,2)+QF(I+1,J+1,2)+QF(I+1,J,2))/4
      QC(I,J,3)=(QF(I,J,3)+QF(I,J+1,3)+QF(I+1,J+1,3)+QF(I+1,J,3))/4

C     COMPUTATION OF CHORDWISE VECTORS DS(IJ,1,2,3),
C     TANGENTIAL AND NORMAL VECTORS DS(IJ,4 TO 9), PANEL AREA DS(IJ,1-10)
C     AND SOURCE STRENGTH (SIGMA)

      CALL PANEL(QF(I,J,1),QF(I,J,2),QF(I,J,3),QF(I+1,J,1),QF(I+1,J,2),
      1QF(I+1,J,3),QF(I,J+1,1),QF(I,J+1,2),QF(I,J+1,3),QF(I+1,J+1,1),
      2QF(I+1,J+1,2),QF(I+1,J+1,3),DS(I,J,1),DS(I,J,2),DS(I,J,3),
      3DS(I,J,4),DS(I,J,5),DS(I,J,6),DS(I,J,7),DS(I,J,8),DS(I,J,9),
      4DS(I,J,10))

      SIGMA(I,J)=DS(I,J,7)*UT+DS(I,J,9)*WT
      4 CONTINUE

C     B -IS FULL SPAN, C -ROOT CHORD, S - AREA
      S=0.5*B*(CROOT+CTIP)
      C=S/B
      AR=B*B/S

C     TRANSFORM THE 4 PANEL CORNER POINTS INTO PANEL FRAME OF REF.
C     THIS IS NEEDED LATER TO CALCULATE THE INFLUENCE COEFFICIENTS
      DO 5 J=1,JB
      DO 5 I=1,IB1
      CALL CONVERT(QC(I,J,1),QC(I,J,2),QC(I,J,3),
      1 QF(I,J,1),QF(I,J,2),QF(I,J,3),
      2 DS(I,J,1),DS(I,J,2),DS(I,J,3),
      3 DS(I,J,4),DS(I,J,5),DS(I,J,6),
      4 DS(I,J,7),DS(I,J,8),DS(I,J,9),
      5 CR(I,J,1),CR(I,J,2),CR(I,J,3) )
      CALL CONVERT(QC(I,J,1),QC(I,J,2),QC(I,J,3),
      1 QF(I+1,J,1),QF(I+1,J,2),QF(I+1,J,3),
      2 DS(I,J,1),DS(I,J,2),DS(I,J,3),
      3 DS(I,J,4),DS(I,J,5),DS(I,J,6),
      4 DS(I,J,7),DS(I,J,8),DS(I,J,9),
      5 CR(I,J,4),CR(I,J,5),CR(I,J,6) )
      CALL CONVERT(QC(I,J,1),QC(I,J,2),QC(I,J,3),
      1 QF(I+1,J+1,1),QF(I+1,J+1,2),QF(I+1,J+1,3),
      2 DS(I,J,1),DS(I,J,2),DS(I,J,3),
      3 DS(I,J,4),DS(I,J,5),DS(I,J,6),
      4 DS(I,J,7),DS(I,J,8),DS(I,J,9),
      5 CR(I,J,7),CR(I,J,8),CR(I,J,9) )
      CALL CONVERT(QC(I,J,1),QC(I,J,2),QC(I,J,3),
      1 QF(I,J+1,1),QF(I,J+1,2),QF(I,J+1,3),
      2 DS(I,J,1),DS(I,J,2),DS(I,J,3),
      3 DS(I,J,4),DS(I,J,5),DS(I,J,6),
      4 DS(I,J,7),DS(I,J,8),DS(I,J,9),
      5 CR(I,J,10),CR(I,J,11),CR(I,J,12) )
      5 CONTINUE
      RETURN
      END

      SUBROUTINE PANEL(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,C1,C2,C3,
      1 T1,T2,T3,V1,V2,V3,S)
C     X,Y,Z-PANEL CORNERPOINTS, C,T,V-CHORDWISE, TANGENTIAL, NORMAL VECTORS
      D.3 Time-Dependent Programs 593
C     FIRST CALCULATE CHORWISE VECTOR
      A1=((X2+X4)-(X1+X3))/2.0
      A2=((Y2+Y4)-(Y1+Y3))/2.0
      A3=((Z2+Z4)-(Z1+Z3))/2.0
      AA=SQRT(A1**2+A2**2+A3**2)
      C1=A1/AA
      C2=A2/AA
      C3=A3/AA
C     NEXT, ANOTHER VECTOR IN THIS PLANE
      B1=X4-X1
      B2=Y4-Y1
      B3=Z4-Z1
C     NORMAL VECTOR
      V1=C2*B3-C3*B2
      V2=B1*C3-C1*B3
      V3=C1*B2-C2*B1
      VV=SQRT(V1**2+V2**2+V3**2)
      V1=V1/VV
      V2=V2/VV
      V3=V3/VV
C     TANGENTIAL VECTOR
      T1=V2*C3-V3*C2
      T2=C1*V3-V1*C3
      T3=V1*C2-V2*C1
C     CALCULATION OF PANEL AREA
      E1=X3-X1
      E2=Y3-Y1
      E3=Z3-Z1
      F1=X2-X1
      F2=Y2-Y1
      F3=Z2-Z1
C     NORMAL AREAS (F*B+B*E)
      S11=F2*B3-F3*B2
      S12=B1*F3-F1*B3
      S13=F1*B2-F2*B1
      S21=B2*E3-B3*E2
      S22=E1*B3-B1*E3
      S23=B1*E2-B2*E1
      S=0.5*(SQRT(S11**2+S12**2+S13**2)+SQRT(S21**2+S22**2+S23**2))
      RETURN
      END

      SUBROUTINE CONVERT(XO,YO,ZO,XB,YB,ZB,C1,C2,C3,T1,
      *T2,T3,V1,V2,V3,XP,YP,ZP)
C     TRANSFORMATION OF A FIELD POINT XB,YB,ZB INTO PANEL COORDINATES
C     XO,YO,ZO - PANEL COLLOCATION POINT, C,T,V - ARE CHORDWISE,
C     TANGENTIAL, AND NORMAL VECTORS
      XP=(XB-XO)*C1+(YB-YO)*C2+(ZB-ZO)*C3
      YP=(XB-XO)*T1+(YB-YO)*T2+(ZB-ZO)*T3
      ZP=(XB-XO)*V1+(YB-YO)*V2+(ZB-ZO)*V3
      RETURN
      END

      SUBROUTINE INFLUENCE(A,B,XC,YC,ZC,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,
      1 X4,Y4,Z4)
C     DOUBLET (A) AND SOURCE (B) INFLUENCE AT POINT (XC,YC,ZC) DUE TO PANEL
C     (X1,Y1,Z1,...X4,Y4,Z4), SEE KATZ & PLOTKIN PP 283-6. BY M. VEST, 1993.
      PI=3.141592653580732
      EP=0.000001
      594 Appendix D / Sample Computer Programs
C     EP, PANEL SIDE CUTOFF DISTANCE
C     PANEL SIDE (D) DISTANCE (R), E, AND H (EQS. 10.90 & 10.92-10.94)
      R1=SQRT((XC-X1)**2+(YC-Y1)**2+ZC**2)
      R2=SQRT((XC-X2)**2+(YC-Y2)**2+ZC**2)
      R3=SQRT((XC-X3)**2+(YC-Y3)**2+ZC**2)
      R4=SQRT((XC-X4)**2+(YC-Y4)**2+ZC**2)

      D1=SQRT((X2-X1)**2+(Y2-Y1)**2)
      D2=SQRT((X3-X2)**2+(Y3-Y2)**2)
      D3=SQRT((X4-X3)**2+(Y4-Y3)**2)
      D4=SQRT((X1-X4)**2+(Y1-Y4)**2)

      E1=(XC-X1)**2+ZC**2
      E2=(XC-X2)**2+ZC**2
      E3=(XC-X3)**2+ZC**2
      E4=(XC-X4)**2+ZC**2

      H1=(XC-X1)*(YC-Y1)
      H2=(XC-X2)*(YC-Y2)
      H3=(XC-X3)*(YC-Y3)
      H4=(XC-X4)*(YC-Y4)

C     SOURCE (S, B) AND DOUBLET (Q, A) INFLUENCE IN PANEL COORDINATES
C     FOR TRIANGULAR PANEL THE 4TH SIDE CONTRIBUTION IS ZERO

      IF (D1.LT.EP) THEN
      S1=0.
      Q1=0.
      ELSE
      F=(Y2-Y1)*E1-(X2-X1)*H1
      G=(Y2-Y1)*E2-(X2-X1)*H2
      Q1=ATAN2(ZC*(X2-X1)*(F*R2-G*R1),ZC**2*(X2-X1)**2*R1*R2+F*G)
      S1=((XC-X1)*(Y2-Y1)-(YC-Y1)*(X2-X1))/D1*LOG((R1+R2+D1)/
      * (R1+R2-D1))
      ENDIF

      IF (D2.LT.EP) THEN
      S2=0.
      Q2=0.
      ELSE
      F=(Y3-Y2)*E2-(X3-X2)*H2
      G=(Y3-Y2)*E3-(X3-X2)*H3
      Q2=ATAN2(ZC*(X3-X2)*(F*R3-G*R2),ZC**2*(X3-X2)**2*R2*R3+F*G)
      S2=((XC-X2)*(Y3-Y2)-(YC-Y2)*(X3-X2))/D2*LOG((R2+R3+D2)/
      * (R2+R3-D2))
      ENDIF

      IF (D3.LT.EP) THEN
      S3=0.
      Q3=0.
      ELSE
      F=(Y4-Y3)*E3-(X4-X3)*H3
      G=(Y4-Y3)*E4-(X4-X3)*H4
      Q3=ATAN2(ZC*(X4-X3)*(F*R4-G*R3),ZC**2*(X4-X3)**2*R3*R4+F*G)
      S3=((XC-X3)*(Y4-Y3)-(YC-Y3)*(X4-X3))/D3*LOG((R3+R4+D3)/
      * (R3+R4-D3))
      ENDIF

      IF (D4.LT.EP) THEN
      D.3 Time-Dependent Programs 595
      S4=0.
      Q4=0.
      ELSE
      F=(Y1-Y4)*E4-(X1-X4)*H4
      G=(Y1-Y4)*E1-(X1-X4)*H1
      Q4=ATAN2(ZC*(X1-X4)*(F*R1-G*R4),ZC**2*(X1-X4)**2*R4*R1+F*G)
      S4=((XC-X4)*(Y1-Y4)-(YC-Y4)*(X1-X4))/D4*LOG((R4+R1+D4)/
      * (R4+R1-D4))
      ENDIF

C     ADD CONTRIBUTIONS FROM THE 4 SIDES

      A=-(Q1+Q2+Q3+Q4)/4./PI ! times doublet strength
      IF(ABS(ZC).LT.EP) A=0.
      B=-(S1+S2+S3+S4)/4./PI-ZC*A ! times source strength
      RETURN
      END


      SUBROUTINE DECOMP(N,NDIM,A,IP)
      REAL A(NDIM,NDIM),T
      INTEGER IP(NDIM)
C     MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION.
C     N = ORDER OF MATRIX. NDIM = DECLARED DIMENSION OF ARRAY A.
C     A = MATRIX TO BE TRIANGULARIZED.
C     IP(K) , K .LT. N = INDEX OF K-TH PIVOT ROW.

      IP(N) = 1
      DO 6 K = 1, N
      IF(K.EQ.N) GOTO 5
      KP1 = K + 1
      M = K
      DO 1 I = KP1, N
      IF( ABS(A(I,K)).GT.ABS(A(M,K))) M=I
      1 CONTINUE
      IP(K) = M
      IF(M.NE.K) IP(N) = -IP(N)
      T = A(M,K)
      A(M,K) = A(K,K)
      A(K,K) = T
      IF(T.EQ.0.E0) GO TO 5
      DO 2 I = KP1, N
      2 A(I,K) = -A(I,K)/T
      DO 4 J = KP1, N
      T = A(M,J)
      A(M,J) = A(K,J)
      A(K,J) = T
      IF(T .EQ. 0.E0) GO TO 4
      DO 3 I = KP1, N
      3 A(I,J) = A(I,J) + A(I,K)*T
      4 CONTINUE
      5 IF(A(K,K) .EQ. 0.E0) IP(N) = 0
      6 CONTINUE
      RETURN
      END

      SUBROUTINE SOLVER(N,NDIM,A,B,IP)
      REAL A(NDIM,NDIM), B(NDIM), T
      INTEGER IP(NDIM)
      596 Appendix D / Sample Computer Programs
C     SOLUTION OF LINEAR SYSTEM, A*X = B.
C     N = ORDER OF MATRIX.
C     NDIM = DECLARED DIMENSION OF THE ARRAY A.
C     B = RIGHT HAND SIDE VECTOR.
C     IP = PIVOT VECTOR OBTAINED FROM SUBROUTINE DECOMP.
C     B = SOLUTION VECTOR, X.

      IF(N.EQ.1) GOTO 9
      NM1 = N - 1
      DO 7 K = 1, NM1
      KP1 = K + 1
      M = IP(K)
      T = B(M)
      B(M) = B(K)
      B(K) = T
      DO 7 I = KP1, N
      7 B(I) = B(I) + A(I,K)*T
      DO 8 KB = 1, NM1
      KM1 = N - KB
      K = KM1 + 1
      B(K) = B(K)/A(K,K)
      T = -B(K)
      DO 8 I = 1, KM1
      8 B(I) = B(I) + A(I,K)*T
      9 B(1) = B(1)/A(1,1)
      RETURN
      END
C     TYPICAL INPUT FOR SUBROUTINE GRID
      19 NACA 0012 AIRFOIL
      1.000 0.000
      0.905 -0.012
      0.794 -0.026
      0.655 -0.046
      0.500 -0.058
      0.345 -0.060
      0.206 -0.050
      0.095 -0.038
      0.024 -0.021
      0.000 0.000
      0.024 0.021
      0.095 0.038
      0.206 0.050
      0.345 0.060
      0.500 0.058
      0.655 0.046
      0.794 0.026
      0.905 0.012
      1.000 0.000
