C     PROGRAM No. 13: RECTANGULAR LIFTING SURFACE (VLM)
C     -------------------------------------------------
C     3D-VLM CODE FOR SIMPLE WING PLANFORMS WITH GROUND EFFECT(BYJOE KATZ,1974).
      DIMENSION QF(6,14,3),QC(4,13,3),DS(4,13,4)
      DIMENSION GAMA(4,13),DL(4,13),DD(4,13),DP(4,13)
      DIMENSION A(52,52),GAMA1(52),DW(52),IP(52)
      DIMENSION A1(5,13),DLY(13),GAMA1J(5),X(4)

      COMMON/NO1/ DS,X,B,C,S,AR,SN1,CS1
      COMMON/NO2/ IB,JB,CH,SIGN
      COMMON/NO3/ A1
      COMMON/NO4/ QF,QC,DXW

C     ==========
C     INPUT DATA
C     ==========
      IB=4
      JB=13
      X(1)=0.
      X(2)=0.
      X(3)=4.
      X(4)=4.
      B=13.
      VT=1.0
      ALPHA1=5.0
      CH=1000.
C     X(1) TO X(4) ARE X-COORDINATES OF THE WING'S FOUR CORNERPOINTS.
C     B - WING SPAN, VT - FREE STREAM SPEED, B - WING SPAN,
C     CH - HEIGHT ABOVE GROUND

C     CONSTANTS
      DXW=100.0*B
      DO 1 I=1,IB
         DO 1 J=1,JB
C           GAMA(I,J)=1.0 IS REQUIRED FOR INFLUENCE MATRIX CALCULATIONS.
 1          GAMA(I,J)=1.0

      RO=1.
      PAY=3.141592654
      ALPHA=ALPHA1*PAY/180.
      SN1=SIN(ALPHA)
      CS1=COS(ALPHA)
      IB1=IB+1
      IB2=IB+2
      JB1=JB+1

C     =============
C     WING GEOMETRY
C     =============
      CALL GRID
      WRITE(6,101)
      WRITE(6,102) ALPHA1,B,C,S,AR,VT,IB,JB,CH

C     ========================
C     AERODYNAMIC CALCULATIONS
C     ========================

C     INFLUENCE COEFFICIENTS CALCULATION
      K=0
      DO 14 I=1,IB
         DO 14 J=1,JB
            SIGN=0.0
            K=K+1
            CALL WING(QC(I,J,1),QC(I,J,2),QC(I,J,3),GAMA,U,V,W,1.0,I,J)
            L=0

            DO 10 I1=1,IB
               DO 10 J1=1,JB
                     L=L+1
C                 A(K,L) - IS THE NORMAL VELOCITY COMPONENT DUE TO A UNIT VORTEX
C                 LATTICE.
 10               A(K,L)=A1(I1,J1)

C           ADD INFLUENCE OF WING'S OTHER HALF
            CALL WING(QC(I,J,1),-QC(I,J,2),QC(I,J,3),GAMA,U,V,W,1.0,I,J)
            L=0
            DO 11 I1=1,IB
               DO 11 J1=1,JB
                  L=L+1
 11               A(K,L)=A(K,L)+A1(I1,J1)

            IF(CH.GT.100.0) GOTO 12

C           ADD INFLUENCE OF MIRROR IMAGE (DUE TO GROUND)
            SIGN=10.0
            CALL WING(QC(I,J,1),QC(I,J,2),-QC(I,J,3),GAMA,U,V,W,1.0,I,J)
            L=0
            DO 8 I1=1,IB
               DO 8 J1=1,JB
                  L=L+1
 8                A(K,L)=A(K,L)+A1(I1,J1)

C           ADD MIRROR IMAGE INFLUENCE OF WING'S OTHER HALF.
            CALL WING(QC(I,J,1),-QC(I,J,2),-QC(I,J,3),GAMA,U,V,W,1.0,I,J)
            L=0
            DO 9 I1=1,IB
               DO 9 J1=1,JB
                  L=L+1
 9                A(K,L)=A(K,L)+A1(I1,J1)
            SIGN=0.0

 12      CONTINUE
 13      CONTINUE

C        CALCULATE WING GEOMETRICAL DOWNWASH
         UINF=VT
         VINF=0.0
         WINF=0.0

C        THIS IS THE GENERAL FORMULATION FOR RIGHT HAND SIDE.
         DW(K)=-(UINF*DS(I,J,1)+VINF*DS(I,J,2)+WINF*DS(I,J,3))
 14   CONTINUE

C     SOLUTION OF THE PROBLEM: DW(I)=A(I,J)*GAMA(I)
      K1=IB*JB
      DO 15 K=1,K1
 15      GAMA1(K)=DW(K)

      CALL DECOMP(K1,52,A,IP)
 16   CONTINUE
      CALL SOLVER(K1,52,A,GAMA1,IP)
C     HERE * THE SAME ARRAY SIZE IS REQUIRED,
C     AS SPECIFIED IN THE BEGINNING OF THE CODE

C     WING VORTEX LATTICE LISTING
      K=0
      DO 17 I=1,IB
         DO 17 J=1,JB
            K=K+1
 17         GAMA(I,J)=GAMA1(K)

C     ==================
C     FORCES CALCULATION
C     ==================
      FL=0.
      FD=0.
      FM=0.
      QUE=0.5*RO*VT*VT
      DO 20 J=1,JB
         DLY(J)=0.
         DO 20 I=1,IB
            IF(I.EQ.1) GAMAIJ=GAMA(I,J)
            IF(I.GT.1) GAMAIJ=GAMA(I,J)-GAMA(I-1,J)
            DYM=QF(I,J+1,2)-QF(I,J,2)
            DL(I,J)=RO*VT*GAMAIJ*DYM
C           INDUCED DRAG CALCULATION
            CALL WING(QC(I,J,1),QC(I,J,2),QC(I,J,3),GAMA,U1,V1,W1,0.0,I,J)
             CALL WING(QC(I,J,1),-QC(I,J,2),QC(I,J,3),GAMA,U2,V2,W2,0.0,I,J)
            IF(CH.GT.100.0) GOTO 194
             CALL WING(QC(I,J,1),QC(I,J,2),-QC(I,J,3),GAMA,U3,V3,W3,0.0,I,J)
            CALL WING(QC(I,J,1),-QC(I,J,2),-QC(I,J,3),GAMA,U4,V4,W4,0.0,I,J)
            GOTO 195
            194 W3=0.
            W4=0.
            195 WIND=W1+W2-W3-W4

C           ADD INFLUENCE OF MIRROR IMAGE (GROUND).
            ALFI=-WIND/VT
            DD(I,J)=RO*DYM*VT*GAMAIJ*ALFI
            DP(I,J)=DL(I,J)/DS(I,J,4)/QUE
            DLY(J)=DLY(J)+DL(I,J)
            FL=FL+DL(I,J)
            FD=FD+DD(I,J)
            FM=FM+DL(I,J)*(QF(I,J,1)-X(1))
 20   CONTINUE
      CL=FL/(QUE*S)
      CD=FD/(QUE*S)
      CM=FM/(QUE*S*C)

C     OUTPUT
      WRITE(6,104) CL,FL,CM,CD
      WRITE(6,110)
      DO 21 J=1,JB
            DO 211 I=2,IB
 211              GAMA1J(I)=GAMA(I,J)-GAMA(I-1,J)
            DLYJ=DLY(J)/B*JB
 21   WRITE(6,103) J,DLYJ,DP(1,J),DP(2,J),DP(3,J),DP(4,J),GAMA(1,J),
     1 GAMA1J(2),GAMA1J(3),GAMA1J(4)

C     END OF PROGRAM
 100  CONTINUE

C     FORMATS
 101  FORMAT(1H ,/,20X,'WING LIFT DISTRIBUTION CALCULATION (WITH GROUND
     1 EFFECT)',/,20X,56('-'))
 102  FORMAT(1H ,/,10X,'ALFA:',F10.2,8X,'B :',
     1F10.2,8X,'C :',F13.2,/,10X,
     2'S :',F10.2,8X,'AR :',F10.2,8X,'V(INF) :',F10.2,/,10X,
     3'IB :',I10,8X,'JB :',I10,8X,'L.E. HEIGHT:', F6.2,/)
 103  FORMAT(1H ,I3,' I ',F9.3,' II ',4(F9.3,' I '),' I ',4(F9.3,' I '))
 104  FORMAT(/,1H ,'CL=',F10.4,2X,'L=',F10.4,4X,'CM=',F10.4,3X,
     1'CD=',F10.4)
 110  FORMAT(1H ,/,5X,'I DL',4X,'II',22X,'DCP',22X,'I I',25X,
     1'GAMA',/,118('='),/,5X,'I',15X,'I= 1',11X,'2',11X,'3',11X,
     2'4',5X,'I I',5X,'1',11X,'2',11X,'3',11X,'4',/,118('='))
 112  FORMAT(1H ,'QF(I=',I2,',J,X.Y.Z)= ',15(F6.1))
 113  FORMAT(1H ,110('='))

      STOP
      END

      SUBROUTINE GRID
      DIMENSION QF(6,14,3),QC(4,13,3),DS(4,13,4),X(4)
      COMMON/NO1/ DS,X,B,C,S,AR,SN1,CS1
      COMMON/NO2/ IB,JB,CH,SIGN
      COMMON/NO4/ QF,QC,DXW

      PAY=3.141592654
C     X(1) - IS ROOT L.E., X(2) TIP L.E., X(3) TIP T.E., AND X(4) IS ROOT T.E.
C     IB: NO. OF CHORDWISE BOXES, JB: NO. OF SPANWISE BOXES
      IB1=IB+1
      IB2=IB+2
      JB1=JB+1

C     WING FIXED VORTICES LOCATION ( QF(I,J,(X,Y,Z))...)
      DY=B/JB
      DO 3 J=1,JB1
         YLE=DY*(J-1)
         XLE=X(1)+(X(2)-X(1))*YLE/B
         XTE=X(4)+(X(3)-X(4))*YLE/B
C        XLE AND XTE ARE L.E. AND T.E. X-COORDINATES
         DX=(XTE-XLE)/IB
         DO 1 I=1,IB1
            QF(I,J,1)=(XLE+DX*(I-0.75))*CS1
            QF(I,J,2)=YLE
            QF(I,J,3)=-QF(I,J,1)*SN1+CH
 1       CONTINUE
C        WAKE FAR FIELD POINTS
         QF(IB2,J,1)=XTE+DXW
         QF(IB2,J,2)=QF(IB1,J,2)
 3    QF(IB2,J,3)=QF(IB1,J,3)

C     WING COLLOCATION POINTS
      DO 4 J=1,JB
         DO 4 I=1,IB
            QC(I,J,1)=(QF(I,J,1)+QF(I,J+1,1)+QF(I+1,J+1,1)+QF(I+1,J,1))/4
            QC(I,J,2)=(QF(I,J,2)+QF(I,J+1,2)+QF(I+1,J+1,2)+QF(I+1,J,2))/4
            QC(I,J,3)=(QF(I,J,3)+QF(I,J+1,3)+QF(I+1,J+1,3)+QF(I+1,J,3))/4

C           COMPUTATION OF NORMAL VECTORS
            CALL PANEL(QF(I,J,1),QF(I,J,2),QF(I,J,3),QF(I+1,J,1),QF(I+1,J,2),
            1QF(I+1,J,3),QF(I,J+1,1),QF(I,J+1,2),QF(I,J+1,3),QF(I+1,J+1,1),
            2QF(I+1,J+1,2),QF(I+1,J+1,3),DS(I,J,1),DS(I,J,2),DS(I,J,3),
            3DS(I,J,4))
 4    CONTINUE

C     B -IS SEMI SPAN, C -AV. CHORD, S - AREA
      S=0.5*(X(3)-X(2)+X(4)-X(1))*B
      C=S/B
      AR=2.*B*B/S

      RETURN
      END

      SUBROUTINE PANEL(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,C1,C2,C3,S)
C     CALCULATION OF PANEL AREA AND NORMAL VECTOR.
      A1=X2-X3
      A2=Y2-Y3
      A3=Z2-Z3
      B1=X4-X1
      B2=Y4-Y1
      B3=Z4-Z1

C     NORMAL VECTOR
      X=A2*B3-A3*B2
      Y=B1*A3-A1*B3
      Z=A1*B2-A2*B1
      A=SQRT(X**2+Y**2+Z**2)
      C1=X/A
      C2=Y/A
      C3=Z/A

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

      SUBROUTINE VORTEX(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,GAMA,U,V,W)
C     SUBROUTINE VORTEX CALCULATES THE INDUCED VELOCITY (U,V,W) AT A POI
C     (X,Y,Z) DUE TO A VORTEX ELEMENT VITH STRENGTH GAMA PER UNIT LENGTH
C     POINTING TO THE DIRECTION (X2,Y2,Z2)-(X1,Y1,Z1).
      PAY=3.141592654
      RCUT=1.0E-10

C     CALCULATION OF R1 X R2
      R1R2X=(Y-Y1)*(Z-Z2)-(Z-Z1)*(Y-Y2)
      R1R2Y=-((X-X1)*(Z-Z2)-(Z-Z1)*(X-X2))
      R1R2Z=(X-X1)*(Y-Y2)-(Y-Y1)*(X-X2)

C     CALCULATION OF (R1 X R2 )**2
      SQUARE=R1R2X*R1R2X+R1R2Y*R1R2Y+R1R2Z*R1R2Z

C     CALCULATION OF R0(R1/R(R1)-R2/R(R2))
      R1=SQRT((X-X1)*(X-X1)+(Y-Y1)*(Y-Y1)+(Z-Z1)*(Z-Z1))
      R2=SQRT((X-X2)*(X-X2)+(Y-Y2)*(Y-Y2)+(Z-Z2)*(Z-Z2))
      IF((R1.LT.RCUT).OR.(R2.LT.RCUT).OR.(SQUARE.LT.RCUT)) GOTO 1 GROUND
      R0R1=(X2-X1)*(X-X1)+(Y2-Y1)*(Y-Y1)+(Z2-Z1)*(Z-Z1)
      R0R2=(X2-X1)*(X-X2)+(Y2-Y1)*(Y-Y2)+(Z2-Z1)*(Z-Z2)
      COEF=GAMA/(4.0*PAY*SQUARE)*(R0R1/R1-R0R2/R2)
      U=R1R2X*COEF
      V=R1R2Y*COEF
      W=R1R2Z*COEF
      GOTO 2

C     WHEN POINT (X,Y,Z) LIES ON VORTEX ELEMENT; ITS INDUCED VELOCITY IS
 1    U=0.
      V=0.
      W=0.
 2    CONTINUE
      RETURN
      END

      SUBROUTINE WING(X,Y,Z,GAMA,U,V,W,ONOFF,I1,J1)
      DIMENSION GAMA(4,13),QF(6,14,3),A1(5,13)
      DIMENSION DS(4,13,4)
      COMMON/NO1/ DS
      COMMON/NO2/ IB,JB,CH,SIGN
      COMMON/NO3/ A1
      COMMON/NO4/ QF

C     CALCULATES INDUCED VELOCITY AT A POINT (X,Y,Z), DUE TO VORTICITY
C     DISTRIBUTION GAMA(I,J), OF SEMI-CONFIGURATION - IN A WING FIXED
C     COORDINATE SYSTEM.
      U=0
      V=0
      W=0
      IB1=IB+1
      DO 1 I=1,IB1
      DO 1 J=1,JB

C     I3 IS WAKE VORTEX COUNTER
      I3=I
      IF(I.EQ.IB1) I3=IB
      VORTIC=GAMA(I3,J)
      IF(ONOFF.LT.0.1) GOTO 2
      CALL VORTEX(X,Y,Z,QF(I,J,1),QF(I,J,2),QF(I,J,3),QF(I,J+1,1),QF(I,J
     1+1,2),QF(I,J+1,3),VORTIC,U1,V1,W1)
      CALL VORTEX(X,Y,Z,QF(I+1,J+1,1),QF(I+1,J+1,2),QF(I+1,J+1,3),
     3QF(I+1,J,1),QF(I+1,J,2),QF(I+1,J,3),VORTIC,U3,V3,W3)
 2    CALL VORTEX(X,Y,Z,QF(I,J+1,1),QF(I,J+1,2),QF(I,J+1,3),QF(I+1,J+1,1
     2),QF(I+1,J+1,2),QF(I+1,J+1,3),VORTIC,U2,V2,W2)
      CALL VORTEX(X,Y,Z,QF(I+1,J,1),QF(I+1,J,2),QF(I+1,J,3),QF(I,J,1),
     4QF(I,J,2),QF(I,J,3),VORTIC,U4,V4,W4)

      U0=U2+U4+(U1+U3)*ONOFF
      V0=V2+V4+(V1+V3)*ONOFF
      W0=W2+W4+(W1+W3)*ONOFF
      A1(I,J)=U0*DS(I1,J1,1)+V0*DS(I1,J1,2)+W0*DS(I1,J1,3)
      IF(SIGN.GE.1.0)
      * A1(I,J)=U0*DS(I1,J1,1)+V0*DS(I1,J1,2)-W0*DS(I1,J1,3)
      IF(I.EQ.IB1) A1(IB,J)=A1(IB,J)+A1(IB1,J)
      U=U+U0
      V=V+V0
      W=W+W0

 1    CONTINUE
      RETURN
      END
