C     PROGRAM No. 12: INFLUENCE COEFF. OF A RECTILINEAR SOURCE/DOUBLET PANEL
C     -----------------------------------------------------------------
C     THIS PROGRAM CALCULATES THE INFLUENCE OF A RECTILINEAR PANEL AT AN
C        ARBITRARY POINT. (PROGRAM BY LINDSEY BROWNE, 1988).
      DIMENSION X(5),Y(5),Z(5)

      PI=3.14159
      TWOPI=2.0*PI
      EPS=1.E-06
      PNDS=1.0

C     INPUT DOUBLET AND SOURCE STRENGTHS
      DUB=1.0/(4.0*PI)
      SIG=1.0/(4.0*PI)

      OPEN(2,FILE='INDIV',STATUS='UNKNOWN')

C     SQUARE/FLAT PANEL
C     INPUT COORDINATES
      X(1)=-.5
      X(2)=.5
      X(3)=.5
      X(4)=-.5
      Y(1)=-.5
      Y(2)=-.5
      Y(3)=.5
      Y(4)=.5

      DO 5 I=1,4
         Z(I)=0.0
 5          CONTINUE

      X(5)=X(1)
      Y(5)=Y(1)
      Z(5)=Z(1)

C     MID-POINT AT (0,0,0)
      PXO=0.0
      PYO=0.0
      PZO=0.0

 100     CONTINUE

      RJ31 = 0.0
      CJK1 = 0.0
      VXS = 0.0
      VYS = 0.0
      VZS = 0.0
      VXD = 0.0
      VYD = 0.0
      VZD = 0.0
      VX = 0.0
      VY = 0.0
      VZ = 0.0

C     INPUT POINT OF INTEREST
      WRITE(*,*) 'ENTER POINT OF INTEREST (X,Y,Z):'
      READ(*,*) PXI, PYI, PZI

      PX=PXI-PXO
      PY=PYI-PYO
      PZ=PZI-PZO
      RDIST=SQRT(PX*PX+PY*PY+PZ*PZ)

      PNLX=.25*(X(1)+X(2)+X(3)+X(4))
      PNLY=.25*(Y(1)+Y(2)+Y(3)+Y(4))
      PNLZ=.25*(Z(1)+Z(2)+Z(3)+Z(4))
      PNX=PX-PNLX
      PNY=PY-PNLY
      PNZ=PZ-PNLZ
      PNS=SQRT(PNX*PNX+PNY*PNY+PNZ*PNZ)

      D1X=X(3)-X(1)
      D1Y=Y(3)-Y(1)
      D1Z=Z(3)-Z(1)
      D2X=X(4)-X(2)
      D2Y=Y(4)-Y(2)
      D2Z=Z(4)-Z(2)

      CRX=D1Y*D2Z-D2Y*D1Z
      CRY=D2X*D1Z-D1X*D2Z
      CRZ=D1X*D2Y-D2X*D1Y
      CRSQ=SQRT(CRX*CRX+CRY*CRY+CRZ*CRZ)

      AREA=CRSQ/2.
      CNX=CRX/CRSQ
      CNY=CRY/CRSQ
      CNZ=CRZ/CRSQ
      PNN=CNX*PNX+CNY*PNY+CNZ*PNZ

      TCMX=(X(3)+X(4))/2. - PNLX
      TCMY=(Y(3)+Y(4))/2. - PNLY
      TCMZ=(Z(3)+Z(4))/2. - PNLZ
      TMS=SQRT(TCMX*TCMX+TCMY*TCMY+TCMZ*TCMZ)

      CMX=((X(3)+X(4))/2. - PNLX)/TMS
      CMY=((Y(3)+Y(4))/2. - PNLY)/TMS
      CMZ=((Z(3)+Z(4))/2. - PNLZ)/TMS
      CLX=CMY*CNZ-CNY*CMZ
      CLY=CNX*CMZ-CMX*CNZ
      CLZ=CMX*CNY-CNX*CMY

      DO 20 J=1,4
         K=J+1
         AX=PX-X(J)
         AY=PY-Y(J)
         AZ=PZ-Z(J)
         BX=PX-X(K)
         BY=PY-Y(K)
         BZ=PZ-Z(K)
         SX=X(K)-X(J)
         SY=Y(K)-Y(J)
         SZ=Z(K)-Z(J)

         A=SQRT(AX*AX + AY*AY + AZ*AZ)
         B=SQRT(BX*BX + BY*BY + BZ*BZ)
         S=SQRT(SX*SX + SY*SY + SZ*SZ)

C        SOURCE CONTRIBUTION
         SM=SX*CMX+SY*CMY+SZ*CMZ
         SL=SX*CLX+SY*CLY+SZ*CLZ
         AM=AX*CMX+AY*CMY+AZ*CMZ
         AL=AX*CLX+AY*CLY+AZ*CLZ
         BM=BX*CMX+BY*CMY+BZ*CMZ
         ALL=AM*SL-AL*SM

         IF((A+B-S).GT.0.0.AND.S.GT.0.0)THEN
            RJ3=ALOG((A+B+S)/(A+B-S))/S
         ELSE
            RJ3=0.0
         ENDIF

         PA=PNZ*PNZ*SL + ALL*AM
         PB=PA - ALL*SM
         RNUM=SM*PNZ*(B*PA - A*PB)
         DNOM=PA*PB + PNZ*PNZ*A*B*SM*SM

         IF(ABS(PNZ).LT.EPS)THEN
            DE=0.0
         ELSE
            IF(RNUM.NE.0)THEN
                  DE=ATAN2(RNUM,DNOM)
            ELSE
                  DE=0.0
            ENDIF
         ENDIF

         RJ31 = RJ31 - SIG*ALL*RJ3
         CJK1 = CJK1 - DUB*DE
         VXS=VXS+SIG*(RJ3*(SM*CLX-SL*CMX)+DE*CNX)
         VYS=VYS+SIG*(RJ3*(SM*CLY-SL*CMY)+DE*CNY)
         VZS=VZS+SIG*(RJ3*(SM*CLZ-SL*CMZ)+DE*CNZ)

C        DOUBLET CONTRIBUTION
         AVBX=AY*BZ - AZ*BY
         AVBY=AZ*BX - AX*BZ
         AVBZ=AX*BY - AY*BX
         ADB=AX*BX + AY*BY + AZ*BZ
         VMOD=(A+B)/(A*B*(A*B + ADB))

         VXD=VXD + DUB*VMOD*AVBX
         VYD=VYD + DUB*VMOD*AVBY
         VZD=VZD + DUB*VMOD*AVBZ
 20         CONTINUE

C     LIMITING CASES
      DTT=TWOPI
      IF(RDIST.GT.0.0) PNDS=PNZ**2/RDIST

      IF(PNDS.LT.EPS.AND.RDIST.GT.EPS)THEN
            DTT=PNZ*AREA/SQRT(RDIST)/RDIST
      ENDIF

      IF(ABS(DTT).LT.ABS(CJK1))CJK1=DTT
      IF(RDIST.LT.EPS*EPS)CJK1=-TWOPI

C     TOTAL
      CJK = CJK1
      BJK = RJ31 - PNZ*CJK1
      VX=VXD+VXS
      VY=VYD+VYS
      VZ=VZD+VZS

      TVS=SQRT(VXS*VXS+VYS*VYS+VZS*VZS)
      TVD=SQRT(VXD*VXD+VYD*VYD+VZD*VZD)
      TV=SQRT(VX*VX+VY*VY+VZ*VZ)

      WRITE(*,*)'AREA OF PANEL=',AREA
      WRITE(*,*)'SOURCE (POTENTIAL)=',BJK
      WRITE(*,*)'SOURCE (VELOCITY):'
      WRITE(*,*)'VXS=',VXS
      WRITE(*,*)'VYS=',VYS
      WRITE(*,*)'VZS=',VZS
      WRITE(*,*)'TVS=',TVS
      WRITE(*,*)'DOUBLET (POTENTIAL):', CJK
      WRITE(*,*)'DOUBLET (VELOCITY):'
      WRITE(*,*)'VXD=',VXD
      WRITE(*,*)'VYD=',VYD
      WRITE(*,*)'VZD=',VZD
      WRITE(*,*)'TVD=',TVD
      WRITE(*,*)'TOTAL VELOCITY:'
      WRITE(*,*)'VX=',VX
      WRITE(*,*)'VY=',VY
      WRITE(*,*)'VZ=',VZ
      WRITE(*,*)'TV=',TV

      WRITE(*,*)'DO YOU WANT ANOTHER TRY? 1:YES, 2:NO'
      READ(*,*) NTRY
      IF(NTRY.EQ.1)GO TO 100

      STOP
      END
