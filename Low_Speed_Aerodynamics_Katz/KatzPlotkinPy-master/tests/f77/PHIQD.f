C     PROGRAM No. 11: QUADRATIC STRENGTH DOUBLET POTENTIAL
C     ----------------------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C        BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF DOUBLET PANELS WITH
C        QUADRATIC STRENGTH (DIRICHLET B.C., PROGRAM BY STEVEN YON, 1989).
      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400,3),G(400)
      REAL DL(400),U1(400),A1(400),B1(400),TH(400)

      OPEN(8,FILE='CPQD.DAT',STATUS='NEW')
      OPEN(9,FILE='AFOIL2.DAT',STATUS='OLD')

      WRITE(6,*) 'ENTER NUMBER OF PANELS'
      READ(5,*) M
      N=M+1
      WRITE(6,*) 'ENTER ANGLE OF ATTACK IN DEGREES'
      READ(5,*) ALPHA
      AL=ALPHA/57.2958

C     READ IN THE PANEL END POINTS
      DO I=1,M+1
         READ(9,*) EPT(I,1), EPT(I,2)
      END DO

C     CONVERT PANELING TO CLOCKWISE
      DO I=1,M+1
         EP(I,1)=EPT(N-I+1,1)
         EP(I,2)=EPT(N-I+1,2)
      END DO

C     ESTABLISH COORDINATES OF PANEL END POINTS
      DO I=1,M
         PT1(I,1)=EP(I,1)
         PT2(I,1)=EP(I+1,1)
         PT1(I,2)=EP(I,2)
         PT2(I,2)=EP(I+1,2)
      END DO

C     FIND PANEL ANGLES TH(J)
      DO I=1,M
         DZ=PT2(I,2)-PT1(I,2)
         DX=PT2(I,1)-PT1(I,1)
         TH(I)=ATAN2(DZ,DX)
      END DO

C     ESTABLISH COLLOCATION POINTS
      DO I=1,M
         CO(I,1)=(PT2(I,1)-PT1(I,1))/2+PT1(I,1)
         CO(I,2)=(PT2(I,2)-PT1(I,2))/2+PT1(I,2)
      END DO

C     ESTABLISH LOCATION OF ADDITIONAL COLLOCATION POINT
         WRITE(6,*) 'ENTER X COORD. OF INTERNAL POINT'
         READ(5,*) XX
         CO(M+1,1)=XX
         CO(M+1,2)=0.0

C     ESTABLISH INFLUENCE COEFFICIENTS
      DO I=1,M+1
         DO J=1,M
C           CONVERT COLLOCATION POINT TO LOCAL PANEL COORDS.
            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)
            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)

            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))
            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0

C           SAVE PANEL LENGTHS FOR LATER USE
            IF(I.EQ.1) THEN
               DL(J)=X2
            END IF

C           FIND TH1, TH2, AND R1, R2
            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)
            TH1=ATAN2(Z,X)
            TH2=ATAN2(Z,X-X2)

C           COMPUTE THE INFLUENCE COEFFICIENTS IN
C           THE 'B' MATRIX (UNREDUCED).
            IF(I.EQ.J) THEN
               B(I,J,1)=0.5
               B(I,J,2)=0.5*X
               B(I,J,3)=0.5*X**2
            ELSE
               B(I,J,1)=-0.15916*(TH2-TH1)
               B(I,J,2)=-0.15916*(X*(TH2-TH1)+Z*LOG(R2/R1))
               B(I,J,3)=0.15916*((X**2-Z**2)*(TH1-TH2)
     *            -2*X*Z*LOG(R2/R1)-Z*X2)
            END IF
         END DO
      END DO

C     ADD DOUBLET GRADIENT CONDITION
      B(M+2,1,1)=0
      B(M+2,1,2)=1
      B(M+2,1,3)=0
      B(M+2,M,1)=0
      B(M+2,M,2)=1
      B(M+2,M,3)=2*DL(M)

C     ADD KUTTA CONDITION
      B(M+3,1,1)=-1
      B(M+3,1,2)=0
      B(M+3,1,3)=0
      B(M+3,M,1)=1
      B(M+3,M,2)=DL(M)
      B(M+3,M,3)=(DL(M))**2

C     BACK SUBSTITUTE THE 'B' MATRIX WITH THE
C     REGRESSION FORMULA TO GET THE COMPONENTS
C     OF THE 'A' MATRIX.
      DO I=1,M+3
         DO J=M-1,1,-1
            B(I,J,1)=B(I,J,1)+B(I,J+1,1)
            B(I,J,2)=B(I,J,2)+B(I,J+1,1)*DL(J)+B(I,J+1,2)
            B(I,J,3)=B(I,J,3)+B(I,J+1,1)
     *         *(DL(J))**2+2*B(I,J+1,2)*DL(J)
         END DO
            A(I,1)=B(I,1,1)
            A(I,2)=B(I,1,2)
         DO J=1,M
            A(I,J+2)=B(I,J,3)
         END DO
      END DO

C     ADD INFLUENCE OF WAKE AS A(I,M+3)
C     AND RHS AS A(I,M+4)
      DO I=1,M+1
         XW=CO(I,1)-PT2(M,1)
         ZW=CO(I,2)-PT2(M,2)
         DTHW=-ATAN(ZW/XW)
         A(I,M+3)=-0.15916*DTHW
         A(I,M+4)=(CO(I,1)*COS(AL)+CO(I,2)*SIN(AL))
      END DO

C     COMPLETE KUTTA COND. BY ADDING WAKE COEFF AND RHS
C     TO ROWS M+2 AND M+3
      A(M+2,M+3)=0
      A(M+2,M+4)=0
      A(M+3,M+3)=-1
      A(M+3,M+4)=0
      N=M+4

C     SOLVE FOR THE SOLUTION VECTOR OF DOUBLET STRENGTHS
      CALL MATRX(A,N,G)

C     CONVERT DOUBLET STRENGTHS, LINEAR CORRECTION
C     AND QUADRATIC CORRECTION INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND CP'S
C     ON EACH OF THE PANELS.

 200  CONTINUE

C     FORWARD SUBSTITUTE USING THE SOLUTION VECTOR TO
C     GET THE DOUBLET STRENGTH PARAMETERS
C     FOR EACH PANEL.
      U1(1)=G(1)
      A1(1)=G(2)
      DO I=3,M+2
         B1(I-2)=G(I)
      END DO

      DO I=1,M-1
         U1(I+1)=U1(I)+A1(I)*DL(I)+B1(I)*(DL(I))**2
         A1(I+1)=A1(I)+2*B1(I)*DL(I)
      END DO

C     THE DERIVATIVE OF THE DOUBLET STRENGTH IS THE
C     SURFACE SPEED ALONG EACH PANEL.
      DO I=1,M
         VEL=A1(I)+B1(I)*DL(I)
         CP=1-VEL**2
         WRITE(8,*) CO(I,1),' ,',CP
      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=', G(M+3)

      STOP
      END
