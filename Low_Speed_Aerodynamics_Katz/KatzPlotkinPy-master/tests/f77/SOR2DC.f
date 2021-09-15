C     PROGRAM No. 3: CONSTANT STRENGTH SOURCE
C       ---------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C       BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF SOURCE PANELS WITH
C       CONST. STRENGTH (ALPHA=0, NEUMANN B.C., PROGRAM BY STEVEN YON, 1989).

      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL TH(400)

      OPEN(8,FILE='CPS.DAT',STATUS='NEW')
      OPEN(9,FILE='AFOIL2.DAT',STATUS='OLD')

      WRITE(6,*) 'ENTER NUMBER OF PANELS'
      READ(5,*) M
      N=M+1

      WRITE(6,*)'SKIP THE MATRIX REDUCTION? 1=YES,2=N0'
      READ(5,*) ANS

C     READ IN THE PANEL END POINTS
      DO I=1,M+1
       READ(9,*) EPT(I,1),EPT(I,2)
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

C     ESTABLISH INFLUENCE COEFFICIENTS
      DO I=1,M
         DO J=1,M552

C     CONVERT COLLOCATION POINT TO LOCAL PANEL COORDS.
            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)
            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)

            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))
            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0

C     FIND R1, R2, TH1, TH2
            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)

            TH1=ATAN2(Z,X)

            TH2=ATAN2(Z,X-X2)

C     COMPUTE VELOCITY IN LOCAL REF. FRAME
            IF(I.EQ.J) THEN
               UL=0
               WL=0.5
            ELSE
               UL=1/(2*3.141593)*LOG(R1/R2)
               WL=1/(2*3.141593)*(TH2-TH1)
            END IF

C     RETURN VELOCITY TO GLOBAL REF. FRAME
            U=UL*COS(-TH(J))+WL*SIN(-TH(J))
            W=-UL*SIN(-TH(J))+WL*COS(-TH(J))

C     A(I,J) IS THE INFLUENCE COEFF. DEFINED BY THE
C     TANGENCY CONDITION. B(I,J) IS THE INDUCED LOCAL
C     TANGENTIAL VELOCITY TO BE USED IN CP CALCULATION.
            A(I,J)=-U*SIN(TH(I))+W*COS(TH(I))
            B(I,J)=U*COS(TH(I))+W*SIN(TH(I))

         END DO

         A(I,N)=SIN(TH(I))

      END DO

C     SOLVE FOR THE SOLUTION VECTOR OF SOURCE STRENGTHS

      IF(ANS.EQ.1) GOTO 200
      CALL MATRX(A,N,G)

C     CONVERT SOURCE STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND CP'S
C     ON EACH OF THE PANELS

 200  CONTINUE

      DO I=1,M
         VEL=0
         DO J=1,M
            VEL=VEL+B(I,J)*G(J)
         END DO
         CP=1-(VEL+COS(TH(I)))**2
         WRITE(8,*) CO(I,1),' ,',CP
      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=0'

      STOP
      END
