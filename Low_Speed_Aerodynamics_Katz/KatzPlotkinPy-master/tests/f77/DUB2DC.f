C     PROGRAM No. 4: CONSTANT STRENGTH DOUBLET
C       ----------------------------------------

C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C       BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF DOUBLET PANELS WITH
C       CONST. STRENGTH (NEUMANN B.C., PROGRAM BY STEVEN YON, 1989).

      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL TH(400)

      OPEN(8,FILE='CPD.DAT',STATUS='NEW')
      OPEN(9,FILE='AFOIL2.DAT',STATUS='OLD')

      WRITE(6,*) 'ENTER NUMBER OF PANELS'
      READ(5,*) M
      N=M+1
      WRITE(6,*) 'ENTER ANGLE OF ATTACK IN DEGREES'
      READ(5,*) ALPHA
      AL=ALPHA/57.2958

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
         DO J=1,M

C     CONVERT THE COLLOCATION POINT
C     TO LOCAL PANEL COORDS.
            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)
            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)
            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))
            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0

            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)

C     COMPUTE THE VELOCITY INDUCED AT THE ITH
C     COLLOCATION POINT BY THE JTH PANEL
            IF(I.EQ.J) THEN
               UL=0
               WL=-1/(3.14159265*X)
            ELSE
               UL=0.15916*(Z/(R1**2)-Z/(R2**2))
               WL=-0.15916*(X/(R1**2)-(X-X2)/(R2**2))
            END IF

            U=UL*COS(-TH(J))+WL*SIN(-TH(J))
            W=-UL*SIN(-TH(J))+WL*COS(-TH(J))

C        A(I,J) IS THE COMPONENT OF VELOCITY INDUCED IN THE
C        DIRECTION NORMAL TO PANEL I BY PANEL J AT THE ITH
C        COLLOCATION POINT

            A(I,J)=-U*SIN(TH(I))+W*COS(TH(I))
            B(I,J)=U*COS(TH(I))+W*SIN(TH(I))

         END DO

C     INCLUDE THE INFLUENCE OF THE WAKE PANEL
         R=SQRT((CO(I,1)-PT2(M,1))**2+(CO(I,2)-PT2(M,2))**2)
         U=0.15916*(CO(I,2)/(R**2))
         W=-0.15916*(CO(I,1)-PT2(M,1))/(R**2)

         A(I,N)=-U*SIN(TH(I))+W*COS(TH(I))
         B(I,N)=U*COS(TH(I))+W*SIN(TH(I))

         A(I,N+1)=COS(AL)*SIN(TH(I))-SIN(AL)*COS(TH(I))

      END DO

C     PREPARE THE MATRIX FOR SOLUTION BY PROVIDING
C     A KUTTA CONDITION
      DO I=1,N+1
         A(N,I)=0
      END DO

      A(N,1)=-1
      A(N,M)=1
      A(N,N)=-1

C     SOLVE FOR THE SOLUTION VECTOR OF DOUBLET STRENGTHS

      N=N+1

      CALL MATRX(A,N,G)

C     CONVERT DOUBLET STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND CP'S
C     ON EACH OF THE PANELS

 200  CONTINUE

      DO I=1,M
         TEMP=0
         DO J=1,M+1
            TEMP=TEMP+B(I,J)*G(J)
         END DO
         IF(I.NE.1.AND.I.NE.M) THEN
            R=SQRT((CO(I+1,1)-CO(I-1,1))**2+(CO(I+1,2)-CO(I-1,2))**2)
            VLOC=(G(I+1)-G(I-1))/R
         ELSE IF(I.EQ.1) THEN
            R=SQRT((CO(2,1)-CO(1,1))**2+(CO(2,2)-CO(1,2))**2)
            VLOC=(G(2)-G(1))/R
         ELSE IF(I.EQ.M) THEN
            R=SQRT((CO(M,1)-CO(M-1,1))**2+(CO(M,2)-CO(M-1,2))**2)
            VLOC=(G(M)-G(M-1))/R
         END IF
         VEL=COS(AL)*COS(TH(I))+SIN(AL)*SIN(TH(I))+TEMP+VLOC/2
         CP=1-VEL**2
         WRITE(8,*) CO(I,1),' ,',CP

      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=',G(M+1)

      STOP
      END
