C     PROGRAM No. 5: CONSTANT STRENGTH VORTEX
C     ---------------------------------------

C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C      BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF VORTEX PANELS WITH
C      CONST. STRENGTH (NEUMANN B.C., PROGRAM BY STEVEN YON, 1989).
      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL VEL(400),VELT(400),TH(400),DL(400)

      OPEN(8,FILE='CPV.DAT',STATUS='NEW')
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
      DO I=1,N
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

C     CONVERT COLLOCATION POINT INTO LOCAL PANEL COORDS.
            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)
            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)

            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0
            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))

C     SAVE PANEL LENGTHS FOR LATER USE
            IF(I.EQ.1) THEN
               DL(J)=X2
            END IF

            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)
            TH1=ATAN2(Z,X)
            TH2=ATAN2(Z,X-X2)

            IF(I.EQ.J) THEN
               UL=0.5
               WL=0
            ELSE
               UL=0.15916*(TH2-TH1)
               WL=0.15916*LOG(R2/R1)
            END IF

            U=UL*COS(-TH(J))+WL*SIN(-TH(J))
            W=-UL*SIN(-TH(J))+WL*COS(-TH(J))

C     A(I,J) IS THE COMPONENT OF VELOCITY NORMAL TO
C     THE AIRFOIL INDUCED BY THE JTH PANEL AT THE
C     ITH COLLOCATION POINT.
            A(I,J)=-U*SIN(TH(I))+W*COS(TH(I))
            B(I,J)=U*COS(TH(I))+W*SIN(TH(I))
         END DO

         A(I,N)=COS(AL)*SIN(TH(I))-SIN(AL)*COS(TH(I))

      END DO

C     REPLACE EQUATION M/4 WITH A KUTTA CONDITION
      DO J=I,M+1
         A(M/4,J)=0
      END DO

      A(M/4,1)=1
      A(M/4,M)=1

C     SOLVE FOR THE SOLUTION VECTOR OF VORTEX STRENGTHS

      CALL MATRX(A,N,G)


C     CONVERT SOURCE STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND CP'S
C     ON EACH OF THE PANELS

 200   CONTINUE

      CL=0
      DO I=1,M
         TEMP=0
         DO J=1,M
            TEMP=TEMP+B(I,J)*G(J)
         END DO
         VEL(I)=TEMP+COS(AL)*COS(TH(I))+SIN(AL)*SIN(TH(I))
      CL=CL+VEL(I)*DL(I)
      END DO

      WRITE(6,*) 'SMOOTH THE VELOCITY DISTRIBUTION?'
      WRITE(6,*) '1=YES'
      WRITE(6,*) '2=NO'
      READ(5,*) ANS1

      DO I=2,M
         IF(ANS1.EQ.1) THEN
            CP=1-((VEL(I)+VEL(I-1))/2)**2
            WRITE(8,*) PT2(I-1,1),' ,',CP
         ELSE
            CP=1-VEL(I)**2
            WRITE(8,*) CO(I,1),' ,',CP
         END IF
      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=',CL

      STOP
      END
