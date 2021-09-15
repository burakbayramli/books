C     PROGRAM No. 6: LINEAR STRENGTH SOURCE
C     -------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C        BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF SOURCE PANELS WITH
C        LINEAR STRENGTH (ALPHA=0, NEUMANN B.C., PROGRAM BY STEVEN YON, 1989).

      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400),V(400)
      REAL TH(400)

      OPEN(8,FILE='CPLS.DAT',STATUS='NEW')
      OPEN(9,FILE='AFOIL2.DAT',STATUS='OLD')

      WRITE(6,*) 'ENTER NUMBER OF PANELS'
      READ(5,*) M
      N=M+1
      AL=0

C     READ IN THE PANEL END POINTS
      DO I=1,M+1
         READ(9,*) EPT(I,1), EPT(I,2)
      END DO

C CONVERT PANELING TO CLOCKWISE
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

      TH(M+1)=0

C     ESTABLISH COLLOCATION POINTS
      DO I=1,M
         CO(I,1)=(PT2(I,1)-PT1(I,1))/2+PT1(I,1)
         CO(I,2)=(PT2(I,2)-PT1(I,2))/2+PT1(I,2)
      END DO

      WRITE(6,*) 'ENTER X COORD. OF WAKE POINT'
      READ(5,*) XX
         CO(M+1,1)=XX
         CO(M+1,2)=0

C ESTABLISH INFLUENCE COEFFICIENTS
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

C           FIND R1, R2, TH1, TH2
            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)
            TH1=ATAN2(Z,X)
            TH2=ATAN2(Z,X-X2)

C           COMPUTE VELOCITY COMPONENTS AS FUNCTIONS OF
C           SIGMA1 AND SIGMA2. THESE VELOCITIES ARE IN
C           THE JTH REFERENCE FRAME.
            IF(I.EQ.J) THEN
               U1L=0.15916
               U2L=-0.15916
               W1L=-0.5*(X-X2)/X2
               W2L=0.5*(X)/X2
            ELSE
               W1L=-(Z*LOG(R2/R1)+X*(TH2-TH1)-X2*(TH2-TH1))/
     *            (6.28319*X2)
               W2L=(Z*LOG(R2/R1)+X*(TH2-TH1))/(6.28319*X2)
               U1L=((X2-Z*(TH2-TH1))-X*LOG(R1/R2)+
     *            X2*LOG(R1/R2))/(6.28319*X2)
               U2L=-((X2-Z*(TH2-TH1))-X*LOG(R1/R2))/(6.28319*X2)
            END IF

C           TRANSFORM THE LOCAL VELOCITIES INTO THE GLOBAL
C           REFERENCE FRAME.
            U1=U1L*COS(-TH(J))+W1L*SIN(-TH(J))
            U2=U2L*COS(-TH(J))+W2L*SIN(-TH(J))
            W1=-U1L*SIN(-TH(J))+W1L*COS(-TH(J))
            W2=-U2L*SIN(-TH(J))+W2L*COS(-TH(J))

C           COMPUTE THE COEFFICIENTS OF SIGMA IN THE
C           INFLUENCE MATRIX
            IF(J.EQ.1) THEN
               A(I,1)=-U1*SIN(TH(I))+W1*COS(TH(I))
               HOLDA=-U2*SIN(TH(I))+W2*COS(TH(I))
               B(I,1)=U1*COS(TH(I))+W1*SIN(TH(I))
               HOLDB=U2*COS(TH(I))+W2*SIN(TH(I))
            ELSE IF(J.EQ.M) THEN
               A(I,M)=-U1*SIN(TH(I))+W1*COS(TH(I))+HOLDA
               A(I,N)=-U2*SIN(TH(I))+W2*COS(TH(I))
               B(I,M)=U1*COS(TH(I))+W1*SIN(TH(I))+HOLDB
               B(I,N)=U2*COS(TH(I))+W2*SIN(TH(I))
            ELSE
               A(I,J)=-U1*SIN(TH(I))+W1*COS(TH(I))+HOLDA
               HOLDA=-U2*SIN(TH(I))+W2*COS(TH(I))
               B(I,J)=U1*COS(TH(I))+W1*SIN(TH(I))+HOLDB
               HOLDB=U2*COS(TH(I))+W2*SIN(TH(I))
            END IF
         END DO

      A(I,N+1)=SIN(TH(I))
      END DO

      N=M+2

      IF(M.EQ.10) THEN
         DO I=1,11
            WRITE(6,10) A(I,1),A(I,2),A(I,3),A(I,4),A(I,5),A(I,6),
     *         A(I,7),A(I,8),A(I,9),A(I,10),A(I,11)
         END DO
      END IF

C     SOLVE FOR THE SOLUTION VECTOR OF SOURCE STRENGTHS
      CALL MATRX(A,N,G)

C     CONVERT SOURCE STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND CP'S
C     ON EACH OF THE PANELS.

 200  CONTINUE

      N=M+1

      DO I=1,M
         VEL=0
         DO J=1,N
            VEL=VEL+B(I,J)*G(J)
         END DO
         V(I)=VEL+COS(AL)*COS(TH(I))+SIN(AL)*SIN(TH(I))
      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'SMOOTH THE VELOCITY DISTRIBUTION?'
      WRITE(6,*) '1=YES'
      WRITE(6,*) '2=NO'
      READ(5,*) ANS1

      DO I=2,M
         IF(ANS1.EQ.1) THEN
            VA=(V(I)+V(I-1))/2
            CP=1-VA**2
            WRITE(8,*) PT1(I,1), ' ,',CP
         ELSE
            CP=1-V(I)**2
            WRITE(8,*) CO(I,1),' ,',CP
         END IF
      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=0'

      STOP
 10   FORMAT( /,F6.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,
     * F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2)

      END
