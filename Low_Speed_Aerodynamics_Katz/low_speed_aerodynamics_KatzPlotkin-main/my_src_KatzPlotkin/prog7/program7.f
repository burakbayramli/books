C     PROGRAM No. 7: LINEAR STRENGTH VORTEX
C     -------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C     BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF VORTEX PANELS WITH
C     LINEAR STRENGTH (NEUMANN B.C., PROGRAM BY STEVEN YON, 1989).

      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL TH(400),DL(400)

      OPEN(8,FILE='CPLV.DAT',STATUS='NEW')
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


C     CONVERT COLLOCATION POINT TO LOCAL PANEL COORDS.
            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)

            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)

            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))

            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0


C     SAVE PANEL LENGTHS FOR LIFT COEFF. CALC.
            IF(I.EQ.1) THEN
                  DL(J)=X2
            END IF


C     FIND R1, R2, TH1, TH2
            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)

            TH1=ATAN2(Z,X)
            TH2=ATAN2(Z,X-X2)


C     COMPUTE VELOCITY COMPONANTS AS FUNCTIONS OF
C     GAMMA1 AND GAMMA2. THESE VELOCITIES ARE IN
C     THE JTH REFERENCE FRAME.
            IF(I.EQ.J) THEN
                  U1L=-0.5*(X-X2)/(X2)
                  U2L=0.5*(X)/(X2)
                  W1L=-0.15916
                  W2L=0.15916
            ELSE
                  U1L=-(Z*LOG(R2/R1)+X*(TH2-TH1)-X2*(TH2-TH1))/
     *            (6.28319*X2)

                  U2L=(Z*LOG(R2/R1)+X*(TH2-TH1))/(6.28319*X2)

                  W1L=-((X2-Z*(TH2-TH1))-X*LOG(R1/R2)
     *            +X2*LOG(R1/R2))/(6.28319*X2)

                  W2L=((X2-Z*(TH2-TH1))-X*LOG(R1/R2))/(6.28319*X2)
            END IF


C     TRANSFORM THE LOCAL VELOCITIES INTO THE
C     GLOBAL REFERENCE FRAME.
            U1=U1L*COS(-TH(J))+W1L*SIN(-TH(J))
            U2=U2L*COS(-TH(J))+W2L*SIN(-TH(J))
            W1=-U1L*SIN(-TH(J))+W1L*COS(-TH(J))
            W2=-U2L*SIN(-TH(J))+W2L*COS(-TH(J))


C     COMPUTE THE COEFFICIENTS OF GAMMA IN THE
C     INFLUENCE MATRIX.
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

            A(I,N+1)=COS(AL)*SIN(TH(I))-SIN(AL)*COS(TH(I))

      END DO


C ADD THE KUTTA CONDITION
      A(N,1)=1
      A(N,N)=1

      IF(M.EQ.10) THEN
            DO I=1,11
                  WRITE(6,10) A(I,1),A(I,2),A(I,3),A(I,4),A(I,5),
     *            A(I,6),A(I,7),A(I,8),A(I,9),A(I,10),A(I,11)  
            END DO
      END IF

      N=N+1


C     SOLVE FOR THE SOLUTION VECTOR OF VORTEX STRENGTHS
      CALL MATRX(A,N,G)


C     CONVERT VORTEX STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND CP'S
C     ON EACH OF THE PANELS.
 200  CONTINUE

      N=M+1

      CL=0

      DO I=1,M

            VEL=0

            DO J=1,N
                  VEL=VEL+B(I,J)*G(J)
            END DO

            V=VEL+COS(AL)*COS(TH(I))+SIN(AL)*SIN(TH(I))
            CL=CL+V*DL(I)
            CP=1-V**2

            WRITE(8,*) CO(I,1),' ,',CP

      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=',CL


C     JOEGI MOD
      CLOSE(8)
      CLOSE(9)
C     JOEGI MOD


      STOP


 10   FORMAT(/,F6.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,
     * 1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2)


      END





C     *****************************************************

      SUBROUTINE MATRX(A,N,G)

C     MATRX IS A MATRIX REDUCER OF THE GAUSSIAN TYPE
C     A(I,J) IS THE MATRIX, A(I,N) IS THE RHS VECTOR
C     AND G(I) IS THE SOLUTION VECTOR.

      REAL A(400,400),TEMP(400,400),G(400)


C     INITIALIZE THE G VECTOR TO ALL ZEROES
      DO I=1,N-1
            G(I)=0
      END DO


C     CONVERT COEFFICIENT MATRIX TO
C     UPPER TRIANGULAR FORM
      DO I=1,N-1
 5          IF(ABS(A(I,I)).LT.0.0000001) GOTO 9

            P=A(I,I)

            DO J=I,N
                  A(I,J)=A(I,J)/P
            END DO

            DO K=I+1,N-1

                  P2=A(K,I)

                  DO L=I,N
                        A(K,L)=A(K,L)-P2*A(I,L)
                  END DO
            END DO
      END DO


C     BACK SUBSTITUTE TRIANGULARIZED MATRIX TO GET
C     VALUES OF SOLUTION VECTOR
      DO I=N-1,1,-1

            G(I)=A(I,N)

            DO J=1,N-1
                  A(I,I)=0
                  G(I)=G(I)-A(I,J)*G(J)
            END DO
      END DO



      RETURN


C     ORDER MATRIX SO THAT DIAGONAL COEFFICIENTS ARE
C     NOT =0 AND STOP IS MATRIX IS SINGULAR

 9    IF(I.NE.N-1) THEN

            DO J=1,N
                  TEMP(I,J)=A(I,J)
                  A(I,J)=A(I+1,J)
                  A(I+1,J)=TEMP(I,J)
            END DO

            GOTO 5

      ELSE
            GOTO 10
      END IF

 10   WRITE(6,*) 'NO SOLUTION'

      STOP
      END

C     *****************************************************

