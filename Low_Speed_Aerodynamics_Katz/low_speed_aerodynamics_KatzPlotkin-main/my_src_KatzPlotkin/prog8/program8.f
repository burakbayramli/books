C     PROGRAM No. 8: CONSTANT STRENGTH DOUBLET POTENTIAL
C     --------------------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C     BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF DOUBLET PANELS WITH
C     CONSTANT STRENGTH (DIRICHLET B.C., PROGRAM BY STEVEN YON, 1989).

      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL TH(400),DL(400)

      OPEN(8,FILE='CPDP.DAT',STATUS='NEW')
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


C     CONVERT THE PANELING TO CLOCKWISE
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


C     CONVERT COLLOCATION POINTS TO LOCAL
C     PANEL(J) COORDINATES.
            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)

            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)

            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0

            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))


C     SAVE PANEL LENGTHS
            IF(I.EQ.1) THEN
                  DL(J)=X2
            END IF


C     FIND R AND THETA COMPONENTS
            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)

            TH1=ATAN2(Z,X)
            TH2=ATAN2(Z,X-X2)


C     COMPUTE INFLUENCE COEFS. A(I,J)
            IF(I.EQ.J) THEN
                  A(I,J)=0.5
            ELSE
                  A(I,J)=-0.15916*(TH2-TH1)
            END IF

            END DO


C     ADD WAKE INFLUENCE
            XW=CO(I,1)-PT2(M,1)
            ZW=CO(I,2)-PT2(M,2)

            DTHW=-ATAN(ZW/XW)

            A(I,N)=-0.15916*(DTHW)
            A(I,N+1)=(CO(I,1)*COS(AL)+CO(I,2)*SIN(AL))

      END DO


C     ADD AN EXPLICIT KUTTA CONDITION
      A(N,1)=-1
      A(N,M)=1
      A(N,N)=-1


C     SOLVE FOR THE SOLUTION VECTOR OF DOUBLET STRENGTHS
      N=N+1

      CALL MATRX(A,N,G)


C     CONVERT DOUBLET STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND
C     CP'S ON EACH OF THE PANELS.
 200  CONTINUE

      DO I=1,M-1
            R=(DL(I)+DL(I+1))/2
            VEL=(G(I+1)-G(I))/R
            CP=1-VEL**2
            
            WRITE(8,*) PT2(I,1),' ,',CP
      END DO


      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=', G(M+1)


C     JOEGI MOD
      CLOSE(8)
      CLOSE(9)
C     JOEGI MOD


      STOP

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

