C     PROGRAM No. 3: CONSTANT STRENGTH SOURCE
C     ---------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C     BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF SOURCE PANELS WITH
C     CONST. STRENGTH (ALPHA=0, NEUMANN B.C., PROGRAM BY STEVEN YON, 1989).

      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL TH(400)

      OPEN(8,FILE='CPS.DAT',STATUS='NEW')
      OPEN(9,FILE='AFOIL2.DAT',STATUS='OLD')

      WRITE(6,*) 'ENTER NUMBER OF PANELS'
      READ(5,*) M
      N=M+1

      WRITE(6,*)'SKIP THE MATRIX REDUCTION? 1=YES, 2=N0'
      READ(5,*) ANS


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


C     JOEGI MOD
C           WRITE(6,*) 'COLLOCATION POINT CO=', CO(I,1)
C           WRITE(6,*) 'CP CO=', CP
C     JOEGI MOD


      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=0'


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