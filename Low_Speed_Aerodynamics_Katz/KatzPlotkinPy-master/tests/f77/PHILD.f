C     PROGRAM No. 10: LINEAR STRENGTH DOUBLET POTENTIAL
C     ------------------------------------------------
C     THIS PROGRAM FINDS THE PRESSURE DISTRIBUTION ON AN ARBITRARY AIRFOIL
C        BY REPRESENTING THE SURFACE AS A FINITE NUMBER OF DOUBLET PANELS WITH
C        LINEAR STRENGTH (DIRICHLET B.C., PROGRAM BY STEVEN YON, 1989).
      REAL EP(400,2),EPT(400,2),PT1(400,2),PT2(400,2)
      REAL CO(400,2),A(400,400),B(400,400),G(400)
      REAL TH(400),DL(400)

      OPEN(8,FILE='CPLD.DAT',STATUS='NEW')
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
C           CONVERT COLLOCATION POINT TO LOCAL PANEL COORDS.
            XT=CO(I,1)-PT1(J,1)
            ZT=CO(I,2)-PT1(J,2)
            X2T=PT2(J,1)-PT1(J,1)
            Z2T=PT2(J,2)-PT1(J,2)

            X=XT*COS(TH(J))+ZT*SIN(TH(J))
            Z=-XT*SIN(TH(J))+ZT*COS(TH(J))
            X2=X2T*COS(TH(J))+Z2T*SIN(TH(J))
            Z2=0

C           SAVE PANEL LENGTHS
            IF(I.EQ.1) THEN
               DL(J)=X2
            END IF

C           FIND TH1, TH2, AND R1,R2
            R1=SQRT(X**2+Z**2)
            R2=SQRT((X-X2)**2+Z**2)
            TH1=ATAN2(Z,X)
            TH2=ATAN2(Z,X-X2)

C           COMPUTE THE POTENTIAL COMPONENTS AS
C           FUNCTIONS OF R, TH
            IF(I.EQ.J) THEN
               PH1=-0.5*(X/X2-1)
               PH2=0.5*(X/X2)
            ELSE
               PH1=0.15916*(X/X2*(TH2-TH1)+Z/X2*
     *            LOG(R2/R1)-(TH2-TH1))
               PH2=-0.15916*(X/X2*(TH2-TH1)+Z/X2*LOG(R2/R1))
            END IF

C           COMPUTE THE COEFFICIENTS IN THE INFLUENCE MATRIX
            IF(J.EQ.1) THEN
               A(I,1)=PH1
               HOLDA=PH2
            ELSE IF(J.EQ.M) THEN
               A(I,M)=HOLDA+PH1
               A(I,M+1)=PH2
            ELSE
               A(I,J)=HOLDA+PH1
               HOLDA=PH2
            END IF
         END DO

C     ADD INFLUENCE OF WAKE AS A(M+2)
         XW=CO(I,1)-PT2(M,1)
         ZW=CO(I,2)-PT2(M,2)
         DTHW=-ATAN(ZW/XW)
         A(I,M+2)=-0.15916*DTHW

         A(I,M+3)=(CO(I,1)*COS(AL)+CO(I,2)*SIN(AL))
      END DO

C     ADD THE DOUBLET GRADIENT CONDITION
      A(M+1,1)=-1
      A(M+1,2)=1
      A(M+1,M)=1
      A(M+1,M+1)=-1

C     ADD THE KUTTA CONDITION
      A(M+2,1)=-1
      A(M+2,M+1)=1
      A(M+2,M+2)=-1
      N=M+3

C     SOLVE FOR THE SOLUTION VECTOR OF DOUBLET STRENGTHS
      CALL MATRX(A,N,G)

C     CONVERT DOUBLET STRENGTHS INTO TANGENTIAL
C     VELOCITIES ALONG THE AIRFOIL SURFACE AND
C     CP'S ON EACH OF THE PANELS.
 200  CONTINUE

      DO I=1,M-1
         R=(DL(I)+DL(I+1))/2
         T1=(G(I)+G(I+1))/2
         T2=(G(I+1)+G(I+2))/2
         VEL=(T2-T1)/R
         CP=1-VEL**2
         WRITE(8,*) PT2(I,1),' ,',CP
      END DO

      WRITE(6,*) ' '
      WRITE(6,*) 'LIFT COEFFICIENT=', G(M+2)

      STOP
      END
