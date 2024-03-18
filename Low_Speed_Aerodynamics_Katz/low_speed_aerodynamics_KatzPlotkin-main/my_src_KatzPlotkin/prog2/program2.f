C     PROGRAM No. 2: DISCRETE VORTEX METHOD (THIN WING, ELLIPTIC CAMBER)
C     -----------------------------------------------------------------
C
C     DISCRETE VORTEX MODEL FOR THIN AIRFOILS (JOE KATZ, CIRCA 1986)
      DIMENSION GAMMA(52),XC(52),ZC(52),X(52),Z(52)
      DIMENSION ENX(52),ENZ(52),A(52,52),IP(52)
      DIMENSION DL(52),DCP(52),DCP1(52)


C
      N=10
      C=1.0
      EPSILON=0.1*C
      ALFA1=10.0
      PAY=3.141592654
      ALFA=ALFA1*PAY/180.0
      RO=1.
      V=1.
      UINF=COS(ALFA)*V
      WINF=SIN(ALFA)*V
      QUE=0.5*RO*V*V


C
C     GRID GENERATION (N PANELS)
C
      DX=C/N
      DO 1 I=1,N


C     COLLOCATION POINT
      XC(I) = C/N*(I-0.25)
      ZC(I) = 4.*EPSILON*XC(I)/C*(1.-XC(I)/C)


C     VORTEX POINT
      X(I) = C/N*(I-0.75)
      Z(I) = 4.*EPSILON*X(I)/C*(1.-X(I)/C)


C     NORMAL AT COLLOCATION POINT; N=(ENX,ENZ)
      DETADX=4.*EPSILON/C*(1.-2.*XC(I)/C)
      SQ=SQRT(1+DETADX**2)
      ENX(I)= -DETADX/SQ
 1    ENZ(I)= 1./SQ


C
C     INFLUENCE COEFFICIENTS
C
      DO 3 I=1,N
            DO 2 J=1,N
                  CALL VOR2D(XC(I),ZC(I),X(J),Z(J),1.0,U,W)
                  A(I,J)=U*ENX(I)+W*ENZ(I)
 2    CONTINUE

C     THE RHS VECTOR IS PLACED IN THE GAMMA VECTOR
            GAMMA(I)=-UINF*ENX(I)-WINF*ENZ(I)

 3    CONTINUE


C
C     SOLUTION OF THE PROBLEM: RHS(I)=A(I,J)*GAMMA(I)
C
      CALL DECOMP(N,52,A,IP)
      CALL SOLVER(N,52,A,GAMMA,IP)


C
C     AERODYNAMIC LOADS
C
      BL=0.0

      DO 4 I=1,N
            DL(I)=RO*V*GAMMA(I)
            DCP(I)=DL(I)/DX/QUE

C     DCP1 IS THE ANALYTIC SOLUTION
            DD=32.*EPSILON/C*SQRT(X(I)/C*(1.-X(I)/C))
            DCP1(I)=4.*SQRT((C-X(I))/X(I))*ALFA+DD
 4          BL=BL+DL(I)

      CL=BL/(QUE*C)
      CL1=2.*PAY*(ALFA+2*EPSILON/C)
C     CL1, DCP1 - ARE THE EXACT SOLUTIONS


C
C     OUTPUT
      WRITE(6,14)
      WRITE(6,15) V,CL,CL1,N,ALFA1


      DO 5 I=1,N
 5    WRITE(6,16)I,X(I),DCP(I),DCP1(I)


C
 14   FORMAT( ' THIN AIRFOIL WITH ELLIPTIC CAMBER ')
 15   FORMAT( ' V=',F7.1,3X,'CL=',F7.3,3X,'CL(EXACT)=',F7.3,3X,
     *'N= ',I6,3X,'ALPHA= ',F6.1)
 16   FORMAT( I5,3X,'X=',F8.2,5X,'DCP=',F8.2,3X,'DCP(EXACT)=',5F6.2)


C
C     PLOTTER OUTPUT IS PLACED HERE (e.g. DCP AND DCP1 - VS - X/C)
C
      STOP
      END
C





C     *****************************************************

      SUBROUTINE VOR2D(X,Z,X1,Z1,GAMMA,U,W)
C     CALCULATES INFLUENCE OF VORTEX AT (X1,Z1)
      PAY=3.141592654
      U=0.0
      W=0.0
      RX=X-X1
      RZ=Z-Z1
      R=SQRT(RX**2+RZ**2)

      IF(R.LT.0.001) GOTO 1
            V=0.5/PAY*GAMMA/R
            U=V*(RZ/R)
            W=V*(-RX/R)
 1    CONTINUE

      RETURN

      END

C     *****************************************************



C     *****************************************************

C
C     THE FOLLOWING SUBROUTINES ARE LISTED WITH THE STEADY STATE
C     VORTEX LATTICE SOLVER (PROGRAM No. 13).
C
C     SUBROUTINE DECOMP(N,NDIM,A,IP)
C     SUBROUTINE SOLVER(N,NDIM,A,B,IP)
C
C

C     *****************************************************



C     *****************************************************

      SUBROUTINE DECOMP(N,NDIM,A,IP)
      REAL A(NDIM,NDIM),T
      INTEGER IP(NDIM)

C     MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION.
C     N = ORDER OF MATRIX. NDIM = DECLARED DIMENSION OF ARRAY A.
C     A = MATRIX TO BE TRIANGULARIZED.
C     IP(K) , K .LT. N = INDEX OF K-TH PIVOT ROW.
C

      IP(N) = 1

      DO 6 K = 1, N
            IF(K.EQ.N) GOTO 5
            KP1 = K + 1
            M = K

            DO 1 I = KP1, N
                  IF( ABS(A(I,K)).GT.ABS(A(M,K))) M=I
 1          CONTINUE

            IP(K) = M

            IF(M.NE.K) IP(N) = -IP(N)

            T = A(M,K)
            A(M,K) = A(K,K)
            A(K,K) = T

            IF(T.EQ.0.E0) GO TO 5

            DO 2 I = KP1, N

 2                A(I,K) = -A(I,K)/T

            DO 4 J = KP1, N

                  T = A(M,J)
                  A(M,J) = A(K,J)
                  A(K,J) = T

                  IF(T .EQ. 0.E0) GO TO 4

            DO 3 I = KP1, N

 3                A(I,J) = A(I,J) + A(I,K)*T

 4          CONTINUE

 5    IF(A(K,K) .EQ. 0.E0) IP(N) = 0

 6    CONTINUE

      RETURN
      END

C     *****************************************************



C     *****************************************************

      SUBROUTINE SOLVER(N,NDIM,A,B,IP)
      REAL A(NDIM,NDIM), B(NDIM), T
      INTEGER IP(NDIM)

C     SOLUTION OF LINEAR SYSTEM, A*X = B.
C     N = ORDER OF MATRIX.
C     NDIM = DECLARED DIMENSION OF THE ARRAY A.
C     B = RIGHT HAND SIDE VECTOR.
C     IP = PIVOT VECTOR OBTAINED FROM SUBROUTINE DECOMP.
C     B = SOLUTION VECTOR, X.
C

      IF(N.EQ.1) GOTO 9

       NM1 = N - 1

      DO 7 K = 1, NM1
            KP1 = K + 1
            M = IP(K)
            T = B(M)
            B(M) = B(K)
            B(K) = T

      DO 7 I = KP1, N

 7          B(I) = B(I) + A(I,K)*T

      DO 8 KB = 1, NM1
            KM1 = N - KB
            K = KM1 + 1
            B(K) = B(K)/A(K,K)
            T = -B(K)

      DO 8 I = 1, KM1
 8          B(I) = B(I) + A(I,K)*T
 9          B(1) = B(1)/A(1,1)
 
      RETURN
      END

C     *****************************************************