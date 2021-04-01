        PROGRAM AIRFOIL

        PARAMETER ( M = 12 )
        DIMENSION XB(M+1),YB(M+1),X(M),Y(M),S(M),SINE(M),COSINE(M), 
     * THETA(M) ,V(M) ,CP(M), GAMA(M+1) ,RHS(M+1) ,CN1(M,M),
     * CN2(M,M), CT1(M,M) ,CT2(M,M) ,AN(M+1,M+1) ,AT(M,M+1),
     * CN11(M)

        DATA XB 
     * /1.,.933,.750,.500,.250,.067,.0,.067,.25,.500,.750,.933, 1.0/
        DATA YB 
     * /.0,-.005,-.017,-.033,-.042,-.033,.0,.045,.076,.072,.044,.013,0./

        MP1 = M+1
        PI = 4.0 * ATAN(1.0)
        ALPHA =  8. * PI/180.

! C COORDINATES (X, Y) OF CONTROL POIN'!' PANEL LENGTH S ARE
! C COMPUTED FOR EACH OF THE VORTEX PANEL,. RHS REPRESEN'l'S
! C THE RIGHT-HAND SIDE OF EQ. (5.47).
        DO I = 1, M
        IP1 = I + 1
        X(I) = 0.5*(XB(I)+XB(IP1))
        Y(I) = 0.5*(YB(I)+YB(IP1))
        S(I) = SQRT( (XB(IP1)-XB(I))**2 + (YB(IP1)-YB(I))**2)
        THETA(I) = ATAN2( (YB(IP1)-YB(I)), (XB(IP1)-XB(I)) )
        SINE(I) = SIN( THETA (I) )
        COSINE(I) = COS( THETA (I) )
        RHS(I) = SIN( THETA(I)-ALPHA )
        END DO
        DO I = 1, M
        DO J = 1, M
        IF ( I .EQ. J ) THEN
        CN1(I,J) = -1.0
        CN2(I,J) = 1.0
        CT1(I,J) = 0.5*PI
        CT2(I,J) = 0.5*PI
        ELSE
        A = -(X(I)-XB(J))*COSINE(J) - (Y(I)-YB(J))*SINE(J)
        B = (X(I)-XB(J))**2 + (Y(I)-YB(J))**2
        C = SIN( THETA(I)-THETA(J) )
        D = COS ( THETA(I)-THETA(J) )
        E = (X(I)-XB(J))*SINE(J) - (Y(I)-YB(J))*COSINE(J)
        floatInp = (1. + S(J)*(S(J)+2.*A)/B)
        F = ALOG( 1.0 + S(J)*(S(J)+2.*A)/B )
        G = ATAN2( E*S(J), B+A*S(J) )
        P = (X(I)-XB(J)) * SIN( THETA(I)-2.*THETA(J) ) 
     *    + (Y(I)-YB(J)) * COS( THETA(I)-2.*THETA(J) )
        Q = (X(I)-XB(J)) * COS( THETA(I)-2.*THETA(J) ) 
     *    - (Y(I)-YB(J)) * SIN( THETA(I)-2.*THETA(J) )
        CN2(I,J) = D + .5*Q*F/S(J) - (A*C+D*E)*G/S(J)
        CN1(I,J) = .5*D*F + C*G - CN2(I,J)
        CT2(I,J) = C + .5*P*F/S(J) + (A*D-C*E)*G/S(J)
        CT1(I,J) = .5*C*F - D*G - CT2(I,J)
        ! For debugging purposes
        ! write(*, *) 'ID: ', I, J
        ! WRITE( *, * ) A, B, C, D, E, F, G, P, Q
        ! write(*, *) 'CN2 result: ', CN2(I,J) 
        ! write(*, *) 'CN1 result: ', CN1(I,J)
        ! write(*, *) 'CT2 result: ', CT2(I,J) 
        ! write(*, *) 'CT1 result: ', CT1(I,J) 
        END IF
        END DO
        END DO
C COMPUTE INFLUENCE COEFFICIENTS IN EQS.(s.47) AND (5.49),
C RESPECTIVELY.
        DO I = 1, M
        AN(I,1)     = CN1(I,1)
        AN(I,MP1)   = CN2(I,M)
        AT(I,1)     = CT1(I,1)
        AT(I,MP1)   = CT2(I,M)
            DO J = 2, M
            AN(I,J) = CN1(I,J) + CN2(I,J-1)
            AT(I,J) = CT1(I,J) + CT2(I,J-1)
            END DO
        END DO
        AN(MP1,1) = 1.0
        AN(MP1,MP1) = 1.0
        DO J = 2, M
        AN(MP1,J) = 0.0
        END DO
        RHS(MP1) = 0.0
C SOLVE EQ. (5.47) FOR DIMENSIONLESS STRENGTHS GAMA USING
C CRAMER'S RULE. THEN COMPUTE AND PRINT DIMENSIONLESS
C VELOCITY AND PRESSURE COEFFICIENT AT CONTROL POINTS.
        WRITE (6,6)
6       FORMAT(1H1///11X,1HI,4X,4HX(I),4X,4HY(I),4X,8HTHETA(I),
     *               3X,4HS(I),3X,7HGAMA(I),3X,4HV(I),6X,5HCP(I)/
     *               10X,3H---,3X,4H----,4X,4H----,4X,8H--------,
     *               3X,4H----,3X,7H-------,3X,4H----,6X,5H-----)
        CALL CRAMER ( AN, RHS, GAMA, MP1 )
        DO I = 1, M
        V(I) = COS( THETA(I)-ALPHA )
            DO J = 1, MP1
            V(I) = V(I) + AT(I,J)*GAMA(J)
            CP(I) = 1.0 - V(I)**2
            END DO
        WRITE(6,9) I,X(I),Y(I),THETA(I),S(I),GAMA(I),V(I),CP(I)
        END DO
9       FORMAT(10X,I2,F8.4,F9.4,F10.4,F8.4,2F9.4,F10.4)
        WRITE(6,10) MP1,GAMA(MP1)
10      FORMAT(10X,I2,35X,F9.4)



        STOP
        END

        SUBROUTINE CRAMER( C, A, X, N )
C THIS SUBROUTINE SOLVES A SET OF ALGEBRAIC EQUATIONS
C C(I,J)*X(J) = A(I), I=1,2,---,N
C IT IS TAKEN FROM P.114 OF CHOW(1979)
        PARAMETER ( M = 12 )
        DIMENSION C(M+1,M+1),CC(M+1,M+1),A(M+1),X(M+1)
        DENOM = DETERM( C, N )
        write (*,*), C
        DO K = 1, N
            DO I = 1, N
                DO J = 1, N
                CC(I,J) = C(I,J)
                END DO
            END DO
            DO I = 1, N
            CC(I,K) = A(I)
            END DO
        PUNOM = DETERM( CC, N )
        X(K) = DETERM( CC, N ) / DENOM
        ! Debugging
        ! write (*, *) "Denominator", DENOM
        ! write (*, *) "Numerator", PUNOM
        ! write (*, *) "GAMMA", X(K)

        END DO
        RETURN
        END
        FUNCTION DETERM ( ARRAY, N )
C DETERM IS THE VALUE OF THE DETERMINANT OF AN N*N
C MATRIX CALLED ARRAY, COMPUTED BY THE TECHNIQUE
C OF PIVOTAL CONDENSATION. THIS FUNCTION IS TAKEN
C FROM PP.113-114 OF CHOW(1979)
        PARAMETER ( M = 12 )
        DIMENSION ARRAY(M+1,M+1),A(M+1,M+1)
        DO I = 1, N
        DO J = 1, N
        A(I,J) = ARRAY(I,J)
        END DO
        END DO
        L = 1
1       K = L + 1
        DO I = K, N
        RATIO = A(I,L)/A(L,L)
            DO J = K, N
            A(I,J) = A(I,J) - A(L,J)*RATIO
            END DO
        END DO
        L = L + 1
        IF( L .LT. N ) GO TO 1
        DETERM =1.
        DO L = 1, N
        DETERM = DETERM * A(L,L)
        END DO
        RETURN
        END
