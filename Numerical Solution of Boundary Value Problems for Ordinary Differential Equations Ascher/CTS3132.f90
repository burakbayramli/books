MODULE CTS3132
! Define Cash Test Set, problems 31-32.
  
! Declare global parameters 
  INTEGER :: TP
  DOUBLE PRECISION :: EPS
  
  INTEGER, PARAMETER :: NODE=4,LEFTBC=2,RIGHTBC=NODE-LEFTBC 
  
CONTAINS

  SUBROUTINE PROBLEM_DEFN(A,B,EPS0,EPSF)
    DOUBLE PRECISION :: A,B,EPS0,EPSF
    SELECT CASE (TP)
      CASE (1:30)
        PRINT *,'To solve a problem in the range 1-30, you must '
        PRINT *,'USE CTS130 instead of USE CTS3132.'
        STOP
      CASE (31)
        A = 0D0
        B = 1D0
        EPSF = 0.01D0
        EPS0 = EPSF
      CASE (32)
        A = 0D0
        B = 1D0
        EPS0 = 1D0/100D0
        EPSF = 1D0/10000D0
 !       EPS0 = EPSF
      CASE DEFAULT
        PRINT *,'Test problem not recognized.'
        STOP
    END SELECT
  END SUBROUTINE PROBLEM_DEFN

  SUBROUTINE FSUB(X,Y,F)
	DOUBLE PRECISION :: X,Y(NODE),F(NODE)
    DOUBLE PRECISION :: yx,theta,M,Q,T,R
    SELECT CASE (TP)
      CASE (31)
        yx = Y(1)   ! Use yx instead of y to avoid collision of names.
        theta = Y(2)
        M = Y(3)
        Q = Y(4)
        T = 1D0/COS(theta) + EPS*Q*TAN(theta)
        F(1) = SIN(theta)
        F(2) = M
        F(3) = -Q/EPS
        F(4) = ( (yx - 1D0)*COS(theta) - M*T )/EPS
      CASE (32)
        F(1) = Y(2)
        F(2) = Y(3)
        F(3) = Y(4)
        R = 1D0/EPS
        F(4) = R*(Y(2)*Y(3) - Y(1)*Y(4))
      CASE DEFAULT
        PRINT *,'Test problem not implemented.'
        STOP        
    END SELECT
  END SUBROUTINE FSUB

  SUBROUTINE BCSUB(YA,YB,BCA,BCB)
	DOUBLE PRECISION :: YA(NODE),YB(NODE),BCA(LEFTBC),BCB(RIGHTBC)
    SELECT CASE (TP)
      CASE (31)
      ! Y(1) is y, Y(3) is M
        BCA = (/ YA(1),YA(3) /)
        BCB = (/ YB(1),YB(3) /)
      CASE (32)
        BCA = (/ YA(1),YA(2) /)
        BCB = (/ YB(1) - 1D0,YB(2) /)
      CASE DEFAULT
        PRINT *,'Test problem not implemented.'
        STOP        
    END SELECT
  END SUBROUTINE BCSUB

END MODULE CTS3132
