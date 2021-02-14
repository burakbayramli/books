MODULE DEFINE_FCN

! Define global variables for the number of differential
! equations, NEQNS, and the number of boundary conditions
! at the left end of the interval, LEFTBC.  The number of
! boundary conditions at the right end is NODE-LEFTBC.
  INTEGER, PARAMETER :: NEQNS=2,LEFTBC=1,RIGHTBC=NEQNS-LEFTBC  

CONTAINS

  SUBROUTINE FSUB(X,Y,F)
	DOUBLE PRECISION :: X,Y(NEQNS),F(NEQNS)

    F = (/ Y(2), -(X*Y(1) - 1D0)*Y(1) /)
    
  END SUBROUTINE FSUB
  
  SUBROUTINE DF(X,Y,PD)
	DOUBLE PRECISION :: X,Y(NEQNS),PD(NEQNS,NEQNS)

    PD = 0D0
    PD(1,2) = 1D0
    PD(2,1) = 1D0 - 2D0*X*Y(1)

  END SUBROUTINE DF    

  SUBROUTINE BCSUB(YA,YB,BCA,BCB)
	DOUBLE PRECISION :: YA(NEQNS),YB(NEQNS),BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(2)
	BCB(1) = YB(1) + YB(2)

  END SUBROUTINE BCSUB
  
  SUBROUTINE GUESS_Y(X,Y)
    DOUBLE PRECISION :: X,Y(NEQNS)

    IF (X <= 1.5D0) THEN
      Y(1) = 2D0
      Y(2) = 0D0
    ELSE
      Y(1) = 2D0*EXP(1.5D0 - X)
      Y(2) = - Y(1)
    END IF
    
  END SUBROUTINE GUESS_Y 

END MODULE DEFINE_FCN

!************************************************************

PROGRAM ESI
! U.M. Ascher and R.D. Russell, Reformulation of boundary
! value problems into `standard' form, SIAM Review 23 (1981)
! 238-254 use a BVP from electromagnetic self-interaction
! theory to discuss problems with singular coefficients set
! on an infinite interval.  After some preparation they solve
!
!   u'' + 4*u'/t + (t*u - 1)*u = 0
!
! with u'(0) = 0 and u(L) + u'(L) = 0. The problem is set on
! an infinite interval, so some experimentation is necessary
! to verify that a sufficiently large L has been specified.
! They present results for u(0) when L = 5,8,10,20.  They
! use the initial guess u(t) = 2 for 0 <= t <= 1.5 and 
! u(t) = 2*exp(1.5 - t) for t > 1.5.  We use this guess for
! the first L and thereafter use the solution for one L as
! the guess for the next, extending to the right with the
! value from their guess (which has the right asymptotic
! behavior).

  USE DEFINE_FCN
  USE BVP_M
  
  TYPE(BVP_SOL) :: SOL
  DOUBLE PRECISION :: S(NEQNS,NEQNS),L(4),AR(4),YA(NEQNS),YBNEW(NEQNS)
  INTEGER :: I

! Define the singular term.
  S = 0D0
  S(2,2) = -4D0

  L = (/ 5D0, 8D0, 10D0, 20D0 /)
! Results obtained by Ascher/Russell.  
  AR = (/ 2.03464, 2.11986, 2.11997, 2.11997 /) 
  PRINT *,'  L      BVP_SOLVER      ASCHER/RUSSELL' 
  PRINT *,' '
  DO I = 1,4
    IF (I == 1) THEN
      SOL = BVP_INIT(NEQNS,LEFTBC,(/ 0D0,L(I) /),GUESS_Y)
    ELSE
      ! Use current approximate solution info to extend initial guess.
      ! Extend interval to the right. Left end is fixed at 0.
      CALL BVP_EVAL(SOL,0D0,YA)  ! Get solution at 0.0D0.    
      CALL GUESS_Y(L(I),YBNEW)     ! Get solution at L(I).
      SOL = BVP_EXTEND(SOL,0D0,L(I),YA,YBNEW)
    END IF

  ! The code can solve this problem with finite difference Jacobians,
  ! but it is easy to provide the analytical Jacobian and doing so
  ! helps confirm that the code handles this option properly when 
  ! the BVP is singular.
    SOL = BVP_SOLVER(SOL,FSUB,BCSUB,SINGULARTERM=S,DFDY=DF)
 
    CALL BVP_EVAL(SOL,0.0D0,YA)
    WRITE(*,FMT="(F5.0,F14.5,F17.5)") L(I),YA(1),AR(I)
  END DO

  CALL BVP_TERMINATE(SOL)

END PROGRAM ESI
