
MODULE DEFINE_FCN

! Define global variables for the number of differential
! equations, NEQNS, and the number of boundary conditions
! at the left end of the interval, LEFTBC.  The number of
! boundary conditions at the right end is NODE-LEFTBC.
  INTEGER, PARAMETER :: NEQNS=2,LEFTBC=1,RIGHTBC=NEQNS-LEFTBC  


CONTAINS

  SUBROUTINE FSUB(X,Y,F)
	DOUBLE PRECISION :: X,Y(NEQNS),F(NEQNS)

    F = (/ Y(2), -Y(1)**5 /)
    
  END SUBROUTINE FSUB

  SUBROUTINE BCSUB(YA,YB,BCA,BCB)
	DOUBLE PRECISION :: YA(NEQNS),YB(NEQNS),BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(2)
	BCB(1) = YB(1) - SQRT(0.75D0)

  END SUBROUTINE BCSUB

  FUNCTION EXACT_SOL(X) RESULT(Y_EXACT)
    DOUBLE PRECISION :: X,Y_EXACT(NEQNS)

	Y_EXACT(1) = 1D0/SQRT(1D0 + X**2 /3D0)
	Y_EXACT(2) = -9D0/(9D0+3D0*X**2)**1.5D0 * X

  END FUNCTION EXACT_SOL  

END MODULE DEFINE_FCN

!************************************************************

PROGRAM EMDEN
! This is a test problem of R.D. Russell and L.F. Shampine,
! Numerical methods for singular boundary value problems,
! SIAM j. Numer. Anal. 12 (1975) 479-497.  It is Emden's
! equation for a spherical body of gas such as a star. 
! For the parameter value and boundary condition used here,
! there is an analytical solution.

  USE DEFINE_FCN
  USE BVP_M

  IMPLICIT NONE

  TYPE(BVP_SOL) :: SOL

  DOUBLE PRECISION :: S(NEQNS,NEQNS),Y_GUESS(NEQNS),Y_EXACT(NEQNS)
  
  DOUBLE PRECISION :: MAXERR,ERREST,WT(NEQNS)
  INTEGER :: I,METH=6

  ! This guess does NOT satisfy the necessary conditon at the origin.
  Y_GUESS = (/ SQRT(0.75D0), 1D-4 /)

  SOL = BVP_INIT(NEQNS,LEFTBC,(/ 0D0,1D0 /),Y_GUESS)

  ! Define the singular term.
  S = 0D0
  S(2,2) = -2D0
  
  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,SINGULARTERM=S,YERROR=ERREST,METHOD=METH,&
                   TRACE=2)

  PRINT *,"Emden's equation for a spherical body of gas"
  PRINT *,'has a coordinate singularity at the origin.'
  PRINT *,'It is solved with default values except for'
  PRINT *,'METHOD =',METH
  PRINT *,' '
  
  ! The error in component y(i) is measured relative to 1 + |y(i)|.
  MAXERR = 0D0
  DO I = 1,SOL%NPTS
    WT = 1D0 + ABS(SOL%Y(:,I))    
	Y_EXACT = EXACT_SOL(SOL%X(I))
	MAXERR = MAX( MAXERR, MAXVAL(ABS(SOL%Y(:,I) - Y_EXACT)/WT) )
  END DO

  PRINT *,'True weighted error on mesh was',MAXERR
  PRINT *,'Estimate of weighted error was ',ERREST
  
  CALL BVP_TERMINATE(SOL)

END PROGRAM EMDEN
