!Example 1.20 from AMR 1995
!Modified from BVP_SOLVER test suite

 MODULE DEFINE_FCN

! Declare global problem dependent parameter.
  DOUBLE PRECISION :: EPS

! Define global variables for the number of differential 
! equations, NODE, and the number of boundary conditions
! at the left end of the interval, LEFTBC.  The number of
! boundary conditions at the right end is NODE-LEFTBC.
  INTEGER, PARAMETER :: NODE=6,LEFTBC=3,RIGHTBC=NODE-LEFTBC
  

CONTAINS

!ODEs
SUBROUTINE FSUB(T,Y,F)
    DOUBLE PRECISION :: T,Y(NODE),F(NODE)

	F(1) = Y(2)
	F(2) = (Y(1)*Y(4) - Y(3)*Y(2))/EPS
	F(3) = Y(4)
	F(4) = Y(5)
	F(5) = Y(6)
	F(6) = (-Y(3)*Y(6) - Y(1)*Y(2))/EPS

END SUBROUTINE FSUB

! Boundary conditions.
SUBROUTINE BCSUB(YA,YB,BCA,BCB)
    DOUBLE PRECISION :: YA(NODE),YB(NODE),&
                        BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(1) + 1D0
	BCA(2) = YA(3)
	BCA(3) = YA(4)

	BCB(1) = YB(1) - 1D0
	BCB(2) = YB(3)
	BCB(3) = YB(4)

END SUBROUTINE BCSUB


! Initial guess
SUBROUTINE GUESS_Y(X,Y)  
    DOUBLE PRECISION :: X,Y(NODE)

    Y = 0D0
    Y(1) = 2D0*X - 1D0
    Y(2) = 2D0
    
END SUBROUTINE GUESS_Y

END MODULE DEFINE_FCN



PROGRAM SWIRL_III
USE DEFINE_FCN
USE BVP_M

IMPLICIT NONE

TYPE(BVP_SOL) :: SOL

! Variables for BVP_SOLVER
INTEGER :: I ! counter for solution output
INTEGER :: METH ! Method to be employed by BVP_SOLVER.
INTEGER :: ERRORCON !error control method
DOUBLE PRECISION :: ERREST ! Estimate of global error.
DOUBLE PRECISION :: TTOL !tolerance
DOUBLE PRECISION :: A=0D0,B=1D0 ! Define interval using global
DOUBLE PRECISION :: YPLOT(NODE), XPLOT !Plot solution

!Set method
METH=6
!Set tolerance
TTOL=1E-6
!Set Bp
EPS = 0.01D0
!Set ERROR_CONTROL_METHOD
ERRORCON = 1 !for defect control

 
SOL = BVP_INIT(NODE,LEFTBC,(/A,B/),GUESS_Y,MAX_NUM_SUBINTERVALS=1000000)
SOL = BVP_SOLVER(SOL,FSUB,BCSUB,METHOD=METH, &
	ERROR_CONTROL=ERRORCON,TOL=TTOL,HOERROR=ERREST)
			
PRINT *, "Estimated global error: ", ERREST

!Plot solution
OPEN(UNIT=7,FILE="amr120.dat")
  DO I = 1,100
    XPLOT = A + (I-1)*(B - A)/99D0
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(6D12.4)") XPLOT,YPLOT(1),YPLOT(3)
END DO
  
CLOSE(7)

CALL BVP_TERMINATE(SOL)

  
  
  


END PROGRAM SWIRL_III
