MODULE DEFINE_FCN

! Declare global problem dependent parameter.
  DOUBLE PRECISION :: EPS

! Define global variables for the number of differential 
! equations, NODE, and the number of boundary conditions
! at the left end of the interval, LEFTBC.  The number of
! boundary conditions at the right end is NODE-LEFTBC.
  INTEGER, PARAMETER :: NODE=6,LEFTBC=3,RIGHTBC=NODE-LEFTBC
  

CONTAINS

! Right hand side of ODE system.
  SUBROUTINE FSUB(T,Y,F)
    DOUBLE PRECISION :: T,Y(NODE),F(NODE)

	F(1) = Y(2)
	F(2) = (Y(1)*Y(4) - Y(3)*Y(2))/EPS
	F(3) = Y(4)
	F(4) = Y(5)
	F(5) = Y(6)
	F(6) = (-Y(3)*Y(6) - Y(1)*Y(2))/EPS

  END SUBROUTINE FSUB

! Jacobian of ODE system.
  SUBROUTINE DF(T,Y,PD)
    DOUBLE PRECISION :: T,Y(NODE),PD(NODE,NODE)

	PD = 0D0
	PD(1,2) = 1D0
	PD(2,1) =  Y(4)/EPS
	PD(2,2) = -Y(3)/EPS
	PD(2,3) = -Y(2)/EPS
	PD(2,4) =  Y(1)/EPS
	PD(3,4) = 1D0
	PD(4,5) = 1D0
	PD(5,6) = 1D0
	PD(6,1) = -Y(2)/EPS
	PD(6,2) = -Y(1)/EPS
	PD(6,3) = -Y(6)/EPS
	PD(6,6) = -Y(3)/EPS

  END SUBROUTINE DF

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

! Jacobian of Boundary conditions.
  SUBROUTINE DBC(YA,YB,DYA,DYB)
	DOUBLE PRECISION :: YA(NODE),YB(NODE), &
                        DYA(LEFTBC,NODE),DYB(RIGHTBC,NODE)

	DYA = 0D0
    DYA(1,1) = 1D0
    DYA(2,3) = 1D0
    DYA(3,4) = 1D0

    DYB = 0D0
    DYB(1,1) = 1D0
    DYB(2,3) = 1D0
    DYB(3,4) = 1D0

  END SUBROUTINE DBC

  SUBROUTINE GUESS_Y(X,Y)
    DOUBLE PRECISION :: X,Y(NODE)

    Y = 0D0
    Y(1) = 2D0*X - 1D0
    Y(2) = 2D0
    
  END SUBROUTINE GUESS_Y

END MODULE DEFINE_FCN

!************************************************************

PROGRAM SWIRL_III
  
! "Swirling Flow III", a test problem in Ascher, Mattheij, 
! and Russell, Numerical Solution of Boundary Value Problems 
! for Ordinary Differential Equations", Classics in Applied 
! Mathematics Series, SIAM, Philadelphia, 1995].

! Import problem dependent functions and parameters.
  USE DEFINE_FCN

! Import BVP_SOLVER functions, variables, and types. 
  USE BVP_M

! Declare a structure for the numerical solution and associated 
! information.
  TYPE(BVP_SOL) :: SOL

! Define the interval.
  DOUBLE PRECISION :: A=0D0,B=1D0
  
! Declare variables for plotting the solution.
  INTEGER :: I
  DOUBLE PRECISION :: XPLOT,YPLOT(NODE)


! NODE and LEFTBC are defined as global variables in DEFINE_FCN.
! Use a default mesh of 10 equally spaced points and a guess
! specified by the function GUESS_Y.
  SOL = BVP_INIT(NODE,LEFTBC,(/ A,B /),GUESS_Y)

  PRINT *,'       Swirling Flow III '
  PRINT *,' '
  PRINT *,'Choices corresponding to Enright/Muir paper,'
  PRINT *,'Table 5.6: METHOD = 6, TOL = 1D-5, EPS = 0.01.'
  PRINT *,' '
  EPS = 0.01D0
  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,METHOD=6,TOL=1D-5,DFDY=DF,DBCDY=DBC)

! Evaluate solution for plotting by calling BVP_EVAL and writing
! values to an output file.
  OPEN(UNIT=7,FILE="swirl_III.dat")
  DO I = 1,100
    XPLOT = A + (I-1)*(B - A)/99D0
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(7D12.4)") XPLOT,YPLOT
  END DO

  PRINT *
  PRINT *,'Solution at right end point'
  WRITE(*,FMT="(6D12.4)") YPLOT
  PRINT *

  PRINT *,'The solution evaluated at 100 equally spaced points'
  PRINT *,'can be imported into Matlab with'
  PRINT *,' '
  PRINT *,'   >> [x,y] = swirl_III;'
  PRINT *,' '
  PRINT *,'For this the output file "swirl_III.dat" must be'
  PRINT *,'the same directory as swirl_III.m.'
  PRINT *,' '
  PRINT *,'After importing the data, the solution can be plotted'
  PRINT *,'as usual in Matlab. For instance, the third solution'
  PRINT *,'component is plotted by'
  PRINT *,' '
  PRINT *,'   >> plot(x,y(:,3))'
  PRINT *,' '

  CALL BVP_TERMINATE(SOL)

END PROGRAM SWIRL_III
