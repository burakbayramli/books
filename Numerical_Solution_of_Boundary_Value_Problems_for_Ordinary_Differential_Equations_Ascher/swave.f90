
MODULE DEFINE_FCN

! Declare global problem dependent parameters.  
  DOUBLE PRECISION, PARAMETER :: EPS=0.01D0, GAMMA=1.4D0

! Define global variables for the number of differential
! equations, NEQNS, and the number of boundary conditions
! at the left end of the interval, LEFTBC.  The number of
! boundary conditions at the right end is NODE-LEFTBC.
  INTEGER, PARAMETER :: NEQNS=2,LEFTBC=1,RIGHTBC=NEQNS-LEFTBC  


CONTAINS

  SUBROUTINE FSUB(T,Y,F)
	DOUBLE PRECISION :: T,Y(NEQNS),F(NEQNS)
	DOUBLE PRECISION :: TERM1,TERM2

    TERM1 = 1D0/(EPS*(1D0 + T**2))
	TERM2 = 0.5D0 + 0.5D0*GAMMA - 2D0*EPS*T

	F(1) = Y(2)
	F(2) = (TERM1/Y(1))*( TERM2*Y(1)*Y(2) - Y(2)/Y(1) &
          - (2D0*T/(1D0+T**2))*(1D0-0.5D0*(GAMMA-1D0)*Y(1)**2) )

  END SUBROUTINE FSUB

  SUBROUTINE DF(T,Y,PD)
	DOUBLE PRECISION :: T,Y(NEQNS),PD(NEQNS,NEQNS)
	DOUBLE PRECISION :: TERM1,TERM2

    TERM1 = 1D0/(EPS*(1D0 + T**2))
	TERM2 = 0.5D0 + 0.5D0*GAMMA - 2D0*EPS*T

    PD(1,1) = 0D0
    PD(1,2) = 1D0

    PD(2,1) = TERM1*( 2D0*Y(2)/(Y(1)**3) + 2D0*T/ &
	        ((1D0+T**2)*Y(1)**2) + (T/(1D0+T**2))*(GAMMA-1D0) )

    PD(2,2) = (TERM1/Y(1))*( TERM2*Y(1) - 1D0/Y(1) )

  END SUBROUTINE DF

  SUBROUTINE BCSUB(YA,YB,BCA,BCB)
	DOUBLE PRECISION :: YA(NEQNS),YB(NEQNS),BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(1) - 0.9129D0
	BCB(1) = YB(1) - 0.375D0

  END SUBROUTINE BCSUB

  SUBROUTINE DBC(YA,YB,DYA,DYB)
	DOUBLE PRECISION :: YA(NEQNS),YB(NEQNS),&
                        DYA(LEFTBC,NEQNS),DYB(RIGHTBC,NEQNS)
	DYA = 0D0
    DYA(1,1) = 1D0
    
    DYB = 0D0
    DYB(1,1) = 1D0

  END SUBROUTINE DBC

END MODULE DEFINE_FCN

!************************************************************

PROGRAM SWAVE
! "Shock Wave", a test problem in Ascher, Mattheij, and
! Russell, Numerical Solution of Boundary Value Problems 
! for Ordinary Differential Equations", Classics in Applied 
! Mathematics Series, SIAM, Philadelphia, 1995.

! Import problem dependent functions and parameters.
  USE DEFINE_FCN

! Import BVP_SOLVER functions, variables, and types. 
  USE BVP_M

! Declare a structure for the numerical solution and associated 
! information.
  TYPE(BVP_SOL) :: SOL

! Define the interval.
  DOUBLE PRECISION :: A=0D0,B=1D0

! Set the number of mesh points in the initial mesh and
! allocate storage for the guesses for mesh and solution
! and the analytical solution.
  INTEGER, PARAMETER :: NSUB=9
  DOUBLE PRECISION :: X(NSUB+1),Y_GUESS(NEQNS,NSUB+1)

! Some local variables
  DOUBLE PRECISION :: SLOPE,XPLOT,YPLOT(NEQNS)

  PRINT *,'Shock Wave with EPS = 0.01 and GAMMA = 1.4,'
  PRINT *,'solved using default method and tolerance.' 
  PRINT *,' '

  ! The initial mesh is NSUB+1 points equally spaced over interval.
  ! Call the BVP_SOLVER function LINSPACE to define the mesh. The
  ! initial guess for the solution is based on straight lines joining 
  ! the	BCs for each differential equation in the first order system. 
  ! The BCs for component 1 define a line. There are no BCs for the
  ! second component and y2 = y1', so we use the derivative of the
  ! guess for y1.
  X = BVP_LINSPACE(A,B,NSUB+1)
  SLOPE = 0.375D0 - 0.9129D0
  Y_GUESS(1,:) = 0.9129D0 + SLOPE*X 
  Y_GUESS(2,:) = SLOPE

  SOL = BVP_INIT(NEQNS,LEFTBC,X,Y_GUESS)

  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,DFDY=DF,DBCDY=DBC)

  ! Evaluate the solution and write to an output file.
  OPEN(UNIT=7,FILE="swave.dat")
  DO I = 1,100
    XPLOT = A + (I-1)*(B - A)/99D0
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(3D12.4)") XPLOT,YPLOT
  END DO

  PRINT *,'The solution evaluated at 100 equally spaced points'
  PRINT *,'can be imported into Matlab with'
  PRINT *,' '
  PRINT *,"   >> temp = load('swave.dat'); "
  PRINT *,"   >> x = temp(:,1);"
  PRINT *,"   >> y = temp(:,2:end); "
  PRINT *,' '
  PRINT *,'After importing the data, the solution can be plotted'
  PRINT *,'as usual in Matlab. For instance, the first solution'
  PRINT *,'component is plotted by'
  PRINT *,' '
  PRINT *,'   >> plot(x,y(:,1))'
  PRINT *,' '
  PRINT *,'Alternatively, you can do exactly this with SWAVE.m:'
  PRINT *,' '
  PRINT *,'   >> [x,y] = swave; '
  PRINT *,' '

  CALL BVP_TERMINATE(SOL)


END PROGRAM SWAVE
