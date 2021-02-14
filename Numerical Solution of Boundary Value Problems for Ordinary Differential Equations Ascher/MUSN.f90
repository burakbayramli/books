MODULE DEFINE_FCN

! Define global variables for the number of differential 
! equations, NODE, and the number of boundary conditions
! at the left end of the interval, LEFTBC.  The number of
! boundary conditions at the right end is NODE-LEFTBC.
  INTEGER, PARAMETER :: NODE=5,LEFTBC=4,RIGHTBC=NODE-LEFTBC
  
CONTAINS

! Right hand side of ODE system.
  SUBROUTINE FSUB(T,Y,F)
    ! u = Y(1), v = Y(2), w = Y(3), z = Y(4), y = Y(5)
    DOUBLE PRECISION :: T,Y(NODE),F(NODE),Y3MY1,Y3MY5
    
      Y3MY1 = Y(3) - Y(1)
      Y3MY5 = Y(3) - Y(5)  
	F(1) = 0.5D0*Y(1)*Y3MY1/Y(2)
	F(2) = -0.5D0*Y3MY1
	F(3) = (0.9D0 - 1000D0*Y3MY5 - 0.5D0*Y(3)*Y3MY1)/Y(4)
	F(4) = 0.5D0*Y3MY1
	F(5) = 100D0*Y3MY5

  END SUBROUTINE FSUB

! Boundary conditions.
  SUBROUTINE BCSUB(YA,YB,BCA,BCB)
    ! u = Y(1), v = Y(2), w = Y(3), z = Y(4), y = Y(5)  
	DOUBLE PRECISION :: YA(NODE),YB(NODE),&
	                    BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(1) - 1D0
	BCA(2) = YA(2) - 1D0
	BCA(3) = YA(3) - 1D0
      BCA(4) = YA(4) + 10D0

	BCB(1) = YB(3) - YB(5)

  END SUBROUTINE BCSUB

! Jacobian of Boundary conditions.
  SUBROUTINE DBC(YA,YB,DYA,DYB)
	DOUBLE PRECISION :: YA(NODE),YB(NODE), &
                        DYA(LEFTBC,NODE),DYB(RIGHTBC,NODE)

    DYA = 0D0
    DYA(1,1) = 1D0
    DYA(2,2) = 1D0
    DYA(3,3) = 1D0
    DYA(4,4) = 1D0

    DYB = 0D0
    DYB(1,3) =  1D0
    DYB(1,5) = -1D0

  END SUBROUTINE DBC

  SUBROUTINE GUESS_Y(X,Y)
    ! u = Y(1), v = Y(2), w = Y(3), z = Y(4), y = Y(5)    
    DOUBLE PRECISION :: X,Y(NODE)

    Y = 0D0
    Y(1) = 1D0
    Y(2) = 1D0
    Y(3) = 1D0 + 8.91D0*X -4.5D0*X**2
    Y(4) = -10D0
    Y(5) = 0.91D0 + 9D0*X - 4.5D0*X**2
    
  END SUBROUTINE GUESS_Y

END MODULE DEFINE_FCN

!************************************************************

PROGRAM MUSN
  
! This is the example used to illustrate the MUSN solver in
! in Ascher, Mattheij, and Russell, Numerical Solution of 
! Boundary Value Problems for Ordinary Differential Equations", 
! Classics in Applied Mathematics Series, SIAM, Philadelphia, 
! 1995, p. 523-525. The variables correspond here to
! u = Y(1), v = Y(2), w = Y(3), z = Y(4), y = Y(5)
! We follow the text in supplying partial derivatives of the
! boundary conditions, but not the partial derivative of the 
! ODEs.  We also use their guess function for the solution.

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

! Use a default mesh of 10 equally spaced points in [A,B].
  SOL = BVP_INIT(NODE,LEFTBC,(/A,B/),GUESS_Y)

  PRINT *,'             MUSN example '
  PRINT *,' '
  PRINT *,'Solved with default method and tolerance.'
  PRINT *,' '
 
  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,DBCDY=DBC)


! Evaluate solution for plotting by calling BVP_EVAL and writing
! values to an output file.
  OPEN(UNIT=7,FILE="MUSN.dat")
  DO I = 1,100
    XPLOT = A + (I-1)*(B - A)/99D0
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(6D12.4)") XPLOT,YPLOT
  END DO

  PRINT *
  PRINT *,'Solution at right end where Y(3) = Y(5).'
  WRITE(*,FMT="(5D12.4)") YPLOT
  PRINT *

  PRINT *,'The solution evaluated at 100 equally spaced points'
  PRINT *,'can be imported into Matlab with'
  PRINT *,' '
  PRINT *,'   >> [x,y] = MUSN;'
  PRINT *,' '
  PRINT *,'For this the output file "MUSN.dat" must be'
  PRINT *,'the same directory as MUSN.m.'
  PRINT *,' '
  PRINT *,'After importing the data, the solution can be plotted'
  PRINT *,'as usual in Matlab. For instance, the third solution'
  PRINT *,'component w is plotted by'
  PRINT *,' '
  PRINT *,'   >> plot(x,y(:,3))'
  PRINT *,' '

  CALL BVP_TERMINATE(SOL)

END PROGRAM MUSN
