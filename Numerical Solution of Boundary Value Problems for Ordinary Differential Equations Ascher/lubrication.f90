MODULE DEFINE_FCN

! Declare global parameters.  
  DOUBLE PRECISION, PARAMETER :: EPS=0.1D0,&
                                 PI=3.141592653589793D0

! Define global variables for the number of differential
! equations, NODE, the number of unknown parameters, NPAR,
! and the number of boundary conditions at the left end of 
! the interval, LEFTBC.  The number of boundary conditions 
! at the right end is NODE+NPAR-LEFTBC.
  INTEGER, PARAMETER :: NODE=1,NPAR=1,&
                        LEFTBC=1,RIGHTBC=NODE+NPAR-LEFTBC  
  
CONTAINS

  SUBROUTINE FSUB(T,Y,P,F)
	DOUBLE PRECISION :: T,Y(NODE),P(NPAR),F(NODE)

	F(1) = ( SIN(T)**2 - P(1)*SIN(T)**4/Y(1) ) / EPS

  END SUBROUTINE FSUB

  SUBROUTINE DFSUB(T,Y,P,DFDY,DFDP)
    DOUBLE PRECISION :: T,Y(NODE),P(NPAR),&
                        DFDY(NODE,NODE),DFDP(NODE,NPAR)

    DFDY = 0D0
    DFDY(1,1) = ( P(1)*SIN(T)**4/Y(1)**2 ) / EPS
    DFDP = 0D0
    DFDP(1,1) = (- SIN(T)**4/Y(1) ) / EPS

  END SUBROUTINE DFSUB    


  SUBROUTINE BCSUB(YA,YB,P,BCA,BCB)
	DOUBLE PRECISION :: YA(NODE),YB(NODE),P(NPAR),&
	                    BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(1) - 1D0
	BCB(1) = YB(1) - 1D0
					
  END SUBROUTINE BCSUB

  SUBROUTINE  DGSUB(YA,YB,DYA,DYB,P,DAP,DBP) 
	DOUBLE PRECISION :: YA(NODE),YB(NODE),&
                    DYA(LEFTBC,NODE),DYB(RIGHTBC,NODE),&
                P(NPAR),DAP(LEFTBC,NPAR),DBP(RIGHTBC,NPAR)
                
    DYA = 0D0
    DYA(1,1) = 1D0
    DYB = 0D0
    DYB(1,1) = 1D0
    DAP = 0D0    
    DBP = 0D0
					
  END SUBROUTINE DGSUB

END MODULE DEFINE_FCN

!************************************************************

PROGRAM LUBRICATION

! This is Example 3.5.2 of Solving ODEs with Matlab, a nonlinear
! eigenvalue problem of lubrication theory studied in section
! 6.1 of H.B. Keller, Numerical Methods for Two-Point Boundary-
! Value Problems, Dover, New York, 1992.

! Import problem dependent functions and parameters.
  USE DEFINE_FCN

! Import BVP_SOLVER functions, variables, and types. 
  USE BVP_M

  IMPLICIT NONE

  TYPE(BVP_SOL) :: SOL ! Structure for numerical solution and associated 
                       ! information.

  INTEGER :: METH=4 ! Method to be employed by BVP_SOLVER.
  
  DOUBLE PRECISION :: ERREST ! Estimate of global error.
  
! Reference value computed with TOL=1D-13 and METHOD=6 is
  DOUBLE PRECISION :: LAMBDA = 1.018656761377710D0
  
  DOUBLE PRECISION :: A=-PI/2D0,B=PI/2D0 ! Define interval using global 
                                         ! parameter PI.

! Set the number of mesh points in the initial mesh and
! allocate storage for the guesses for mesh and solution
! and the analytical solution.
  INTEGER, PARAMETER :: NSUB=20
  DOUBLE PRECISION :: X(NSUB+1),Y_GUESS(NODE),P(NPAR)

! Working variables for output.
  INTEGER :: I
  DOUBLE PRECISION :: XPLOT,YPLOT(NODE)  
  
  PRINT *,'Lubrication problem with EPS = 0.1. It '
  PRINT *,'is solved with default values except for'
  PRINT *,'METHOD =',METH
  PRINT *,' '

  X = BVP_LINSPACE(A,B,NSUB+1)
  Y_GUESS = (/ 0.5D0 /)
  P = (/ 1D0 /)
  SOL = BVP_INIT(NODE,LEFTBC,X,Y_GUESS,P)

  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,DFDY=DFSUB,DBCDY=DGSUB,  &
                   METHOD=METH,YERROR=ERREST)
  
! The error in component y(i) is measured relative to 1 + |y(i)| and 
! similarly for the one unknown parameter.
  PRINT *,'Estimated error in the solution on mesh and the'
  PRINT *,'unknown parameter LAMBDA was ',ERREST
  PRINT *,'The true error in LAMBDA was ', &
      ABS( (SOL%PARAMETERS(1) - LAMBDA)/SOL%PARAMETERS(1) )
  PRINT *,' '

! Evaluate the solution and write to an output file.
  OPEN(UNIT=7,FILE="lubrication.dat")
  DO I = 1,100
    XPLOT = A + (I-1)*( (B - A)/99D0 )
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(2D12.4)") XPLOT,YPLOT(1)
  END DO

  PRINT *,'The solution evaluated at 100 equally spaced points'
  PRINT *,'can be imported into Matlab with'
  PRINT *,' '
  PRINT *,'   >> [x,y] = lubrication;'
  PRINT *,' '
  PRINT *,'For this the output file "lubrication.dat" must be'
  PRINT *,'in the same directory as lubrication.m.'
  PRINT *,' '
  PRINT *,'After importing the data, lubrication.m plots'
  PRINT *,'the solution.'
  PRINT *,' '
  
  CALL BVP_TERMINATE(SOL)

END PROGRAM LUBRICATION
