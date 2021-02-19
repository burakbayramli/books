MODULE DEFINE_FCN

! Declare global parameters.  
  DOUBLE PRECISION, PARAMETER :: Q=5D0,&
                                 PI=3.141592653589793D0

! Define global variables for the number of differential
! equations, NODE, the number of unknown parameters, NPAR,
! and the number of boundary conditions at the left end of 
! the interval, LEFTBC.  The number of boundary conditions 
! at the right end is NODE+NPAR-LEFTBC.
  INTEGER, PARAMETER :: NODE=2,NPAR=1,&
                        LEFTBC=2,RIGHTBC=NODE+NPAR-LEFTBC  
  
CONTAINS

  SUBROUTINE FSUB(X,Y,P,DYDX)
	DOUBLE PRECISION :: X,Y(NODE),P(NPAR),LAMBDA,DYDX(NODE)
    
    LAMBDA = P(1)
	DYDX(1) = Y(2)
    DYDX(2) = -(LAMBDA - 2D0*Q*COS(2D0*X))*Y(1)

  END SUBROUTINE FSUB

  SUBROUTINE DFSUB(X,Y,P,DFDY,DFDP)
    DOUBLE PRECISION :: X,Y(NODE),P(NPAR),LAMBDA,&
                        DFDY(NODE,NODE),DFDP(NODE,NPAR)

    LAMBDA = P(1)
    DFDY = 0D0
    DFDY(1,2) = 1D0
    DFDY(2,1) = -(LAMBDA - 2D0*Q*COS(2D0*X))
    DFDP = 0D0
    DFDP(2,1) = - Y(1)

  END SUBROUTINE DFSUB    

  SUBROUTINE BCSUB(YA,YB,P,BCA,BCB)
	DOUBLE PRECISION :: YA(NODE),YB(NODE),P(NPAR),&
	                    BCA(LEFTBC),BCB(RIGHTBC)

	BCA(1) = YA(2)
    BCA(2) = YA(1) - 1D0
    
	BCB(1) = YB(2)
					
  END SUBROUTINE BCSUB

  SUBROUTINE  DGSUB(YA,YB,DYA,DYB,P,DAP,DBP) 
	DOUBLE PRECISION :: YA(NODE),YB(NODE),&
                    DYA(LEFTBC,NODE),DYB(RIGHTBC,NODE),&
                P(NPAR),DAP(LEFTBC,NPAR),DBP(RIGHTBC,NPAR)
                
    DYA = 0D0
    DYA(1,2) = 1D0
    DYA(2,1) = 1D0
    DYB = 0D0
    DYB(1,2) = 1D0
    DAP = 0D0    
    DBP = 0D0
					
  END SUBROUTINE DGSUB
  
  SUBROUTINE GUESS_Y(X,Y)
    DOUBLE PRECISION :: X,Y(NODE)

    Y(1) = COS(4D0*X)
    Y(2) = -4D0*SIN(4D0*X)
    
  END SUBROUTINE GUESS_Y

END MODULE DEFINE_FCN

!************************************************************
PROGRAM MATHIEU

! This is the MAT4BVP example of Matlab.  It finds the fourth
! eigenvalue of Mathieu's equation
!
!      y'' + (lambda - 2*q*cos(2*x))*y = 0
!   
! on the interval [0, pi] with boundary conditions y'(0) = 0, 
! y'(pi) = 0 when the parameter q = 5.
!
! Special codes for Sturm-Liouville problems can compute specific
! eigenvalues. The general-purpose code BVP_SOLVER can only compute 
! an eigenvalue near to a guessed value. We can make it much more 
! likely that we compute the eigenfunction corresponding to the fourth 
! eigenvalue by supplying a guess that has the correct qualitative 
! behavior. The eigenfunction y(x) is determined only to a constant 
! multiple, so the normalizing condition y(0) = 1 is used to specify 
! a particular solution.

  USE DEFINE_FCN
  USE BVP_M
  
  TYPE(BVP_SOL) :: SOL

  ! Working variables for output.
  INTEGER :: I
  DOUBLE PRECISION :: XPLOT,YPLOT(NODE),LAMBDA(NPAR)  

  DOUBLE PRECISION :: A=0D0,B=PI
  
! BVP_INIT is used to form an initial guess for a mesh of 10 equally spaced
! points. The guess cos(4x) for y(x) and its derivative as guess for y'(x) 
! are evaluated in GUESS_Y. The desired eigenvalue is the one nearest the 
! guess lambda = 15. A guess for unknown parameters is supplied in an array
! as the last argument of BVP_INIT. 
  
  SOL = BVP_INIT(NODE,LEFTBC,(/ A,B /),GUESS_Y,(/ 15D0 /))

  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,DFDY=DFSUB,DBCDY=DGSUB)
  
! Call BVP_EVAL to get the approximation for the parameter.
! Parameters are held in an array even when there is only 
! one unknown parameter, LAMBDA(1). The parameters are also
! available as SOL%PARAMETERS.
  CALL BVP_EVAL(SOL,LAMBDA)  
  PRINT *,'Computed the parameter lambda to be  ', LAMBDA(1)
  PRINT *,'The mat4bvp program of MATLAB says it is about 17.097.'
  PRINT *,' '

! Evaluate the solution and write it to an output file.
  OPEN(UNIT=7,FILE="mathieu.dat")
  WRITE(UNIT=7,FMT="(2D14.5)") LAMBDA(1),LAMBDA(1)
  DO I = 1,100
    XPLOT = A + (I-1)*(B - A)/99D0
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(2D12.4)") XPLOT,YPLOT(1)
  END DO

  PRINT *,'The solution evaluated at 100 equally spaced points'
  PRINT *,'can be imported into Matlab with'
  PRINT *,' '
  PRINT *,'   >> [x,y] = mathieu;'
  PRINT *,' '
  PRINT *,'For this the output file "mathieu.dat" must be'
  PRINT *,'in the same directory as mathieu.m.'
  PRINT *,' '
  PRINT *,'After importing the data, mathieu.m plots the solution.'
  PRINT *,' '

  CALL BVP_TERMINATE(SOL)

END PROGRAM MATHIEU
