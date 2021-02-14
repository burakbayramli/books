MODULE DEFINE_FCN

! Import data type from BVP_M. 
  USE BVP_M
  
! Declare global parameters.  
  DOUBLE PRECISION :: R,D,S,G,DEL,YRIGHT

! Define global variables for the number of differential
! equations, NODE, the number of unknown parameters, NPAR,
! and the number of boundary conditions at the left end of 
! the interval, LEFTBC.  The number of boundary conditions 
! at the right end is NODE+NPAR-LEFTBC.
  INTEGER, PARAMETER :: NODE=2,NPAR=1,&
                        LEFTBC=1,RIGHTBC=NODE+NPAR-LEFTBC  

CONTAINS

  SUBROUTINE FSUB(X,Y,P,F)
    DOUBLE PRECISION :: X,Y(NODE),P(NPAR),F(NODE)
    DOUBLE PRECISION :: TEMP1,TEMP2

    TEMP1 = D**2/(2D0*S**2) * X**2 * (1D0 - X)**2
    TEMP2 = R*Y(1) - D*X/R + G*R*TEMP1*Y(2)**2 &
            + G*D*X*(1D0 - X)*Y(2)
    F = (/ Y(2), TEMP2/TEMP1 /)

  END SUBROUTINE FSUB

  SUBROUTINE DFSUB(X,Y,P,DFDY,DFDP)
    DOUBLE PRECISION :: X,Y(NODE),P(NPAR),&
                        DFDY(NODE,NODE),DFDP(NODE,NPAR)
    DOUBLE PRECISION :: TEMP1
        
    DFDY = 0D0
    DFDY(1,2) = 1D0
    TEMP1 = D**2/(2D0*S**2) * X**2 * (1D0 - X)**2
    DFDY(2,1) = R/TEMP1
    DFDY(2,2) = (G*R*TEMP1*2D0*Y(2) + G*D*X*(1D0 - X))/TEMP1

    DFDP = 0D0

  END SUBROUTINE DFSUB    

  SUBROUTINE BCSUB(YA,YB,P,BCA,BCB)
  ! Match analytical approximations to y(del),
  ! y(1-del), and y'(1-del).
    DOUBLE PRECISION :: YA(NODE),YB(NODE),P(NPAR),&
	                    BCA(LEFTBC),BCB(RIGHTBC)
    DOUBLE PRECISION :: ANALYTICAL(NODE)

    ANALYTICAL = LEFTSOL(DEL)
    BCA(1) = YA(1) - ANALYTICAL(1)
    BCB = YB - RIGHTSOL(1D0-DEL,P(1))
					
  END SUBROUTINE BCSUB


  SUBROUTINE GUESS_Y(X,Y)
  ! Use RIGHTSOL with guess for unknown parameter
  ! that makes it satisfy both boundary conditions.
    DOUBLE PRECISION :: X,Y(NODE)

    Y = RIGHTSOL(X,(/ -YRIGHT /))
    
  END SUBROUTINE GUESS_Y

  FUNCTION LEFTSOL(X) RESULT(Y)
  ! Approximate (y(x),y'(x)) with
  ! y(x)  = b*x + ..., y'(x) = b + ...
    DOUBLE PRECISION :: X,Y(NODE),B
    B = D/(R*(R+G*D))
    Y = (/ B*X, B /)

  END FUNCTION LEFTSOL

  FUNCTION RIGHTSOL(X,P) RESULT(Y)
  ! Approximate [y(x);y'(x)] with
  ! y(x)  = d/r^2 + p*(1-x)^c + ..., 
  ! y'(x) = -c*p(1-x)^(c-1) + ...
    DOUBLE PRECISION :: X,P(NPAR),Y(NODE),ALPHA,BETA,C

    ALPHA = D**2/(2D0*S**2)
    BETA = G*D - ALPHA
    ! Get C by solving ALPHA*C**2 + BETA*C - R = 0
    C = (-BETA + SQRT(BETA**2 + 4D0*ALPHA*R))/(2D0*ALPHA)
    Y = (/ YRIGHT + P(1)*(1D0 - X)**C, -C*P(1)*(1D0 - X)**(C-1D0) /)
 
  END FUNCTION RIGHTSOL

  FUNCTION EVAL_Y(SOL,X) RESULT(YX)
  ! Evaluate numerical and analytical approximations
  ! to y(x), but not y'(x).

    TYPE(BVP_SOL) :: SOL
    DOUBLE PRECISION :: X,YX,YTEMP(NODE),P(NPAR)

    IF (X < DEL) THEN
      YTEMP = LEFTSOL(X)
    ELSEIF (X < 1D0-DEL) THEN
      CALL BVP_EVAL(SOL,X,YTEMP)
    ELSEIF (X < 1D0) THEN
    ! Get current approximation to the unknown parameters.
      CALL BVP_EVAL(SOL,P)
      YTEMP = RIGHTSOL(X,P)
    END IF
    
    IF (X < 1D0) THEN
      YX = YTEMP(1)
    ELSE
      YX = YRIGHT
    END IF

  END FUNCTION EVAL_Y
END MODULE DEFINE_FCN
!*********************************************************************

PROGRAM ECON

! Neng Wang, a professor of finance and economics at Columbia 
! Business School, formulated a difficult BVP when studying an 
! optimal consumption problem with learning.  Here we solve the 
! problem for a set of parameter values supplied by Professor Wang.
! The problem is difficult because the second order ODE is singular 
! at both ends of the interval [0,1]. Solving this problem numerically 
! involves an unknown parameter and continuation in the interval.
!
! Notation of the problem: p  f  r  delta sigma gamma 
! Notation used here:      X  Y  R    D     S     G

! Import problem dependent functions and parameters.
  USE DEFINE_FCN

! Import BVP_SOLVER functions, variables, and types. 
  USE BVP_M
  
! Declare a structure for the numerical solution and associated 
! information.
  TYPE(BVP_SOL) :: SOL

! Working variables
  INTEGER :: I,J
  DOUBLE PRECISION :: ANEW,YANEW(NODE),BNEW,YBNEW(NODE)
  DOUBLE PRECISION :: YTEMP,YERR(0:100),ERRMAX,P(NPAR)
  DOUBLE PRECISION :: XPLOT,YPLOT(1)

! Assign values to global parameters.
  R = 0.04D0
  D = 0.2D0
  S = 0.2D0
  G = 5D0

! The boundary value y(1) is defined as a global variable.
  YRIGHT = D/R**2

! Use continuation in the interval [DEL,1-DEL] until the computed
! solutions are consistent. 
  DEL = 0.1D0 
  
  DO I = 1,10
    IF (I == 1) THEN
      ! This initial guess for the unknown parameter makes the
      ! approximation from RIGHTSOL satisfy both boundary 
      ! conditions. This approximation is coded in GUESS_Y.
      SOL = BVP_INIT(NODE,LEFTBC,BVP_LINSPACE(DEL,1D0-DEL,20),&
                     GUESS_Y,(/ -YRIGHT /))
    ELSE
      ! Use asymptotic approximations to get values at new
      ! end points, hence values that get better as DEL -> 0.
      ANEW = DEL
      YANEW = LEFTSOL(ANEW)
      BNEW = 1D0 - DEL
      ! Get current approximation to the unknown parameters.
      CALL BVP_EVAL(SOL,P)
      YBNEW = RIGHTSOL(BNEW,P)
      SOL = BVP_EXTEND(SOL,ANEW,BNEW,YANEW,YBNEW)
    END IF
    
    SOL = BVP_SOLVER(SOL,FSUB,BCSUB,DFDY=DFSUB,TOL=1D-3)
    
    IF (I == 1) THEN
      ! Initialize check for consistency.
      DO J = 0,100
        YERR(J) = EVAL_Y(SOL,J/100D0)
      END DO
    ELSE
      ! Check consistency by computing absolute change in y(x) 
      ! relative to its biggest value, y(1).
      ERRMAX = 0D0
      DO J = 0,100
        YTEMP = EVAL_Y(SOL,J/100D0)        
        ERRMAX = MAX(ERRMAX,ABS( (YERR(J) - YTEMP)/YRIGHT ))
        YERR(J) = YTEMP
      END DO
      WRITE(*,"(/,A,ES9.1,A)")' Solution with DEL = ',DEL,' differs from'
      WRITE(*,"(A,ES9.1,A)")' solution with previous DEL by no more than'&
                                                                   ,ERRMAX
      IF (ERRMAX < 1D-4) EXIT
    END IF

    DEL = DEL/2D0
  END DO

  ! Write the solution to an output file. Extend the 
  ! numerical solution to include values at 0 and 1.
  OPEN(UNIT=7,FILE="econ.dat")  
  WRITE(UNIT=7,FMT="(2D12.4)") R,D 
  WRITE(UNIT=7,FMT="(2D12.4)") S,G
  WRITE(UNIT=7,FMT="(2D12.4)") 0D0,0D0
  DO I = 1,99
    XPLOT = I/100D0
    CALL BVP_EVAL(SOL,XPLOT,YPLOT)
    WRITE(UNIT=7,FMT="(2D12.4)") XPLOT,YPLOT(1)
  END DO
  WRITE(UNIT=7,FMT="(2D12.4)") 1D0,YRIGHT
  
  PRINT *,' '
  PRINT *,'The solution evaluated at 0, 1 and 99 equally spaced points'
  PRINT *,'in between can be imported into Matlab with'
  PRINT *,' '
  PRINT *,'   >> [p,f] = econ;'
  PRINT *,' '
  PRINT *,'For this the output file "econ.dat" must be'
  PRINT *,'in the same directory as the econ.m function.'
  PRINT *,'After importing the data, econ.m plots the solution.'
 
  CALL BVP_TERMINATE(SOL)

END PROGRAM ECON
