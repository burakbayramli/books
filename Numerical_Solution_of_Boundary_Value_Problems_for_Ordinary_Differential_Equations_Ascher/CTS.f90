PROGRAM CTS
! This program solves the problems of a Test Set formulated by J. Cash. It can
! be downloaded from http://www.ma.ic.ac.uk/~jcash/BVP_software/readme.php
! There are 32 problems.  Problems 1-30 are all scalar second order ODEs that
! involve a parameter EPS. They are solved as a pair of first order equations.
! Problems 31-32 are both systems of four first order ODEs.  The ACDC code has 
! automatic continuation.  It asks for starting and ending values of the known 
! parameter, so Cash specifies values of EPS for each problem.  Here we use 
! BVP_SOLVER to solve these problems by continuation in EPS. Continuation was 
! actually useful only for problems #24,30,32, so for the other problems we solved 
! only for the final value of EPS.  We use default settings and in particular, use 
! finite difference partial derivatives.  We use a nominal error tolerance of 1D-3.  
! It proved convenient to define the test set in two files.  Specifically, problems 
! 1-30 are defined in CTS130 and problems 31-32 in CTS3132. This program can USE 
! only one of these files at a time. At run time CTS asks which problem is to be 
! solved.  After solving the problem, the solution is written to a data file. An 
! M-file is provided that will import this data into Matlab and plot the solution.

! Import problem dependent functions and parameters. Comment out one of the
! following lines.
!  USE CTS130
  USE CTS3132

! Import BVP_SOLVER functions, variables, and types. 
  USE BVP_M

! Declare a structure for the numerical solution and associated 
! information.
  TYPE(BVP_SOL) :: SOL

! Working variables
  INTEGER :: MXNSUB,I,OK_NPTS
  DOUBLE PRECISION :: A,B,EPS0,EPSF,OK_EPS,FACTOR,GUESS(NODE)
! The arrays XGUESS,YGUESS are given their maximum size to
! simplify the program. Storage could be managed more efficiently
! by using ALLOCATABLE arrays.
  DOUBLE PRECISION :: XGUESS(3000),YGUESS(NODE,3000)
  DOUBLE PRECISION :: XPLOT,YPLOT(4)
 
! Obtain TP, the number of the test problem.  It is a globally
! defined parameter. 
  PRINT *,'Which test problem (1-32)?'
  READ *,TP
  CALL PROBLEM_DEFN(A,B,EPS0,EPSF)

! We start with a value of the global parameter EPS for which the
! BVP is easy to solve. Try a default mesh of 10 equally spaced 
! points and a constant guess of 1 for all solution components.
! Use a nominal accuracy tolerance of 1D-3.
  EPS = EPS0
  GUESS = 1D0
  SOL = BVP_INIT(NODE,LEFTBC,(/A,B/),GUESS)
  SOL = BVP_SOLVER(SOL,FSUB,BCSUB,TOL=1D-3) 
   
  OK_EPS = EPS

! Extract needed info from SOL fields. 
  OK_NPTS = SOL%NPTS
  XGUESS(1:OK_NPTS) = SOL%X
  YGUESS(:,1:OK_NPTS) = SOL%Y

! If continuation is going to be necessary, deallocate array fields of SOL.
  IF (EPS > EPSF) THEN
    CALL BVP_TERMINATE(SOL)
  END IF
  
  FACTOR = 0.1D0
  DO WHILE (EPS > EPSF)
       
    DO
      
      EPS = FACTOR*OK_EPS  
      IF (EPS <= 1.1D0*EPSF) EPS = EPSF 
      MXNSUB = MIN(2*OK_NPTS-2,3000)
      IF (EPS <= EPSF) MXNSUB = 3000
      SOL = BVP_INIT(NODE,LEFTBC,XGUESS(1:OK_NPTS),YGUESS(:,1:OK_NPTS),&
                     MAX_NUM_SUBINTERVALS=MXNSUB)
      SOL = BVP_SOLVER(SOL,FSUB,BCSUB,STOP_ON_FAIL=.FALSE.,TOL=1D-3)  
      IF (SOL%INFO == -1) THEN
        ! Computation failed.  Restore the solution for OK_EPS and try
        ! again with a smaller change in EPS. The effect of the second
        ! argument in MIN is to try an EPS that is an average of the EPS 
        ! that failed and OK_EPS, which succeeded.
        FACTOR = MIN(1.2D0*FACTOR,0.5D0*(1D0 + FACTOR))   
        IF (MXNSUB == 3000) THEN
          PRINT *,'Continuation failed.'
          STOP
        END IF    
      ELSE
        ! Computation succeeded. If not done, form a new guess to 
        ! be used with a smaller value of EPS.
        IF (EPS <= EPSF) EXIT
        OK_EPS = EPS

        ! Extract needed info from SOL fields and deallocate array 
        ! fields of SOL.
        OK_NPTS = SOL%NPTS
        XGUESS(1:OK_NPTS) = SOL%X
        YGUESS(:,1:OK_NPTS) = SOL%Y
        CALL BVP_TERMINATE(SOL)
      END IF
      
    END DO
      
  END DO

  ! Write the solution to an output file. For export to Matlab
  ! write TP, the number of subintervals, and EPS first.
  OPEN(UNIT=7,FILE="CTS.dat")  
  WRITE(UNIT=7,FMT="(2D25.15)") REAL(TP),REAL(SOL%NPTS+1)
  WRITE(UNIT=7,FMT="(2D25.15)") EPS,EPS
  DO I = 0,200
    XPLOT = A + (I/200D0)*(B - A)
    CALL BVP_EVAL(SOL,XPLOT,YPLOT(1:NODE))

    ! The component plotted depends on the problem.
    IF (TP == 31) THEN
      WRITE(UNIT=7,FMT="(2D25.15)") XPLOT,YPLOT(3)
    ELSEIF (TP == 7 .OR. TP == 32) THEN
      WRITE(UNIT=7,FMT="(2D25.15)") XPLOT,YPLOT(2)
    ELSE
      WRITE(UNIT=7,FMT="(2D25.15)") XPLOT,YPLOT(1)
    END IF
  END DO
  
  PRINT *,' '
  PRINT *,'The solution evaluated at 201 equally spaced points'
  PRINT *,'in the interval can be imported into Matlab with'
  PRINT *,' '
  PRINT *,'   >> [x,y] = CTS;'
  PRINT *,' '
  PRINT *,'For this the output file "CTS.dat" must be'
  PRINT *,'in the same directory as the CTS.m function.'
  PRINT *,' '
  PRINT *,'After importing the data, CTS plots the solution.'
  PRINT *,' '
  PRINT *,' '

  CALL BVP_TERMINATE(SOL)
 
END PROGRAM CTS
