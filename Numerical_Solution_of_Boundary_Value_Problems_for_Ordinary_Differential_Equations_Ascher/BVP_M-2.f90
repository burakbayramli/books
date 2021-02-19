! BVP_SOLVER, Release 2, with global error estimation and control options.
! Copyright (c) 2012, Jason Boisvert, Paul Muir, Ray Spiteri.
! Jason Boisvert, Ray Spiteri, Department of Computer Science, University of Saskatchewan.
! Paul Muir, Mathematics and Computing Science, Saint Mary's University.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!     * Neither Saint Mary's University nor Southern Methodist University nor 
!       the names of its contributors may be used to endorse or promote products
!       derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY Jason Boisvert, Paul Muir, and Ray Spiteri ''AS IS'' AND ANY
! EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL Paul Muir and Larry Shampine BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
! See documentation below for FUNCTION BVP_SOLVER for a description of the changes
! to the argument list for BVP_SOLVER. 

! Copyright (c) 2006, Paul Muir and Larry Shampine.
! Paul Muir, Mathematics and Computing Science, Saint Mary's University.
! Larry Shampine, Mathematics, Southern Methodist University.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!     * Neither Saint Mary's University nor Southern Methodist University nor 
!       the names of its contributors may be used to endorse or promote products
!       derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY Paul Muir and Larry Shampine ''AS IS'' AND ANY
! EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL Paul Muir and Larry Shampine BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

! L.F. Shampine, P.H. Muir, H. Xu, A user-friendly Fortran BVP solver, 
! J. Numer. Anal. Indust. Appl. Math., 1, 2006, 201--217.

     MODULE BVP_M  
    ! In the declarations part of this module, we first define the BVP_SOL type, 
    ! which represents the numerical solution. We next define the interface 
    ! BVP_INIT, which is overloaded to provide a number of functions for allowing 
    ! the user to specify initial information for the computation, the interface 
    ! BVP_EXTEND, which is overloaded to provide functions for extending the 
    ! numerical solution to a new domain that are useful in the context of parameter
    ! continuation, and the interface BVP_EVAL, which is overloaded to provide a
    ! function for retrieving solution information. Next a number of global variables 
    ! employed throughout the code are defined.
    !
    ! A number of the PUBLIC routines available to the user access the BVP_SOL 
    ! structured type and allocate or deallocate the array fields of this type. 
    ! In order to avoid "memory leaks", care must be taken to explicitly
    ! deallocate these array fields prior to reallocating them and at the end of
    ! the computation. This is particularly important when BVP_SOLVER is called 
    ! several times within the same program. The following table provides pre and 
    ! post conditions on array fields of SOL for public routines that access SOL. 
    ! For each routine the pre condition specifies which array fields of SOL must 
    ! be allocated prior to calling the routine, while the post condition specifies 
    ! which array fields of SOL will be allocated upon return from the routine.
    ! 
    ! BVP_SOLVER     Pre: X, Y,(optionally) PARAMETERS.
    !                Post/Successful Return: X, Y,(optionally) PARAMETERS,IWORK,WORK.
    !                Post/Unsuccessful Return: No fields of SOL are allocated.
    !
    ! BVP_EVAL       Pre: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !                Post: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !  
    ! BVP_INIT       Pre: No fields of SOL are allocated.
    !                Post: X, Y,(optionally) PARAMETERS.
    !
    ! BVP_EXTEND     Pre: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !                Post: X, Y,(optionally) PARAMETERS.
    !
    ! BVP_SAVE       Pre: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !                Post: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !
    ! BVP_GET        Pre: No fields of SOL are allocated.
    !                Post: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !
    ! BVP_TERMINATE  Pre: X, Y,(optionally) PARAMETERS, IWORK, WORK.
    !                Post: No fields of SOL are allocated.
    !-------------------------------------------------------------------------------

    IMPLICIT NONE
  
    ! Declare everything in the module to be PRIVATE unless specifically declared
    ! to be PUBLIC.
    PRIVATE
  
    ! PUBLIC subroutines/functions defined in this module.  
    PUBLIC :: BVP_SOLVER,BVP_EVAL,BVP_INIT,BVP_EXTEND,BVP_LINSPACE,            &
    BVP_SAVE,BVP_GET,BVP_TERMINATE

    !----------------------------------------------------------------------------
    ! Type definitions:
    ! Set up structured type that will store solution and associated information.
    TYPE, PUBLIC :: BVP_SOL

        INTEGER :: NODE,NPAR ! number of ODEs; number of unknown parameters.
        INTEGER :: LEFTBC ! number of left boundary conditions.
        INTEGER :: NPTS,INFO ! number of points in current mesh; indicator of
                             ! success (INFO=0) or failure (INFO=-1) of computation.
        INTEGER :: MXNSUB ! maximum number of subintervals allowed.

        DOUBLE PRECISION, DIMENSION(:), POINTER :: X ! current mesh.

        DOUBLE PRECISION, DIMENSION(:,:), POINTER :: Y ! current solution - Ith 
        !   column is solution approximation at Ith mesh point in first NODE locations; 
        !   last NPAR locations contain approximation to unknown parameters.

        DOUBLE PRECISION, DIMENSION(:), POINTER :: PARAMETERS ! Unknown parameters.

        INTEGER, DIMENSION(:), POINTER :: IWORK ! integer workspace.
        DOUBLE PRECISION, DIMENSION(:), POINTER :: WORK ! double precision workspace.
    !   The workspaces are used to save information that can be accessed by SOL_EVAL 
    !   for evaluation of the numerical solution.

    END TYPE BVP_SOL
  
    !-------------------------------------------------------------------------------

    
    ! Interface definitions:

    INTERFACE BVP_INIT
        ! The purpose of each of the functions available in this interface is to set 
        ! initial values for the fields of the solution structure SOL and optionally 
        ! to set the global variable, MXNSUB, the maximum number of subintervals, based 
        ! on the value of the MAX_NUM_SUBINTERVALS argument. This interface consists of
        ! three functions, GUESS_1, GUESS_2, and GUESS_3, which give the user with three 
        ! options for specifying the initial guess for the solution. Through each function 
        ! the user also provides values for NODES, the number of ODEs, LEFTBC, the number 
        ! of left boundary conditions, and X, the initial mesh. (The SIZE of X is used to 
        ! set the NPTS field, the number of mesh points.) Optionally an initial guess for 
        ! unknown parameters can be provided through P. The INFO field of SOL is set to 0. 
        ! If X contains only end points, a default mesh of 10 equally spaced points is set.

        MODULE PROCEDURE GUESS_1,GUESS_2,GUESS_3
    
    !   GUESS_?(NODE,LEFTBC,X,?,P,MAX_NUM_SUBINTERVALS) RESULT(SOL)

    !   GUESS_1 - the ? argument is a vector of NODE components which represents the 
    !            constant vector to be assigned to all columns of the Y field of SOL,
    !   GUESS_2 - the ? argument is an array of size NODE by NPTS representing, in 
    !            each column, an initial solution guess at the corresponding mesh 
    !            point,
    !   GUESS_3 - the ? argument is the name of a subroutine that can be called to 
    !            give a solution guess vector of NODE components when it is called
    !            with a scalar value X. This function has the form FCN(X,Y). GUESS_3
    !            calls FCN to get solution guesses for each of the points in the
    !            array X and loads them in corresponding columns of the Y field.

    END INTERFACE ! BVP_INIT


    INTERFACE BVP_EXTEND
        ! The purpose of each of the functions available within this interface
        ! is to extend a solution structure on [A,B] to a larger interval [ANEW,BNEW].

        MODULE PROCEDURE EXTEND_1,EXTEND_2

    !   FUNCTION EXTEND_1(SOLIN,ANEW,BNEW,YANEW,YBNEW,P,MAX_NUM_SUBINTERVALS) &
    !                                                         RESULT(SOLOUT)   
    !   This function allows the solution in a structure of type BVP_SOL to be
    !   extended to a solution on a bigger interval. A typical call has the form

    !       SOLOUT = BVP_EXTEND(SOLIN,ANEW,BNEW,YANEW,YBNEW)

    !   Here SOLIN is the input solution structure and SOLOUT is the output 
    !   structure.  If the interval used to compute SOLIN was [A,B], the new 
    !   guess is for [ANEW,BNEW]. If ANEW < A, the mesh is extended to ANEW and 
    !   Y is extended to the value YANEW there. If ANEW >= A, the arguments ANEW
    !   and YANEW are ignored. The right end of the interval is treated in the 
    !   same way. If present, SOLIN%PARAMETERS is used in SOLOUT. To supply a 
    !   different guess P for unknown parameters, use the optional argument P.  
    !   To increase the global variable, MXNSUB, the maximum number of subintervals 
    !   allowed, use the optional argument MAX_NUM_SUBINTERVALS.


    !   FUNCTION EXTEND_2(SOLIN,ANEW,BNEW,ORDER,P,MAX_NUM_SUBINTERVALS) &
    !                                                          RESULT(SOLOUT)   
    !   This function allows the solution in a structure of type BVP_SOL to be
    !   extended to a solution on a bigger interval. A typical call has the form

    !       SOLOUT = BVP_EXTEND(SOLIN,ANEW,BNEW,ORDER)

    !   Here SOLIN is the input solution structure and SOLOUT is the output 
    !   structure.  If the interval used to compute SOLIN was [A,B], the new 
    !   guess is for [ANEW,BNEW]. If ANEW < A, the mesh is extended to ANEW and 
    !   Y is extended to a value YANEW there. If ANEW >= A, no action is taken
    !   at the left end.  The value YANEW is computed by extrapolation.  The 
    !   optional integer ORDER indicates the order of the polynomial used.  
    !   If ORDER = 1, linear extrapolation is done and otherwise, constant 
    !   extrapolation (ORDER = 0). The right end of the interval is treated in 
    !   the same way. If present, SOLIN%PARAMETERS is used in SOLOUT. To supply 
    !   a different guess P for unknown parameters, use the optional argument P.  
    !   To increase the global variable, MXNSUB, the maximum number of subintervals 
    !   allowed, use the optional argument MAX_NUM_SUBINTERVALS.

    END INTERFACE ! BVP_EXTEND


    INTERFACE BVP_EVAL
        ! Assuming a successful computation, this interface provides three routines
        ! for evaluation of solution information. EVAL_S evaluates the numerical
        ! solution at a single point. EVAL_V evaluates the numerical solution at a 
        ! set of points. EVAL_P returns values for unknown parameters, if there are any.
    
        MODULE PROCEDURE EVAL_S,EVAL_V,EVAL_P

    !   SUBROUTINE EVAL_S(SOL,T,Z,DERIVATIVE) 
    !   Evaluate solution (and optionaly first derivative) at a single point
    !   T. The solution is returned in Z; the derivative in DERIVATIVE if 
    !   this parameter is present. 

    !   SUBROUTINE EVAL_V(SOL,T,Z,DERIVATIVE)
    !   Evaluate solution (and optionally first derivative) at all the points
    !   of the vector T. Solutions are returned in the columns of the array Z; 
    !   derivatives are returned in the columns of the array DERIVATIVE if
    !   this parameter is present. 

    !   SUBROUTINE EVAL_P(SOL,PARAMETERS)
    !   Return parameter values in PARAMETERS. 

    END INTERFACE ! BVP_EVAL

    !-------------------------------------------------------------------------------
    ! Global Constants and Variables:

    DOUBLE PRECISION, PARAMETER :: UROUND=EPSILON(1D0) ! Unit roundoff.
  
    DOUBLE PRECISION, PARAMETER :: LAMBDA_MIN=0.01D0 ! Minimum value for damping 
                                                     ! factor for damped Newton 
                                                     ! iteration.
  
    DOUBLE PRECISION :: SQRTU ! Square root of unit roundoff.

    DOUBLE PRECISION, DIMENSION(:,:), POINTER :: MSING,FSING,BCSING
    LOGICAL :: SINGULAR  ! Indicate whether solving a singular BVP.


    ! Declaration of constants, variables and arrays associated with discrete
    ! and continuous Runge-Kutta formulas.

    INTEGER, PARAMETER :: MXS=10 ! Maximum number of stages of Runge-Kutta method.

    INTEGER :: C_S, C_S_STAR, C_P
    ! C_S, number of discrete stages; C_S_STAR, total number of stages required to 
    ! form the interpolant on each subinterval. It includes all the stages of the 
    ! discrete formula plus the additional stages required for the interpolant; 
    ! C_P is the order of the interpolant => local error is O(h**(p+1)), where h 
    ! is the subinterval size.

    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: C_C, C_V, C_B
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: C_X
    ! C_C, C_V, C_B, and C_X are coefficients that define the discrete Runge-Kutta 
    ! formula.  C_C, C_V, and C_B correspond, respectively, to the Runge-Kutta 
    ! coefficient vectors c, v, and b; C_X corresponds to the Runge-Kutta coefficient 
    ! matrix X.

    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: C_C_STAR, C_V_STAR
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: C_X_STAR
    ! C_C_STAR, C_V_STAR, and C_X_STAR are coefficients that define the continuous 
    ! Runge-Kutta formula.  C_C_STAR and C_V_STAR correspond, respectively, to the 
    ! Runge-Kutta coefficient vectors c^star and v^star; C_X_STAR corresponds to 
    ! the Runge-Kutta coefficient matrix X^star.

    DOUBLE PRECISION :: C_TAU_STAR ! Relative position of one of the sample points 
                                   ! within each subinterval. The other sample 
                                   ! point is 1-C_TAU_STAR.


    INTEGER :: MXNSUB ! maximum number of subintervals --  this is used to provide 
    ! an upper bound that can be checked in order to determine when the next mesh 
    ! would too large - this is is preferable to termination based on a failed memory 
    ! allocation call. (Recommended by Skip Thompson, May 26th, 2005.) The default 
    ! value is 3000, but it can be changed with the option MAX_NUM_SUBINTERVALS in 
    ! BVP_INIT and BVP_EXTEND.

    INTEGER :: PROFILE ! Global variable to specify level of TRACE. 

    LOGICAL :: G_STOP_ON_FAIL ! Global variable to allow access to STOP_ON_FAIL
                              ! argument to BVP_SOLVER.

    DOUBLE PRECISION :: BVP_TOL, NEWTON_TOL ! User tolerance for size of defect of 
    ! approximate solution and tolerance for Newton iteration.  BVP_TOL is input TOL 
    ! or a default value.

    INTEGER :: NPTS,NSUB  ! Number of points and number of subintervals for current
                          ! mesh.

    LOGICAL :: HAVE_DFDY,HAVE_DBCDY ! Record presence of DFDY and DBCDY functions.

    INTEGER :: NODE,NPAR,NEQN,NEQNSQ,BVP_METHOD,LEFTBC,RIGHTBC
    ! NODE, number of ODEs, NPAR, number of unknown parameters; number of equations 
    ! is NEQN = NODE+NPAR. NEQNSQ = NEQN**2. BVP_METHOD identifies the Runge-Kutta 
    ! method to be used and LEFTBC and RIGHTBC are the number of left and right BCs, 
    ! respectively. 

    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: G_X, G_Y
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: G_K_DISCRETE
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: G_K_INTERP
    ! Allocatable arrays to be used for storage of the mesh, discrete solution, and 
    ! discrete and continuous Runge-Kutta stages, respectively.

    ! Memory to store factored Newton Matrix
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MTOP
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MBOT
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: MBLOCKS
    INTEGER, DIMENSION(:), ALLOCATABLE :: MPIVOT

    ! Memory to store deferred-correction residual 
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: H_PHI !Holds Residual for 
										    !deferred-corrections approach
    !For Weight Matrices
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: W12,W3

    ! For Conditioning Constant
    DOUBLE PRECISION :: CONCONST

    ! Hold Conditioning Global Error estimate :: 
    DOUBLE PRECISION :: GECON
  
    ! Logical operators to indicate what error routine is operating
    LOGICAL :: UHO = .FALSE. ! Higher-order method for error estimation 
    LOGICAL :: UDC = .FALSE. ! Defered-Corrections method for error estimation
    LOGICAL :: URE = .FALSE. ! Richardson Extrapolation method for error estimation
    LOGICAL :: UCO = .FALSE. ! Find a conditioning constant
  
    ! Logical Mem control
    LOGICAL :: UPDATENEWT = .FALSE.
  
    ! Mesh Selection Strategy
    INTEGER :: USE_MESH_STRAT
  
    ! Factors for mesh selection
    DOUBLE PRECISION :: MESH_ALPHA, MESH_BETA
  
    ! Newton Iters
    INTEGER :: GBL_ITERATIONS = 0D0
  
    ! DEFECT THRESH
    LOGICAL :: USE_DT = .TRUE.

    ! Use GE
    LOGICAL :: UGE = .FALSE.

    CONTAINS

    FUNCTION BVP_SOLVER(SOL_INIT,FSUB,BCSUB,SINGULARTERM,METHOD,TOL,DFDY,&
    DBCDY,TRACE,STOP_ON_FAIL,ERROR_CONTROL,&
    COND, CERROR, REERROR,HOERROR,DCERROR) RESULT(SOL)
        ! This function is called by the user's main program to solve the boundary 
        ! value problem. It assumes that BVP_INIT has been called previously to form
        ! the guess structure, SOL_INIT. The ODEs and boundary conditions are made 
        ! available through the user-provided routines, FSUB and BCSUB, and the
        ! corresponding partial derivatives are optionally available through the 
        ! user-provided routines, DFDY and DBCDY. If there is a singular term, it is
        ! specified by the matrix SINGULARTERM. The Runge-Kutta methods to be employed 
        ! are specified by the value of METHOD.  The possibilities are 2 for the second 
        ! order method, 4 for the fourth order method, 6 for the sixth order method. 
        ! The default is 4. The user-requested tolerance is provided through TOL. The
        ! default is 1D-6.  The level of monitoring of the computation is specified by 
        ! TRACE. The default is 0 for no output unless the computation fails, 1 for 
        ! intermediate output, 2 for full output; for these values of TRACE the solver
        ! STOPs when there is a failure. If TRACE = -1, there is no output under any 
        ! circumstances and the solver does not STOP when there is a failure--the user 
        ! must check SOL%INFO to determine if computation was successful. The user can 
        ! set STOP_ON_FAIL to .FALSE. if the code should not STOP on failure; the default 
        ! (STOP_ON_FAIL=.TRUE.) is to STOP if the computation fails. This code controls
        ! the error in the ODEs and the boundary conditions. The user can choose which
        ! estimate of max error is reduced to below tolerance.  If ERROR_CONTROL=1 (the 
        ! default value), an estimate of the defect is reduced to below tolerance. If 
        ! ERROR_CONTROL=2, an estimate of the global error is reduced to below tolerance.
        ! If ERROR_CONTROL=3 the defect then global error is reduced to below tolerance.
        ! If ERROR_CONTROL=4, a linear combination of defect and global error is reduced 
        ! below tolerance. The optional parameter COND returns an estimate of the 
        ! conditioning of the BVP. The following optional parameters return an estimate of
        ! the maximum of the global error to the solution y(i) retaliative to 1+|y(i)| for all
        ! i and all mesh points.  However, each parameter determines the error differently. 
        ! The parameter CERROR returns an estimate of global error using the conditioning 
        ! of the BVP.  The parameter REERROR returns an estimate of the global error using
        ! Richardson extrapolation. HOERROR returns an estimate of the global error using 
        ! METHOD+2 Runge--Kutta method.  DCERROR  returns an estimate of the global error 
        ! using a deferred corrections method. 
        !-------------------------------------------------------------------------------
        !     CALLED BY: User program
        !     CALLS: RK_TABLEAU,INTERP_TABLEAU,CHECK_STAT,MIRKDC,EYE,DUMMY_DF,DUMMY_DBC,
        !            PINVSQ, RE_GERROR, HO_GERROR, DC_GERROR, CLEAR_MEM
        !-------------------------------------------------------------------------------
        !   Input arguments:
        TYPE(BVP_SOL) :: SOL_INIT
        DOUBLE PRECISION, DIMENSION(:,:), OPTIONAL :: SINGULARTERM
        INTEGER, OPTIONAL :: METHOD,TRACE
        LOGICAL, OPTIONAL :: STOP_ON_FAIL
        DOUBLE PRECISION, OPTIONAL :: TOL
        INTEGER, OPTIONAL :: ERROR_CONTROL

    
        !   User-supplied subroutines:
        EXTERNAL FSUB,BCSUB,DFDY,DBCDY
        OPTIONAL :: DFDY,DBCDY
           
        !   Output arguments:
        TYPE(BVP_SOL) ::  SOL
        DOUBLE PRECISION, OPTIONAL :: REERROR !Richardson extrapolation global error
        DOUBLE PRECISION, OPTIONAL :: HOERROR !Higher-order global error
        DOUBLE PRECISION, OPTIONAL :: DCERROR !Deferred-correction global error estimate
        DOUBLE PRECISION, OPTIONAL :: COND ! Conditioning constant
        DOUBLE PRECISION, OPTIONAL :: CERROR ! Conditioning error
       
        !   Local variables:
        INTEGER :: N_IWORK,N_WORK ! Dimensions of work array fields of SOL, IWORK 
                                  ! and WORK.
        INTEGER :: IER ! Dymanic array allocation error flag.
        INTEGER :: NDEX,M  ! Array index and loop index, respectively.
        INTEGER :: INFO ! Communication on status of computation (error flag) from
                        ! MIRKDC: INFO=0 for success; INFO=-1 for failure.
        INTEGER :: I, GE_INFO ! Loop index; INFO flag for global error NewIter call.
        DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: TEMPY  ! Temporary storage.
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ORIGY,WT ! Temporary storage.
    


        !   Global variables:
        !   NODE is the number of ODEs.
        !   NPAR is the number of unknown parameters.
        !   NEQN equals NODE + NPAR.
        !   NEQNSQ equals NEQN**2.
        !   LEFTBC and RIGHTBC are the number of left and right boundary conditions, 
        !     respectively. 
        !   MXNSUB is the maximum number of subintervals.
        !   MSING,FSING,BCSING, and SINGULAR are quantities used in dealing with 
        !     singular BVPs.
        !   UROUND is unit roundoff.
        !   SQRTU is the square root of unit roundoff.
        !   BVP_METHOD identifies which Runge-Kutta methods are to be employed. 
        !   C_S and C_C_STAR are the number of discrete Runge-Kutta stages and continuous 
        !     Runge-Kutta stages, respectively.     
        !   C_C, C_V, C_B, and C_X are coefficients that define the discrete Runge-Kutta 
        !     formula.  
        !   C_C_STAR, C_V_STAR, and C_X_STAR are coefficients that define the continuous 
        !     Runge-Kutta formula.  
        !   BVP_TOL is input TOL or the default value.
        !   NEWTON_TOL is the tolerance applied to the Newton iteration for the 
        !     determination of the discrete solution.
        !   PROFILE gives the level of output to be provided.
        !   NPTS is the number of points for the current mesh.
        !   NSUB is the number of subintervals for the current mesh.
        !   G_X, current mesh; its dimension is 0:NSUB.
        !   G_Y, current discrete solution approximation, in column form; 
        !     its dimension is NEQN*(NSUB+1).
        !   G_K_DISCRETE contains the discrete Runge-Kutta stages.  It has dimension
        !     C_S*NEQN*NSUB.
        !   G_K_INTERP contains the discrete continuous Runge-Kutta stages. It has 
        !     dimension (C_S_STAR-C_S)*NEQN*NSUB.
        !   HAVE_DFDY,HAVE_DBCDY record presence of DFDY and DBCDY functions.
        !   G_STOP_ON_FAIL determines whether or not the code stops itself after a 
        !     failure or simply returns with INFO=-1.
        !------------------------------------------------------------------------

       
        !   Load values for the global variables, NODE, NPAR, and LEFTBC from SOL_INIT.
        !   Compute values for global variables NEQN, NEQNSQ, and RIGHTBC.
        NODE = SOL_INIT%NODE
        NPAR = SOL_INIT%NPAR
        NEQN = NODE + NPAR
        NEQNSQ = NEQN**2
        LEFTBC = SOL_INIT%LEFTBC
        RIGHTBC = NEQN - LEFTBC

        !   Load value for global variable MXNSUB from SOL_INIT.
        MXNSUB = SOL_INIT%MXNSUB    

        !   Set global variable INFO to initial value defined in SOL_INIT.
        INFO = SOL_INIT%INFO

        !   Set global variables NPTS, NSUB.
        NPTS = SOL_INIT%NPTS
        NSUB = NPTS - 1 

        !   If the problem has a singular term, set up associated quantities that 
        !   will be needed for the computation.
        IF (PRESENT(SINGULARTERM)) THEN
            SINGULAR = .TRUE.                 
            ALLOCATE(MSING(NODE,NODE),FSING(NODE,NODE),BCSING(NODE,NODE),STAT=IER)
            CALL CHECK_STAT(IER)
            MSING = SINGULARTERM
            FSING = PINVSQ( EYE(NODE)-MSING ,NODE)
            BCSING = EYE(NODE) - MATMUL(PINVSQ(MSING,NODE), MSING)
        ELSE
            SINGULAR = .FALSE.
        END IF

        !   Set global variable SQRTU.
        SQRTU = SQRT(UROUND)
      
        !   METHOD is optional and the default value is 4. METHOD is communicated 
        !   throughout this code by the global variable BVP_METHOD. 
        IF (PRESENT(METHOD)) THEN
            BVP_METHOD = METHOD
        ELSE
            BVP_METHOD = 4
        END IF
      
        !   Define C_S and C_S_STAR values based on BVP_METHOD.
        SELECT CASE (BVP_METHOD)
            CASE (2)
                C_S = 2
                C_S_STAR = 2
            CASE (4)
                C_S = 3
                C_S_STAR = 4
            CASE (6)
                C_S = 5
                C_S_STAR = 8
        END SELECT 

        !   Define remaining discrete Runge-Kutta coefficients.
        CALL RK_TABLEAU
        !   Define remaining continuous Runge-Kutta coefficients.
        CALL INTERP_TABLEAU

        !   Set global variables BVP_TOL and NEWTON_TOL.
        IF (PRESENT(TOL)) THEN
            !     IF (TOL < 100*UROUND) THEN
            !       PRINT *,' Input tolerance is too small; choose a larger value.'
            !       STOP
            !     END IF       
            BVP_TOL = TOL
        ELSE
            BVP_TOL = 1D-6
        END IF
        NEWTON_TOL = BVP_TOL/100D0
    
        !   Set level of output with global variable PROFILE, based on value of TRACE.
        IF (PRESENT(TRACE)) THEN
            IF (TRACE < -1 .OR. TRACE > 2) THEN
                PRINT *,' TRACE must be -1, 0, 1, or 2.'
                STOP
            END IF
            PROFILE = TRACE
        ELSE
            PROFILE = 0
        ENDIF
     
        !   Set global variable G_STOP_ON_FAIL. Default is to stop on a failure.
        IF (PRESENT(STOP_ON_FAIL)) THEN
            G_STOP_ON_FAIL = STOP_ON_FAIL
        ELSE
            G_STOP_ON_FAIL = .TRUE.
        ENDIF
    
        !	Mesh selection strategy
        IF (PRESENT(ERROR_CONTROL)) THEN
            IF (ERROR_CONTROL >= 1 .AND. ERROR_CONTROL <=4 ) THEN
            	USE_MESH_STRAT= ERROR_CONTROL
            ELSE
            	PRINT *, "Please choose an error control method between 1 and 4"
            	PRINT *, "where: "
            	PRINT *, "1: Defect control"
            	PRINT *, "2: Global error control"
            	PRINT *, "1: Defect the global error control"
            	PRINT *, "1: Hybrid defect and global error control"
            	STOP
           END IF !(ERROR_CONTROL >= 1 .OR. <=4 ) 
        ELSE
            USE_MESH_STRAT = 1 !Use defect control
        END IF
	
        !	Defect threshold based on error control method.
        IF (USE_MESH_STRAT .EQ. 1) THEN
            USE_DT = .TRUE.
        ELSE
            USE_DT = .FALSE.
        END IF !(USE_MESH_STRAT .EQ. 1)

        ! Calculate a conditioning constant only if a defect error control method is used
        IF (USE_MESH_STRAT .EQ. 1) THEN
        	IF (PRESENT(COND) .OR. PRESENT(CERROR)) THEN
            	UCO = .TRUE.
        	ELSE
            	UCO = .FALSE.
        	END IF !(PRESENT(COND) .OR. PRESENT(CERROR))
        ELSE
        	PRINT *, "An estimate of the defect must be obtained in order to estimate"
        	PRINT *, "a conditioning constant.  Please choose ERROR_CONTROL=1." 
        	STOP
        END IF !(USE_MESH_STRAT .EQ. 1)
	
 


        !   Allocate space for global arrays G_X, G_Y, G_K_DISCRETE and G_K_INTERP; 
        !   these will be used within MIRKDC, where they will be deallocated and 
        !   reallocated, as the mesh is refined and the solution approximation improved. 
        ALLOCATE(G_X(0:NSUB),G_Y(NPTS*NEQN),G_K_DISCRETE(C_S*NEQN*NSUB),             &
        G_K_INTERP((C_S_STAR-C_S)*NEQN*NSUB),STAT=IER)
        CALL CHECK_STAT(IER) 

        !   Store the initial mesh - obtained from SOL_INIT - in G_X.
        G_X = SOL_INIT%X

        !   Set up a copy of initial solution in G_Y. Account for unknown parameters.
        IF (NPAR == 0) THEN
            G_Y = RESHAPE(SOL_INIT%Y,(/NPTS*NEQN/))
        ELSE
            ALLOCATE(TEMPY(NEQN,NPTS),STAT=IER)
            CALL CHECK_STAT(IER)
            TEMPY(1:NODE,1:NPTS) = SOL_INIT%Y
            DO M = 1,NPTS
                TEMPY(NODE+1:NEQN,M) = SOL_INIT%PARAMETERS
            END DO
            G_Y = RESHAPE(TEMPY,(/NPTS*NEQN/))
            DEALLOCATE(TEMPY,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF
     
        !   If problem includes a singular term, impose necessary condition on  
        !   approximation at left end point.
        IF (SINGULAR) THEN
            G_Y(1:NODE) = MATMUL(BCSING,G_Y(1:NODE))
        END IF

        !   All information has now been copied from SOL_INIT. Deallocate array 
        !   fields in SOL_INIT: X, Y, and optionally PARAMETERS.

        DEALLOCATE(SOL_INIT%X, SOL_INIT%Y,STAT=IER)
        CALL CHECK_STAT(IER)
        IF (NPAR > 0) THEN
            DEALLOCATE(SOL_INIT%PARAMETERS,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF


        !   Call MIRKDC with argument list that depends on whether or not 
        !   DFDY and DGDY are present.    
        HAVE_DFDY = PRESENT(DFDY)
        HAVE_DBCDY = PRESENT(DBCDY)
        IF (HAVE_DFDY) THEN
            IF (HAVE_DBCDY) THEN
                CALL MIRKDC(INFO,FSUB,BCSUB,DFDY,DBCDY)
            ELSE
                CALL MIRKDC(INFO,FSUB,BCSUB,DFDY,DUMMY_DBC)
            END IF  
        ELSE
            IF (HAVE_DBCDY) THEN
                CALL MIRKDC(INFO,FSUB,BCSUB,DUMMY_DF,DBCDY)
            ELSE
                CALL MIRKDC(INFO,FSUB,BCSUB,DUMMY_DF,DUMMY_DBC)
            END IF
        END IF
     
        !   Failure of the MIRKDC call is indicated by INFO = -1
        !   (Maximum mesh size, MXNSUB, exceeded during attempt to
        !   determine new mesh.)
        !   Load INFO and NPTS fields of SOL (even on failure.)
        SOL%INFO = INFO
        NPTS = NSUB + 1
        SOL%NPTS = NPTS

        !   If computation performed by MIRKDC was successful ...
        IF (INFO >= 0) THEN

            !	  Load Solution into BVP_SOL Struct
            CALL LOAD_SOL(G_X,G_Y,G_K_DISCRETE,G_K_INTERP,INFO,SOL)
	  
            IF (PRESENT(REERROR)) THEN
                !		The call of the Richardson extrapolation routine depends on 
                !		the use of exact partial derivatives of both FSUB and BCSUB
                IF (HAVE_DFDY) THEN
                    IF (HAVE_DBCDY) THEN
                        CALL RE_GERROR(SOL,FSUB,BCSUB,DFDY,DBCDY, INFO,REERROR)
                    ELSE
                        CALL RE_GERROR(SOL,FSUB,BCSUB,DFDY,DUMMY_DBC, INFO,REERROR)
                    END IF  
                ELSE
                    IF (HAVE_DBCDY) THEN
                        CALL RE_GERROR(SOL,FSUB,BCSUB,DUMMY_DF,DBCDY, INFO,REERROR)
                    ELSE
                        CALL RE_GERROR(SOL,FSUB,BCSUB,DUMMY_DF,DUMMY_DBC, INFO,REERROR)
                    END IF
                END IF
        
               
            END IF ! End of optional computation of YERROR
      
            !    Option to estimate global error with a higher-order method
            IF(PRESENT(HOERROR)) THEN
	
               
		
                IF (HAVE_DFDY) THEN
                    IF (HAVE_DBCDY) THEN
                        CALL HO_GERROR(SOL,FSUB,BCSUB,DFDY,DBCDY, INFO,HOERROR)
                    ELSE
                        CALL HO_GERROR(SOL,FSUB,BCSUB,DFDY,DUMMY_DBC, INFO,HOERROR)
                    END IF  
                ELSE
                    IF (HAVE_DBCDY) THEN
                        CALL HO_GERROR(SOL,FSUB,BCSUB,DUMMY_DF,DBCDY, INFO,HOERROR)
                    ELSE
                        CALL HO_GERROR(SOL,FSUB,BCSUB,DUMMY_DF,DUMMY_DBC, INFO,HOERROR)
                    END IF
                END IF
        
               
		
            END IF !PRESENT(HOERROR)
	
            !    Option to estimate global error with a deferred-correction method
            IF(PRESENT(DCERROR)) THEN
	
                
		
                IF (HAVE_DFDY) THEN
                    IF (HAVE_DBCDY) THEN
                        CALL DC_GERROR(SOL,FSUB,BCSUB,DFDY,DBCDY, INFO,DCERROR)
                    ELSE
                        CALL DC_GERROR(SOL,FSUB,BCSUB,DFDY,DUMMY_DBC, INFO,DCERROR)
                    END IF  
                ELSE
                    IF (HAVE_DBCDY) THEN
                        CALL DC_GERROR(SOL,FSUB,BCSUB,DUMMY_DF,DBCDY, INFO,DCERROR)
                    ELSE
                        CALL DC_GERROR(SOL,FSUB,BCSUB,DUMMY_DF,DUMMY_DBC, INFO,DCERROR)
                    END IF
                END IF
	
               
            END IF !PRESENT(DCERROR)
	
            IF (UCO) THEN
		
                IF (PRESENT(COND)) THEN
                	COND = CONCONST
            	END IF !(PRESENT(COND))
                IF (PRESENT(CERROR)) THEN
                    CERROR = GECON
                END IF 	!(PRESENT(CERROR))        
            END IF !(UCO) 
	
           
      
        END IF ! (INFO >= 0)
    
        !   Deallocate global arrays before return.
        DEALLOCATE(G_X,G_Y,G_K_DISCRETE,G_K_INTERP,C_C,C_V,C_B,C_X,C_C_STAR,         &
        C_V_STAR,C_X_STAR,STAT=IER)
        CALL CHECK_STAT(IER)
        IF (SINGULAR) THEN
            DEALLOCATE(MSING,FSING,BCSING,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF
        
        ! Clean Mem
        CALL CLEAN_MEM()

     
    END FUNCTION BVP_SOLVER

    SUBROUTINE MIRKDC(INFO,FSUB,GSUB,DFSUB,DGSUB)
        ! This routine controls the overall computation. Each iteration consists of three
        ! main steps: the setup and solution of the nonlinear system arising from the
        ! boundary conditions and the discretization of the ODEs for the current mesh,
        ! the setup of the interpolant and the evaluation of the defect, and determination 
        ! of a new mesh. Success or failure is indicated by INFO.  It is 0 for a successful 
        ! termination, i.e., the computed solution has a defect within the user-specified 
        ! tolerance.  It is -1 for an unsuccessful termination; the mesh selection algorithm 
        ! needed to use more than the maximum number of subintervals allowed; thus the 
        ! solver has not been able to compute a solution that passes the test on the defect. 
        ! Internally, INFO indicates difficulties with the computation such as failed Newton 
        ! iterations, singular Newton matrices, etc. with values of 1,2,3 or 4. These are 
        ! recoverable error situations since the code will restart the computation on a 
        ! different mesh, if possible. FSUB,GSUB,DFSUB,DGSUB are user-supplied subroutines 
        ! for the ODEs and boundary conditions and their derivatives.
        !--------------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !     CALLS: NEWITER,DEFECT_ESTIMATE,MESH_SELECTOR,INTERP_EVAL,HALF_MESH,
        !            CHECK_STAT
        !---------------------------------------------------------------------------------
        !   Output arguments:
        INTEGER :: INFO

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB,DFSUB,DGSUB
        
        !   Local variables:
        INTEGER :: I ! Loop index.
        INTEGER :: IER ! Error status indicator for storage allocation.
        INTEGER :: NSUB_STAR ! Number of subintervals for new mesh.
        DOUBLE PRECISION :: DEFECT_NORM ! Weighted maximum norm of defect estimate.
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: Y_OLD ! Copy of current discrete 
                                                           ! solution. 
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: DEFECT ! Estimate of maximum 
                          ! defect for each solution component on each subinterval.
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: MESH_NEW ! New mesh.  
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: Y_NEW ! Iniital guess for  
                                                           ! solution on new mesh.  
        DOUBLE PRECISION :: GE_NORM !  Global-error norm
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: GE_VECTOR
        DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE :: GE_SUB ! Global error estimate for each 
                                                            ! subinterval
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: HYBRID_SUB ! Error estimate for each subinterval
														          ! that consists of a combination of
														          ! defect and global error
        DOUBLE PRECISION :: ERROR_NORM ! An error norm that composes of some linear
							           !combination of the global error norm and defect
							           !norm
        DOUBLE PRECISION :: ALPHA, BETA ! weight factors of the defect norm and global error
								        ! norm.
        DOUBLE PRECISION :: GE_TOL !Tolerance to compute the global error
        TYPE(BVP_SOL) :: GSOL ! Sol structure for global error estimation
        INTEGER :: MESH_STRAT ! Use a hybrid mesh selection strategy
        LOGICAL :: COMPUTE_GE, COMPUTE_DE ! Compute the global error and compute the defect
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: INTWORK,V,BWORK !For Conditioning
        DOUBLE PRECISION :: CONEST !Conditioning Constant Estimate
        INTEGER :: FACTOR ! For Cond est
        DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: PHI,DELTA_0
        INTEGER :: GE_INFO ! get info of ge estimation method
        LOGICAL :: USE_DEFECT_FIRST=.TRUE. !a flag to allow for an estimate of the defect
                                                         
        !   Global variables:
        !   MXNSUB is the maximum number of subintervals allowed.
        !   PROFILE gives the amount of trace information about the computation that is
        !     desired by the user.
        !   BVP_METHOD identifies which continuous Runge-Kutta method is to be set up. 
        !   BVP_TOL is input TOL or the default value.
        !   NODE is the number of ODEs.
        !   NPAR is the number of unknown parameters.
        !   NEQN equals NODE + NPAR.
        !   NSUB is the number of subintervals of the current mesh.
        !   G_X, current mesh; its dimension is 0:NSUB.
        !   G_Y, current discrete solution approximation, in column form; 
        !     its dimension is NEQN*(NSUB+1).
        !   G_K_DISCRETE contains the discrete Runge-Kutta stages.  It has dimension
        !     C_S*NEQN*NSUB.
        !   G_K_INTERP contains the discrete continuous Runge-Kutta stages. It has 
        !     dimension (C_S_STAR-C_S)*NEQN*NSUB.
        !   C_S and C_C_STAR are the number of discrete Runge-Kutta stages and 
        !     continuous Runge-Kutta stages, respectively. 
        !   G_STOP_ON_FAIL determines whether or not the code stops itself after a 
        !     failure or simply returns with INFO=-1.
        !----------------------------------------------------------------------------
        IF (PROFILE > 0) THEN
            PRINT *,' '
            PRINT *,'The order of the Runge-Kutta method is ',BVP_METHOD,'.'
            PRINT *,'The number of ODEs is ',NODE,'.'
            IF (NPAR > 0) THEN
                PRINT *,'The number of unknown parameters is ',NPAR,'.'
            END IF
            PRINT *,' '
        END IF

        IF (PROFILE > 0) THEN
            PRINT *,'The initial mesh has ',NSUB,' subintervals.' 
            PRINT *,' '
        END IF

        IF (PROFILE > 1) THEN
            WRITE(*,"(1X,A19,I5,A14)")'The initial mesh is:'
            WRITE(*,"(7F10.6)")(G_X(I),I=0,NSUB)
            PRINT *,' '
        END IF
    


        !   INFO is already zero from initialization by BVP_INIT; 
        !   initialize DEFECT_NORM so that loop will execute.
        DEFECT_NORM = 2D0*BVP_TOL

        !  Sets mesh selection strategy
        IF (USE_MESH_STRAT .EQ. 1) THEN
            !	Defect mesh selection strategy
            IF (PROFILE > 0) THEN
                PRINT *,'Use a defect mesh selection strategy.' 
            END IF
            ! Set parameters for defect control
            ALPHA = 1.0D0
            BETA = 0.0D0
            GE_NORM = 2D0*BVP_TOL
            GE_TOL = 0.1D0
            MESH_STRAT = 0
            COMPUTE_GE = .FALSE.
            COMPUTE_DE =.TRUE.
        ELSE IF (USE_MESH_STRAT .EQ. 2) THEN
            ! Set parameters for global error 
            IF (PROFILE > 0) THEN
                PRINT *,'Use a global-error mesh selection strategy.' 
            END IF
            COMPUTE_GE = .TRUE.
            COMPUTE_DE=.FALSE.
            MESH_STRAT = 2
            ALPHA = 0.0D0
            BETA = 1.0D0
            GE_NORM = 2D0*BVP_TOL
            GE_TOL = 1E-2
        ELSE IF (USE_MESH_STRAT .EQ. 3) THEN
            ! Set parameters for defect then global error control
            IF (PROFILE > 0) THEN
                PRINT *,'Use a defect mesh selection then GE mesh selection.' 
            END IF
            ALPHA = 1.0D0
            BETA = 0.0D0
            GE_NORM = 2D0*BVP_TOL
            GE_TOL = 0.1D0
            MESH_STRAT = 0
            COMPUTE_GE = .FALSE.
            COMPUTE_DE =.TRUE.
            USE_DEFECT_FIRST = .TRUE.
   
        ELSE IF (USE_MESH_STRAT .EQ. 4) THEN
            ! Set parameters for linear combination of errors
            IF (PROFILE > 0) THEN
                PRINT *,'Use a Hybrid mesh selection strategy.' 
            END IF
            COMPUTE_GE = .TRUE.
            COMPUTE_DE=.TRUE.
            MESH_STRAT = 1
            ALPHA = 1.0D0
            BETA = 1.0D0
            GE_NORM = 2D0*BVP_TOL
            GE_TOL = 0.01D0
        END IF
   
   	
        !  This code returns a solution if the user-supplied tolerance 
        !  is less than Alpha*NEFECT_MORM + BETA*GE_NORM
   
        ERROR_NORM = ALPHA * DEFECT_NORM + BETA * GE_NORM

    
        !   Execute loop until we have an acceptable solution or until
        !   we exceed the number allowed for subintervals.
        DO WHILE ((INFO == 0) .AND. (ERROR_NORM > BVP_TOL)) 
            IF (PROFILE > 0) THEN
                PRINT *,'Begin the Newton iteration:' 
                PRINT *,' '
            END IF

            !     Allocate storage for Y_OLD, DEFECT, MESH_NEW, and Y_NEW. 
            ALLOCATE(Y_OLD(NEQN*(NSUB+1)),DEFECT(NEQN*NSUB),MESH_NEW(0:4*NSUB),        &
            Y_NEW(NEQN*(4*NSUB+1)),W12(NEQN*(NSUB+1)),W3(NEQN*(NSUB+1)),STAT=IER)
            CALL CHECK_STAT(IER)

            !     Save a copy of the current solution approximation.
            Y_OLD = G_Y

            !     Setup and solve the Newton system for the current mesh.
            CALL NEWITER(NEQN,NSUB,G_X,G_Y,INFO,G_K_DISCRETE,FSUB,GSUB,DFSUB,DGSUB)
            
            IF ((INFO /= 0) .AND. (PROFILE == 1)) THEN 
                WRITE(*,*)'The Newton iteration failed.'
                PRINT *,' '
            END IF
 
            IF ((INFO /= 0 ) .AND. (PROFILE > 1)) THEN
                IF (INFO == 1) THEN
                    PRINT *,'The Newton iteration failed because the',                 &
                    ' maximum number of Newton iterations was exceeded.'
                    PRINT *,' '
                END IF
                IF (INFO == 2) THEN
                    PRINT *,'The Newton iteration failed because a ',                  &
                    'singular Newton matrix was encountered.'
                    PRINT *,' '
                END IF
                IF (INFO == 3) THEN
                    PRINT *,'The Newton iteration has been terminated because ',       &
                    'it is not converging at a satisfactory rate.' 
                    PRINT *,' '
                END IF
            END IF    

            !     If the Newton iteration has converged, compute an error estimate.

            IF (INFO == 0) THEN
      

                CALL INTERP_SETUP_MESH (NEQN, NSUB, G_X, G_Y, G_K_DISCRETE, &
                G_K_INTERP, FSUB)
       
                IF (USE_MESH_STRAT == 1) THEN
                    IF(PROFILE > 0) THEN
                        PRINT *, 'Compute only defect'
                    END IF !PROFILE > 0
		
                    DEFECT=0D0
                    ! compute defect
                    CALL DEFECT_ESTIMATE(NEQN,NSUB,G_X,G_Y,DEFECT,DEFECT_NORM,INFO,&
                    G_K_DISCRETE,G_K_INTERP,FSUB)
                    ERROR_NORM = DEFECT_NORM 
                
		
                    IF (PROFILE > 0) THEN
                        PRINT *,'The norm of the defect is ',DEFECT_NORM
                    END IF
		
                ELSE IF(USE_MESH_STRAT == 2) THEN
                    ! compute ge
                    INFO = 0
                    NPTS = NSUB+1 !ensure that we do have the correct npts
                    CALL LOAD_SOL(G_X,G_Y,G_K_DISCRETE,G_K_INTERP, INFO,GSOL) !load sol
                    ALLOCATE(GE_VECTOR((NSUB+1)*NEQN),STAT=IER)
                    CALL DC_GERROR(GSOL,FSUB,GSUB,DFSUB,DGSUB, INFO, GE_NORM, GE_VECTOR) !get ge
                    IF(PROFILE > 0) THEN
                        PRINT *,'The estimated norm of the  global error', GE_NORM           
                    END IF !PROFILE > 0
                    CALL BVP_TERMINATE(GSOL)  
                    ERROR_NORM = GE_NORM
                    
                ELSE IF (USE_MESH_STRAT == 3) THEN
                    IF (DEFECT_NORM > BVP_TOL) THEN
                        ! first compute defect
                        DEFECT=0D0
                        CALL DEFECT_ESTIMATE(NEQN,NSUB,G_X,G_Y,DEFECT,DEFECT_NORM,INFO,&
                        G_K_DISCRETE,G_K_INTERP,FSUB)
                        IF (PROFILE > 0) THEN
                            PRINT *,'The norm of the defect is ',DEFECT_NORM
                        END IF
                        MESH_STRAT = 0
                        BETA = 1D0
                        ALPHA = 0D0
                        ERROR_NORM = DEFECT_NORM
                        
                    END IF
                    IF(DEFECT_NORM <= BVP_TOL) THEN
                        INFO = 0
                        DEFECT_NORM=0D0 !ensure that we ignore defect norm
                        NPTS = NSUB+1
                        CALL LOAD_SOL(G_X,G_Y,G_K_DISCRETE,G_K_INTERP, INFO,GSOL)
                        ALLOCATE(GE_VECTOR((NSUB+1)*NEQN),STAT=IER)
                        GE_VECTOR=0D0
                        CALL DC_GERROR(GSOL,FSUB,GSUB,DFSUB,DGSUB, INFO, GE_NORM,GE_VECTOR)
                        IF(PROFILE > 0) THEN
                            PRINT *,'The estimated norm of the global error', GE_NORM
                        END IF !PROFILE > 0
                        CALL BVP_TERMINATE(GSOL)
                        MESH_STRAT = 2
                        BETA = 1D0
                        ALPHA = 0D0
                        ERROR_NORM = GE_NORM
                    END IF
                
                ELSE IF (USE_MESH_STRAT == 4) THEN
                    DEFECT=0D0
                    CALL DEFECT_ESTIMATE(NEQN,NSUB,G_X,G_Y,DEFECT,DEFECT_NORM,INFO,&
                    G_K_DISCRETE,G_K_INTERP,FSUB)
                    NPTS = NSUB+1
                    CALL LOAD_SOL(G_X,G_Y,G_K_DISCRETE,G_K_INTERP, INFO,GSOL)
                    ALLOCATE(GE_VECTOR(NPTS*NEQN),STAT=IER)
                    CALL CHECK_STAT(IER)
                    CALL DC_GERROR(GSOL,FSUB,GSUB,DFSUB,DGSUB, INFO, GE_NORM,GE_VECTOR)
                    IF(PROFILE > 0) THEN
                        PRINT *,'The norm of the defect is ',DEFECT_NORM
                        PRINT *,'The estimated norm of the global error', GE_NORM
                        PRINT *,'The estimated norm of the hybrid error', & 
                        ALPHA * DEFECT_NORM + BETA * GE_NORM
                    END IF !PROFILE > 0
                    CALL BVP_TERMINATE(GSOL)
                    ALPHA = 1D0
                    BETA = 1D0
                    ERROR_NORM = ALPHA*DEFECT_NORM + BETA*GE_NORM
                ELSE
                    MESH_STRAT = 0
                END IF
		
	  
            END IF ! (INFO == 0)

            !     If the Newton iteration failed or the defect was not acceptable, INFO is 
            !     nonzero and we will proceed to the ELSE clause of the following 
            !     IF-THEN-ELSE statement where mesh halving will be attempted. Otherwise, 
            !     we will proceed to the THEN clause where mesh redistribution/refinement 
            !     will be attempted, unless the defect satisfies the tolerance.
            IF (INFO == 0) THEN                                              
                !       Check user tolerance. If it is not satisfied, try again on a new mesh, 
                !       if possible.
                IF (ERROR_NORM > BVP_TOL) THEN

                    IF (PROFILE > 0) THEN
                        PRINT *,'User defined tolerance',BVP_TOL,' has not been satisfied.'
                        PRINT *,'Construct a new mesh which equidistributes the defect.'
                        PRINT *,' '
                    END IF

                    !         Attempt to select a new mesh based on a desired strategy.
			
                    IF (MESH_STRAT .EQ. 0) THEN
                        IF (PROFILE > 0) THEN
                            PRINT *,'Perform defect mesh selection strategy'
                        END IF
                        UGE = .FALSE.
                        CALL MESH_SELECTOR(NEQN,NSUB,G_X,NSUB_STAR,MESH_NEW,INFO,DEFECT=DEFECT)
                    ELSE IF (MESH_STRAT .EQ. 1) THEN
                        IF (PROFILE > 0) THEN
                            PRINT *,'Perform hybrid mesh selection strategy'
                            print *, 'ALPHA: ', ALPHA
                            print *, 'BETA: ',BETA
                        END IF
                        ALLOCATE (GE_SUB(NSUB*NEQN),STAT=IER)
                        CALL CHECK_STAT(IER)
                        GE_SUB=0D0
                        CALL GE_SUBINTERVAL(GE_VECTOR,GE_SUB)
                        UGE = .TRUE.
                        CALL MESH_SELECTOR(NEQN,NSUB,G_X,NSUB_STAR,MESH_NEW,INFO, &
                            DEFECT=DEFECT,GE=GE_SUB)
                        UGE = .FALSE.
                        DEALLOCATE(GE_SUB,GE_VECTOR,STAT=IER)
                        CALL CHECK_STAT(IER)
          	
                    ELSE IF (MESH_STRAT .EQ. 2) THEN
                        IF (PROFILE > 0) THEN
                            PRINT *,'Perform global error mesh selection strategy'
                        END IF
                        ALLOCATE (GE_SUB(NSUB*NEQN),STAT=IER)
                        CALL CHECK_STAT(IER)
                        GE_SUB=0D0
                        CALL GE_SUBINTERVAL(GE_VECTOR,GE_SUB)
                        DEALLOCATE (GE_VECTOR,STAT=IER)
                        CALL CHECK_STAT(IER)
                        UGE = .TRUE.
                        CALL MESH_SELECTOR(NEQN,NSUB,G_X,NSUB_STAR,MESH_NEW,INFO,&
                            GE=GE_SUB)
                        UGE = .FALSE. 
                        DEALLOCATE (GE_SUB,STAT=IER)
                        CALL CHECK_STAT(IER)
                    END IF
		
                    !         If we were successful in obtaining a new mesh, compute a new estimate
                    !         for the solution on this mesh and then update NSUB, MESH, and Y.
                    IF (INFO == 0) THEN
                        IF (PROFILE > 0) THEN
                            PRINT *,'The new mesh will have ',NSUB_STAR,' subintervals.'
                            PRINT *,' '                              
                        END IF
                        IF (PROFILE > 1) THEN
                            WRITE(*,"(7F10.6)")(MESH_NEW(I), I=0,NSUB_STAR)
                            PRINT *,' '
                        END IF
                        !pause
            

                        !           Use current computed solution to generate next initial guess. 
                        
                        DO I = 0, NSUB_STAR
                            CALL INTERP_EVAL(NEQN,NSUB,G_X,G_Y,MESH_NEW(I),                    &
                            Y_NEW(I*NEQN+1:I*NEQN+NEQN),G_K_DISCRETE,         &
                            G_K_INTERP)
                        END DO
                        
                     

                        !           Deallocate current storage for mesh and the solution and allocate
                        !           new storage of appropriate size.
                        DEALLOCATE(G_X,G_Y,STAT=IER)
                        CALL CHECK_STAT(IER)
                        ALLOCATE(G_X(0:NSUB_STAR),G_Y(NEQN*(NSUB_STAR+1)),STAT=IER)
                        CALL CHECK_STAT(IER)

                        !           Copy new mesh to G_X, new solution approximation to G_Y, and 
                        !           update NSUB.
                        G_X(0:NSUB_STAR) = MESH_NEW(0:NSUB_STAR)
                        G_Y(1:(NSUB_STAR+1)*NEQN) = Y_NEW(1:(NSUB_STAR+1)*NEQN)                        
                        NSUB = NSUB_STAR
            
                        !           Release current storage for stages and allocate new storage 
                        !           of appropriate size.
                        DEALLOCATE(G_K_DISCRETE,G_K_INTERP,STAT=IER)
                        CALL CHECK_STAT(IER)
                        ALLOCATE(G_K_DISCRETE(C_S*NEQN*NSUB),G_K_INTERP((C_S_STAR-C_S)*      &
                        NEQN*NSUB), STAT=IER)
                        CALL CHECK_STAT(IER)
            
                    END IF ! (INFO == 0)
                END IF ! (ERROR_NORM > BVP_TOL)

                IF ( (INFO == 0) .AND. (ERROR_NORM < BVP_TOL) .AND. (PROFILE > 0) ) THEN
                    PRINT *,'The user defined tolerance',BVP_TOL,' has been satisfied.'
                    PRINT *,'Successful computation.'
                    PRINT *,' '
                END IF

            ELSE 
                !       The Newton iteration failed or the defect was not acceptable.
                IF (PROFILE > 0) THEN
                    PRINT *,'Cannot obtain a solution for the current mesh.'
                    PRINT *,' '
                END IF

                !       Halve the current mesh, if possible.
                IF (2*NSUB > MXNSUB) THEN
                    !         New mesh would be too large. Terminate the computation.
                    IF (PROFILE > 0) THEN
                        PRINT *,'Number of subintervals needed for new mesh would ',         &
                        'exceed current allowed maximum.'
                        PRINT *,' '
                    END IF
                    INFO = -1
                ELSE
                    IF (PROFILE > 0) THEN
                        PRINT *,'Y will be reset to previous solution approximation.'
                        PRINT *,' '
                    END IF

                    !         Deallocate current storage for the solution and allocate new storage
                    !         of appropriate size.
                    DEALLOCATE(G_Y,STAT=IER)
                    CALL CHECK_STAT(IER)
                    ALLOCATE(G_Y(NEQN*(2*NSUB+1)),STAT=IER)
                    CALL CHECK_STAT(IER)
          
                    !         Reset to previous solution approximation saved in Y_OLD. The mesh has
                    !         been doubled, so copy the values into every other location in G_Y and
                    !         average the values at midpoints.
                    G_Y(1:NEQN) = Y_OLD(1:NEQN)
                    DO I = 1,NSUB
                        G_Y(2*I*NEQN+1:2*I*NEQN+NEQN) = Y_OLD(I*NEQN+1:I*NEQN+NEQN)
                        G_Y(2*I*NEQN-NEQN+1:2*I*NEQN) = (Y_OLD((I-1)*NEQN+1:(I-1)*NEQN+NEQN) &
                        + Y_OLD(I*NEQN+1:I*NEQN+NEQN))/2D0
                    END DO

                    !         Double number of subintervals and call HALF_MESH to compute new mesh.
                    NSUB_STAR = 2*NSUB
                    CALL HALF_MESH(NSUB,MESH_NEW)

                    !         Deallocate current storage for mesh and allocate new storage
                    !         of appropriate size.
                    DEALLOCATE(G_X,STAT=IER)
                    CALL CHECK_STAT(IER)
                    ALLOCATE(G_X(0:NSUB_STAR),STAT=IER)
                    CALL CHECK_STAT(IER)

                    !         Copy new mesh to G_X and update NSUB.
                    G_X(0:NSUB_STAR) = MESH_NEW(0:NSUB_STAR) 
                    NSUB = NSUB_STAR

                    !         Release current storage for stages and allocate new storage 
                    !         of appropriate size.
                    DEALLOCATE(G_K_DISCRETE,G_K_INTERP,STAT=IER)
                    CALL CHECK_STAT(IER)
                    ALLOCATE(G_K_DISCRETE(C_S*NEQN*NSUB), &
                    G_K_INTERP((C_S_STAR-C_S)*NEQN*NSUB),STAT=IER)
                    CALL CHECK_STAT(IER)

                    IF (PROFILE > 0) THEN
                        PRINT *,'Halve each subinterval of the current mesh and try again.'
                        PRINT *,'The number of subintervals in the new mesh will be ',       & 
                        NSUB,'.'
                        PRINT *,' '
                    END IF
                    IF (PROFILE > 1) THEN
                        WRITE(*,"(7F10.6)")(G_X(I), I = 0,NSUB)
                        PRINT *,' '
                    END IF 

                    !         Reset INFO and DEFECT_NORM > BVP_TOL to allow another iteration. 
                    INFO = 0            
                    DEFECT_NORM = 2D0*BVP_TOL

                END IF ! (2*NSUB > MXNSUB)
            END IF ! (INFO == 0)
      
            IF((INFO == 0) .AND. (ERROR_NORM > BVP_TOL)) THEN
                DEALLOCATE(W12,W3)
            END IF

            DEALLOCATE(Y_OLD, DEFECT, MESH_NEW, Y_NEW, STAT=IER)
            CALL CHECK_STAT(IER)
        END DO !  WHILE ((INFO == 0) .AND. (DEFECT_NORM > BVP_TOL))

        IF (INFO /= 0) THEN
            !     The computation has failed. A brief report and a STOP follow unless the 
            !     user has suppressed this action by setting STOP_ON_FAIL = .FALSE.
            IF (G_STOP_ON_FAIL) THEN
                PRINT *,' '
                PRINT *,'The computation has failed.'
                PRINT *,' '
                PRINT *,'1) If the profiling information begins with and simply proceeds'  
                PRINT *,'through a series of failed Newton iterations each followed by a'
                PRINT *,'doubling of the mesh, then there may be an error in the coding'
                PRINT *,'of the problem. It is recommended that the routines FSUB, GSUB,'
                PRINT *,'DFSUB, and DGSUB be checked. A better initial guess may also help.'
                PRINT *,' ' 
                PRINT *,'2) If the profiling information shows a series of successful,'
                PRINT *,'converged Newton iterations with a halt simply because the maximum'
                PRINT *,'number of subintervals was exceeded, then it is recommended that'
                PRINT *,'the program be run again with a larger value for this maximum.'
                PRINT *,'This can be done by making a call, prior to the call to BVP_SOLVER,'
                PRINT *,'to the routine BVP_INIT, with an additional argument,'
                PRINT *,'MAX_NUM_SUBINTERVALS, set equal to the desired maximum number of'
                PRINT *,'subintervals.'
                PRINT *,' '
                PRINT *,'3) More information about the computation may be obtained by choosing '
                PRINT *,'a larger value for the input argument TRACE. The maximum value for this'
                PRINT *,'input argument is 2.'

                !       Since computation has failed, terminate now.
                STOP
            END IF ! (G_STOP_ON_FAIL)
      
        END IF ! (INFO \= 0)
        
    
    
        ! Perform the optional estimate of the conditioning constant
        IF (UCO) THEN !UCO		    
            ALLOCATE(INTWORK(NEQN*(Nsub+1)),V((NSUB+1)*NEQN),BWORK(NEQN*(NSUB+1)), &
            STAT=IER)
            CALL CHECK_STAT(IER)
    	
            !Call routine to estimate the conditioning constant
            Call BSPNORMMAX((NSUB+1)*NEQN,MTOP,LEFTBC,NEQN,MBLOCKS,NEQN,2*NEQN, &
            NSUB,MBOT,NEQN-LEFTBC,CONEST,V,INTWORK,BWORK, &
            MPIVOT,W12,W3,FACTOR)
        
            ! store estimate of the conditioning constant constant
            CONCONST = CONEST
            ! because we have defect_norm, determine an estimate of ge.
            GECON = CONCONST * DEFECT_NORM
            	
            DEALLOCATE(INTWORK,V,BWORK,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF !(UCO)
   	

    END SUBROUTINE MIRKDC
    
    SUBROUTINE  CLEAN_MEM()
        ! This routine runs at the end of BVP_SOLVER to clear some global variables from
        ! memory.
        !--------------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !     CALLS: CHECK CHECSTAT
        !--------------------------------------------------------------------------------
        ! 	Local
        INTEGER :: IER ! Mem deallocation flag
	
        !	Clear the Newton Matrix from memory
        IF(UPDATENEWT) THEN
            UPDATENEWT = .FALSE.
            DEALLOCATE(MBLOCKS,MPIVOT,MTOP,MBOT,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF !UPDATENEWT
        
        !Deallocate weights used for conditioning constant 
        DEALLOCATE(W12,W3,STAT=IER)
        CALL CHECK_STAT(IER)
        
        !Re-set some variables
        UPDATENEWT = .FALSE.
        CONCONST = 0.0D0
        GECON = 0D0
        
	
	END SUBROUTINE CLEAN_MEM


    SUBROUTINE CRITERION(NEQN,NSUB,MESH,Y,TOP,BLOCKS,BOT,PIVOT,PHI,                &
    DELTA,G,K_DISCRETE,FSUB,GSUB)
        ! This routine evaluates the natural criterion function for the current
        ! iterate Y, specifically G = 0.5 * || inverse(J)*PHI(Y) ||**2.  Here
        ! PHI(Y) is the residual function evaluated at Y and J is the Jacobian
        ! of PHI evaluated at Y, or possibly at a previous value of the iterate
        ! if a Fixed Jacobian iteration has been employed.  Details are found in
        ! Ascher, Mattheij, and Russell, p. 335.  Some intermediate quantities
        ! are returned for use in the next step of the iteration, namely
        ! PHI = PHI(Y) and DELTA = inverse(J)*PHI(Y).  The input arrays 
        ! TOP, BLOCKS, BOT, PIVOT contain J in factored form. NEQN is the
        ! number of ODEs plus the number of unknown parameters; NSUB is the
        ! number of subintervals in the mesh, stored in MESH. By-products of
        ! this computation are the Runge-Kutta stages, returned from this
        ! routine in K_DISCRETE. FSUB and GSUB are user-supplied subroutines
        ! for the ODEs and boundary conditions.
        !-----------------------------------------------------------------------
        !     CALLED BY: FIXED_JACOB,DAMP_FACTOR
        !     CALLS: RESID,CRSLVE
        !-----------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB,PIVOT(NEQN*(NSUB+1))
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1))
        DOUBLE PRECISION :: TOP(NEQN*LEFTBC),BLOCKS(2*NEQN**2 * NSUB),               &
        BOT(NEQN*RIGHTBC)

        !   Output arguments:
        DOUBLE PRECISION :: PHI(NEQN*(NSUB+1)),DELTA(NEQN*(NSUB+1)),                 &
        G,K_DISCRETE(MXS*NEQN*NSUB)

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB

        !   Local variable:
        DOUBLE PRECISION :: PHI_COPY(NEQN*(NSUB+1))

        !   Global variables:
        !   MXS is the (integer) maximum number of stages in the Runge-Kutta formula.
        !   LEFTBC is the number of left boundary conditions.
        !   RIGHTBC is the number of right boundary conditions.
        !   PROFILE specifies the level of output.
        EXTERNAL CRSLVE
        !----------------------------------------------------------------------------  
        !   Compute the residual PHI(Y).
        CALL RESID(NEQN,NSUB,MESH,Y,PHI,K_DISCRETE,FSUB,GSUB)

        !   Compute DELTA = inverse(J)*PHI(Y) by solving a linear system.  The factored
        !   matrix J is stored in TOP, BLOCKS, BOT, and PIVOT.  CRSLVE overwrites the
        !   right hand side in solving the system, so we call it with a copy of PHI.
        PHI_COPY = PHI
    
   	
        CALL CRSLVE(TOP,LEFTBC,NEQN,BLOCKS,NEQN,2*NEQN,NSUB,BOT,RIGHTBC,PIVOT, &
        PHI_COPY,DELTA)
                

        G = MAXVAL(ABS(DELTA))
        IF (G < SQRT(HUGE(1D0))) THEN
            G = 0.5D0*G**2
        ELSE

            !     G is much too large.  Set a negative value for G which will be 
            !     trapped in the calling routine and the iteration terminated.
            G = -1D0
            IF (PROFILE > 1) THEN
                PRINT *,'Computed Newton correction is much too large.',&
                ' Terminate current Newton interation.'
                PRINT *,' '
            END IF
        END IF

    END SUBROUTINE CRITERION


    SUBROUTINE DAMP_FACTOR(NEQN,NSUB,MESH,Y,DELTA_0,G_0,TOP,BOT,BLOCKS,PIVOT,      &
    LAMBDA,PHI,DELTA,G,FIXED_JACOBIAN,INFO,K_DISCRETE,FSUB,GSUB)
        ! This routine computes a damping factor for a damped Newton step and updates
        ! the Newton iterate Y, which represents a discrete solution to the BVP on a 
        ! mesh, MESH, of NSUB subintervals. NEQN is the number of ODEs plus the number 
        ! of unknown parameters. On input LAMBDA is an estimate for the damping factor; 
        ! on output, it contains the damping factor used to perform the update. TOP, BOT, 
        ! BLOCKS, and PIVOT provide the Newton matrix in factored form. DELTA_0 is the 
        ! Newton correction for the input Y. G_0 is the natural criterion function value
        ! corresponding to the input Y. PHI is the residual function value for the new 
        ! iterate. DELTA is the Newton correction for the new iterate and G is the 
        ! corresponding natural criterion function value. On output FIXED_JACOBIAN is 
        ! .TRUE. if the next step should be a fixed Jacobian step and .FALSE. if the next 
        ! step should be another damped Newton step. Success is indicated by INFO = 0. 
        ! A large natural criterion function value is indicated by INFO = 3. If it is 
        ! impossible to find a suitable damping factor, we set INFO = 3. A by-product of 
        ! this computation is the Runge-Kutta stages, which are returned in K_DISCRETE. 
        ! FSUB and GSUB are provided by the user to define the ODEs and boundary 
        ! conditions.
        !------------------------------------------------------------------------
        !     CALLED BY: DAMPED_NEWT
        !     CALLS: CRITERION
        !------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB, PIVOT(NEQN*(NSUB+1))  
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1)),DELTA_0(NEQN*(NSUB+1)),    &
        G_0,TOP(NEQN**2), BOT(NEQN**2),BLOCKS(2*NEQN**2*NSUB),  &
        LAMBDA
        !   Output arguments:
        LOGICAL :: FIXED_JACOBIAN
        INTEGER :: INFO
        DOUBLE PRECISION :: PHI(NEQN*(NSUB+1)),DELTA(NEQN*(NSUB+1)),G,               &
        K_DISCRETE(MXS*NEQN*NSUB)

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB
 
        !   Local variables:
        LOGICAL :: ACCEPT ! Used to monitor search for new damping factor.
        DOUBLE PRECISION, PARAMETER :: SIGMA=0.01D0,TAU=0.1D0 
        !   SIGMA ensures that reduction in size of natural criterion function value 
        !   for new iterate will be nonnegligible. TAU controls how much values for 
        !   LAMBDA are allowed to change from one step to the next in the iteration 
        !   performed by this routine. 
        DOUBLE PRECISION :: Y_0(NEQN*(NSUB+1)) ! Current iterate saved here while 
                                               ! new iterate is determined.
        !   Global variables:
        !   MXS is the maximum number of Runge-Kutta stages.
        !   PROFILE gives the level of output to be provided.
        !   LAMBDA_MIN is the minimum value for the damping factor.
        !   SINGULAR specifies whether or not the BVP has a singular term.
        !   BCSING is a quantity associated with the treatment of a singular term.
        !   NODE is the number of ODEs.
        !--------------------------------------------------------------------------
        !   Iterative determination of new damping factor.
        Y_0 = Y
        ACCEPT = .FALSE.
        DO WHILE ( (.NOT. ACCEPT) .AND. (INFO == 0) )
            !     Update iterate value using input damping factor and Newton correction.
            Y = Y_0 - LAMBDA*DELTA_0

            IF (SINGULAR) THEN
                ! Impose necessary condition on approximation at left end point.
                Y(1:NODE) = MATMUL(BCSING,Y(1:NODE))
            END IF
             
            !     Measure suitability of current damping factor by seeing how much the
            !     trial iterate reduces the natural criterion function. CRITERION returns 
            !     G, PHI, and DELTA = inv(J)*PHI, with J evaluated at the previous iterate.
            CALL CRITERION(NEQN,NSUB,MESH,Y,TOP,BLOCKS,BOT,PIVOT,PHI,DELTA,G,  &
            K_DISCRETE,FSUB,GSUB)

            !     If natural criterion function value has overflowed, CRITERION will 
            !     trap this and return G = -1. Set INFO = 3 here to cause a return.
            IF (G < 0D0) THEN
                INFO = 3
            ELSE
                !       Check G to ensure that reduction in size is sufficient. If so, LAMBDA
                !       and thus Y are acceptable. 
                IF (G <= (1D0 - 2D0*LAMBDA*SIGMA)*G_0) THEN
                    ACCEPT = .TRUE.
                    IF ( ABS(LAMBDA - 1D0) <= 0D0 ) FIXED_JACOBIAN = .TRUE.
                ELSE
                    !         Reduction in G is not sufficient. Compute a new damping factor.
                    LAMBDA = MAX(TAU*LAMBDA,(LAMBDA**2 * G_0)/((2*LAMBDA-1D0)*G_0+G))
                    IF (PROFILE > 1) THEN
                        PRINT *,'Damped Newton step: value of damping factor is ', LAMBDA
                        PRINT *,' '
                    END IF

                    !         If new LAMBDA is too small, set INFO = 3 to cause a return.
                    IF (LAMBDA < LAMBDA_MIN) THEN
                        INFO = 3
                    END IF
                END IF ! (G <= (1D0-2D0*LAMBDA*SIGMA)*G_0)
            END IF ! (G < 0D0)
        END DO

    END SUBROUTINE DAMP_FACTOR

    SUBROUTINE DAMPED_NEWT(NEQN,NSUB,MESH,Y,LAMBDA,PHI,TOP,BOT,BLOCKS,PIVOT,&
    FIXED_JACOBIAN,CONVRG,DELTA,DELTA_0_NORM,G,INFO,&
    K_DISCRETE,FSUB,GSUB,DFSUB,DGSUB)
        ! This routine takes one damped Newton step. Y contains the current iterate 
        ! and PHI contains the corresponding residual function value for the current 
        ! mesh of NSUB subintervals stored in MESH. NEQN is the number of ODEs plus 
        ! the number of unknown parameters. LAMBDA is the damping factor. TOP, BOT, 
        ! BLOCKS, and PIVOT, store the Newton matrix in factored form. FIXED_JACOBIAN 
        ! indicates whether the next step should be a fixed Jacobian step and CONVRG 
        ! indicates whether the iteration has converged. DELTA is the Newton correction 
        ! vector and DELTA_0_NORM is its norm. G is the value of natural criterion 
        ! function. A successful return is indicated by INFO = 0. If a singular matrix 
        ! is encountered, INFO = 2. A large natural criterion function value is indicated
        ! by INFO = 3. A by-product of this computation is the Runge-Kutta stages which 
        ! are returned in the array K_DISCRETE. The subroutines FSUB, GSUB, DFSUB, and 
        ! DGSUB are provided by the user to define the ODEs and boundary conditions and 
        ! their partial derivatives.
        !------------------------------------------------------------------------
        !     CALLED BY: NEWITER
        !     CALLS: NEWMAT,COLROW,DAMP_FACTOR
        !----------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1)),LAMBDA,                    &
        PHI(NEQN*(NSUB+1)),DELTA(NEQN*(NSUB+1)),DELTA_0_NORM

        !   Output arguments:
        INTEGER :: INFO,PIVOT(NEQN*(NSUB+1))
        DOUBLE PRECISION :: TOP(NEQN**2),BOT(NEQN**2),BLOCKS(2*NEQN**2*NSUB),        &
        G,K_DISCRETE(MXS*NEQN*NSUB)
        LOGICAL :: FIXED_JACOBIAN, CONVRG
      
        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB,DFSUB,DGSUB

        !   Local variables:
        INTEGER :: FACTOR ! Trouble flag for call to COLROW.
        DOUBLE PRECISION :: G_0 ! Natural criterion function value for input Y.
        DOUBLE PRECISION :: MU ! Predicted value for new damping factor.
        DOUBLE PRECISION :: DELTA_0(NEQN*(NSUB+1)) ! Copy of Newton correction 
                                                   ! vector.
        INTEGER :: I,J
        DOUBLE PRECISION :: Y_OLD(NEQN*(NSUB+1))
      
        !   Global variables:
        !   MXS is the number of maximum number of Runge-Kutta stages.
        !   LEFTBC is the number of left boundary conditions.
        !   RIGHTBC is the number of right boundary conditions.
        !   PROFILE defines the level of output to be provided.
        !   NEWTON_TOL is the tolerance on the Newton iteration.
        !   LAMBDA_MIN is the minimum value for the damping factor.
        EXTERNAL COLROW,CRSLVE
        !----------------------------------------------------------------------------
    
        INFO = 0  
    
        
    
        !	If HO, DC error-estimation routine is being used, simply use the factored 
        !	Matrix already in memory
        IF(UHO .OR. UDC) THEN
            !IF (.FALSE.) THEN
            
            DO J=1,NEQN**2
                TOP(J) = MTOP(J)
                BOT(J) = MBOT(J)
            END DO
            DO J=1,(2*NEQN**2)*NSUB
                BLOCKS(J) = MBLOCKS(J)
            END DO
            DO J=1,NEQN*(NSUB+1)
                PIVOT(J) = MPIVOT(J)
            END DO
                
            !   	Factor the Newton matrix and solve a linear system with the input
            !   	vector PHI as right hand side to get the Newton correction DELTA_0.
        
           
            CALL CRSLVE(TOP,LEFTBC,NEQN,BLOCKS,NEQN,2*NEQN,NSUB,BOT,RIGHTBC, PIVOT, &
            PHI,DELTA_0)
   
        ELSE IF (URE .AND. (GBL_ITERATIONS > 1)) THEN
            CALL CRSLVE(TOP,LEFTBC,NEQN,BLOCKS,NEQN,2*NEQN,NSUB,BOT,RIGHTBC, PIVOT, &
            PHI,DELTA_0)
        ELSE
    	

            !   	Form the Newton matrix with NEWMAT.
            CALL NEWMAT(LEFTBC,RIGHTBC,NEQN,NSUB,MESH,Y,TOP,BLOCKS,BOT, &
            K_DISCRETE,FSUB,GSUB,DFSUB,DGSUB)
                
            !   	Factor the Newton matrix and solve a linear system with the input
            !   	vector PHI as right hand side to get the Newton correction DELTA_0.
            CALL COLROW((NSUB+1)*NEQN,TOP,LEFTBC,NEQN,BLOCKS, &
            NEQN,2*NEQN,NSUB,BOT,RIGHTBC,PIVOT,PHI,DELTA_0,FACTOR)
        end if
        	
 	
        IF ( FACTOR == -1 ) THEN
            INFO = 2 ! Singular Newton matrix encountered.
        END IF

        IF (INFO == 0) THEN
            !     Compute natural criterion function value.
            G_0 = MAXVAL(ABS(DELTA_0))
            IF (G_0 > SQRT(HUGE(1D0))) THEN
                IF (PROFILE > 1) THEN
                    PRINT *,'Computed Newton correction is much too large.',               &
                    ' Terminate current Newton interation.'
                    PRINT *,' '
                END IF
                INFO = 3
            ELSE
                G_0 = 0.5D0 * G_0**2
            END IF
        END IF
      
        IF (INFO == 0) THEN
            !     Test if norm of scaled Newton correction, DELTA_0, is sufficiently small
            !     to signal convergence. 
            CONVRG = ALL( ABS(DELTA_0) <= NEWTON_TOL*(ABS(Y) + 1D0) )
            IF (CONVRG .OR. URE .OR. UHO .OR. UDC) THEN
                !IF (CONVRG) THEN
                !       Update iterate with full Newton correction.
                Y_OLD = Y - DELTA_0
                CONVRG = .TRUE.
                Y= Y_OLD
            ELSE           
                !       Attempt to update the Newton iterate with a damped Newton correction. 
                !       Compute a prediction for new LAMBDA based on previous LAMBDA value. 
                !       If the input LAMBDA = 0, the previous iteration was not a damped Newton 
                !       step and appropriate information for the prediction of a new LAMBDA 
                !       value is not available.
                IF ( ABS(LAMBDA) <= 0D0 ) THEN
                    LAMBDA = 1D0
                ELSE
                    !         Predict LAMBDA based on previous LAMBDA, DELTA, and DELTA_0_NORM.
                    DELTA = DELTA - DELTA_0
                    MU = (LAMBDA*DELTA_0_NORM) / MAXVAL(ABS(DELTA))
                    LAMBDA = MAX(LAMBDA_MIN,MIN(1D0,MU))
                END IF
                IF (PROFILE > 1) THEN
                    PRINT *,'Damped Newton iteration: value of initial damping',           &
                    ' factor is', LAMBDA
                    PRINT *,' '
                END IF
        
                !       Update norm of Newton correction.
                DELTA_0_NORM = SQRT(2D0*G_0)

                !       Compute a new iterate by updating Y with a damped Newton correction.
                CALL DAMP_FACTOR(NEQN,NSUB,MESH,Y,DELTA_0,G_0,TOP,BOT,BLOCKS,PIVOT,      & 
                LAMBDA,PHI,DELTA,G,FIXED_JACOBIAN,INFO,K_DISCRETE,      &
                FSUB,GSUB)
                         
                !       Upon return DAMP_FACTOR has computed an appropriate damping factor, 
                !       LAMBDA, and updated the iterate, Y, accordingly. This routine has also
                !       updated the corresponding residual function value, PHI, the Newton 
                !       correction DELTA, and the natural criterion function value, G, all for 
                !       the newly updated Y.

                IF ((INFO == 0) .AND. (PROFILE > 1)) THEN 
                    PRINT *,'Norm of damped Newton correction is',LAMBDA*DELTA_0_NORM
                    PRINT *,' '
                END IF
            END IF ! IF (CONVERG)
        END IF ! IF (INFO==0)

    END SUBROUTINE DAMPED_NEWT
    
    SUBROUTINE DC_GERROR(GSOL,FSUB,BCSUB,DFDY,DBCDY, INFO,GENORM,GEVECTOR)
    !       Use a MIRK deferred corrections method to determine an order 
    !       BVP_METHOD+2 solution (see J. R. Cash, M. H. Wright 
    !       "A deferred correction method for nonlinear two-point boundary value 
    !       problems: implementation and numerical evaluation) and use the 
    !       solution to estimate the GE error in solution component y(i) relative 
    !       to 1 + |y(i)|. Return the maximum GE over all components and mesh  
    !       points as GENORM. The same estimate applies to unknown parameters. 
    !       the estimate is computed by using a deferred Corrections approach.
    !-----------------------------------------------------------------------------
    ! CALLED BY: BVP_SOLVER, MIRKDC
    ! CALLS BVP_EVAL, NEWITER, GLOBAL_ERROR
    !-----------------------------------------------------------------------------
    
    !   Input
        TYPE(BVP_SOL) :: GSOL
        EXTERNAL :: FSUB ! User-supplied ODEs routine
        EXTERNAL :: BCSUB ! User-supplied boundary conditions routine
        EXTERNAL :: DFDY ! Partial-derivative routine for ODEs
        EXTERNAL :: DBCDY ! Partial-derivative routine for boundary conditions
        INTEGER :: INFO ! Success flag
	
    !   Output
        DOUBLE PRECISION, OPTIONAL, DIMENSION (:) :: GEVECTOR ! Global-error vector 
        DOUBLE PRECISION :: GENORM ! Global-error norm 
	
    !   Local variables
        INTEGER :: IER ! Memory allocation flag
        INTEGER :: ST,EN ! Array location
        INTEGER :: I ! Counters
        INTEGER :: NDEX !Work Array Locatoion for solution
        INTEGER :: GE_INFO ! Newton solver flag
        INTEGER :: NITERS ! Number of Newton Iterations
        DOUBLE PRECISION, ALLOCATABLE,DIMENSION (:) :: Y_HIGH, Y_LOW ! Hold solutions
    	!Holds the result from the Newton solver.
        DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: GE_Y,GE_X,GE_K_DISCRETE
        REAL :: T

	
    !   Allocate Arrays
        ALLOCATE(GE_X(0:NSUB),GE_Y((NSUB+1)*NEQN),STAT=IER)
        CALL CHECK_STAT(IER)
    
        ALLOCATE(Y_HIGH(NEQN*(NSUB+1)),Y_LOW(NEQN*(NSUB+1)),STAT=IER)
        CALL CHECK_STAT(IER)  
    
    !   Fill arrays
        GE_X = GSOL%X
        NDEX = C_S_STAR*NEQN*NSUB + (NSUB+1)
        GE_Y = GSOL%WORK(NDEX+1:NDEX+(NSUB+1)*NEQN)
	
    !   Change Mirk fourmulas to orders.
        SELECT CASE (BVP_METHOD)
            CASE (2)
                C_S = 3
                C_S_STAR = 4
                BVP_METHOD = 4
            CASE (4)
                C_S = 5
                C_S_STAR = 8
                BVP_METHOD=6
            CASE (6)
                C_S = 9
                C_S_STAR = 8
                BVP_METHOD = 8
        END SELECT 

        DEALLOCATE(C_C,C_V,C_B,C_X,STAT=IER)
        CALL CHECK_STAT(IER)

    !    Defines MIRK coefficents
        CALL RK_TABLEAU
    
    !    Allocate H_PHI
        ALLOCATE(H_PHI(NEQN*(NSUB+1)))
        CALL CHECK_STAT(IER)
        H_PHI(:) = 0D0 
	  
    !	Allocate Mem for GE_K_DISCRETE
        ALLOCATE(GE_K_DISCRETE(C_S*NEQN*(NSUB)),STAT=IER)
    !	Get residual from higher-order formulas
        CALL RESID(NEQN,NSUB,GE_X,GE_Y,H_PHI,GE_K_DISCRETE,FSUB,BCSUB)

    !	Deallocate We will need a different size GE_K_DISCRETE for Newton's 
    !	method.  Deallocate the array now
        DEALLOCATE(GE_K_DISCRETE,STAT=IER)
        CALL CHECK_STAT(IER)

    !  Change back to oringinal MIRK orders

        SELECT CASE (BVP_METHOD)
            CASE (4)
                C_S = 2
                C_S_STAR = 2
                BVP_METHOD = 2
            CASE (6)
                C_S = 3
                C_S_STAR = 4
                BVP_METHOD= 4
            CASE (8)
                C_S = 5
                C_S_STAR = 8
                BVP_METHOD = 6 
        END SELECT 

        DEALLOCATE(C_C,C_V,C_B,C_X,STAT=IER)
        CALL CHECK_STAT(IER)
        CALL RK_TABLEAU
  	
    !   Re-allocate mem for GE_K_DISCRETE with appropriate size
        ALLOCATE(GE_K_DISCRETE(C_S*NEQN*(NSUB)),STAT=IER)
 
    !   Run the Newton solver with deferred corrections
 
 	
        UDC=.TRUE.
        CALL NEWITER(NEQN,NSUB,GE_X,GE_Y,GE_INFO,GE_K_DISCRETE,             &
        FSUB,BCSUB,DFDY,DBCDY, USAGE=2)
        UDC = .FALSE.
     	
        IF (GE_INFO /= 0) THEN
            IF (G_STOP_ON_FAIL) THEN
                PRINT *,'Computation of deferred-corrections error failed--the solution SOL is in doubt.'
                PRINT *,'INFO: ', GE_INFO
              !STOP
            END IF
        END IF
     
    !	Get Both Solutions
        Y_HIGH = GE_Y
        Y_LOW = GSOL%WORK(NDEX+1:NDEX+(NSUB+1)*NEQN) 
	
    !	Get an estimate of the scaled global-error norm
    !	and optionally a scaled global-error vector
        GENORM = 0D0
        
        IF (PRESENT(GEVECTOR)) THEN
            CALL GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM,GEVECTOR)
        ELSE 
            CALL GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM)
        END IF
    
    !   Deallocate mem
        DEALLOCATE(GE_X,GE_Y,GE_K_DISCRETE,STAT=IER)
        CALL CHECK_STAT(IER)
        DEALLOCATE(Y_LOW,Y_HIGH,STAT=IER)
        CALL CHECK_STAT(IER)
        DEALLOCATE(H_PHI,STAT=IER)
        CALL CHECK_STAT(IER)
	
    END SUBROUTINE


    SUBROUTINE DEFECT_ESTIMATE(NEQN,NSUB,MESH,Y,DEFECT,DEFECT_NORM,                &
    INFO,K_DISCRETE,K_INTERP,FSUB,DEFECT_MESH) 
        ! This routine uses the discrete approximate solution Y on the mesh stored in
        ! MESH of NSUB subintervals together with the Runge-Kutta stages stored
        ! in K_DISCRETE and some new stages stored in K_INTERP to construct a
        ! continuous approximate solution on the whole interval. NEQN is the number 
        ! of differential equations plus the number of unknown parameters. The defect
        ! of the continuous extension is estimated on each subinterval for each solution 
        ! component. The Ith block of NEQN locations in the vector DEFECT holds the 
        ! defect information associated with subinterval I. A weighted maximum norm of 
        ! DEFECT is returned in DEFECT_NORM. A successful return is indicated by  
        ! INFO = 0. If DEFECT_NORM is bigger than 10%, INFO is set to 4 to cause  
        ! rejection of the current Y. The subroutine FSUB is provided by the user 
        ! to define the ODEs.
        !
        ! The defect on subinterval I evaluated at TAU is 
        !    Z'(MESH(I-1)+TAU*HI) - F(MESH(I-1)+TAU*HI,Z(MESH(I-1)+TAU*HI)).
        ! A weighted measure of the defect at this point, namely |DEFECT|/(|F| + 1), 
        ! is computed. The maximum defect on each subinterval is estimated by the 
        ! larger of the defect values at TAU as just described and at 1-TAU.  The value 
        ! of TAU is set as a global variable appropriate to the method.
        !
        ! The boundary conditions are solved during the computation of the discrete 
        ! solution. Thus the boundary conditions will be satisfied to within the Newton 
        ! tolerance, which is well below the user's tolerance.  Accordingly, the defect 
        ! of the boundary conditions will be less than the user's tolerance for all 
        ! converged discrete solutions, and there is no need to monitor it here. 
        !-----------------------------------------------------------------------------
        !     CALLED BY: MIRKDC
        !     CALLS: INTERP_WEIGHTS,INTERP_SETUP,SUM_STAGES,P_FSUB
        !------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1)),K_DISCRETE(MXS*NEQN*NSUB)
        DOUBLE PRECISION, OPTIONAL :: DEFECT_MESH(:)

        !   Output arguments:
        INTEGER :: INFO
        DOUBLE PRECISION :: DEFECT(NSUB*NEQN),DEFECT_NORM,K_INTERP(MXS*NEQN*NSUB)
    
        !   User-supplied subroutines:
        EXTERNAL FSUB

        !   Local variables:
        INTEGER :: I,J ! Loop indices.
        DOUBLE PRECISION :: HI ! Size of the i-th subinterval.
        DOUBLE PRECISION :: WEIGHTS(MXS,MXS) ! Weight polynomials evaluated at
                                             ! sample points.
        DOUBLE PRECISION :: WEIGHTS_PRIME(MXS,MXS) ! Derivatives of weight 
                                                   ! polynomials evaluated at 
                                                   ! sample points.
        DOUBLE PRECISION MAX_ESTIMATE, ESTIMATE(MXS) ! Maximum defect and estimates.                
        DOUBLE PRECISION :: Z(NEQN) ! Argument for f evaluation.
        DOUBLE PRECISION :: Z_PRIME(NEQN) ! Derivative of interpolant.
        DOUBLE PRECISION :: F_I(NEQN),DEFECT_I(NEQN) ! Temporary storage.
        DOUBLE PRECISION :: SAMPLE(2) ! Sample points.

        !   Global variables:
        !   MXS is the maximum number of stages of the Runge-Kutta method.
        !   C_S is the number of discrete Runge-Kutta stages. 
        !   C_S_STAR is the total number of stages required. It is the number of discrete
        !   stages plus the number of additional stages required for the interpolant. 
        !   C_TAU_STAR  is the relative position of one of the sample points within each 
        !   subinterval; the other sample point has the relative position 1-C_TAU_STAR.
        !--------------------------------------------------------------------------------
        !   Setup information about sample points.
        SAMPLE = (/ C_TAU_STAR, 1D0-C_TAU_STAR /)
      
        !   Setup the weights for the interpolant and the derivative. 
        CALL INTERP_WEIGHTS(C_S_STAR,SAMPLE(1),WEIGHTS(:,1),WEIGHTS_PRIME(:,1))
        CALL INTERP_WEIGHTS(C_S_STAR,SAMPLE(2),WEIGHTS(:,2),WEIGHTS_PRIME(:,2))
        

        W12 = 0D0
        W3 = 0D0
        DO I = 1, NSUB
            !     Setup new stages for this subinterval.
            HI = MESH(I) - MESH(I-1)
            
                 
            MAX_ESTIMATE = 0D0
            DO J = 1,2
             
                !       Compute value of interpolant and its first derivative at sample 
                !       point J by calling SUM_STAGES. The interpolant is returned in Z;
                !       the first derivative in Z_PRIME.
                CALL SUM_STAGES(NEQN,HI,Y((I-1)*NEQN+1),C_S,K_DISCRETE((I-1)*C_S*NEQN+1),&
                C_S_STAR,K_INTERP((I-1)*(C_S_STAR-C_S)*NEQN+1),          &
                WEIGHTS(:,J),Z,WEIGHTS_PRIME(:,J),Z_PRIME)

                !       Evaluate f(Z) and return in F_I.
                CALL P_FSUB(FSUB,NEQN,MESH(I-1)+SAMPLE(J)*HI,Z,F_I)

                !       Compute scaled defect and store in DEFECT_I. 
                DEFECT_I = (Z_PRIME - F_I)/(ABS(F_I) + 1D0)
      

		 
                !       Compute norm of defect and update the maximum defect.
                ESTIMATE(J) = MAXVAL(ABS(DEFECT_I))
                IF (ESTIMATE(J) > MAX_ESTIMATE) THEN
                    MAX_ESTIMATE = ESTIMATE(J)
                    DEFECT((I-1)*NEQN+1:I*NEQN) = DEFECT_I
                    IF (PRESENT(DEFECT_MESH)) THEN
                        DEFECT_MESH(I) =  MESH(I-1) + SAMPLE(J)*HI
                    END IF
        
                END IF
      		
            END DO ! J = 1,2
      
            !Assign W12, W3
            If (I .GT. 1) THEN
                W12((I-2)*NEQN + 1:(I-2)*NEQN+NEQN) = 1D0/(ABS(F_I)+1D0)
            END IF
      
            W3((I-1)*NEQN + 1:(I-1)*NEQN + NEQN) = & 
            1D0/(ABS(Y((I-1)*NEQN + 1:(I-1)*NEQN + NEQN)) +1D0)
      
        END DO ! I = 1,NSUB
   
        CALL P_FSUB(FSUB,NEQN,MESH(0),Z,F_I)
        W12((NSUB-1)*NEQN + 1:(NSUB-1)*NEQN+NEQN) = 1D0/(ABS(F_I)+1D0)
        CALL P_FSUB(FSUB,NEQN,MESH(NSUB),Z,F_I)
        W12((NSUB)*NEQN + 1:(NSUB)*NEQN+NEQN) = 1D0/(ABS(F_I)+1D0)
    
        W3((NSUB)*NEQN + 1:(NSUB)*NEQN + NEQN) = & 
        1D0/(ABS(Y((NSUB)*NEQN + 1:(NSUB)*NEQN + NEQN)) +1D0)
      		
        DEFECT_NORM = MAXVAL(ABS(DEFECT))

        !   If the defect is too big, the current solution cannot be trusted,
        !   so start over as if the Newton iteration had failed.
        IF (USE_DT) THEN
            IF (DEFECT_NORM > 0.1D0 ) INFO = 4

        END IF
    	
    END SUBROUTINE DEFECT_ESTIMATE


    SUBROUTINE FIXED_JACOB(NEQN,NSUB,MESH,Y,DELTA,G,PHI,TOP,BOT,BLOCKS,            &
    PIVOT,FIXED_JACOBIAN,LAMBDA,CONVRG,INFO,                &
    K_DISCRETE,FSUB,GSUB)
        ! This routine performs one fixed Jacobian iteration, i.e., one Newton step
        ! using a Newton iteration matrix evaluated at a previous iterate. Y contains 
        ! the current iterate and PHI contains the corresponding residual function 
        ! value for the current mesh, MESH, of NSUB subintervals. NEQN is the number 
        ! of ODEs plus the number of unknown parameters. TOP, BOT, BLOCKS, and PIVOT
        ! store the Newton matrix from a previous iteration in factored form. 
        ! FIXED_JACOBIAN indicates whether the next step should be a fixed Jacobian 
        ! step. CONVRG indicates whether the iteration converged. DELTA is the Newton 
        ! correction vector. INFO = 0 indicates a successful computation. INFO = 3
        ! indicates a failure as signalled by a large natural criterion function value 
        ! G. A by-product of this computation is the Runge-Kutta stages, which are 
        ! returned in the array K_DISCRETE. FSUB and GSUB define the ODEs and boundary 
        ! conditions.
        !
        ! When a fixed Jacobian step is to be taken, the following quantities are
        ! assumed to be available from the previous iteration step: i) the current 
        ! iterate Y (ii) the residual function PHI evaluated at Y (iii) the Newton 
        ! correction vector DELTA = inverse of J *PHI, where the Jacobian J was 
        ! evaluated at some previous iterate (iv) the value of the natural criterion 
        ! function G. These quantities are provided by DAMPED_NEWT if the previous step 
        ! was a damped Newton step, or FIXED_JACOB if the previous step was a fixed
        ! Jacobian step. 
        !
        ! The fixed Jacobian step first computes a new iterate in the local array
        ! Y_HAT = Y - DELTA. The acceptability of Y_HAT is determined by evaluating 
        ! the natural criterion function at Y_HAT.  The value G_HAT is compared to the 
        ! previous value of the natural criterion function. If it is sufficiently  
        ! smaller, the iterate is accepted and another fixed Jacobian step is attempted.  
        ! If not, the code returns with FIXED_JACOBIAN set to FALSE and LAMBDA set to 
        ! 0 to indicate that information for the prediction of a new damping factor for  
        ! the upcoming damped Newton step is not available.
        !------------------------------------------------------------------------------
        !     CALLED BY : NEWITER
        !     CALLS : CRITERION,RESID
        !------------------------------------------------------------------------------     
        !   Input arguments:
        INTEGER :: NEQN,NSUB,PIVOT(NEQN*(NSUB+1))
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1)),DELTA(NEQN*(NSUB+1)),      &
        G,PHI(NEQN*(NSUB+1)),TOP(NEQN**2),BOT(NEQN**2),          &
        BLOCKS(2*NEQN**2*NSUB)

        !   Output arguments:
        LOGICAL :: FIXED_JACOBIAN,CONVRG
        INTEGER :: INFO
        DOUBLE PRECISION :: LAMBDA,K_DISCRETE(MXS*NEQN*NSUB)

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB
        ! 
        !   Local variables:
        DOUBLE PRECISION :: G_HAT ! value of the natural criterion function 
                                  ! at the trial iterate.
        DOUBLE PRECISION :: RHO ! used in measuring performance of fixed 
                                ! jacobian iteration.  
        DOUBLE PRECISION :: Y_HAT(NEQN*(NSUB+1)) ! trial iterate value.
        DOUBLE PRECISION :: PHI_HAT(NEQN*(NSUB+1)) ! Residual function value 

        !   Global variables:
        !   MXS is the number of maximum number of Runge-Kutta stages.
        !   PROFILE gives the level of output to be provided.
        !   NEWTON_TOL is the Newton tolerance.
        !------------------------------------------------------------------------------     
        INFO = 0
        RHO = 0.5D0

        IF (PROFILE > 1) THEN
            PRINT *,'Norm of Newton correction', MAXVAL(ABS(DELTA))
            PRINT *,' '
        END IF
      
        !   If the Newton correction, DELTA, is less than the tolerance, the 
        !   iteration has converged. 
        CONVRG = ALL( ABS(DELTA) <= NEWTON_TOL*(ABS(Y) + 1D0))

        IF (CONVRG) THEN
            Y = Y - DELTA
        ELSE
            !     Compute trial iterate. 
            Y_HAT = Y - DELTA
                  
            !     Compute natural criterion function to assess the trial iterate. 
            !     By-products of this calculation are PHI_HAT and DELTA evaluated at Y_HAT. 
            !     These MAY be useful for the next iteration step, but if Y_HAT is not 
            !     acceptable, values of PHI and G associated with the iterate Y will be 
            !     needed. Here we take care that they are not overwritten by providing
            !     only copies Y_HAT, PHI_HAT, and G_HAT to CRITERION.
            CALL CRITERION(NEQN,NSUB,MESH,Y_HAT,TOP,BLOCKS,BOT,PIVOT,PHI_HAT,          &
            DELTA,G_HAT,K_DISCRETE,FSUB,GSUB)

            !     If the natural criterion function value is about to overflow, the CRITERION
            !     subroutine will set G_HAT = -1 ( a value which cannot otherwise arise).
            !     Set INFO = 3 to report failure.
            IF (G_HAT < 0D0) THEN
                INFO = 3
            ELSE
                IF ( G_HAT <= RHO * G ) THEN
                    !         The new iterate is acceptable.
                    Y = Y_HAT
                    PHI = PHI_HAT
                    G = G_HAT
                ELSE
                    !         The new iterate is not acceptable. Signal a damped Newton step. 
                    FIXED_JACOBIAN = .FALSE.
                    LAMBDA = 0D0

                    !         If G_HAT > RHO * G, but G_HAT < G, use the new iterate and 
                    !         corresponding residual to begin the subsequent full Newton step. 
                    IF (G_HAT < G) THEN
                        Y = Y_HAT
                        PHI = PHI_HAT
                    ELSE
                        !           Begin the full Newton step using the values in Y and PHI from 
                        !           the beginning of this step. A call to RESID is made here to 
                        !           obtain the K_DISCRETE values, evaluated at Y, as these must be 
                        !           up-to-date with respect to Y before the damped Newton step begins.
                        CALL RESID(NEQN,NSUB,MESH,Y,PHI,K_DISCRETE,FSUB,GSUB)
                    END IF ! (G_HAT < G)
          
                END IF ! (G_HAT <= RHO*G)
        
            END IF ! (G_HAT < 0D0)
      
        END IF ! (CONVRG)

    END SUBROUTINE FIXED_JACOB
    
    SUBROUTINE GE_SUBINTERVAL(GE_VECTOR,GE_SUB)
    ! This routine assigns a global error estimate for each subinterval
    !-------------------------------------------------------------------------------
    !	CALLED BY: MIRKDC
    !	CALLS:
    !-------------------------------------------------------------------------------
    !	Input arguments:
        DOUBLE PRECISION, DIMENSION (:) :: GE_VECTOR
    !	Output arguments: 
        DOUBLE PRECISION, DIMENSION (:) :: GE_SUB
    !	Local arguments:
        INTEGER :: I, J ! Counters
        DOUBLE PRECISION :: ESTIMATE(0:1)
        DOUBLE PRECISION :: MAX_ESTIMATE
        DOUBLE PRECISION :: ERROR_I(NEQN)
	
	
        DO I = 1, NSUB
            MAX_ESTIMATE = 0D0
            DO J = 0,1
                ERROR_I = GE_VECTOR((I+(J-1))*NEQN+1:(I+J)*NEQN)
                ESTIMATE(J) = MAXVAL(DABS(ERROR_I))
                IF (ESTIMATE(J) > MAX_ESTIMATE) THEN
                    MAX_ESTIMATE = ESTIMATE(J)
                    GE_SUB((I-1)*NEQN+1:I*NEQN) = ERROR_I
                END IF
  		
            END DO ! J = 0,1
    
        END DO ! I = 1,NSUB
	
    END SUBROUTINE
    
    SUBROUTINE GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM,GEVECTOR)
	!  Computes the Norm and, optionally, a scaled Vector of the global error relative to 
    ! |1-Y(X)|.
    !---------------------------------------------------------------------
    !	CALLED BY: RE_GERROR,HO_GERROR,DC_GERROR
    !	CALLS: CHECK_STAT
    !_____________________________________________________________________
        !   Input arguments:
        DOUBLE PRECISION, DIMENSION(:) :: Y_LOW, Y_HIGH
	
        !	Output arguments: 
        DOUBLE PRECISION:: GENORM  !Scaled Norm of the Global error 
        DOUBLE PRECISION, DIMENSION (:), OPTIONAL :: GEVECTOR !Scaled vector of global error for
						              !each solution point
	
        !	Local Variables:
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: WT !Scale
        INTEGER :: IER ! Memory allocation flag
        INTEGER :: I=1	! Counter
        INTEGER :: ST,EN !Index locations
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: TEMP_GEVECTOR !Temp vector of the 
								        !Global error
    
        !	Alllocate WT, TEMP_GEVECTOR
        ALLOCATE(WT(NEQN),TEMP_GEVECTOR(NEQN*(NSUB+1)),STAT=IER)
        CALL CHECK_STAT(IER)
   
        !	Compute scaled global error vector
        TEMP_GEVECTOR = 0D0
        GENORM = 0D0
        WT=0D0
        DO I = 1,(NSUB+1)
            ST= (I-1)*NEQN+1
            EN = (I-1)*NEQN + NEQN
            WT = 1D0 + ABS(Y_LOW(ST:EN)) 
            TEMP_GEVECTOR(ST:EN) = (Y_LOW(ST:EN)-Y_HIGH(ST:EN))/WT
	           
        END DO !(I = 1,(NSUB+1))

     
        !   Load GEVECTOR
        IF(PRESENT(GEVECTOR)) THEN
            GEVECTOR = TEMP_GEVECTOR
        END IF ! PRESENT(GEVECTOR)
  
        !	Compute scaled global error norm
        GENORM = MAXVAL(DABS(TEMP_GEVECTOR))
    
        !	Deallocate mem
        DEALLOCATE(WT,TEMP_GEVECTOR,STAT=IER)
        CALL CHECK_STAT(IER)
       
    END SUBROUTINE 


    SUBROUTINE HALF_MESH(NSUB,MESH_NEW)
        ! This routine takes as input a mesh of NSUB subintervals stored in the
        ! globally defined vector G_X.  It returns in MESH_NEW a new mesh which 
        ! has 2*NSUB subintervals obtained by halving each subinterval of G_X.
        !------------------------------------------------------------------------------
        !     CALLED BY: MIRKDC, MESH_SELECTOR
        !------------------------------------------------------------------------------
        !   Input variable:
        INTEGER :: NSUB

        !   Output variable:
        DOUBLE PRECISION :: MESH_NEW(0:2*NSUB)

        !   Global variable:
        !   The global vector G_X is a mesh of NSUB subintervals.
        !----------------------------------------------------------------------------  
        MESH_NEW(0:2*NSUB:2)   = G_X(0:NSUB)
        MESH_NEW(1:2*NSUB-1:2) = (G_X(1:NSUB) + G_X(0:NSUB-1) )/2D0

    END SUBROUTINE HALF_MESH
    
    SUBROUTINE HO_GERROR(GSOL,FSUB,BCSUB,DFDY,DBCDY, INFO, GENORM,GEVECTOR)
	!		Uses a solution obtained from a BVP_METHOD+2 order MIRKDC
	!		method in order to estimate the global error in solution component 
	!		y(i)  relative to 1 + |y(i)|. Return the maximum GE over all 
	!       solution components and mesh points as YERROR. The same estimate 
	!       applies to unknown parameters.  The estimate is computed 
    !       by using higher-order MIRK formulas.
    !-----------------------------------------------------------------------------
    ! CALLED BY: BVP_SOLVER
    ! CALLS BVP_EVAL, NEWITER, GLOBAL_ERROR
    !-----------------------------------------------------------------------------
        
    !   Input arguments:
        TYPE(BVP_SOL) :: GSOL
        EXTERNAL :: FSUB ! User-supplied ODEs routine
        EXTERNAL :: BCSUB ! User-supplied boundary conditions routine
        EXTERNAL :: DFDY ! Partial-derivative routine for ODEs
        EXTERNAL :: DBCDY ! Partial-derivative routine for boundary conditions
        INTEGER  :: INFO ! Success fla
	
    !   Output Arguments:
        DOUBLE PRECISION, OPTIONAL, DIMENSION (:) :: GEVECTOR ! Global-error vector 
        DOUBLE PRECISION :: GENORM ! Global-error norm 
	
    !   Local variables
        INTEGER :: IER ! Memory allocation flag
        INTEGER :: ST,EN ! Array location
        INTEGER :: I ! Counters
        INTEGER :: NDEX !Work Array Locatoion for solution
        INTEGER :: GE_INFO ! Newton solver flag
        INTEGER :: NITERS ! Number of Newton Iterations
        DOUBLE PRECISION, ALLOCATABLE,DIMENSION (:) :: Y_HIGH, Y_LOW ! Hold solutions
    !   Holds the result from the Newton solver.
        DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: GE_Y,GE_X,GE_K_DISCRETE
        REAL T
	
	
    !   Allocate Arrays
        ALLOCATE(GE_X(0:NSUB),GE_Y((NSUB+1)*NEQN),STAT=IER)
        CALL CHECK_STAT(IER)
    
        ALLOCATE(Y_HIGH(NEQN*(NPTS)),Y_LOW(NEQN*(NPTS)),STAT=IER)
        CALL CHECK_STAT(IER)  
    
    !   Fill arrays
        GE_X = GSOL%X
        NDEX = C_S_STAR*NEQN*NSUB + (NSUB+1)
        GE_Y = GSOL%WORK(NDEX+1:NDEX+(NSUB+1)*NEQN)
	
    !	Set Higher MIRK orders
        SELECT CASE (BVP_METHOD)
            CASE (2)
                C_S = 3
                C_S_STAR = 4
                BVP_METHOD = 4
            CASE (4)
                C_S = 5
                C_S_STAR = 8
                BVP_METHOD=6
            CASE (6)
                C_S = 9
                C_S_STAR = 8
                BVP_METHOD = 8
        END SELECT 
        DEALLOCATE(C_C,C_V,C_B,C_X,STAT=IER)
        CALL CHECK_STAT(IER)
	
    !    Defines MIRK coefficents
        CALL RK_TABLEAU
     
     
    !	 Allocate mem for Newton's method
        ALLOCATE(GE_K_DISCRETE(C_S*NEQN*(NSUB)),STAT=IER)
        CALL CHECK_STAT(IER)
   
    
    !	Solve with Newton's method. First set the logical operator to use a fixed Newton
    !   matrix.  Might be changed to include it as a parameter
	
	
        UHO = .TRUE.
        Y_LOW = GE_Y
        CALL NEWITER(NEQN,NSUB,GE_X,GE_Y,GE_INFO,GE_K_DISCRETE,             &
        FSUB,BCSUB,DFDY,DBCDY, USAGE=2)
                         
        UHO = .FALSE.
    
        IF (GE_INFO /= 0) THEN
            IF (G_STOP_ON_FAIL) THEN
                PRINT *,'Computation of higher-order error failed--the solution SOL is in doubt.'
                PRINT *,'INFO: ', GE_INFO
              !STOP
            END IF
        END IF
    
        INFO=GE_INFO
        
    !   Set Original MIRK Orders
        SELECT CASE (BVP_METHOD)
            CASE (4)
                C_S = 2
                C_S_STAR = 2
                BVP_METHOD = 2


            CASE (6)
                C_S = 3
                C_S_STAR = 4
                BVP_METHOD= 4
            CASE (8)
                C_S = 5
                C_S_STAR = 8
                BVP_METHOD = 6 
        END SELECT
        DEALLOCATE(C_C,C_V,C_B,C_X,STAT=IER)
        CALL CHECK_STAT(IER)
	
    !    Defines MIRK coefficents
        CALL RK_TABLEAU
    
    !	Get Both Solutions
        Y_HIGH = GE_Y
        Y_LOW = GSOL%WORK(NDEX+1:NDEX+(NSUB+1)*NEQN) 

    !	Get an estimate of the scaled global-error norm
    !	and optionally a scaled global-error vector
        GENORM = 0D0
        IF (PRESENT(GEVECTOR)) THEN
            CALL GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM,GEVECTOR)
        ELSE
            CALL GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM)
        END IF

    !	Deallocate Memory
        DEALLOCATE(GE_X,GE_Y,GE_K_DISCRETE,STAT=IER)
        CALL CHECK_STAT(IER)
        DEALLOCATE(Y_LOW,Y_HIGH,STAT=IER)
        CALL CHECK_STAT(IER)
 	
    END SUBROUTINE
    
    SUBROUTINE HYBRID_ERROR(ALPHA,BETA,GE_SUB,DEFECT_SUB,HYBRID_SUB)
    ! This routine assigns a hybrid error estimate for each subinterval
    !-------------------------------------------------------------------------------
    !	CALLED BY: MIRKDC
    !	CALLS:
    !-------------------------------------------------------------------------------
    !	Input arguments:
        DOUBLE PRECISION :: ALPHA !Defect factor
        DOUBLE PRECISION :: BETA  !Global error factor
        DOUBLE PRECISION, DIMENSION (:) :: GE_SUB !Global error estimate for each suninterval
        DOUBLE PRECISION, DIMENSION (:) :: DEFECT_SUB !Defect estimate for each subinterval
	
    !	Output arguments:
        DOUBLE PRECISION, DIMENSION (:) :: HYBRID_SUB!Hybrid error estimate for each subinterval
	
    !	Local arguments:
        INTEGER :: I ! Counter
        INTEGER :: ST,EN !Start and End of error estimate for each subinterval
	
    !   Linear combination of the error measurements. 
        DO I=1,NSUB
            ST= (I-1)*NEQN+1
            EN = (I-1)*NEQN + NEQN
            HYBRID_SUB(ST:EN) =  ALPHA*DABS(DEFECT_SUB(ST:EN))+BETA*DABS(GE_SUB(ST:EN))
	   
        END DO !(I=1,NSUB)
		
    END SUBROUTINE


    SUBROUTINE INTERP_EVAL(NEQN,NSUB,MESH,Y,T,Z,K_DISCRETE,K_INTERP)
        ! This routine uses the current discrete solution approximation Y 
        ! on a mesh of NSUB subintervals stored in MESH plus additional stage 
        ! information associated with the underlying continuous Runge-Kutta 
        ! method provided in K_DISCRETE and K_INTERP to evaluate the continuous
        ! solution approximation at T.  The result is returned in Z, a vector of 
        ! length NEQN, where NEQN is the number of differential equations, NODE, 
        ! plus the number of unknown parameters, NPAR. The first NODE locations 
        ! of Z contain the approximation to the solution and the remaining locations 
        ! contain the approximation to the unknown parameters.
        !---------------------------------------------------------------------------
        !     CALLED BY: MIRKDC
        !     CALLS: INTERVAL,INTERP_WEIGHTS,SUM_STAGES
        !---------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1)),T,                         &
        K_DISCRETE(MXS*NEQN*NSUB),K_INTERP(MXS*NEQN*NSUB)
        !   Output arguments:
        DOUBLE PRECISION :: Z(NEQN)

        !   Local variables:
        INTEGER :: I ! Index of subinterval.
        DOUBLE PRECISION :: HI ! Size of subinterval I.
        DOUBLE PRECISION :: TAU ! Relative postion of T in subinterval I.
        DOUBLE PRECISION :: WEIGHTS(MXS) ! Weight polynomials evaluated at TAU.
  
        !   Global variables:
        !   MXS is the maximum number of Runge-Kutta stages.
        !   C_S and C_C_STAR, are number of discrete Runge-Kutta stages and number of 
        !   continuous Runge-Kutta stages, respectively. 
        !-----------------------------------------------------------------------------                                           
        !   The INTERVAL subroutine returns the index of an interval containing T.
        CALL INTERVAL(NSUB,MESH,T,I)
                                                                        
        !   Compute TAU, the relative location of T in subinterval I.
        HI = MESH(I) - MESH(I-1)
        TAU = (T - MESH(I-1))/HI

        !   Evaluate WEIGHTS at TAU.
        CALL INTERP_WEIGHTS(C_S_STAR,TAU,WEIGHTS)

        !   Evaluate interpolant at TAU by taking weighted sum of Runge-Kutta stages 
        !   through a call to SUM_STAGES. Result is returned in Z. 
        CALL SUM_STAGES(NEQN,HI,Y((I-1)*NEQN+1),C_S,K_DISCRETE((I-1)*C_S*NEQN+1),    &
        C_S_STAR,K_INTERP((I-1)*(C_S_STAR-C_S)*NEQN+1),WEIGHTS,Z)
    END SUBROUTINE INTERP_EVAL
    
    

      
    SUBROUTINE INTERP_SETUP(NEQN,TIM1,HI,YIM1,YI,S,KI_DISCRETE,S_STAR,KI_INTERP,   &
    FSUB)
        ! This routine constructs the S_STAR-S extra stages needed by the interpolant for 
        ! a single subinterval. S is the number of stages of the discrete Runge-Kutta  
        ! method; S_STAR is the number of stages of the continuous Runge-Kutta method. 
        ! NEQN is the number of differential equations plus the number of unknown  
        ! parameters. HI is the width of the subinterval; TIM1 is t_{i-1}, the left  
        ! endpoint of the subinterval; YIM1 is y_{i-1}, the solution approximation at  
        ! t_{i-1}; YI is y_i, the solution approximation at t_i. KI_DISCRETE contains  
        ! the S stages of the discrete formula Runge-Kutta on subinterval i. The 
        ! interpolant will use these together with the extra stages computed by this 
        ! routine, which are returned in KI_INTERP. FSUB is provided by the user to 
        ! define the ODEs.
        !-------------------------------------------------------------------------------
        !     CALLED BY: DEFECT_ESTIMATE
        !     CALLS: P_FSUB
        !---------------------------------------------------------------------------- 
        !   Input arguments:
        INTEGER :: NEQN,S,S_STAR
        DOUBLE PRECISION :: TIM1,HI,YIM1(NEQN),YI(NEQN),KI_DISCRETE(S*NEQN)

        !   Output arguments:
        DOUBLE PRECISION :: KI_INTERP((S_STAR-S)*NEQN)

        !   User-supplied subroutines:
        EXTERNAL FSUB

        !   Local variables:
        INTEGER :: R,J ! Loop indexes.
        DOUBLE PRECISION :: Y_ARG(NEQN) ! Argument for the function evaluation for 
                                        ! each new stage.
        !   Global variables:
        !   C_C_STAR, C_V_STAR, C_X_STAR are Runge-Kutta coefficients.
        !----------------------------------------------------------------------------
        DO R = 1,S_STAR-S
      
            Y_ARG = 0D0

            !     Contributions from the discrete stages.                    
            DO J = 1,S
                Y_ARG = Y_ARG + C_X_STAR(R,J)*KI_DISCRETE((J-1)*NEQN+1:J*NEQN)
            END DO

            !     Contributions from the previous computed new stages.
            DO J = 1,R-1
                Y_ARG = Y_ARG + C_X_STAR(R,S+J)*KI_INTERP((J-1)*NEQN+1:J*NEQN)
            END DO

            !     Complete computation of Y_ARG.
            Y_ARG = C_V_STAR(R)*YI + (1D0-C_V_STAR(R))*YIM1 + HI*Y_ARG

            !     Evaluate P_FSUB at Y_ARG and TIM1+C_C_STAR(R)*HI to get new stage. 
            !     Stage value is stored in KI_INTERP.
            CALL P_FSUB(FSUB,NEQN,TIM1+C_C_STAR(R)*HI,Y_ARG,KI_INTERP((R-1)*NEQN+1))
  
        END DO
    
    END SUBROUTINE INTERP_SETUP
  
    SUBROUTINE INTERP_SETUP_MESH (NEQN, NSUB, MESH, Y, K_DISCRETE, &
    K_INTERP, FSUB)
        ! This routine constructs the S_STAR-S extra stages needed by the interpolant for 
        ! a every subinterval. 
        !--------------------------------------------------------------------------
        !	 CALLED BY:
        !    CALLS: 
        !--------------------------------------------------------------------------
        ! Input Arguments:
        INTEGER :: NEQN ! Number of equations
        INTEGER :: NSUB ! Number of subintervals in the mesh
        DOUBLE PRECISION :: MESH (0:NSUB) ! Mesh
        DOUBLE PRECISION :: Y(NEQN*(NSUB+1)) ! Solution
        DOUBLE PRECISION :: K_DISCRETE(MXS*NEQN*NSUB) ! Discrete stages
        DOUBLE PRECISION :: K_INTERP(MXS*NEQN*NSUB) ! Interp stages
        EXTERNAL :: FSUB
  
        ! Local Arguments 
        INTEGER :: I ! Counter
        DOUBLE PRECISION :: HI ! Sepsize 
  
        DO I=1,NSUB
            HI = MESH(I) - MESH(I-1)
            CALL INTERP_SETUP(NEQN,MESH(I-1),HI,Y((I-1)*NEQN+1),Y(I*NEQN+1),C_S,       &
            K_DISCRETE((I-1)*C_S*NEQN+1),C_S_STAR,                 &   
            K_INTERP((I-1)*(C_S_STAR-C_S)*NEQN+1),FSUB)
        END DO !I=1,NSUB

    END SUBROUTINE


    SUBROUTINE INTERP_TABLEAU
        ! This routine defines the extra coefficients for the continuous extension of the
        ! discrete Runge-Kutta scheme, i.e., the interpolant. The global variable 
        ! BVP_METHOD indicates which Runge-Kutta formula is to be used. This routine also 
        ! defines the sample point for the defect associated with this interpolant and 
        ! the order of the interpolant.
        !--------------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !     CALLS: CHECK_STAT
        !--------------------------------------------------------------------------------
        !   Local variable: 
        INTEGER IER ! error flag for allocation call.
        !
        !   Global variables:
        !   BVP_METHOD identifies which continuous Runge-Kutta method is to be set up. 
        !   C_S, C_S_STAR are the number of discrete Runge-Kutta stages and the number of 
        !     continuous Runge-Kutta stages, respectively. 
        !   C_C_STAR, C_V_STAR, C_X_STAR are Runge-Kutta coefficients for the 
        !     continuous scheme.
        !   C_P is the order of the continuous Runge-Kutta formula.  
        !   C_TAU_STAR is the relative sample point location for the defect 
        !     sampling process. 
        !--------------------------------------------------------------------------------
        ALLOCATE(C_C_STAR(C_S_STAR-C_S),C_V_STAR(C_S_STAR-C_S),                      &
        C_X_STAR(C_S_STAR-C_S,C_S_STAR),STAT=IER)
        CALL CHECK_STAT(IER)
   
        SELECT CASE (BVP_METHOD)
            CASE (2)
                !       The interpolant for the 2-stage, 2nd order MIRK, stage order 1, requires  
                !       no extra stages. C_S_STAR has been set to 2 in BVP_SOLVER.
                C_TAU_STAR = 0.25D0
                C_P = 2
            CASE (4)
                !       Define coefficients for a 4th order, 4-stage, stage order 3 continuous 
                !       scheme. There is one extra stage. C_S_STAR has been set to 4 in 
                !       BVP_SOLVER.
        
                C_C_STAR(1) = 2D0/5D0
        
                C_V_STAR(1) = 2D0/5D0
        
                !       Column 1 of C_X_STAR
                C_X_STAR(1,1) = 17D0/125D0

                !       Column 2 of C_X_STAR
                C_X_STAR(1,2) = -13D0/125D0

                !       Column 3 of C_X_STAR
                C_X_STAR(1,3) = -4D0/125D0

                !       Column 4 of C_X_STAR
                C_X_STAR(1,4) = 0D0

                C_TAU_STAR = 0.226D0
                C_P = 4
            CASE (6)
                !       Define coefficients for an 8-stage, 6th order, stage order 3 continuous 
                !       scheme. There are three extra stages. C_S_STAR has been set to 8 in 
                !       BVP_SOLVER.

                C_C_STAR(1) = 1D0/2D0
                C_C_STAR(2) = 1D0/2D0 - SQRT(7D0)/14D0
                C_C_STAR(3) = 87D0/100D0

                C_V_STAR(1) = 1D0/2D0
                C_V_STAR(2) = 1D0/2D0 - SQRT(7D0)/14D0
                C_V_STAR(3) = 87D0/100D0

                !       Column 1 of C_X_STAR
                C_X_STAR(1,1) = 1D0/64D0
                C_X_STAR(2,1) = 3D0/112D0 + 9D0/1960D0*SQRT(7D0)
                C_X_STAR(3,1) = 2707592511D0/1000000000000D0 - 1006699707D0/             &
                1000000000000D0*SQRT(7D0)

                !       Column 2 of C_X_STAR
                C_X_STAR(1,2) = -1D0/64D0
                C_X_STAR(2,2) = -3D0/112D0 + 9D0/1960D0*SQRT(7D0)
                C_X_STAR(3,2) = -51527976591D0/1000000000000D0 - 1006699707D0/           &
                1000000000000D0*SQRT(7D0)

                !       Column 3 of C_X_STAR
                C_X_STAR(1,3) = 7D0/192D0*SQRT(21D0)
                C_X_STAR(2,3) = 11D0/840D0*SQRT(7D0) + 3D0/112D0*SQRT(7D0)*SQRT(3D0)
                C_X_STAR(3,3) = -610366393D0/75000000000D0 + 7046897949D0/               &
                1000000000000D0*SQRT(7D0) + 14508670449D0/1000000000000D0                &
                *SQRT(7D0)*SQRT(3D0)

                !       Column 4 of C_X_STAR
                C_X_STAR(1,4) = -7D0/192D0*SQRT(21D0)
                C_X_STAR(2,4) = 11D0/840D0*SQRT(7D0 )- 3D0/112D0*SQRT(7D0)*SQRT(3D0)
                C_X_STAR(3,4) = -610366393D0/75000000000D0 + 7046897949D0/               &
                1000000000000D0*SQRT(7D0) - 14508670449D0/1000000000000D0                &
                *SQRT(7D0)*SQRT(3D0)

                !       Column 5 of C_X_STAR
                C_X_STAR(1,5) = 0D0
                C_X_STAR(2,5) = 88D0/5145D0*SQRT(7D0)
                C_X_STAR(3,5) = -12456457D0/1171875000D0 + 1006699707D0/                 &
                109375000000D0*SQRT(7D0)

                !       Column 6 of C_X_STAR
                C_X_STAR(1,6) = 0D0
                C_X_STAR(2,6) = -18D0/343D0*SQRT(7D0)
                C_X_STAR(3,6) = 3020099121D0/437500000000D0*SQRT(7D0) +                  &
                47328957D0/625000000D0

                !       Column 7 of C_X_STAR
                C_X_STAR(1,7) = 0D0
                C_X_STAR(2,7) = 0D0
                C_X_STAR(3,7) = -7046897949D0/250000000000D0*SQRT(7D0)

                !       Column 8 of C_X_STAR
                C_X_STAR(1,8) = 0D0
                C_X_STAR(2,8) = 0D0
                C_X_STAR(3,8) = 0D0

                C_TAU_STAR = 0.4D0
                C_P = 6
        END SELECT

    END SUBROUTINE INTERP_TABLEAU
      
    
    SUBROUTINE INTERP_WEIGHTS(S_STAR,TAU,W,WP)
        ! This routine evaluates the weight polynomials, and optionally their 
        ! first derivatives, at the relative position TAU with a subinterval. The
        ! weight polynomials correspond to the S_STAR stages used to form the 
        ! continuous Runge-Kutta interpolant. The results are returned in W and WP. 
        !----------------------------------------------------------------------------
        !     CALLED BY: DEFECT_ESTIMATE,INTERP_EVAL,SOL_EVAL
        !----------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: S_STAR
        DOUBLE PRECISION :: TAU

        !   Output arguments:
        DOUBLE PRECISION :: W(S_STAR)
        DOUBLE PRECISION, OPTIONAL :: WP(S_STAR)
  
        !   Global variables:
        !   BVP_METHOD identifies which discrete and continuous Runge-Kutta methods are 
        !     being used, and thus which polynomials should be evaluated.
        !----------------------------------------------------------------------------
        SELECT CASE (BVP_METHOD)
            CASE (2)
                !       2-stage, 2nd order CMIRK formula.
                W(1) = TAU*(1D0 - TAU/2D0)
                W(2) = TAU**2 /2D0  
                IF ( PRESENT(WP) ) THEN  ! Derivative polynomials
                    WP(1) = 1D0 - TAU
                    WP(2) = TAU
                END IF
            CASE (4)
                !       Improved 4-stage, 4th order CMIRK formula.
                W(1) = -TAU*(3D0*TAU-4D0)* (5D0*TAU**2 - 6D0*TAU + 3D0)/12D0
                W(2) = TAU**2*(5D0*TAU**2 - 6D0*TAU + 2D0)/6D0
                W(3) = -2D0/3D0*TAU**2*(3D0*TAU - 2D0)*(5D0*TAU - 6D0)
                W(4) = 125D0/12D0*TAU**2*(TAU - 1D0)**2
                IF ( PRESENT(WP) ) THEN  ! Derivative polynomials
                    WP(1) = -(TAU-1.D0)*(5.D0*TAU-2.D0)*(2.D0*TAU-1.D0)/2.D0
                    WP(2) = TAU*(5D0*TAU - 2D0)*(2D0*TAU - 1D0)/3D0
                    WP(3) = -8D0*TAU*(TAU - 1D0)*(5D0*TAU - 2D0)
                    WP(4) = 125D0/6D0*TAU*(TAU - 1D0)*(2D0*TAU - 1D0)
                END IF
            CASE (6)
                !       Improved 8-stage, 6th order CMIRK formula. 
                W(1) =                                                                   &
                -(12233+1450*SQRT(7D0))*(800086000*TAU**5+63579600*                      &
                SQRT(7D0)*TAU**4-2936650584D0*TAU**4+4235152620D0*TAU**3                 &
                -201404565*SQRT(7D0)*TAU**3+232506630D0*SQRT(7D0)*TAU**2                 &
                -3033109390D0*TAU**2+1116511695D0*TAU-116253315D0*SQRT(7D0)              &
                *TAU+22707000D0*SQRT(7D0)-191568780D0)*TAU/2112984835740D0

                W(2) = -(-10799D0+650D0*SQRT(7D0))*(24962000D0*TAU**4+                   &
                473200D0*SQRT(7.D0)*TAU**3-67024328D0*TAU**3-751855D0*                   &
                SQRT(7D0)*TAU**2+66629600D0*TAU**2-29507250D0*TAU+236210*                &
                SQRT(7D0)*TAU+5080365D0+50895D0*SQRT(7D0))*TAU**2/                       &
                29551834260D0

                W(3) = 7D0/1274940D0*(259D0+50D0*SQRT(7D0))*                             &
                (14000D0*TAU**4-48216D0*TAU**3+1200D0*SQRT(7D0)*TAU**3-                  &
                3555D0*SQRT(7D0)*TAU**2+62790D0*TAU**2+3610D0*SQRT(7D0)*                 &
                TAU-37450D0*TAU+9135D0-1305D0*SQRT(7D0))*TAU**2

                W(4) = 7D0/1274940D0*(259D0+50D0*SQRT(7D0))*(14000D0*TAU**4              &
                -48216D0*TAU**3+1200D0*SQRT(7D0)*TAU**3-3555D0*SQRT(7D0)*                &
                TAU**2+62790D0*TAU**2+3610D0*SQRT(7D0)*TAU-37450D0*TAU+                  &
                9135D0-1305D0*SQRT(7D0))*TAU**2

                W(5) = 16D0/2231145D0*(259D0+50D0*SQRT(7D0))*(14000D0*                   &
                TAU**4- 48216D0*TAU**3+1200D0*SQRT(7D0)*TAU**3-3555D0*                   &
                SQRT(7D0)*TAU**2+62790D0*TAU**2+3610D0*SQRT(7D0)*                        &
                TAU-37450D0*TAU+9135D0-1305D0*SQRT(7D0))*TAU**2

                W(6) = 4D0/1227278493D0*(740D0*SQRT(7D0)-6083D0)*                        &
                (1561000D0*TAU**2-2461284D0*TAU-109520D0*SQRT(7D0)*TAU+                  &
                979272D0+86913D0*SQRT(7D0))*(TAU-1D0)**2*TAU**2

                W(7) = -49D0/63747D0*SQRT(7D0)*(20000D0*TAU**2-20000D0*                  &
                TAU+3393D0)*(TAU-1D0)**2*TAU**2

                W(8) = -1250000000D0/889206903D0*(28D0*TAU**2-28D0*TAU+                  &
                9D0)*(TAU-1D0)**2*TAU**2

                IF ( PRESENT(WP) ) THEN  ! Derivative polynomials
                    WP(1) = (1450D0*SQRT(7D0)+12233D0)*(14D0*TAU-7D0+                      &
                    SQRT(7D0))*(TAU-1D0)*(-400043D0*TAU+75481D0+2083D0*                    &
                    SQRT(7D0))*(100D0*TAU-87D0)*(2D0*TAU-1D0)/493029795006D0

                    WP(2) = -(650D0*SQRT(7D0)-10799D0)*(14D0*TAU-7D0+                      &
                    SQRT(7D0))*(37443D0*TAU-13762D0-2083D0*SQRT(7D0))*                     &
                    (100D0*TAU-87D0)*(2D0*TAU-1D0)*TAU/20686283982D0

                    WP(3) = 7D0/42498D0*(259D0+50D0*SQRT(7D0))*(14D0*TAU-                  &
                    7D0+SQRT(7D0))*(TAU-1D0)*(100D0*TAU-87D0)*(2D0*TAU-1D0)*TAU
          
                    WP(4) = 7D0/42498D0*(259D0+50D0*SQRT(7D0))*(14D0*TAU-                  &
                    7D0+SQRT(7D0))*(TAU-1D0)*(100D0*TAU-87D0)*(2D0*TAU-1D0)*TAU

                    WP(5) = 32D0/148743D0*(259D0+50D0*SQRT(7D0))*(14D0*TAU-                &
                    7D0+SQRT(7D0))*(TAU-1D0)*(100D0*TAU-87D0)*(2D0*TAU-1D0)*TAU

                    WP(6) = 4D0/1227278493D0*(740D0*SQRT(7D0)-6083D0)*                     &
                    (14D0*TAU-7D0+SQRT(7D0))*(TAU-1D0)*(100D0*TAU-87D0)*                   &
                    (6690D0*TAU-4085D0-869D0*SQRT(7D0))*TAU

                    WP(7) = -98D0/21249D0*SQRT(7D0)*(TAU-1D0)*(100D0*TAU-                  &
                    13D0)*(100D0*TAU-87D0)*(2D0*TAU-1D0)*TAU

                    WP(8) = -1250000000D0/2074816107D0*(14D0*TAU-7D0+SQRT(7D0))*           &
                    (TAU-1D0)*(14D0*TAU-7D0-SQRT(7D0))*(2D0*TAU-1D0)*TAU
                END IF

        END SELECT

    END SUBROUTINE INTERP_WEIGHTS
  

    SUBROUTINE INTERVAL(NSUB,MESH,T,I)
        ! Finds I such that MESH(I-1) =< T < MESH(I).  Returns 1 if T <= MESH(0) 
        ! and NSUB if MESH(NSUB) <= T. MESH contains the current mesh of NSUB
        ! subintervals.
        !---------------------------------------------------------
        !     CALLED BY: INTERP_EVAL, SOL_EVAL
        !----------------------------------------------------------
        !   Input arguments:
        INTEGER :: NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),T

        !   Output arguments:
        INTEGER :: I

        !   Local variables:
        INTEGER :: NDX(1)
        INTEGER :: MINS,MAXS,MID
        LOGICAL :: SUCC
        !----------------------------------------------------------

        IF ( T <= MESH(0) ) THEN
            I = 1
        ELSE IF ( MESH(NSUB) <= T ) THEN
            I = NSUB
        ELSE
            
            !NDX = MINLOC( MESH,MASK=(MESH >= T) )
            MINS = 0
            MAXS = NSUB
            SUCC = .TRUE.
            DO WHILE (SUCC)
                MID = (MINS+MAXS)/2D0
                !write (*,*) "MID: ", MID
                IF ( T >= MESH(MID) .AND. T < MESH(MID+1)) THEN
                    SUCC = .FALSE.
                    I=MID
                ELSE IF (T > MESH(MID)) THEN
                    MINS=MID+1
                ELSE
                    MAXS=MID-1
                END IF
            END DO
            I=I+1
        END IF

    END SUBROUTINE INTERVAL

    SUBROUTINE JACBLK(NEQN,HI,TI,YI,YIP1,K,LLI,RRI,FSUB,DFSUB)
        ! This routine computes the left and right blocks of the Jacobian of the 
        ! residual function associated with the subinterval I. They are returned 
        ! in the vectors LLI and RRI.  Each is of size NEQN**2, where NEQN is 
        ! the number of ODEs plus the number of unknown parameters. The size of the 
        ! subinterval I is HI and its left endpoint is TI. The solution at the left 
        ! end of the subinterval is stored in YI and the solution at the right end is
        ! stored in YIP1. This computation uses the previously computed Runge-Kutta 
        ! stages provided in the vector K. FSUB and DFSUB are user-supplied routines
        ! for the ODEs and their derivatives.
        !----------------------------------------------------------------------------
        !     CALLED BY: NEWMAT
        !     CALLS: P_DF,PD_F
        !----------------------------------------------------------------------------
        !   Modified by Mark Adams, 1996, to avoid unnecessary matrix multiplies.
        !----------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN
        DOUBLE PRECISION :: HI,TI,YI(NEQN),YIP1(NEQN),K(MXS*NEQN)

        !   Output arguments:
        DOUBLE PRECISION :: LLI(NEQN**2),RRI(NEQN**2)

        !   User-supplied subroutines:
        EXTERNAL FSUB,DFSUB
    
        !   Local variables:
        INTEGER :: I,R,J,I1,I2 ! Loop and array indicies.
        DOUBLE PRECISION :: SUM ! Accumulator.
        INTEGER :: ALPHA, BETA, SIGMA ! Array indicies.      
        DOUBLE PRECISION :: TR ! T argument for Rth DF/DY evaluation.
            
        LOGICAL :: DKDYI_ZERO(MXS) ! Records presence of zeros in partial
        LOGICAL :: DKDYIP1_ZERO(MXS) ! derivatives to avoid matrix multiplies.
            
        LOGICAL :: MATRIX_MULT_DKDYI ! Indicator of special form of Runge-Kutta
        LOGICAL :: MATRIX_MULT_DKDYIP1 ! stage that can avoid matrix multiplies.
            
        DOUBLE PRECISION :: YR(NEQN) ! Y argument for Rth DF/DY evaluation.
        DOUBLE PRECISION :: PD(NEQN,NEQN) ! Store partial derivative.
        DOUBLE PRECISION :: DFDY(NEQN**2) ! Reshaped PD.
        DOUBLE PRECISION :: UR(NEQN**2) ! Intermediate values arising in 
                                        ! computation of partial derivative.                                           
            
        DOUBLE PRECISION :: DKDYI(NEQN**2*MXS)   ! Partial derivatives wrt
        DOUBLE PRECISION :: DKDYIP1(NEQN**2*MXS) ! YI and YIP1 associated with
                                                 ! the derivatives of the Runge-
                                                 ! Kutta stages.

        !   Global variables:
        !   MXS is the number of maximum number of Runge-Kutta stages.
        !   NEQNSQ equals NEQN**2.
        !   C_S is the number of discrete Runge-Kutta stages. 
        !   The vectors C_C, C_V, C_B, and the array C_X contain the coefficients 
        !   that define the discrete Runge-Kutta formula. 
        !------------------------------------------------------------------------------
        !   Compute partial derivatives in pairs.  In Rth pass through the loop, form 
        !   d(KR)/d(YI) and d(KR)/d(YI+1) and store them in the appropriate segments of 
        !   DKDYI and DKDYIP1.
        !
        !   Set DKDYI_ZERO and DKDYIP1_ZERO to .TRUE. to indicate that all of the partial  
        !   derivatives are currently zero.
        DKDYI_ZERO   = .TRUE.
        DKDYIP1_ZERO = .TRUE.
      
        DO R = 1,C_S
            !     PART 1: COMPUTE DF/DY. 
            !     Define abscissa.
            TR = TI + C_C(R)*HI
      
            !     Construct Y argument.
            YR = 0D0
            !     Add on weighted contributions from first R-1 stages; stage J is  
            !     available in the Jth set of NEQN locations of K.
            DO J = 1,R-1
                I1 = (J-1)*C_S + R
                I2 = (J-1)*NEQN
                !       If C_X(R,J) is nonzero ...
                IF ( ABS(C_X(R,J)) > 0D0 ) THEN
                    YR = YR + C_X(R,J)*K(I2+1:I2+NEQN)
                END IF
            END DO
            YR = (1D0 - C_V(R))*YI + C_V(R)*YIP1 + HI*YR
 
            !     Arguments for DFDY are ready; compute DFDY by calling P_DF if analytic
            !     form is available or by calling PD_F to approximate the derivatives 
            !     using finite differences.
            IF (HAVE_DFDY) THEN
                CALL P_DF(DFSUB,TR,YR,PD,NEQN)
            ELSE
                CALL PD_F(FSUB,TR,YR,PD,NEQN)
            END IF

            !     Output from P_DF or PD_F is a 2D array; reshape into vector DFDY.
            DFDY = RESHAPE(PD,(/NEQNSQ/))


            !     PART 2: COMPUTE D(KR)/D(YI) AND STORE RESULT IN DKDYI.
            !     Set MATRIX_MULT_DKDYI to .TRUE. indicating that the partial derivative 
            !     is not of the form (1-C_V(R))*dF/dY and that multiplying the matrix UR
            !     by DFDY may be necessary.
            MATRIX_MULT_DKDYI = .TRUE.
      
            !     Accumulate weighted linear combination of previous R-1 partial 
            !     derivative matrices in UR. 
            UR = 0D0
            DO J = 1, R-1
                I1 = (J-1)*C_S + R
                I2 = (J-1)*NEQNSQ

                !       Skip if C_X(I1) = 0 or the Jth partial derivative is zero. 
                IF ( ABS(C_X(R,J)) > 0D0  .AND. ( .NOT. DKDYI_ZERO(J) ) ) THEN
                    DKDYI_ZERO(R) = .FALSE. ! UR nonzero and we will need to do a 
                                            ! matrix multiply later.
                    UR = UR + C_X(R,J)*DKDYI(I2+1:I2+NEQNSQ)
                END IF
            END DO
           
            !     If UR is nonzero, scale it by HI.
            IF ( .NOT. DKDYI_ZERO(R) ) THEN
                UR = HI*UR
            END IF

            ALPHA = (R-1)*NEQNSQ
            IF ( ABS(C_V(R) - 1D0) > 0D0 ) THEN
                IF ( DKDYI_ZERO(R) ) THEN
                    !         Multiplying UR by DFDY will be unnecessary later in the computation.
                    MATRIX_MULT_DKDYI = .FALSE.
                    DKDYI(ALPHA+1:ALPHA+NEQNSQ) = ( 1D0 - C_V(R) )*DFDY
                ELSE
                    UR(1:NEQNSQ:NEQN+1) = 1D0 - C_V(R) + UR(1:NEQNSQ:NEQN+1)
                END IF

                !       Partial derivative of stage R is nonzero.
                DKDYI_ZERO(R) = .FALSE.
            END IF

            !     Multiply UR and DFDY. Since both are stored as vectors, we code
            !     the multiply with loops, dot products, and array index offsets.
            !
            !     Skip the multiplication if UR is zero or if partial derivative is
            !     just (1-C_V(R))*DFDY.
            IF ( MATRIX_MULT_DKDYI ) THEN
                IF ( .NOT. DKDYI_ZERO(R) ) THEN
                    DO I =1, NEQN
                        BETA = ALPHA + I
                        DO J = 1,NEQN
                            SIGMA = (J-1)*NEQN
                            DKDYI(SIGMA+BETA) = DOT_PRODUCT(DFDY(I:(NEQN-1)*NEQN+I:NEQN),      &
                            UR(SIGMA+1:SIGMA+NEQN) )
                        END DO
                    END DO
                ELSE
                    !         If UR is zero, then the Rth partial derivative is zero. Value of 
                    !         DKDYI_ZERO(R) will left as .TRUE. to avoid later matrix multiplies.
                    DKDYI(ALPHA+1:ALPHA+NEQNSQ) = 0D0
                END IF ! .NOT. DKDYI_ZERO(R) 
            END IF ! MATRIX_MULT_DKDYI


            !     PART 3: COMPUTE D(KR)/D(YI+1) AND STORE RESULT IN DKDYIP1.
            !     Set MATRIX_MULT_DKDYIP1 to .TRUE. This indicates that the partial derivative 
            !     is not of the form C_V(R)*dF/dY and that multiplying the matrix UR by
            !     DFDY may be necessary.
            MATRIX_MULT_DKDYIP1 = .TRUE.

            !     Accumulate weighted linear combination of previous R-1 partial 
            !     derivative matrices in UR.                  
            UR = 0D0
            DO J = 1, R-1
                I1 = (J-1)*C_S + R
                I2 = (J-1)*NEQNSQ
                !       Skip if C_X(I1) is zero or the Jth partial derivative is zero.    
                IF ( ABS(C_X(R,J)) > 0D0  .AND.( .NOT. DKDYIP1_ZERO(J)) ) THEN
                    DKDYIP1_ZERO(R) = .FALSE. ! UR is nonzero, so we will need to do a  
                                              ! matrix multiply later.
                    UR = UR + C_X(R,J)*DKDYIP1(I2+1:I2+NEQNSQ)
                END IF
            END DO

            !     If UR is nonzero, scale it.
            IF ( .NOT. DKDYIP1_ZERO(R) ) THEN
                UR = HI*UR
            END IF

            IF ( ABS(C_V(R)) > 0D0 ) THEN
                IF ( DKDYIP1_ZERO(R) ) THEN
                    !         Multiplying UR by DFDY will be unnecessary later in the computation.
                    MATRIX_MULT_DKDYIP1 = .FALSE.
                    DKDYIP1(ALPHA+1:ALPHA+NEQNSQ) = C_V(R)*DFDY

                ELSE
                    UR(1:NEQNSQ:NEQN+1) = C_V(R) + UR(1:NEQNSQ:NEQN+1)
                END IF

                !       Partial derivative of stage R stage is nonzero.
                DKDYIP1_ZERO(R) = .FALSE.
            END IF

            !     Multiply UR by DFDY using loops and dot products since matrices are stored 
            !     as vectors. Skip multiplication if UR = 0 or if partial derivative is just 
            !     C_V(R)*DFDY.
            IF ( MATRIX_MULT_DKDYIP1 ) THEN
                IF ( .NOT. DKDYIP1_ZERO(R) ) THEN
                    DO I = 1,NEQN
                        BETA = ALPHA + I
                        DO J = 1,NEQN
                            SIGMA = (J-1)*NEQN
                            DKDYIP1(SIGMA+BETA) = DOT_PRODUCT(DFDY(I:(NEQN-1)*NEQN+I:NEQN),    &
                            UR(SIGMA+1:SIGMA+NEQN) )
                        END DO
                    END DO
                ELSE
                    !         If UR = 0 then Rth partial derivative is 0. Value of DKDYIP1_ZERO(R)
                    !         will left as .TRUE. to avoid later matrix multiplies.
                    DKDYIP1(ALPHA+1:ALPHA+NEQNSQ) = 0D0
                END IF ! .NOT. DKDYIP1_ZERO(R)
            END IF !  MATRIX_MULT_DKDYIP1
        END DO


        !   PART 4: COMPUTATION OF THE BLOCKS, LLI AND RRI.
        LLI = 0D0
        RRI = 0D0
        DO R = 1,C_S
            I1 = (R-1)*NEQNSQ
            LLI = LLI - C_B(R)*DKDYI(I1+1:I1+NEQNSQ)
            RRI = RRI - C_B(R)*DKDYIP1(I1+1:I1+NEQNSQ)
        END DO
        LLI = HI*LLI
        RRI = HI*RRI
        !   Add on -I and I, respectively
        LLI(1:NEQNSQ:NEQN+1) = -1D0 + LLI(1:NEQNSQ:NEQN+1)
        RRI(1:NEQNSQ:NEQN+1) =  1D0 + RRI(1:NEQNSQ:NEQN+1)

    END SUBROUTINE JACBLK


    SUBROUTINE MESH_SELECTOR(NEQN,NSUB,MESH_CURRENT,NSUB_STAR,MESH_NEW,INFO,DEFECT,GE)
        ! The purpose of this routine is to determine a new mesh. The NSUB+1 points 
        ! describing the current mesh are provided in MESH_CURRENT. An estimate of the 
        ! scaled defect of each solution component on each subinterval of the current 
        ! mesh is input through DEFECT. NEQN is the number of differential equations 
        ! plus the number of unknown parameters. This routine determines NSUB_STAR, 
        ! the number of subintervals for the new mesh from accuracy considerations. 
        ! The placement of new mesh points approximately equidistributes a monitor 
        ! function based on the scaled defect. The new mesh is returned in MESH_NEW. 
        ! If NSUB_STAR becomes greater than MXNSUB, the routine fails and returns
        ! with INFO = -1. Success is indicated by INFO = 0.
        !--------------------------------------------------------------------------------
        !     CALLED BY: MIRKDC
        !     CALLS : HALF_MESH,REDISTRIBUTE
        !--------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB
        DOUBLE PRECISION :: MESH_CURRENT(0:NSUB)
        
        DOUBLE PRECISION,OPTIONAL :: DEFECT(NEQN*NSUB),GE(NEQN*NSUB)

        !   Output arguments:
        INTEGER :: NSUB_STAR,INFO
        DOUBLE PRECISION MESH_NEW(0:4*NSUB) ! New mesh will have at most 4 times as
                                            ! many subintervals as current mesh.
        !   Local variables:
        INTEGER :: I ! Loop index.
        INTEGER :: OFFSET ! Array index.
        INTEGER :: NSUB_PRED ! Predicted number of subintervals in new mesh.
        DOUBLE PRECISION :: HI ! Size of subinterval I of current mesh.
        DOUBLE PRECISION :: R1 ! Maximum size of weighted mesh function values.                                  
        DOUBLE PRECISION :: R2 ! Sum of weighted mesh function values; it gives an 
                               ! estimate of predicted number of points for new mesh.
        DOUBLE PRECISION :: R3 ! Average of weighted mesh function values.
        DOUBLE PRECISION :: NORM1,NORM2 ! Max norm of defect for subinterval I.
        DOUBLE PRECISION :: SAFETY_FACTOR  ! Safety factor to overestimate 
                                                  ! number of points in new mesh.
        DOUBLE PRECISION :: RHO ! Threshold to trigger a remeshing based on ratio
                                      ! of maximum mesh function value to average value.
                                      ! (RHO = 1D0 forces a redistribution everytime.)
        DOUBLE PRECISION :: RELAX
    
  
        DOUBLE PRECISION :: UPPER_NEW_MESH = 4D0   ! Upper and lower bounds on ratio.
        DOUBLE PRECISION :: LOWER_NEW_MESH = 0.5D0 ! of NSUB_STAR to NSUB.
        DOUBLE PRECISION :: S_HAT(NSUB) ! Mesh function values, one component per 
                                        ! subinterval. 
                                    
        DOUBLE PRECISION :: DEFECT_I(NEQN*NSUB)
        DOUBLE PRECISION :: ORDER, OMOD
        DOUBLE PRECISION :: OLD_TOL
        LOGICAL :: USE_DEFECT, USE_GE, USE_BOTH
    
        !   Global variables:
        !   PROFILE gives the level of output to be provided.
        !   C_P is the order of the Runge-Kutta formula.  
        !   MXNSUB is the maximum number of subintervals.
        !   BVP_TOL is the user-provided tolerance applied to the defect.
        !-----------------------------------------------------------------------------
        INFO = 0
        R1 = 0D0
        R2 = 0D0
        OLD_TOL = BVP_TOL
        SAFETY_FACTOR=1.3D0
        
        USE_GE = .FALSE.
        USE_DEFECT = .FALSE.
        USE_BOTH = .FALSE.
    
        IF (PRESENT(DEFECT) .AND. PRESENT(GE)) THEN
            RHO = 2D0
            USE_BOTH = .TRUE. 
        ELSE IF (PRESENT(DEFECT)) THEN
            USE_DEFECT = .TRUE.
            RHO = 1D0
        ELSE IF (PRESENT(GE)) THEN
            USE_GE = .TRUE.
            RHO = 2D0
        ELSE 
            PRINT *, "No error passed."
            STOP
        END IF
        
    
        DO I = 1,NSUB
    
            OFFSET = (I-1)*NEQN
            HI = MESH_CURRENT(I) - MESH_CURRENT(I-1)

           
            
      	

            !    Compute corresponding mesh function value.
            IF (USE_DEFECT) THEN
                NORM1 = MAXVAL(DABS(DEFECT(OFFSET+1:OFFSET+NEQN)))
                S_HAT(I) = ((NORM1)/(BVP_TOL))**(1D0/(C_P+1)) /HI
            ELSE IF (USE_GE) THEN
                NORM1 = MAXVAL(DABS(GE(OFFSET+1:OFFSET+NEQN)))
                S_HAT(I) = ((NORM1)/(BVP_TOL))**(1D0/(C_P)) /HI
            ELSE IF (USE_BOTH) THEN
                NORM1 = MAXVAL(DABS(DEFECT(OFFSET+1:OFFSET+NEQN)))
                NORM2 = MAXVAL(DABS(GE(OFFSET+1:OFFSET+NEQN)))
                S_HAT(I) = (1.0D0*(((NORM1)/(BVP_TOL))**(1D0/(C_P+1)))+1.0D0*(((NORM2)/(BVP_TOL))**(1D0/(C_P))))/HI
            ELSE
                WRITE (*,*) " Mesh selection error."
                STOP
            END IF
     
            !     Update maximum weighted mesh function value, if necessary.
            R1 = MAX(R1,S_HAT(I)*HI)

            !     Accumulate Ith weighted mesh function value in R2.
            R2 = R2 + S_HAT(I)*HI
        END DO
       
        !   Compute average of weighted mesh function values.
        R3 = R2/NSUB

        !   Estimate number of subintervals that new mesh should have. 
        NSUB_PRED = SAFETY_FACTOR*R2 + 1
    

        !   To avoid cycling, make sure new mesh has a few more points than previous one.
        IF ( ABS( (NSUB_PRED - NSUB)/(1D0*NSUB)) < 0.1D0) NSUB_PRED = 1.1D0 * NSUB

        !   Compute new mesh of NSUB_PRED points if maximum weighted mesh function value
        !   is more than RHO times the average value. Otherwise simply halve the mesh.
        If (PROFILE > 0) THEN
            PRINT *, "Mesh selector will use RHO: ", RHO
        END IF
    
        IF (R1 <= RHO*R3) THEN
            !     Halve current mesh.
            If (PROFILE > 0) THEN
                PRINT *, "Mesh selector will double mesh"
            END IF
            NSUB_STAR = 2*NSUB
            IF (NSUB_STAR > MXNSUB) THEN
                !       New mesh would be too large.
                INFO = -1
                IF (PROFILE > 0) THEN
                    PRINT *,'Number of subintervals needed for new mesh would ',           &
                    'exceed current allowed maximum of ', MXNSUB,'.'
                    PRINT *,' '
                END IF
            ELSE 
                IF (PROFILE > 0) THEN
                    PRINT *,'Halve each subinterval of the current mesh and try again.'
                    PRINT *,'The number of subintervals in the new mesh will be ', NSUB,'.'
                    PRINT *,' '
                END IF
                CALL HALF_MESH(NSUB,MESH_NEW)
            END IF ! (NSUB_STAR > MXNSUB)  
        ELSE
    
            If (PROFILE > 0) THEN
                PRINT *, "Mesh selector will choose a mesh to equdistribute error"
            END IF
            !     Compute new mesh of NSUB_PRED points.  
            NSUB_star = NSUB_pred
            IF ( NSUB_STAR > UPPER_NEW_MESH * NSUB ) NSUB_STAR = UPPER_NEW_MESH * NSUB
            IF ( NSUB_STAR < LOWER_NEW_MESH * NSUB ) NSUB_STAR = LOWER_NEW_MESH * NSUB
            IF (NSUB_STAR > MXNSUB) THEN
                !       New mesh would be too large.
                INFO = -1
                IF (PROFILE > 0) THEN
                    PRINT *,'Number of subintervals needed for new mesh would ',           &
                    'exceed current allowed maximum of ', MXNSUB,'.'
                    PRINT *,' '
                END IF
            ELSE 
                CALL REDISTRIBUTE(MESH_CURRENT,S_HAT,NSUB_STAR,MESH_NEW)          
            END IF ! (NSUB_STAR > MXNSUB)
        END IF ! (R1 <= RHO*R3)

        BVP_TOL = OLD_TOL
    !pause
    END SUBROUTINE MESH_SELECTOR
  

    SUBROUTINE NEWITER(NEQN,NSUB,MESH,Y,INFO,K_DISCRETE,FSUB,GSUB,DFSUB,DGSUB, USAGE)
        ! This routine takes an approximate solution Y on a mesh, stored in MESH, of NSUB
        ! subintervals and computes a new approximation by applying a modified Newton's 
        ! method to the nonlinear system PHI(Y) = 0, where PHI(Y) is the residual 
        ! function. If the computation succeeds, the new approximation is returned in Y.
        ! As a by-product, the Runge-Kutta stage values are returned in the array 
        ! K_DISCRETE. The subroutines FSUB,GSUB,DFSUB, and DGSUB define the BVP. NEQN is
        ! the number of ODEs plus the number of unknown parameters. If the computation 
        ! succeeds, INFO = 0.  It can fail in three ways: If the Newton iteration takes 
        ! too many steps, INFO = 1. If the Newton matrix is singular, INFO = 2. If a 
        ! suitable damping factor cannot be found, INFO = 3. FSUB,GSUB,DFSUB,DGSUB are 
        ! user-supplied subroutines for the ODEs and boundary conditions and their 
        ! derivatives.
        !-------------------------------------------------------------------------------
        !      CALLED BY: MIRKDC
        !      CALLS: FIXED_JACOB,DAMPED_NEWT,RESID
        !-------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN, NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1))
        INTEGER, OPTIONAL :: USAGE
   

        !   Output arguments:
        INTEGER :: INFO
        DOUBLE PRECISION :: K_DISCRETE(MXS*NEQN*NSUB)

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB,DFSUB,DGSUB

        !   Local variables:
        DOUBLE PRECISION :: PHI(NEQN*(NSUB+1)) ! Residual function.
        DOUBLE PRECISION :: DELTA(NEQN*(NSUB+1)) ! Newton correction.
        DOUBLE PRECISION :: TOP(NEQN**2) ! Derivatives of left boundary conditions.
        DOUBLE PRECISION :: BOT(NEQN**2) ! Derivatives of right boundary conditions.
        DOUBLE PRECISION  :: BLOCKS(2*NEQN**2 * NSUB) ! Derivatives of PHI(Y).
        !   Together TOP, BLOCKS, and BOT contain an almost block diagonal matrix 
        !   that is the Newton matrix. After a damped Newton step, TOP, BOT, 
        !   and BLOCKS contain the factored form of this matrix.
        INTEGER :: PIVOT(NEQN*(NSUB+1)) ! Pivoting information from the 
                                        ! factorization of the Newton matrix.
        LOGICAL :: CONVRG ! Monitors convergence of Newton iteration.
        INTEGER :: COUNT, ITERS ! Counts number of damped Newton iterations.
        DOUBLE PRECISION :: DELTA_0_NORM ! Norm of Newton correction vector.
        DOUBLE PRECISION :: LAMBDA ! Damping factor for damped Newton step.
        DOUBLE PRECISION :: G ! Natural criterion function value.
        LOGICAL :: FIXED_JACOBIAN ! .TRUE. if next step is a fixed Jacobian step; 
                                  ! .FALSE. if next step is a damped Newton step.
        INTEGER, PARAMETER :: MAXITER=40 ! Maximum number of damped Newton steps.
    
        INTEGER :: IER ! Mem allocation and deallocation flag
        LOGICAL :: STORE_NEWT 
        DOUBLE PRECISION :: OLD_Y(NEQN*(NSUB+1))
    
        INTEGER :: J,I

        !   Global variables:
        !   MXS is the number of maximum number of Runge-Kutta stages.
        !   PROFILE specifies the level of output.
        !-----------------------------------------------------------------------------
        !   Initialization for modified Newton iteration loop.
        COUNT = 0
        CONVRG = .FALSE.
        INFO = 0

        !	Checks to see if method is being used by MIRKDC or one of the
        !	global error routines
        IF(PRESENT(USAGE)) THEN
            IF (USAGE .EQ. 1) THEN
                STORE_NEWT = .TRUE.
            ELSE 
                STORE_NEWT = .FALSE.
            END IF !USAGE = 1
        ELSE
            STORE_NEWT = .TRUE.
        END IF ! PRESENT(USAGE)

        !   The first iteration step must be a damped Newton step.
        FIXED_JACOBIAN = .FALSE.

        !   When a damped Newton step is used, it is assumed that the current iterate 
        !   Y and residual function PHI evaluated at Y are available. For the first 
        !   step, Y is input, but PHI must be initialized through a call to the RESID 
        !   routine.
    
        CALL RESID(NEQN,NSUB,MESH,Y,PHI,K_DISCRETE,FSUB,GSUB)
    
        !
        !   In predicting a new damping factor, certain quantities from the previous 
        !   iteration are used. LAMBDA = 0 signals that they are not available for
        !   the first step. 
        LAMBDA = 0D0
        ITERS=0
        GBL_ITERATIONS = 0D0
        !   Hybrid damped Newton/Fixed Jacobian iteration. Report failure with INFO > 0.
        DO WHILE ( (.NOT. CONVRG) .AND.  (INFO <= 0) )
            IF (FIXED_JACOBIAN) THEN  
                !       Don't count Fixed Jacobian steps--they are low cost.
  
                IF (PROFILE > 1) THEN
                    PRINT *,'FIXED JACOBIAN STEP:' 
                    PRINT *,' '
                END IF
        
                CALL FIXED_JACOB(NEQN,NSUB,MESH,Y,DELTA,G,PHI,TOP,BOT,BLOCKS,PIVOT,      &
                FIXED_JACOBIAN,LAMBDA,CONVRG,INFO,K_DISCRETE,FSUB,GSUB)
            ELSE
                COUNT = COUNT + 1
                GBL_ITERATIONS = COUNT
       
                IF (PROFILE > 1) THEN
                    PRINT *,'DAMPED NEWTON STEP, FULL NEWTON ITERATION COUNT =',COUNT,'.'
                    PRINT *,' '
                END IF
                CALL DAMPED_NEWT(NEQN,NSUB,MESH,Y,LAMBDA,PHI,TOP,BOT,BLOCKS,PIVOT,       &
                FIXED_JACOBIAN,CONVRG,DELTA,DELTA_0_NORM,G,INFO,        &
                K_DISCRETE,FSUB,GSUB,DFSUB,DGSUB)
            END IF 

            IF ((.NOT. CONVRG) .AND. (INFO <= 0)) THEN
                IF ( COUNT > MAXITER) INFO = 1
            END IF
            ITERS=ITERS+1 
        END DO 

        IF (PROFILE > 0) THEN
            IF (COUNT > 1) THEN 
                WRITE(*,"(1X,A5,I3,A13)") 'AFTER',COUNT,' NEWTON STEPS,'
                PRINT *,' '
            ELSE
                WRITE(*,"(1X,A5,I3,A12)") 'AFTER',COUNT,' NEWTON STEP,'
                PRINT *,' '
            END IF
        END IF
    
        !	Store Factored Newton Matrix in memory if allowed
 
        IF(STORE_NEWT) THEN
            !  	Check For Mem
            IF (UPDATENEWT) THEN
                DEALLOCATE(MBLOCKS,MPIVOT,MTOP,MBOT,STAT=IER)
                CALL CHECK_STAT(IER)
            END IF !UPDATENEWT
 	
            UPDATENEWT = .TRUE.

            !   	Allocate space in mem for factored Newton matrix
            ALLOCATE(MBLOCKS(2*NEQN**2 * NSUB),MTOP(NEQN**2),MBOT(NEQN**2), &
            MPIVOT(NEQN*(NSUB+1)),STAT=IER)
            CALL CHECK_STAT(IER)
    
            !   	Copy Factored Matrix
             DO J=1,NEQN**2
                MTOP(J) = TOP(J)
                MBOT(J) = BOT(J)
            END DO
            DO J=1,(2*NEQN**2)*NSUB
                MBLOCKS(J) = BLOCKS(J)
            END DO
            DO J=1,NEQN*(NSUB+1)
                MPIVOT(J) = PIVOT(J)
            END DO
		

        END IF !STORE_NEWT
	
        
    END SUBROUTINE NEWITER


    SUBROUTINE NEWMAT(LEFTBC,RIGHTBC,NEQN,NSUB,MESH,Y,TOP,BLOCKS,BOT,              &
    K_DISCRETE,FSUB,GSUB,DFSUB,DGSUB)
        ! This routine evaluates the Newton matrix at the current iterate Y and 
        ! returns it in TOP, BOT, and BLOCKS; TOP contains the partial derivatives 
        ! associated with the LEFTBC boundary conditions; BOT contains the corresponding 
        ! information for the RIGHTBC boundary conditions; BLOCKS contains partial

        ! derivatives of the residual function with respect to the unknowns, y_i, 
        ! i=1,..., NSUB, where NSUB is the number of subintervals of the current MESH. 
        ! TOP stores a LEFTBC by NEQN matrix in column form; BOT stores a RIGHTBC by 
        ! NEQN matrix, also in column form. BLOCKS stores pairs of matrices L_i and R_i, 
        ! partial derivatives associated with subinterval i. Each of R_i and L_i is an 
        ! NEQN by NEQN matrix stored in column form. NEQN is the number of ODEs plus 
        ! the number of unknown parameters. This routine accesses the discrete 
        ! Runge-Kutta stages through the array K_DISCRETE which was assigned values in 
        ! a previous call to RESID with the same Y. FSUB,GSUB,DFSUB,DGSUB are 
        ! user-supplied subroutines for the ODEs and boundary conditions and their 
        ! derivatives.
        !----------------------------------------------------------------------------
        !      CALLED BY: DAMPED_NEWT
        !      CALLS: JACBLK
        !-----------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: LEFTBC,RIGHTBC,NEQN,NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1))
        DOUBLE PRECISION :: K_DISCRETE(MXS*NEQN*NSUB)

        !   Output arguments:
        DOUBLE PRECISION ::  TOP(LEFTBC*NEQN),BLOCKS(2*NEQN**2*(NSUB+1)),            &
        BOT(RIGHTBC*NEQN)

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB,DFSUB,DGSUB

        !   Local variables:
        INTEGER :: I ! Loop index.       
        INTEGER :: I1,I2,I3,I4,N1 ! Array indicies.
        INTEGER :: TOPLOC,BOTLOC,WORKLOC ! Array indicies.
        DOUBLE PRECISION :: PD(NEQN,NEQN) ! Local storage of partial derivatives
                                          ! of boundary conditions.
        DOUBLE PRECISION :: WORK(NEQN**2) ! Local storage for reshaping of PD for
                                          ! transfer into TOP and BOT.
        !   Global variables:
        !   MXS is the number of maximum number of Runge-Kutta stages.
        !   C_S is the number of discrete stages.
        !   NEQNSQ equals NEQN**2.
        !   HAVE_DBCDY specifies whether or not the user routine DBCDY was provided.
        !-----------------------------------------------------------------------------
        I1 = C_S*NEQN 
        N1= NSUB*NEQN

        DO I = 1,NSUB
            !     Compute blocks L_I and R_I through call to JACBLK. 
            I2 = (I-1)*NEQN ! Index offset for Y.
            I3 = (I-1)*I1 ! Index offset for K_DISCRETE.
            I4 = 2*(I-1)*NEQNSQ ! Index offset for BLOCKS.

            !     y_I is stored starting at Y((I-1)*NEQN+1); y_{I+1} is stored starting at
            !     Y(I*NEQN+1). The C_S stages associated with subinterval I are in the 
            !     Ith set of C_S*NEQN locations of K_DISCRETE. L_I and R_I are returned in 
            !     the Ith set of 2*NEQNS**2 locations of BLOCKS.
            CALL JACBLK(NEQN,MESH(I)-MESH(I-1),MESH(I-1),Y(I2+1),Y(I2+NEQN+1),         &
            K_DISCRETE(I3+1),BLOCKS(I4+1),BLOCKS(I4+NEQNSQ+1),FSUB,DFSUB)
        END DO

        !   If DGSUB is present, call P_DBC to get partial derivatives; otherwise
        !   call PD_BC to compute the partial derivatives using finite differences.
        !   Y(1:NEQN) is y_0, Y(N1+1:N1+NEQN) is y_NSUB. 
        IF (HAVE_DBCDY) THEN
            CALL P_DBC(DGSUB,Y(1:NEQN), Y(N1+1:N1+NEQN),PD(1:LEFTBC,1:NEQN),           &
            PD(LEFTBC+1:NEQN,1:NEQN),LEFTBC,RIGHTBC,NEQN)               
        ELSE
            CALL PD_BC(GSUB,Y(1:NODE),Y(N1+1:N1+NEQN),PD(1:LEFTBC,1:NEQN),             &
            PD(LEFTBC+1:NEQN,1:NEQN),LEFTBC,RIGHTBC,NEQN)
        END IF

        WORK = RESHAPE(PD,(/NEQNSQ/))

        !   Since WORK contains the elements of the boundary condition matrix stored 
        !   in a columnwise fashion, select the first LEFTBC elements from each set of 
        !   NEQN elements of WORK and copy them into the appropriate locations of TOP. 
        !   Similarly, copy the bottom RIGHTBC elements of each set of NEQN elements 
        !   of WORK into the appropriate locations of BOT.

        DO I = 1,NEQN
            TOPLOC = (I-1)*LEFTBC
            WORKLOC = (I-1)*NEQN
            BOTLOC = (I-1)*RIGHTBC
            TOP(TOPLOC+1:TOPLOC+LEFTBC) = WORK(WORKLOC+1:WORKLOC+LEFTBC)
            BOT(BOTLOC+1:BOTLOC+RIGHTBC) = WORK(WORKLOC+LEFTBC+1:WORKLOC+NEQN)
        END DO

    END SUBROUTINE NEWMAT
    
    SUBROUTINE LOAD_SOL(ARY_X,ARY_Y,ARY_DISCRETE,ARY_INTERP,INFO,SOL)
        ! This routine loads a data into a BVP_SOL structure.
        !----------------------------------------------------------------------------------
        !    CALLED BY: BVP_SOLVER, MIRKDC
        !    CALLS: CHECK_STAT
        !----------------------------------------------------------------------------------
        
        !	Input arguments:
        DOUBLE PRECISION, DIMENSION (:) :: ARY_X ! Solution Mesh 
        DOUBLE PRECISION, DIMENSION (:) :: ARY_Y ! Solution
        DOUBLE PRECISION, DIMENSION (:) :: ARY_DISCRETE ! Info for discrete MIRK stages
        DOUBLE PRECISION, DIMENSION (:) :: ARY_INTERP ! Info for continuous MIRK stages
        INTEGER :: INFO ! Flag for success of MIRKDC
        
        !	Output arguments:
        TYPE(BVP_SOL) :: SOL
	
        !	Local variabels:
        INTEGER :: N_IWORK,NDEX,N_WORK ! Array Values
        DOUBLE PRECISION, DIMENSION (:,:), ALLOCATABLE :: TEMPY !Holds Y
        INTEGER :: IER !Mem Allocation flag
	
        !	State if solver was successful
        SOL%INFO = INFO
        !	Number of mesh points
        SOL%NPTS = NSUB + 1
	
        !	Copy NODE, NPAR, LEFTBC, MXNSUB values from global 
        !   variables to SOL fields.
        SOL%NODE = NODE
        SOL%NPAR = NPAR
        SOL%LEFTBC = LEFTBC
        SOL%MXNSUB = MXNSUB 
      

      
        !    Load mesh and discrete solution into SOL.

        ALLOCATE(SOL%X(NSUB+1),SOL%Y(NEQN,NSUB+1),STAT=IER)
        CALL CHECK_STAT(IER)
        SOL%X = ARY_X
        SOL%Y = RESHAPE(ARY_Y,(/NEQN,NPTS/))
     
        !     If NPAR present, load parameters into SOL.
        IF (NPAR > 0) THEN
            ALLOCATE(SOL%PARAMETERS(NPAR),STAT=IER)
            CALL CHECK_STAT(IER)
            SOL%PARAMETERS = SOL%Y(NODE+1:NEQN,1)
            ALLOCATE(TEMPY(NODE,NSUB+1),STAT=IER)
            CALL CHECK_STAT(IER)
            TEMPY = SOL%Y(1:NODE,1:NSUB+1)
            DEALLOCATE(SOL%Y)
            CALL CHECK_STAT(IER)
            ALLOCATE(SOL%Y(NODE,NSUB+1),STAT=IER)
            CALL CHECK_STAT(IER)
            SOL%Y = TEMPY
            DEALLOCATE(TEMPY)
            CALL CHECK_STAT(IER)
        END IF ! (NPAR>0)

        !     Load SOL%IWORK and SOL%WORK fields needed by BVP_EVAL. 
        N_IWORK = 3
        N_WORK = C_S_STAR*NEQN*NSUB + (NSUB+1) + (NSUB+1)*NEQN

        ALLOCATE(SOL%IWORK(N_IWORK),SOL%WORK(N_WORK),STAT=IER)
        CALL CHECK_STAT(IER)
 
        !     Save global variable values needed by BVP_EVAL. 
        SOL%IWORK(1) = C_S
        SOL%IWORK(2) = C_S_STAR
        SOL%IWORK(3) = BVP_METHOD

        !     Copy ARY_DISCRETE and ARY_INTERP into first two sections of WORK array.
        SOL%WORK(1:C_S*NEQN*NSUB) = ARY_DISCRETE 
        NDEX = C_S*NEQN*NSUB
        SOL%WORK(NDEX+1:NDEX+(C_S_STAR-C_S)*NEQN*NSUB) = ARY_INTERP

        !     Copy ARY_X into the third part of SOL%WORK           
        NDEX = C_S_STAR*NEQN*NSUB
        SOL%WORK(NDEX+1:NDEX+NSUB+1) = ARY_X

        !     Copy ARY_Y into the fourth part of SOL%WORK
        NDEX = C_S_STAR*NEQN*NSUB + (NSUB+1)
        SOL%WORK(NDEX+1:NDEX+(NSUB+1)*NEQN) = ARY_Y

    END SUBROUTINE LOAD_SOL
    
    SUBROUTINE RE_GERROR(GSOL,FSUB,BCSUB,DFDY,DBCDY, INFO,GENORM,GEVECTOR)
    !       Estimate the error in solution component y(i) relative to 1 + |y(i)|.
    !       Return the maximum over all components and mesh points as YERROR. The
    !       same estimate applies to unknown parameters.  The estimate is computed 
    !       by extrapolation from the current solution SOL to a solution on the 
    !       "doubled" mesh. 
    !-----------------------------------------------------------------------------
    ! CALLED BY: BVP_SOLVER
    ! CALLS BVP_EVAL, NEWITER, GLOBAL_ERROR
    !-----------------------------------------------------------------------------
	
    !   Input arguments:
        TYPE(BVP_SOL) :: GSOL
        EXTERNAL :: FSUB ! User-supplied ODEs routine
        EXTERNAL :: BCSUB ! User-supplied boundary conditions routine
        EXTERNAL :: DFDY ! Partial-derivative routine for ODEs
        EXTERNAL :: DBCDY ! Partial-derivative routine for boundary conditions
        INTEGER :: INFO ! Success flag
	
    !   Output arguments:
        DOUBLE PRECISION, OPTIONAL, DIMENSION (:) :: GEVECTOR ! Global-error vector 
        DOUBLE PRECISION :: GENORM ! Global-error norm 
	
    !   Local variables:
        INTEGER :: IER ! Memory allocation flag
        INTEGER :: ST,EN ! Array location
        INTEGER :: I ! Counters
        INTEGER :: NDEX !Work Array Locatoion for solution
        INTEGER :: GE_INFO ! Newton solver flag
        INTEGER :: NITERS ! Number of Newton Itertations
        DOUBLE PRECISION, ALLOCATABLE,DIMENSION (:) :: Y_HIGH, Y_LOW ! Hold solutions
    !   Holds the result from the Newton solver.
        DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:) :: GE_Y,GE_X,GE_K_DISCRETE
	
        ALLOCATE(GE_X(0:2*NSUB),GE_Y((2*NSUB+1)*NEQN),                             &
        GE_K_DISCRETE(C_S*NEQN*(2*NSUB)),STAT=IER)
        CALL CHECK_STAT(IER)
        
        ALLOCATE(Y_HIGH(NEQN*(NPTS)),Y_LOW(NEQN*(NPTS)),STAT=IER)
        CALL CHECK_STAT(IER)    
   
    !	Load mesh values
        GE_X(0:2*NSUB:2) = GSOL%X
        GE_X(1:2*NSUB-1:2) = 0.5D0*(GE_X(0:2*NSUB-2:2) + GE_X(2:2*NSUB:2))
	
        DO I = 1,2*NSUB+1
    !   Solution values.
            CALL BVP_EVAL(GSOL,GE_X(I-1),GE_Y((I-1)*NEQN+1:(I-1)*NEQN+NODE))
       
    !   Parameter values. 
            IF (NPAR > 0 ) THEN
                CALL BVP_EVAL(GSOL,GE_Y((I-1)*NEQN+NODE+1:(I-1)*NEQN+NEQN))
            END IF
        END DO
  
    !   Set global variables NSUB and NPTS for doubled mesh.
        NSUB = 2*NSUB
        NPTS = NSUB+1

    !	Solves on more     
        NEWTON_TOL = NEWTON_TOL/2D0
    
    !	Solve Nonlinear equations
        URE = .TRUE.
        CALL NEWITER(NEQN,NSUB,GE_X,GE_Y,GE_INFO,GE_K_DISCRETE,             &
        FSUB,BCSUB,DFDY,DBCDY, USAGE=2)
        URE = .FALSE.
        
        INFO=GE_INFO
 
    !   Check for failure of Newton solver
        IF (GE_INFO /= 0) THEN
            IF (G_STOP_ON_FAIL) THEN
                PRINT *,'Computation of richardson extraoplation failed--the solution SOL is in doubt.'
                PRINT *,'INFO: ', GE_INFO
              !STOP
            END IF
        END IF
    
    !   Re-sets global variables
        NSUB = NSUB*0.5
        NPTS = NSUB+1
        NEWTON_TOL = NEWTON_TOL*2D0

    !	Load values in Y_HIGH, Y_LOW	
        DO I = 1,GSOL%NPTS
            Y_HIGH((I-1)*NEQN+1:(I-1)*NEQN+NEQN)=GE_Y((I-1)*2*NEQN+1:(I-1)*2*NEQN+NEQN)
        END DO
        NDEX = C_S_STAR*NEQN*NSUB + (NSUB+1)	
        Y_LOW=GSOL%WORK(NDEX+1:NDEX+(NSUB+1)*NEQN)
	
        GENORM = 0D0
        
    !   Get GE norm and GE vector
        IF (PRESENT(GEVECTOR)) THEN
            CALL GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM,GEVECTOR)
        ELSE
 
            CALL GLOBAL_ERROR(Y_LOW,Y_HIGH,GENORM)
        END IF
   
    !   YERROR is multiplied by a Richardson extrapolation term to increase the
    !   accuracy of the global error estimation.  See section 5.5.2 (page 230),
    !   in Ascher, Mattheij, and Russell Numerical Solution of Boundary Value
    !   Problems for Ordinary Differential Equations, for further details.  
    
        GENORM = GENORM*(2**BVP_METHOD)/(2**BVP_METHOD - 1D0)

    !   DEALLOCATE Memory
        DEALLOCATE(GE_X,GE_Y,GE_K_DISCRETE,Y_HIGH,Y_LOW)
        CALL CHECK_STAT(IER)
	
    END SUBROUTINE RE_GERROR

  
    SUBROUTINE REDISTRIBUTE(MESH_CURRENT,S_HAT,NSUB_STAR,MESH_NEW)
        ! This routine is called with a mesh, MESH_CURRENT, of NSUB subintervals and a 
        ! vector of values, S_HAT, of a mesh function that is associated with a measure  
        ! of the relative defect. (More details about the mesh function are found in the
        ! routine MESH_SELECTOR.) It returns a new mesh, MESH_NEW, of NSUB_STAR 
        ! subintervals which equidistributes the integral of the mesh function. 
        ! The idea is to select new mesh points so that the area under the mesh function 
        ! is equally distributed over the subintervals of the new mesh. The quantity ZETA 
        ! is the total area under the mesh function divided by NSUB_STAR. The integral  
        ! over each subinterval of the new mesh is to equal ZETA. The area for each  
        ! subinterval of the old mesh is computed and added to a sum, proceeding from  
        ! left to right. Whenever the sum exceeds ZETA, the fraction of the subinterval  
        ! needed to equal ZETA is added. This fraction is used to place the new mesh 
        ! point. The procedure is repeated until every subinterval of the current mesh is
        ! processed. Chapter 9 of the text by Ascher, Mattheij, and Russell provides 
        ! further details.
        !------------------------------------------------------------------------------
        !   CALLED BY: MESH_SELECTOR
        !------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NSUB_STAR
        DOUBLE PRECISION :: MESH_CURRENT(0:NSUB),S_HAT(NSUB)

        !   Output arguments:
        DOUBLE PRECISION :: MESH_NEW(0:NSUB_STAR)

        !   Local variables:
        INTEGER :: I ! Index for tracking new mesh points.
        INTEGER :: K ! Index of subinterval of current mesh whose corresponding
                     ! mesh function integral is being added to integral for the
                     ! subinterval associated with the next new mesh point.
        DOUBLE PRECISION :: ZETA ! See above.
        DOUBLE PRECISION :: T ! Marker to keep track of how much of mesh function
                              ! integral has been processed.
        DOUBLE PRECISION :: INTEGRAL ! Keeps track of accumuation of integral to be
                                     ! associated with subinterval corresponding to
                                     ! next new mesh point. 0 <= INTEGRAL <= ZETA.
        DOUBLE PRECISION :: NEXT_PIECE ! Portion of mesh function integral processed
                                       ! during each interation of while loop.
        !   Global variables:
        !   NSUB is the number of subintervals in the current mesh.
        !-------------------------------------------------------------------------------
        !   Compute ZETA, the total area under the mesh function divided by NSUB_STAR.
        ZETA = SUM(S_HAT*(MESH_CURRENT(1:NSUB)-MESH_CURRENT(0:NSUB-1)))/NSUB_STAR
        !
        !   The first entry in both meshes is the left end point of the interval.    
        MESH_NEW(0) = MESH_CURRENT(0)

        !   Set indicies, markers, and accumulators.
        K = 1
        I = 0
        T = MESH_CURRENT(0)
        INTEGRAL = 0D0

        DO WHILE (K <= NSUB)
      
            NEXT_PIECE = S_HAT(K)*(MESH_CURRENT(K) - T)
      
            IF ( (INTEGRAL + NEXT_PIECE) > ZETA) THEN
                !       Define new mesh point so that the integral for this new subinterval is 
                !       equal to ZETA.
                MESH_NEW(I+1) = (ZETA - INTEGRAL)/S_HAT(K) + T

                !       Setup for next new meshpoint.
                T = MESH_NEW(I+1)
                INTEGRAL = 0D0        
                I = I + 1
            ELSE
                !       Contribute all of remaining part of integral from subinterval K of 
                !       current mesh to integral for the subinterval associated with next 
                !       new mesh point.
                INTEGRAL = INTEGRAL + NEXT_PIECE
        
                !       Set T and K to refer to next subinterval of current mesh.
                T = MESH_CURRENT(K)
                K = K + 1
            END IF

        END DO

        !   The last entry in both meshes is the right end point of the interval.
        MESH_NEW(NSUB_STAR) = MESH_CURRENT(NSUB)
                                                                              
    END SUBROUTINE REDISTRIBUTE


    SUBROUTINE RESID(NEQN,NSUB,MESH,Y,PHI,K_DISCRETE,FSUB,GSUB)
        ! This subroutine evaluates the residual function PHI at the current iterate Y.  
        ! The current mesh of NSUB subintervals is stored in MESH. Evaluation of PHI 
        ! is done in two phases. The components associated with the interior of the 
        ! interval are evaluated through calls to the subroutine SUBCOM.  The components 
        ! associated with the boundary conditions are then obtained by by calls to 
        ! P_GSUB. A by-product of the call to SUBCOM is the return of the S Runge-Kutta 
        ! stages that arise as intermediate quantities. Each stage is a vector of length 
        ! NEQN where NEQN equals the number of ODEs plus the number of unknown 
        ! parameters. The stages are stored in order in the K_DISCRETE vector. FSUB,GSUB 
        ! are user-supplied subroutines for the ODEs and boundary conditions.
        !-------------------------------------------------------------------------------
        !     CALLED BY: NEWITER,CRITERION,FIXED_JACOB
        !     CALLS: SUBCOM,P_GSUB
        !-------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,NSUB
        DOUBLE PRECISION :: MESH(0:NSUB),Y(NEQN*(NSUB+1))

        !   Output arguments:
        DOUBLE PRECISION :: PHI(NEQN*(NSUB+1)),K_DISCRETE(MXS*NEQN*NSUB)

        !   User-supplied subroutines:
        EXTERNAL FSUB,GSUB

        !   Local variables:
        INTEGER :: I ! Loop index.
        INTEGER :: I1,N1 ! Array indicies.
        DOUBLE PRECISION :: H,T ! Subinterval length and left endpoint of 
                                ! subinterval.
        !   Global variables:
        !   MXS is the maximum number of Runge-Kutta stages.
        !   C_S is the number of discrete stages.
        !   LEFTBC is the number of left boundary conditions.
        !-------------------------------------------------------------------------------
        !   For each subinterval of the mesh,
        DO I = 1,NSUB
            H = MESH(I) - MESH(I-1)
            T = MESH(I-1)
            I1 = (I - 1)*NEQN
      
            !     The current approximate solutions at the end points of subinterval I are
            !     required for the computation of the residual.  The solution at the left
            !     end is stored starting at Y((I-1)*NEQN+1) and the solution at the right
            !     end is stored starting at Y(I*NEQN+1).  In the call to SUBCOM where the
            !     residual is evaluated, PHI(LEFTBC+(I-1)*NEQN+1) is the segment of
            !     PHI where this component of the residual is to be stored.  Similarly, the
            !     stages of the discrete formula are returned in a segment of K_DISCRETE.
      
            CALL SUBCOM(NEQN,C_S,H,T,Y(I1+1:I1+NEQN),                                  &
            Y(I1+NEQN+1:I1+2*NEQN),PHI(LEFTBC+I1+1:LEFTBC+I1+NEQN),        &
            K_DISCRETE(I1*C_S+1:I1*C_S+NEQN*C_S),FSUB)
       
            !   Add deferred-correction residual if required
            IF(UDC) THEN
                PHI(LEFTBC + I1+1:LEFTBC+I1+NEQN) =  PHI(LEFTBC + I1+1:LEFTBC+I1+NEQN) &
                + H_PHI(LEFTBC + I1+1:LEFTBC+I1+NEQN)  
            END IF !UDI

        END DO

        !   The approximate solution at the left end point is stored starting at
        !   Y(1) and the approximate solution at the right end point is stored
        !   starting at Y(NSUB*NEQN+1). Results are returned in the first LEFTBC
        !   locations of PHI and the last NEQN-LEFTBC locations of PHI.
        N1 = NSUB*NEQN
        CALL P_GSUB(GSUB,NEQN,LEFTBC,Y(1:NEQN),Y(N1+1:N1+NEQN),                      &
        PHI(1:LEFTBC),PHI(N1+LEFTBC+1:N1+NEQN))
                


    END SUBROUTINE RESID


    SUBROUTINE RK_TABLEAU
        ! This routine sets up the coefficients for the discrete mono-implicit 
        ! Runge-Kutta formula according to the value of the global variable BVP_METHOD. 
        ! The coefficients are assigned to global variables and arrays as indicated 
        ! below.
        !-------------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !     CALLS: CHECK_STAT
        !-------------------------------------------------------------------------------
        !   Local variable: 
        INTEGER IER ! Error flag for allocation call.
        DOUBLE  PRECISION :: BETA = 0.0D0 ! Temp free parameter 
 									          ! for 8th order MIRK method
       
        !   Global variables:
        !   BVP_METHOD identifies the Runge-Kutta method to be set up.
        !   C_S is the number of Runge-Kutta stages. 
        !   C_C, C_V, C_X, and C_B are arrays containing the Runge-Kutta coefficients.
        !------------------------------------------------------------------------------------
        ALLOCATE(C_C(C_S),C_V(C_S),C_B(C_S),C_X(C_S,C_S),STAT=IER)
        CALL CHECK_STAT(IER)

        SELECT CASE (BVP_METHOD)
            CASE (2)
                !       Define 2-stage, 2nd order, stage order 1 MIRK formula.
                !       C_S has been set to 2.

                C_C(1) = 0D0
                C_C(2) = 1D0

                C_V(1) = 0D0
                C_V(2) = 1D0

                C_B(1) = 1D0/2D0
                C_B(2) = 1D0/2D0

                !       Column 1 of C_X
                C_X(1,1) = 0D0
                C_X(2,1) = 0D0

                !       Column 2 of C_X
                C_X(1,2) = 0D0
                C_X(2,2) = 0D0
            CASE (4)
                !       Define 3-stage, 4th order, stage order 3 MIRK formula.
                !       C_S has been set to 3.
      
                C_C(1) = 0D0
                C_C(2) = 1D0
                C_C(3) = 1D0/2D0

                C_V(1) = 0D0
                C_V(2) = 1D0
                C_V(3) = 1D0/2D0

                C_B(1) = 1D0/6D0
                C_B(2) = 1D0/6D0
                C_B(3) = 2D0/3D0

                !       Column 1 of C_X
                C_X(1,1) = 0D0
                C_X(2,1) = 0D0
                C_X(3,1) = 1D0/8D0

                !       Column 2 of C_X
                C_X(1,2) = 0D0
                C_X(2,2) = 0D0
                C_X(3,2) = -1D0/8D0

                !       Column 3 of C_X
                C_X(1,3) = 0D0
                C_X(2,3) = 0D0
                C_X(3,3) = 0D0
            CASE (6)
                !       Define improved 5-stage, 6th order, stage order 3 MIRK formula.
                !       C_S has been set to 5.

                C_C(1) = 0D0
                C_C(2) = 1D0
                C_C(3) = 1D0/2D0 - SQRT(21D0)/14D0
                C_C(4) = 1D0/2D0 + SQRT(21D0)/14D0
                C_C(5) = 1D0/2D0

                C_V(1) = 0D0
                C_V(2) = 1D0
                C_V(3) = 1D0/2D0 - 9D0*SQRT(21D0)/98D0
                C_V(4) = 1D0/2D0 + 9D0*SQRT(21D0)/98D0
                C_V(5) = 1D0/2D0

                C_B(1) = 1D0/20D0
                C_B(2) = 1D0/20D0
                C_B(3) = 49D0/180D0
                C_B(4) = 49D0/180D0
                C_B(5) = 16D0/45D0

                !       Column 1 of C_X          
                C_X(1,1) = 0D0
                C_X(2,1) = 0D0
                C_X(3,1) = 1D0/14D0 + SQRT(21D0)/98D0
                C_X(4,1) = 1D0/14D0 - SQRT(21D0)/98D0
                C_X(5,1) = -5D0/128D0

                !       Column 2 of C_X
                C_X(1,2) = 0D0
                C_X(2,2) = 0D0
                C_X(3,2) = -1D0/14D0 + SQRT(21D0)/98D0
                C_X(4,2) = -1D0/14D0 - SQRT(21D0)/98D0
                C_X(5,2) = 5D0/128D0
                !
                !       Column 3 of C_X
                C_X(1,3) = 0D0
                C_X(2,3) = 0D0
                C_X(3,3) = 0D0
                C_X(4,3) = 0D0
                C_X(5,3) = 7D0*SQRT(21D0)/128D0
                !
                !       Column 4 of C_X
                C_X(1,4) = 0D0
                C_X(2,4) = 0D0
                C_X(3,4) = 0D0
                C_X(4,4) = 0D0
                C_X(5,4) = -7D0*SQRT(21D0)/128D0
         
                !       Column 5 of C_X
                C_X(1,5) = 0D0
                C_X(2,5) = 0D0
                C_X(3,5) = 0D0
                C_X(4,5) = 0D0
                C_X(5,5) = 0D0
        
            CASE (8)
                !    	Define a 10-stage, 8th order MIRK formula.
                !       C_S has been set to 10

                C_C(1) = 0D0
                C_C(2) = 1D0
                C_C(3) = 1D0/4D0
                C_C(4) = 3D0/4D0
                !       C_C(5) = 1D0/2D0
                C_C(5) = 1D0/8D0
                C_C(6) = 7D0/8D0
                C_C(7) = 7D0/14D0 - SQRT(21D0)/14D0
                C_C(8) = 7D0/14D0 + SQRT(21D0)/14D0
                C_C(9) = 1D0/2D0
        
                C_V(1) = 0D0
                C_V(2) = 1D0
                C_V(3) = 5D0/32D0
                C_V(4) = 27D0/32D0
                !       C_V(5) = 0D0
                C_V(5) = BETA
                C_V(6) = 1D0 - BETA
                C_V(7) = 1D0/2D0 - (2211D0 * SQRT(21D0))/19894D0
                C_V(8) = 1D0/2D0 + (2211D0 * SQRT(21D0))/19894D0
                C_V(9) = 1D0/2D0
        
                C_B(1) = 1D0/20D0
                C_B(2) = 1D0/20D0
                C_B(3) = 0D0
                C_B(4) = 0D0
                !       C_B(5) = 0D0
                C_B(5) = 0D0
                C_B(6) = 0D0
                C_B(7) = 49D0/180D0
                C_B(8) = 49D0/180D0
                C_B(9) = 16D0/45D0
        
                !		Column 1 of C_X

                C_X(1,1) = 0D0
                C_X(2,1) = 0D0
                C_X(3,1) = 9D0/64D0
                C_X(4,1) = 3D0/64D0
                !		C_X(5,1) = -5D0/24D0
                C_X(5,1) = 757D0/9216D0 - BETA/18D0
                C_X(6,1) = -43D0/9216D0 + BETA/18D0
                C_X(7,1) = 3451D0/139258D0 + (717 * SQRT(21D0))/139258D0
                C_X(8,1) = 3451D0/139258D0 - (717 * SQRT(21D0))/139258D0
                C_X(9,1) = 29D0/896D0
		
                !		Column 2 of C_X
                C_X(1,2) = 0D0
                C_X(2,2) = 0D0
                C_X(3,2) = -3D0/64D0
                C_X(4,2) = -9D0/64D0
                !		C_X(5,2) = 5D0/24D0
                C_X(5,2) = 43D0/9216D0 - BETA/18D0
                C_X(6,2) = -757D0/9216D0 + BETA/18D0
                C_X(7,2) = -3451D0/139258D0 + (717 * SQRT(21D0))/139258D0
                C_X(8,2) = -3451D0/139258D0 - (717 * SQRT(21D0))/139258D0
                C_X(9,2) = -29D0/896D0
		
                !       Column 3 of C_X
                C_X(1,3) = 0D0
                C_X(2,3) = 0D0
                C_X(3,3) = 0D0
                C_X(4,3) = 0D0
                !		C_X(5,3) = 2D0/3D0
                C_X(5,3) = 235D0/4608D0 - (4D0 * BETA)/9D0
                C_X(6,3) = 59D0/4608D0 + (4D0 * BETA)/9D0
                C_X(7,3) = 0D0
                C_X(8,3) = 0D0
                C_X(9,3) = 0D0
		
                !		Column 4 of C_X 
                C_X(1,4) = 0D0
                C_X(2,4) = 0D0
                C_X(3,4) = 0D0
                C_X(4,4) = 0D0
                !		C_X(5,4) = -2D0/3D0
                C_X(5,4) = -59D0/4608D0 - (4D0 * BETA)/9D0
                C_X(6,4) = -235D0/4608D0 + (4D0 * BETA)/9D0
                C_X(7,4) = 0D0
                C_X(8,4) = 0D0
                C_X(9,4) = 0D0
		
                !       Column 5 of C_X
                !		C_X(1,5) = 0D0
                !		C_X(2,5) = 0D0
                !		C_X(3,5) = 0D0
                !		C_X(4,5) = 0D0
                !		C_X(5,5) = 0D0
                !		C_X(6,5) = 0D0
                !		C_X(7,5) = 0D0
                !		C_X(8,5) = 0D0
                !		C_X(9,5) = 0D0
                !		C_X(10,5) = 0D0

                !		Column 6 of C_X
                C_X(1,5) = 0D0
                C_X(2,5) = 0D0
                C_X(3,5) = 0D0
                C_X(4,5) = 0D0
                !		C_X(5,6) = 0D0
                C_X(5,5) = 0D0
                C_X(6,5) = 0D0
                C_X(7,5) = 64D0/1029D0 + (1024D0 * SQRT(21D0))/69629D0
                C_X(8,5) = 64D0/1029D0 - (1024D0 * SQRT(21D0))/69629D0
                C_X(9,5) = -2D0/21D0
		
                !		Column 7 of C_X
                C_X(1,6) = 0D0
                C_X(2,6) = 0D0
                C_X(3,6) = 0D0
                C_X(4,6) = 0D0
                !		C_X(5,7) = 0D0
                C_X(5,6) = 0D0
                C_X(6,6) = 0D0
                C_X(7,6) = -64D0/1029D0 + (1024D0 * SQRT(21D0))/69629D0
                C_X(8,6) = -64D0/1029D0 - (1024D0 * SQRT(21D0))/69629D0
                C_X(9,6) = 2D0/21D0
		
                !		Column 8 of C_X
                C_X(1,7) = 0D0
                C_X(2,7) = 0D0
                C_X(3,7) = 0D0
                C_X(4,7) = 0D0
                !		C_X(5,8) = 0D0
                C_X(5,7) = 0D0
                C_X(6,7) = 0D0
                C_X(7,7) = 0D0
                C_X(8,7) = 0D0
                C_X(9,7) = (7D0 * SQRT(21D0))/128D0
		
                !		Column 9 of C_X
                C_X(1,8) = 0D0
                C_X(2,8) = 0D0
                C_X(3,8) = 0D0
                C_X(4,8) = 0D0
                !		C_X(5,8) = 0D0
                C_X(5,8) = 0D0
                C_X(6,8) = 0D0
                C_X(7,8) = 0D0
                C_X(8,8) = 0D0
                C_X(9,8) = -(7D0 * SQRT(21D0))/128D0

                !		Column 10 of C_X
                C_X(1,9) = 0D0
                C_X(2,9) = 0D0
                C_X(3,9) = 0D0
                C_X(4,9) = 0D0
                !		C_X(5,10) = 0D0
                C_X(5,9) = 0D0
                C_X(6,9) = 0D0
                C_X(7,9) = 0D0
                C_X(8,9) = 0D0
                C_X(9,9) = 0D0
        END SELECT
   
    END SUBROUTINE RK_TABLEAU

    
    SUBROUTINE SOL_EVAL(NODE,NEQN,IWORK,WORK,T,Z,Z_PRIME)
        ! This routine evaluates the interpolant at T.  The value is returned in Z and
        ! optionally the value of the first derivative is returned in Z_PRIME. NEQN is 
        ! NODE, the number of ODEs, plus the number of unknown parameters. IWORK and WORK 
        ! contain information needed by the interpolant. The IWORK array contains
        ! S, the number of discrete Runge-Kutta stages, S_STAR, the total number of 
        ! Runge-Kutta stages, and BVP_METHOD, which specifies which Runge-Kutta method is
        ! used. The WORK array contains K_DISCRETE, the discrete stages; K_INTERP, the 
        ! continuous stages; MESH, the mesh points; and Y, the current solution 
        ! approximation at the mesh points.
        !--------------------------------------------------------------------------------
        !     CALLED BY: BVP_EVAL
        !     CALLS: INTERVAL,INTERP_WEIGHTS,SUM_STAGES
        !--------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NODE,NEQN,IWORK(:)
        DOUBLE PRECISION :: WORK(:),T

        !   Output arguments:
        DOUBLE PRECISION :: Z(NODE)
        DOUBLE PRECISION, OPTIONAL :: Z_PRIME(NODE)

        !   Local variables:
        INTEGER :: S,S_STAR ! Number of discrete and continous Runge-Kutta stages.
        INTEGER :: I ! Index to Ith subinterval.
        INTEGER :: I_K_DISCRETE,I_K_INTERP,I_MESH,I_Y ! Work array indices.
        DOUBLE PRECISION :: HI ! Size of subinterval I.
        DOUBLE PRECISION :: TAU ! Relative position of T in subinterval I.
        DOUBLE PRECISION :: WEIGHTS(MXS) ! Weight polynomials for interpolant
                                         ! evaluated at TAU.
        DOUBLE PRECISION :: WEIGHTS_PRIME(MXS) ! Derivatives of weight polynomials
                                               ! evaluated at TAU.
        LOGICAL :: HAVE_Z_PRIME ! Records whether or not Z_PRIME is present
        DOUBLE PRECISION :: Z_LONG(NEQN) ! Contains approximate solution at T in its
                                         ! first NODE locations and approximation to
                                         ! unknown parameters in its last NPAR 
                                         ! locations.
        DOUBLE PRECISION :: Z_PRIME_LONG(NEQN) ! Contains approximation to first 
                                               ! derivative at T in its first NODE 
                                               ! locations and zero in its last 
                                               ! NPAR locations.  
        !   Global variables:
        !   MXS is the maximum number of Runge-Kutta stages.
        !   BVP_METHOD identifies the Runge-Kutta methods.
        !   NSUB is the number of subintervals of the mesh.
        !--------------------------------------------------------------------------------
        HAVE_Z_PRIME = PRESENT(Z_PRIME)

        !   Extract and assign values from IWORK.
        S = IWORK(1)
        S_STAR = IWORK(2)
        BVP_METHOD = IWORK(3) ! Assign value to global variable BVP_METHOD for use 
                              ! inside call to INTERP_WEIGHTS.

        !   Set array indicies for convenient access to WORK.
        I_K_DISCRETE = 1
        I_K_INTERP = S*NEQN*NSUB+1
        I_MESH = S_STAR*NEQN*NSUB+1
        I_Y = S_STAR*NEQN*NSUB+(NSUB+1)+1

        !   The INTERVAL subroutine returns the index of an interval containing T,
        !   if T is within the problem domain. Extrapolation is used for T outside 
        !   [A,B] = [WORK(I_MESH), WORK(I_MESH+NSUB)].
        CALL INTERVAL(NSUB,WORK(I_MESH:I_MESH+NSUB),T,I)
                                                                        
        !   Compute TAU, the relative location of T in subinterval I
        !   for use in the evaluation of the weight polynomials.
        HI = WORK(I_MESH+I) - WORK(I_MESH+(I-1))
        TAU = (T - WORK(I_MESH+(I-1)))/HI

        !   Evaluate WEIGHTS and optionally WEIGHTS_PRIME at TAU.
        IF (HAVE_Z_PRIME) THEN
            CALL INTERP_WEIGHTS(S_STAR,TAU,WEIGHTS,WEIGHTS_PRIME)
        ELSE
            CALL INTERP_WEIGHTS(S_STAR,TAU,WEIGHTS)
        END IF

        !   Evaluate interpolant at TAU, and optionally its derivative, by taking 
        !   weighted sums of Runge-Kutta stages through a call to SUM_STAGES. Results are
        !   returned in Z_LONG and Z_PRIME_LONG. The first NODE locations of Z_LONG 
        !   contain the interpolant value; the remaining locations contain the 
        !   approximation to the unknown parameters. The first NODE locations of 
        !   Z_PRIME_LONG contain the derivative approximation; the remaining locations 
        !   are zero.
        IF (HAVE_Z_PRIME) THEN
            CALL SUM_STAGES(NEQN,HI,WORK(I_Y+(I-1)*NEQN:I_Y+I*NEQN),S,                 &
            WORK(I_K_DISCRETE+(I-1)*S*NEQN:I_K_DISCRETE+I*S*NEQN),     &
            S_STAR,WORK(I_K_INTERP+(I-1)*(S_STAR-S)*NEQN:              &
            I_K_INTERP+I*(S_STAR-S)*NEQN),WEIGHTS,                     &
            Z_LONG,WEIGHTS_PRIME,Z_PRIME_LONG) 
        ELSE
            CALL SUM_STAGES(NEQN,HI,WORK(I_Y+(I-1)*NEQN:I_Y+I*NEQN),S,                 &
            WORK(I_K_DISCRETE+(I-1)*S*NEQN:I_K_DISCRETE+I*S*NEQN),     &
            S_STAR,WORK(I_K_INTERP+(I-1)*(S_STAR-S)*NEQN:              &
            I_K_INTERP+I*(S_STAR-S)*NEQN),WEIGHTS,Z_LONG) 
        END IF

        !   Copy first NODE locations of Z_LONG to Z; if Z_PRIME is present, copy 
        !   first NODE locations of Z_PRIME_LONG to Z_PRIME.
        Z = Z_LONG(1:NODE)
        IF (HAVE_Z_PRIME) THEN
            Z_PRIME = Z_PRIME_LONG(1:NODE)
        END IF
    
    END SUBROUTINE SOL_EVAL
  

    SUBROUTINE SUBCOM(NEQN,C_S,HI,TI,YI,YIP1,PHII,K,FSUB)
        ! This subroutine evaluates the residual PHII associated with subinterval I.
        ! The basic formula is PHII = YIP1 - YI - HI* SUM BR*KR.  The evaluation
        ! takes place for the current iterate Y. However, component I of PHI depends 
        ! only on the solution information directly associated with subinterval I,
        ! namely YI and YIP1. The length of this subinterval is HI and the left hand
        ! end point is TI. During the computation of PHII, we compute C_S intermediate 
        ! vectors, KR, R = 1,..,C_S, called the stages of the formula. Each stage is a
        ! vector of length NEQN; NEQN equals the number of ODEs plus the number of 
        ! unknown parameters. The stages are returned in the vector K, one after the 

        ! other. The stages represent evaluations of the ODEs using the FSUB subroutine. 
        !------------------------------------------------------------------------------
        !   CALLED BY: RESID
        !   CALLS: P_FSUB
        !------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,C_S
        DOUBLE PRECISION :: HI,TI,YI(NEQN),YIP1(NEQN)

        !   Output arguments:
        DOUBLE PRECISION :: PHII(NEQN),K(C_S*NEQN)

        !   User-supplied subroutines:
        EXTERNAL FSUB
           
        !   Local variables:
        INTEGER :: R ! Loop index.
        DOUBLE PRECISION :: TR,Y_ARG(NEQN) ! Local storage for T and Y arguments
                                           ! in call to compute stage values.
        DOUBLE PRECISION :: KMATRIX(NEQN,C_S) ! Local representation of K as a
                                              ! 2D array. Reshaped to give the 1D
                                              ! vector K at end of computation.
        !   Global variables:
        !   The calculation uses the C_S-stage MIRK formula defined by  
        !   C_S, C_C, C_V, C_B, and C_X.
        !------------------------------------------------------------------------------
        KMATRIX = 0D0
        DO R = 1,C_S
            Y_ARG = (1D0 - C_V(R))*YI + C_V(R)*YIP1 + HI*MATMUL(KMATRIX,C_X(R,:))
            TR = TI + C_C(R)*HI
            CALL P_FSUB(FSUB,NEQN,TR,Y_ARG,KMATRIX(:,R))
        END DO
    
        PHII = YIP1 - YI - HI*MATMUL(KMATRIX,C_B)

        K = RESHAPE(KMATRIX,(/NEQN*C_S/))
               
    END SUBROUTINE SUBCOM


    SUBROUTINE SUM_STAGES(NEQN,HI,YIM1,S,KI_DISCRETE,S_STAR,KI_INTERP,WEIGHTS,Z,   &
    WEIGHTS_PRIME,Z_PRIME)
        ! Given the stages and corresponding weights, this routine computes the value of 
        ! the continuous extension, Z, and (optionally) its first derivative, Z_PRIME,
        ! as follows:  Z = YIM1 + HI* SUM WEIGHTS(R)*KI(R)
        !              Z_PRIME = SUM WEIGHTS_PRIME(R)*KI(R)
        ! Here the KI(R)'s are the stages.  The first S stages, contained in KI_DISCRETE,
        ! come from the discrete formula; the remaining (S_STAR-S) stages, contained in
        ! KI_INTERP, are necessary for the interpolant.  Each stage is a vector of
        ! length NEQN; NEQN equals the number of ODEs plus the number of unknown 
        ! parameters.  HI is the size of subinterval I and YIM1 is the discrete solution
        ! at the left end point of the subinterval.  WEIGHTS contains the values of the
        ! weight polynomials of the interpolant and WEIGHTS_PRIME contains the values of
        ! the first derivative of the weight polynomials.
        !---------------------------------------------------------------------------
        !   CALLED BY: DEFECT_EXTIMATE,INTERP_EVAL,SOL_EVAL
        !---------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NEQN,S,S_STAR
        DOUBLE PRECISION :: HI,YIM1(NEQN),KI_DISCRETE(S*NEQN),                      &
        KI_INTERP((S_STAR-S)*NEQN)
        DOUBLE PRECISION :: WEIGHTS(S_STAR)
        DOUBLE PRECISION, OPTIONAL :: WEIGHTS_PRIME(S_STAR)

        !   Output arguments:
        DOUBLE PRECISION :: Z(NEQN)
        DOUBLE PRECISION, OPTIONAL :: Z_PRIME(NEQN)

        !   Local variables:
        INTEGER :: J ! Loop index.
        !---------------------------------------------------------------------------
        !   Compute Z.
        Z = 0D0
        DO J = 1,S
            Z = Z + WEIGHTS(J)*KI_DISCRETE((J-1)*NEQN+1:J*NEQN+1)
        END DO
        DO J = 1,(S_STAR-S)
            Z = Z + WEIGHTS(S+J)*KI_INTERP((J-1)*NEQN+1:J*NEQN+1)
        END DO
        Z = YIM1 + HI*Z

        !   If present, compute Z_PRIME.
        IF (PRESENT(Z_PRIME)) THEN
            Z_PRIME = 0D0
            DO J = 1,S
                Z_PRIME = Z_PRIME + WEIGHTS_PRIME(J)*KI_DISCRETE((J-1)*NEQN+1:J*NEQN+1)
            END DO
            DO J = 1,(S_STAR-S)
                Z_PRIME = Z_PRIME + WEIGHTS_PRIME(S+J)*KI_INTERP((J-1)*NEQN+1:J*NEQN+1)
            END DO
        END IF    
  
    END SUBROUTINE SUM_STAGES


    !================beginning of generic BVP_INIT==========================

    FUNCTION GUESS_1(NODE,LEFTBC,X,Y,P,MAX_NUM_SUBINTERVALS) RESULT(SOL)
        ! Expand constant guess for the solution. Load vector guess, Y, 
        ! for the solution into the columns of the Y field of the solution 
        ! structure, SOL, which has one column for each point of the initial
        ! mesh, X. The vector Y gives NODE initial guesses for the solution 
        ! components and these will be stored in the first NODE locations of
        ! each column of Y. LEFTBC is the number of left boundary conditions. 
        ! We also load the values NPTS (the number of meshpoints) and NODE 
        ! into SOL and initialize the INFO field to zero. P if present gives
        ! initial guesses for the unknown parmeters and these are copied into
        ! the corresponding field of SOL. We also set SOL%MXNSUB, the maximum number 
        ! of subintervals, depending on the MAX_NUM_SUBINTERVALS argument. If X 
        ! contains only the end points, we set a default mesh of 10 equally 
        ! spaced points.
        !-------------------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT,BVP_LINSPACE
        !--------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NODE,LEFTBC
        DOUBLE PRECISION, DIMENSION(:) :: X,Y(NODE)
        DOUBLE PRECISION, DIMENSION(:), OPTIONAL :: P 
        INTEGER, OPTIONAL :: MAX_NUM_SUBINTERVALS

        !   Function output:
        TYPE(BVP_SOL) :: SOL  
    
        !   Local variables:
        INTEGER :: NPTS,IER ! Number of meshpoints and memory allocation error flag.   
        LOGICAL :: DEFAULT_MESH ! Flag to signal default mesh.
        !----------------------------------------------------------------------------------
        !   Set NPTS and check endpoints.
        NPTS = SIZE(X)
        IF (X(1) >= X(NPTS)) THEN
            PRINT *,'The interval [A,B] must have A < B.'
            STOP
        END IF

        !  If NPTS = 2, use default of 10 equally spaced points.      
        DEFAULT_MESH = (NPTS == 2)
        IF (DEFAULT_MESH) NPTS = 10
  
        !  Create storage for allocatable fields of SOL called X and Y, 
        !  where initial mesh and initial solution guess and initial 
        !  parameter guess, if present, will be stored.
        ALLOCATE(SOL%X(NPTS),SOL%Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)

        !  Load X and Y fields; copy Y vector into each column of SOL%Y.
        IF (DEFAULT_MESH) THEN
            SOL%X = BVP_LINSPACE(X(1),X(2),NPTS)
        ELSE
            SOL%X = X
        END IF
        SOL%Y = SPREAD(Y,DIM=2,NCOPIES=NPTS)

        !  Load NPAR and P fields if P present.
        IF (PRESENT(P)) THEN
            ALLOCATE(SOL%PARAMETERS(SIZE(P)),STAT=IER)
            CALL CHECK_STAT(IER)
            SOL%PARAMETERS = P
            SOL%NPAR = SIZE(P)
        ELSE
            SOL%NPAR = 0
        END IF
   
        !  Load remaining fields.
        SOL%NPTS = NPTS
        SOL%NODE = NODE
        SOL%LEFTBC = LEFTBC
        SOL%INFO = 0

        !  Set SOL%MXNSUB.
        IF (PRESENT(MAX_NUM_SUBINTERVALS)) THEN
            SOL%MXNSUB = MAX_NUM_SUBINTERVALS
        ELSE
            SOL%MXNSUB = 3000
        ENDIF
    
    END FUNCTION GUESS_1
  

    FUNCTION GUESS_2(NODE,LEFTBC,X,Y,P,MAX_NUM_SUBINTERVALS) RESULT(SOL)
        ! Load a matrix guess, Y, for the solution - one column for each of 
        ! the points of the initial mesh, X, into the columns of SOL%Y. Each
        ! column of Y gives NODE initial values for the solution components 
        ! which are stored in the first NODE locations of each column of
        ! SOL%Y. LEFTBC is the number of left boundary conditions. We also 
        ! load the values NPTS (the number of meshpoints) and NODE into SOL 
        ! and initialize the INFO field to zero. P if present gives initial 
        ! guesses for the unknown parmeters and these are copied into the 
        ! corresponding field of SOL. We also set SOL%MXNSUB, the maximum number 
        ! of subintervals, depending on the MAX_NUM_SUBINTERVALS argument. If X 
        ! contains only the end points, use a default mesh of 10 equally spaced 
        ! points. 
        !-----------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT,BVP_LINSPACE
        !-----------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NODE,LEFTBC
        DOUBLE PRECISION, DIMENSION(:) :: X
        DOUBLE PRECISION, DIMENSION(NODE,SIZE(X)) :: Y
        DOUBLE PRECISION, DIMENSION(:), OPTIONAL :: P
        INTEGER, OPTIONAL :: MAX_NUM_SUBINTERVALS

        !   Function output:
        TYPE(BVP_SOL) :: SOL 

        !   Local variables:
        INTEGER :: NPTS,IER ! Number of meshpoints and error flag for allocation 
                            ! call.   
        LOGICAL :: DEFAULT_MESH ! Flag to signal default mesh. 
        !----------------------------------------------------------------------------
        !   Set NPTS and check endpoints.
        NPTS = SIZE(X)
        IF (X(1) >= X(NPTS)) THEN
            PRINT *,'The interval [A,B] must have A < B.'
            STOP
        END IF

        !   If NPTS = 2, use default of 10 equally spaced points.      
        DEFAULT_MESH = (NPTS == 2)
        IF (DEFAULT_MESH) NPTS = 10    
        
        !   Create storage for allocatable fields of SOL called X and Y, 
        !   where initial mesh and initial solution guess will be stored.
        ALLOCATE(SOL%X(NPTS),SOL%Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)
    
        !   Load X and Y fields; copy Y array into SOL%Y.
        IF (DEFAULT_MESH) THEN
            SOL%X = BVP_LINSPACE(X(1),X(2),NPTS)
        ELSE
            SOL%X = X
        END IF
        SOL%Y = Y

        !   Load NPAR and P fields if P present.
        IF (PRESENT(P)) THEN
            ALLOCATE(SOL%PARAMETERS(SIZE(P)),STAT=IER)
            CALL CHECK_STAT(IER)
            SOL%PARAMETERS = P
            SOL%NPAR = SIZE(P)
        ELSE
            SOL%NPAR = 0
        END IF

        !   Load remaining fields.
        SOL%NPTS = NPTS
        SOL%NODE = NODE
        SOL%LEFTBC = LEFTBC
        SOL%INFO = 0

        !   Set SOL%MXNSUB.
        IF (PRESENT(MAX_NUM_SUBINTERVALS)) THEN
            SOL%MXNSUB = MAX_NUM_SUBINTERVALS
        ELSE
            SOL%MXNSUB = 3000
        ENDIF
    
    END FUNCTION GUESS_2
  

    FUNCTION GUESS_3(NODE,LEFTBC,X,FCN,P,MAX_NUM_SUBINTERVALS) RESULT(SOL)
        ! Evaluate a subroutine FCN for the initial solution guess at the 
        ! points of the initial mesh, X, and load into the columns of the 
        ! Y field of the solution structure, SOL. The call to FCN will 
        ! provide NODE initial values for the solution components and these
        ! will be stored in the first NODE locations of each column of Y. 
        ! NODE is the number of ODEs. LEFTBC is the number of left boundary 
        ! conditions. We also load the values NPTS (the number of meshpoints) 
        ! and NODE into SOL and initialize the INFO field to zero. P if present 
        ! gives initial guesses for the unknown parmeters and these are copied into
        ! the corresponding field of SOL. We also set SOL%MXNSUB, the maximum number 
        ! of subintervals, depending on the MAX_NUM_SUBINTERVALS argument. If X 
        ! contains only the end points, use a default mesh of 10 equally spaced 
        ! points.
        !----------------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT,BVP_LINSPACE
        !----------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: NODE,LEFTBC
        DOUBLE PRECISION, DIMENSION(:) :: X
        EXTERNAL FCN
        DOUBLE PRECISION, DIMENSION(:), OPTIONAL :: P
        INTEGER, OPTIONAL :: MAX_NUM_SUBINTERVALS

        !   Function output:
        TYPE(BVP_SOL) :: SOL 

        !   Local variables:
        INTEGER :: NPTS,IER,K ! Number of meshpoints, error flag for allocation 
                              ! call, loop index.   
        LOGICAL :: DEFAULT_MESH ! Flag to signal default mesh.   
        !----------------------------------------------------------------------------
        !   Set NPTS and check endpoints.
        NPTS = SIZE(X)
        IF (X(1) >= X(NPTS)) THEN
            PRINT *,'The interval [A,B] must have A < B.'
            STOP
        END IF

        !   If NPTS = 2, use default of 10 equally spaced points.      
        DEFAULT_MESH = (NPTS == 2)
        IF (DEFAULT_MESH) NPTS = 10    
    
        !   Create storage for allocatable fields of SOL called X and Y, 
        !   where initial mesh and initial solution guess and initial 
        !   parameter guess, if present, will be stored.
        ALLOCATE(SOL%X(NPTS),SOL%Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)

        !   Load X and Y fields; copy vector from evaluation of function FCN into 
        !   first NODE locations of each column of SOL%Y. 
        IF (DEFAULT_MESH) THEN
            SOL%X = BVP_LINSPACE(X(1),X(2),NPTS)
        ELSE
            SOL%X = X
        END IF
        DO K = 1,NPTS
            CALL FCN(SOL%X(K),SOL%Y(1:NODE,K))
        END DO

        !   Load NPAR and P fields if P present.
        IF (PRESENT(P)) THEN
            ALLOCATE(SOL%PARAMETERS(SIZE(P)),STAT=IER)
            CALL CHECK_STAT(IER)
            SOL%PARAMETERS = P
            SOL%NPAR = SIZE(P)
        ELSE
            SOL%NPAR = 0
        END IF
    
        !   Load remaining fields.
        SOL%NPTS = NPTS
        SOL%NODE = NODE
        SOL%LEFTBC = LEFTBC
        SOL%INFO = 0

        !   Set SOL%MXNSUB.
        IF (PRESENT(MAX_NUM_SUBINTERVALS)) THEN
            SOL%MXNSUB = MAX_NUM_SUBINTERVALS
        ELSE
            SOL%MXNSUB = 3000
        ENDIF

    END FUNCTION GUESS_3
  
    !================end of generic BVP_INIT===========================

    
    SUBROUTINE P_DF(DFSUB,T,Y,PD,NEQN)
        ! Evaluate analytical partial derivative d FSUB/dY at (T,Y) or d FSUB/dY and
        ! d FSUB/dP at (T,Y,P) if FSUB depends on unknown parameters. (P is available in
        ! Y(NODE+1:NEQN)). If the problem has a singular term, handle this separately. 
        ! DFSUB is the user function which provides the partial derivatives. The partial 
        ! derivatives are returned by this routine in the 2D array PD, which is of size 
        ! NEQN by NEQN, where NEQN equals the number of ODEs plus the number of unknown 
        ! parameters.
        !------------------------------------------------------------------------------
        !     CALLED BY: JACBLK
        !     CALLS: DFSUB
        !------------------------------------------------------------------------------
        !   Input arguments:
        EXTERNAL DFSUB  
        INTEGER :: NEQN  
        DOUBLE PRECISION :: T,Y(NEQN)
    
        !   Output argument:    
        DOUBLE PRECISION :: PD(NEQN,NEQN)

        !   Local variable:
        DOUBLE PRECISION :: A

        !   Global variables:
        !   NPAR is the number of unknown paramters.
        !   NODE is the number of ODEs.
        !   SINGULAR indicates whether or not the BVP has a singular term.
        !   FSING and MSING are quantities associated witht the singular term of
        !     the BVP, if it is present.
        !   G_X is the array of mesh points.
        !------------------------------------------------------------------------------
        PD = 0D0

        !   Call DFSUB with appropriate parameter list, based on whether or not 
        !   there are unknown parameters. This loads the partial derivatives into PD.
        IF (NPAR == 0) THEN
            CALL DFSUB(T,Y,PD)
        ELSE
            !     Call DFSUB with additional P argument (P is available in Y(NODE+1:NEQN)); 
            !     it returns d FSUB/dY and d FSUB/dP in the last two arguments.
            CALL DFSUB(T,Y(1:NODE),Y(NODE+1:NEQN),&
            PD(1:NODE,1:NODE),PD(1:NODE,NODE+1:NEQN))
        END IF
    
        !   If the problem has a singular term, handle the
        !   evaluation of the partial derivatives as a special case.
        IF (SINGULAR) THEN
            !     Extract left end point A from the global array G_X.          
            A = G_X(0)
            !     If T is the left endpoint.
            IF (T <= A) THEN
                PD(1:NODE,1:NODE) = MATMUL(FSING,PD(1:NODE,1:NODE))
            ELSE
                PD(1:NODE,1:NODE) = MSING/(T-A) + PD(1:NODE,1:NODE)
            END IF
        END IF            

    END SUBROUTINE P_DF


    SUBROUTINE PD_F(FSUB,T,Y,PD,NEQN)
        ! Compute divided difference approximation to d FSUB/dY at (T,Y) or d FSUB/dY and
        ! d FSUB/dP at (T,Y,P) if FSUB depends on unknown parameters. (P is available in 
        ! Y(NODE+1:NEQN)). FSUB is the user function which evaluates the right hand side 
        ! of the ODEs. The partial derivatives are returned by this routine in the 2D 
        ! array PD, which is of size NEQN by NEQN, where NEQN equals the number of ODEs 
        ! plus the number of unknown parameters.
        !------------------------------------------------------------------------------
        !     CALLED BY: JACBLK
        !     CALLS: P_FSUB
        !--------------------------------------------------------------------------------
        !   Input arguments:
        EXTERNAL FSUB  
        INTEGER :: NEQN  
        DOUBLE PRECISION :: T,Y(NEQN)

        !   Output argument: 
        DOUBLE PRECISION :: PD(NEQN,NEQN)

        !   Local variables:
        INTEGER :: K ! Loop index.
        DOUBLE PRECISION :: SAVEYK ! Kth component of Y.
        DOUBLE PRECISION :: DELYK ! Denominator for divided difference.
        DOUBLE PRECISION :: FTY(NEQN) ! Value of FSUB at Y.
        DOUBLE PRECISION :: FDELY(NEQN) ! Value of FSUB at Y + DELYK.

        !   Global variables:
        !   SQRTU is the square root of unit roundoff.
        !--------------------------------------------------------------------------------
        !   Evaluate FSUB for input T and Y. P_FSUB calls FSUB with the appropriate 
        !   argument list depending on whether or not FSUB depends on unknown parameters.   
        CALL P_FSUB(FSUB,NEQN,T,Y,FTY)

        !   Compute Jacobian a column at a time.
        DO K = 1,NEQN
     
            SAVEYK = Y(K)
      
            !     Determine appropriate perturbation of Y(K) and perturb Y(K) by this amount.
            IF ( ABS(SAVEYK) > 0D0 ) THEN
                DELYK = SQRTU*ABS(SAVEYK)
            ELSE
                DELYK = SQRTU
            END IF
            Y(K) = SAVEYK + DELYK

            !     Evaluate FSUB for input T and perturbed Y.
            CALL P_FSUB(FSUB,NEQN,T,Y,FDELY)

            !     Compute divided difference giving Kth column of Jacobian. 
            PD(:,K) = (FDELY - FTY) / DELYK
      
            !     Restore Y(K) value.
            Y(K) = SAVEYK

        END DO

    END SUBROUTINE PD_F


    SUBROUTINE P_DBC(DGSUB,YA,YB,PDYA,PDYB,LEFTBC,RIGHTBC,NEQN)
        ! Evaluate analytical partial derivative d GSUB/dYA at (A,YA) and d GSUB/dYB 
        ! at (B,YB) or d GSUB/dYA and d GSUB/dP at (A,YA,P) and d GSUB/dYB and d GSUB/dP 
        ! at (B,YB,P) if GSUB depends on unknown parameters. (P is available in 
        ! YA(NODE+1:NEQN)). DGSUB is the user function which provides the partial 
        ! derivatives. The partial derivatives are returned by this routine in the 2D 
        ! arrays PDYA abd PDYB, which are of size LEFTBC by NEQN and RIGHTBC by NEQN, 
        ! where LEFTBC is the number of left boundary conditions, RIGHTBC is the number 
        ! of right boundary conditions, and NEQN equals the number of ODEs plus the 
        ! number of unknown parameters.
        !------------------------------------------------------------------------------
        !     CALLED BY: NEWMAT
        !------------------------------------------------------------------------------
        !   Input arguments:
        EXTERNAL DGSUB
        INTEGER :: LEFTBC,RIGHTBC,NEQN
        DOUBLE PRECISION :: YA(NEQN),YB(NEQN)

        !   Output arguments:
        DOUBLE PRECISION :: PDYA(LEFTBC,NEQN),PDYB(RIGHTBC,NEQN)

        !   Global variables:
        !   NODE is the number of ODEs.
        !   NPAR is the number of unknown parameters.
        !-----------------------------------------------------------------------------
        PDYA = 0D0
        PDYB = 0D0

        !   Call DGSUB with appropriate argument list depending on whether or not there
        !   are unknown parameters. 
        IF (NPAR == 0) THEN
            CALL DGSUB(YA,YB,PDYA,PDYB)
        ELSE
            !     The third and fourth arguments of DGSUB return d GSUB/dYA at (A,YA) and 
            !     d GSUB/dYB at (B,YB); P (stored in YA(NODE+1:NEQN)) is input as the fifth 
            !     argument and the sixth and seventh arguments of DGSUB return d GSUB/dP at 
            !     (A,YA,P) and d GSUB/dP at (B,YB,P).
            CALL DGSUB(YA(1:NODE),YB(1:NODE),PDYA(:,1:NODE),&
            PDYB(:,1:NODE),YA(NODE+1:NEQN),&
            PDYA(:,NODE+1:NEQN),PDYB(:,NODE+1:NEQN))
        END IF

    END SUBROUTINE P_DBC
  

    SUBROUTINE PD_BC(GSUB,YA,YB,PDYA,PDYB,LEFTBC,RIGHTBC,NEQN)
        ! Compute divided difference approximation to d GSUB/dYA at (A,YA) and d GSUB/dYB 
        ! at (B,YB) or d GSUB/dYA and d GSUB/dP at (A,YA,P) and d GSUB/dYB and d GSUB/dP 
        ! at (B,YB,P) if GSUB depends on unknown parameters. (P is available in 
        ! YA(NODE+1:NEQN)). GSUB is the user function which evaluates the boundary 
        ! conditions. The partial derivatives are returned by this routine in the 2D 
        ! arrays  PDYA and PDYB. YA is of length LEFTBC and YB is of length RIGHTBC, so 
        ! PDYA is LEFTBC by NEQN and PDYB is RIGHTBC by NEQN, where LEFTBC is the number
        ! of left boundary conditions, RIGHTBC is the number of right boundary conditions, 
        ! and NEQN equals the number of ODEs plus the number of unknown parameters.
        !------------------------------------------------------------------------------
        !     CALLED BY: NEWMAT
        !     CALLS: P_GSUB
        !------------------------------------------------------------------------------
        !   Input arguments:
        EXTERNAL GSUB
        INTEGER :: LEFTBC,RIGHTBC,NEQN
        DOUBLE PRECISION :: YA(NEQN),YB(NEQN)

        !   Output arguments:
        DOUBLE PRECISION :: PDYA(LEFTBC,NEQN),PDYB(RIGHTBC,NEQN)
    
        !   Local variables:
        INTEGER :: K ! Loop index.
        DOUBLE PRECISION :: SAVEYA,SAVEYB ! Kth components of YA and YB.
        DOUBLE PRECISION :: DELYA,DELYB ! Denominators for divided differences.
        DOUBLE PRECISION :: BCA(LEFTBC),BCB(RIGHTBC) ! Boundary conditions at YA and YB.
        DOUBLE PRECISION :: PBCA(LEFTBC),PBCB(RIGHTBC) ! Boundary conditions at 
                                                       ! YA + DELYA and YB + DELYB.

        !   Global variables:
        !   SQRTU is the square root of unit roundoff.
        !------------------------------------------------------------------------------    
        !   Evaluate GSUB for input YA and YB. P_GSUB calls GSUB with the appropriate 
        !   argument list depending on whether or not GSUB depends on unknown parameters.   
        CALL P_GSUB(GSUB,NEQN,LEFTBC,YA,YB,BCA,BCB)

        !   Compute Jacobian a column at a time.
        DO K = 1,NEQN   

            SAVEYA = YA(K)

            !     Determine appropriate perturbation of YA(K) and perturb YA(K) by this amount.      
            IF ( ABS(SAVEYA) > 0D0 ) THEN
                DELYA = SQRTU*ABS(SAVEYA)
            ELSE
                DELYA = SQRTU
            END IF
            YA(K) = SAVEYA + DELYA

            SAVEYB = YB(K)

            !     Determine appropriate perturbation of YB(K) and perturb YB(K) by this amount.      
            IF ( ABS(SAVEYB) > 0D0 ) THEN
                DELYB = SQRTU*ABS(SAVEYB)
            ELSE
                DELYB = SQRTU
            END IF
            YB(K) = SAVEYB + DELYB

            !     Evaluate GSUB at perturbed YA and YB.      
            CALL P_GSUB(GSUB,NEQN,LEFTBC,YA,YB,PBCA,PBCB)

            !     Compute divided difference giving the Kth columns of the Jacobians. 
            PDYA(:,K) = (PBCA - BCA) / DELYA
            PDYB(:,K) = (PBCB - BCB) / DELYB

            !     Restore YA(K) and YB(K) values.
            YA(K) = SAVEYA
            YB(K) = SAVEYB

        END DO

    END SUBROUTINE PD_BC
      

    SUBROUTINE CHECK_STAT(IER)
        ! Check status of an attempted array allocation or deallocation by
        ! checking error flag IER. Print error condition and stop if 
        ! allocation/deallocation fails.
        !---------------------------------------------------------------------
        !   CALLED BY: BVP_SOLVER,MIRKDC,INTERP_TABLEAU,RK_TABLEAU,BVP_INIT,
        !              BVP_GET,BVP_EXTEND
        !---------------------------------------------------------------------
        !   Input argument:
        INTEGER :: IER
        !---------------------------------------------------------------------
        IF (IER /= 0) THEN
            PRINT *,'A storage allocation or deallocation error occurred.'
            STOP
        END IF
        
    END SUBROUTINE CHECK_STAT    
      
  
    FUNCTION BVP_LINSPACE(A,B,NPTS) RESULT(X)
        ! Generate a uniform mesh of NPTS points between A and B (inclusive);
        ! return mesh in array X, locations 1 to NPTS.
        !---------------------------------------------------------------------
        !   CALLED BY: User's program,BVP_INIT
        !---------------------------------------------------------------------
        !   Input arguments: 
        INTEGER :: NPTS,MNPTS
        DOUBLE PRECISION :: A,B
    
        !   Function result:
        DOUBLE PRECISION ::  X(NPTS)

        !   Local variables:
        INTEGER :: K ! Loop index.
        DOUBLE PRECISION :: DELTA ! Subinterval size.
        !----------------------------------------------------------------------
        !   Mesh must have at least two points.
        MNPTS = MAX(NPTS,2)

        DELTA = (B - A) / (MNPTS - 1)
        X = (/ (A + DELTA*(K-1), K = 1,MNPTS) /)

    END FUNCTION BVP_LINSPACE
      

    !================beginning of generic BVP_EVAL===========================

    SUBROUTINE EVAL_V(SOL,T,Z,DERIVATIVE)
        ! Evaluate solution (and optionaly derivative) at all the
        ! evaluation points contained in the vector T.
        ! Solutions returned in the columns of the 2D array Z; 
        ! derivatives in the columns of the 2D array DERIVATIVE,
        ! if this parameter is present. All info required for evaluation 
        ! of the solution and derivative is provided in SOL.
        !------------------------------------------------------------------
        !    CALLED BY: user's program 
        !    CALLS: SOL_EVAL
        !-----------------------------------------------------------------------
        !  Input arguments: 
        TYPE(BVP_SOL) :: SOL
        DOUBLE PRECISION :: T(:)
   
        !  Output arguments:
        DOUBLE PRECISION :: Z(SOL%NODE,SIZE(T))
        DOUBLE PRECISION, OPTIONAL :: DERIVATIVE(SOL%NODE,SIZE(T))

        !  Local variables:
        INTEGER :: NEVALPTS ! Number of evaluation points, i.e. size of T.
        INTEGER :: K ! Loop index.

        !  Global variables:
        !  NSUB is the number of subintervals of the current mesh.
        !-----------------------------------------------------------------------
        NEVALPTS = SIZE(T)
  
        IF (SOL%INFO /= 0) THEN
            WRITE(*,*) 'BVP_SOLVER failed, so the solution cannot be saved.'
            STOP
        END IF 
    
        !   Set global variable NSUB; it is referenced inside SOL_EVAL.
        NSUB = SOL%NPTS - 1
    
        IF (PRESENT(DERIVATIVE)) THEN
            DO K = 1,NEVALPTS
                CALL SOL_EVAL(SOL%NODE,SOL%NODE+SOL%NPAR,SOL%IWORK,SOL%WORK,             &
                T(K),Z(:,K),DERIVATIVE(:,K))
            END DO
        ELSE
            DO K = 1,NEVALPTS
                CALL SOL_EVAL(SOL%NODE,SOL%NODE+SOL%NPAR,SOL%IWORK,SOL%WORK,T(K),Z(:,K))
            END DO
        END IF        

    END SUBROUTINE EVAL_V

  
    SUBROUTINE EVAL_S(SOL,T,Z,DERIVATIVE)
        ! Evaluate solution (and optionaly derivative) at a single 
        ! point T. Solution returned in Z; derivative in DERIVATIVE, if 
        ! this parameter is present. All info required for evaluation
        ! of the solution and derivative in SOL.
        !------------------------------------------------------------------
        !    CALLED BY: user's program,BVP_EXTEND 
        !    CALLS: SOL_EVAL
        !-----------------------------------------------------------------------  
        !  Input arguments: 
        TYPE(BVP_SOL) :: SOL
        DOUBLE PRECISION :: T

        !  Output arguments:
        DOUBLE PRECISION :: Z(SOL%NODE)
        DOUBLE PRECISION, OPTIONAL :: DERIVATIVE(SOL%NODE)

        !  Global variables:
        !  NSUB is the number of subintervals of the current mesh.
        !-----------------------------------------------------------------------
        IF (SOL%INFO /= 0) THEN
            WRITE(*,*) 'BVP_SOLVER failed, so the solution cannot be saved.'
            STOP
        END IF
    
        !   Set global variable NSUB; it is referenced inside SOL_EVAL.
        NSUB = SOL%NPTS - 1

        IF (PRESENT(DERIVATIVE)) THEN
            CALL SOL_EVAL(SOL%NODE,SOL%NODE+SOL%NPAR,SOL%IWORK,SOL%WORK,T,Z,DERIVATIVE)
        ELSE
            CALL SOL_EVAL(SOL%NODE,SOL%NODE+SOL%NPAR,SOL%IWORK,SOL%WORK,T,Z)
        END IF        

    END SUBROUTINE EVAL_S


    SUBROUTINE EVAL_P(SOL,PARAMETERS)
        ! Return parameter values in PARAMETERS. All info required for evaluation
        ! of the parameters is provided in SOL.
        !------------------------------------------------------------------
        !    CALLED BY: user's program 
        !-----------------------------------------------------------------------
        !  Input arguments: 
        TYPE(BVP_SOL) :: SOL

        !  Output arguments:    
        DOUBLE PRECISION :: PARAMETERS(SOL%NPAR)
        !-----------------------------------------------------------------------
        IF (SOL%INFO /= 0) THEN
            WRITE(*,*) 'BVP_SOLVER failed, so the solution cannot be saved.'
            STOP
        END IF
    
        PARAMETERS = SOL%PARAMETERS
    
    END SUBROUTINE EVAL_P

    !================end of generic BVP_EVAL===========================


    FUNCTION EYE(N) RESULT(IDENTITY)
        ! Returns a 2D array, IDENTITY, of size N by N containing identity matrix.
        !-------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !-------------------------------------------------------------------------
        !   Input argument:
        INTEGER :: N

        !   Function result:
        DOUBLE PRECISION :: IDENTITY(N,N)

        !   Local variables:
        INTEGER :: I,J ! Loop indexes.
        !-------------------------------------------------------------------------
        IDENTITY = RESHAPE( (/ ((0D0,J=1,I-1),1D0,(0D0,J=I+1,N),                     &
        I=1,N) /), (/ N,N /) )        
    END FUNCTION EYE
 

    SUBROUTINE DUMMY_DF(T,Y,PD)
        ! Dummy subroutine used to fill in for missing DFSUB in call to MIRKDC 
        ! module. Has same argument list as DFSUB.
        !-------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !-------------------------------------------------------------------------
        !   Input arguments:
        DOUBLE PRECISION :: T,Y(:)
    
        !   Output argument:    
        DOUBLE PRECISION :: PD(:,:)
        !-------------------------------------------------------------------------
        !   Dummy statements to avoid compiler warnings.
        T = 1
        Y(1) = 1
        PD(1,1) = 1

    END SUBROUTINE DUMMY_DF


    SUBROUTINE DUMMY_DBC(YA,YB,DYA,DYB)
        ! Dummy subroutine used to fill in for missing DGSUB in call to MIRKDC 
        ! module. Has same argument list as DGSUB.
        !-------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !-------------------------------------------------------------------------
        !   Input arguments:
        DOUBLE PRECISION :: YA(:),YB(:)

        !   Output arguments:    
        DOUBLE PRECISION :: DYA(:,:),DYB(:,:)
        !-------------------------------------------------------------------------
        !   Dummy statements to avoid compiler warnings.
        YA(1) = 1
        YB(1) = 1
        DYA(1,1) = 1
        DYB(1,1) = 1

    END SUBROUTINE DUMMY_DBC


    SUBROUTINE P_FSUB(FSUB,NEQN,T,Y,FTY)
        ! Calls FSUB at (T,Y) or (T,Y,P) depending on whether or not FSUB depends 
        ! on unknown parameters, P. (P is available in Y(NODE+1:NEQN)). Result is 
        ! returned in FTY. NEQN is the number of ODEs plus the number of unknown 
        ! parameters. Also updates FSUB value to handle singular term, if it is present.
        !------------------------------------------------------------------------
        !     CALLED BY: PD_F,DEFECT_ESTIMATE,INTERP_SETUP,SUBCOM
        !------------------------------------------------------------------------
        !   Input arguments:
        EXTERNAL FSUB
        INTEGER :: NEQN
        DOUBLE PRECISION :: T,Y(NEQN)

        !   Output argument:
        DOUBLE PRECISION :: FTY(NEQN)
    
        !   Local variable:
        DOUBLE PRECISION :: A ! Left endpoint of problem interval.

        !   Global variables:
        !   NODE is the number of ODEs.
        !   NPAR is the number of unknown parameters.
        !   SINGULAR is TRUE if the BVP has a singular term; FALSE otherwise.
        !   FSING,MSING are quantities associated with the treatment of the
        !     singular term.
        !   G_X contains the current mesh.
        !------------------------------------------------------------------------
        !   Call FSUB with appropriate parameter list, based on whether or not 
        !   there are unknown parameters. This loads the function values into FTY.
        IF (NPAR == 0) THEN 
            CALL FSUB(T,Y,FTY)
        ELSE
            !     Set FTY to zero since FSUB will only load first NODE locations.
            !     Call FSUB with additional P argument (Y(NODE+1:NEQN)); function
            !     values are loaded into first NODE locations of FTY.
            FTY = 0D0
            CALL FSUB(T,Y(1:NODE),Y(NODE+1:NEQN),FTY(1:NODE))
        END IF

        !   If the problem has a singular term, handle the
        !   evaluation of the function as a special case.
        IF (SINGULAR) THEN
            !     Extract left end point A from the global array G_X.          
            A = G_X(0) 
            !     If T is the left endpoint.
            IF (T <= A) THEN
                FTY(1:NODE) = MATMUL(FSING,FTY(1:NODE))
            ELSE

                FTY(1:NODE) = MATMUL(MSING,Y(1:NODE))/(T-A) + FTY(1:NODE)
            END IF
        END IF

    END SUBROUTINE P_FSUB


    SUBROUTINE P_GSUB(GSUB,NEQN,LEFTBC,YA,YB,BCA,BCB)
        ! Calls GSUB at (YA,YB) or (YA,YB,P) depending on whether or not GSUB depends 
        ! on unknown parameters, P. (P is available in Y(NODE+1:NEQN)). Results are 
        ! returned in BCA and BCB. NEQN is the number of ODEs plus the number of unknown 
        ! parameters. LEFTBC is the number of left boundary conditions.

        !------------------------------------------------------------------------------------
        !     CALLED BY: RESID,PD_BC
        !------------------------------------------------------------------------------------ 
        !   Input arguments:
        EXTERNAL GSUB
        INTEGER :: NEQN,LEFTBC
        DOUBLE PRECISION :: YA(NEQN),YB(NEQN)

        !   Output arguments:
        DOUBLE PRECISION :: BCA(LEFTBC),BCB(NEQN-LEFTBC)

        !   Global variables:
        !   NPAR is the number of unknown parameters.
        !   NODE is the number of ODEs.
        !------------------------------------------------------------------------------------
        !   Call DGSUB with appropriate argument list depending on whether or not there
        !   are unknown parameters. 
        IF (NPAR == 0) THEN
            CALL GSUB(YA,YB,BCA,BCB)
        ELSE
            !     The third argument is P=YA(NODE+1:NEQN).
            CALL GSUB(YA(1:NODE),YB(1:NODE),YA(NODE+1:NEQN),BCA,BCB)
        END IF

    END SUBROUTINE P_GSUB
 

    SUBROUTINE BVP_SAVE(UNUM,FNAME,SOL)
        ! BVP_SAVE saves the solution structure SOL in a file.  UNUM specifies 
        ! the unit to be OPENed.  The unit is CLOSEd after use. The string FNAME 
        ! specifies the name of the file. The structure can be retrieved from 
        ! this file with BVP_GET. Note that BVP_SAVE does not deallocate the
        ! array fields of SOL. This can be done afterwards with a call to
        ! BVP_TERMINATE.
        !------------------------------------------------------------------------
        !     CALLED BY: User's program.
        !------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: UNUM
        CHARACTER(LEN=*) :: FNAME
        TYPE(BVP_SOL) :: SOL

        !   Local variables:
        INTEGER :: LIWORK,LWORK ! Lengths of IWORK and WORK fields of SOL.
        INTEGER :: I,J ! Loop indexes.
        !------------------------------------------------------------------------
        IF (SOL%INFO /= 0) THEN 
            WRITE(*,*) 'BVP_SOLVER failed, so the solution cannot be saved.'
            STOP
        END IF

        OPEN(UNIT=UNUM,FILE=FNAME,FORM="UNFORMATTED",                                &
        STATUS="REPLACE",POSITION="REWIND") 
    
        WRITE(UNUM) SOL%NODE,SOL%NPAR,SOL%LEFTBC,SOL%NPTS,SOL%INFO

        !   Need the lengths of two arrays of pointers.
        LIWORK = SIZE(SOL%IWORK)
        LWORK = SIZE(SOL%WORK)
        WRITE(UNUM) LIWORK,LWORK
    
        DO I = 1,SOL%NPTS
            WRITE(UNUM) SOL%X(I)
        END DO
        DO I = 1,LIWORK
            WRITE(UNUM) SOL%IWORK(I)
        END DO
        DO I = 1,LWORK
            WRITE(UNUM) SOL%WORK(I)
        END DO
        DO I = 1,SOL%NODE
            DO J = 1,SOL%NPTS
                WRITE(UNUM) SOL%Y(I,J)
            END DO
        END DO
        IF (SOL%NPAR > 0) THEN
            DO I = 1,SOL%NPAR
                WRITE(UNUM) SOL%PARAMETERS(I)
            END DO
        END IF
    
        CLOSE(UNUM)

    END SUBROUTINE BVP_SAVE
  

    SUBROUTINE BVP_GET(UNUM,FNAME,SOL)
        ! BVP_GET forms the solution structure SOL from data stored by
        ! BVP_SAVE in the file FNAME.  UNUM specifies the unit to be 
        ! OPENed. The unit is CLOSEd after use.
        !--------------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT
        !--------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: UNUM
        CHARACTER(LEN=*) :: FNAME

        !   Output argument:    
        TYPE(BVP_SOL) :: SOL

        !   Local variables:    
        INTEGER :: NODE,NPAR,NPTS ! Number of ODEs, number of unknown parameters,
                                  ! Number of meshpoints.
        INTEGER :: LIWORK,LWORK ! Lengths of IWORK and WORK fields of SOL.
        INTEGER :: IER,I,J ! Error flag for array allocation, loop indexes.
        !----------------------------------------------------------------------------
        OPEN(UNIT=UNUM,file=FNAME,FORM="UNFORMATTED",                                &
        STATUS="OLD",POSITION="REWIND") 
        READ(UNUM) SOL%NODE,SOL%NPAR,SOL%LEFTBC,SOL%NPTS,SOL%INFO
        NODE = SOL%NODE
        NPAR = SOL%NPAR    
        NPTS = SOL%NPTS
   
        READ(UNUM) LIWORK,LWORK
    
        IF (NPAR == 0) THEN
            ALLOCATE(SOL%X(NPTS),SOL%IWORK(LIWORK),SOL%WORK(LWORK),SOL%Y(NODE,NPTS),   &
            STAT=IER)
        ELSE
            ALLOCATE(SOL%X(NPTS),SOL%IWORK(LIWORK),SOL%WORK(LWORK),SOL%Y(NODE,NPTS),   &
            SOL%PARAMETERS(NPAR),STAT=IER)      
        END IF
        CALL CHECK_STAT(IER)
    
        DO I = 1,SOL%NPTS
            READ(UNUM) SOL%X(I)
        END DO
        DO I = 1,LIWORK
            READ(UNUM) SOL%IWORK(I)
        END DO
        DO I = 1,LWORK
            READ(UNUM) SOL%WORK(I)
        END DO
        DO I = 1,SOL%NODE
            DO J = 1,SOL%NPTS
                READ(UNUM) SOL%Y(I,J)
            END DO
        END DO
        IF (SOL%NPAR > 0) THEN
            DO I = 1,SOL%NPAR
                READ(UNUM) SOL%PARAMETERS(I)
            END DO
        END IF
   
        CLOSE(UNUM)

    END SUBROUTINE BVP_GET

    FUNCTION PINVSQ(A,N) RESULT(APLUS)
        ! Compute the pseudoinverse of an N by N matrix A. Result is returned in the
        ! N by N matrix APLUS. Uses LINPACK code DSVDC and its supporting programs in 
        ! BVP_LA.f. The tolerance is the same as in Matlab's PINV.
        !-------------------------------------------------------------------------------
        !     CALLED BY: BVP_SOLVER
        !     CALLS: DSVDC
        !-------------------------------------------------------------------------------
        !   Input arguments:
        INTEGER :: N
        DOUBLE PRECISION :: A(N,N)

        !   Function argument: 
        DOUBLE PRECISION :: APLUS(N,N)
   
        !   Local variables:
        DOUBLE PRECISION :: COPYA(N,N) ! Saves copy of A.
        INTEGER :: INFO,I ! Info flag for DSVDC, Loop index.
        DOUBLE PRECISION :: S(N),U(N,N),V(N,N) ! S is a vector of singular values in 
                                               ! descending order of magnitude. U is 
                                               ! a matrix of left singular vectors. 
                                               ! V is a matrix of right singular 
                                               ! vectors.  
        DOUBLE PRECISION :: E(N),WORK(N) ! Work arrays required by DSVDC.
        DOUBLE PRECISION :: TOL ! Tolerance on the singular values.

        !   Global variable:
        !   UROUND is the machine unit round off.
        EXTERNAL DSVDC
        !--------------------------------------------------------------------------------
        !   Calculate the singular value decomposition with a call to DSVDC. DSVDC gives
        !   S, U, V such that A = U * diag(S) * V'. Then A+ = V * diag(S)+ * U'.  
        !   Here "+" indicates pseudoinverse. 

        !   DSVDC destroys A which has the side effect of destroying it in 
        !   the calling program, so use a local copy.
        COPYA = A
   
        CALL DSVDC(COPYA,N,N,N,S,E,U,N,V,N,WORK,11,INFO)
    
        IF (INFO /= 0) THEN
            PRINT *,'Computation of pseudoinverse failed.'
            STOP
        END IF
    
        !   Form pseudoinverse with tolerance TOL on the singular values.
        TOL = N*S(1)*UROUND
        APLUS = 0D0
        DO I = 1,N
            IF (S(I) < TOL) EXIT
            APLUS(I,:) = U(:,I)/S(I)
        END DO
        APLUS = MATMUL(V,APLUS)  

    END FUNCTION PINVSQ


    !================beginning of generic BVP_EXTEND==========================

    FUNCTION EXTEND_1(SOLIN,ANEW,BNEW,YANEW,YBNEW,P,MAX_NUM_SUBINTERVALS)          &
    RESULT(SOLOUT)   
        ! This function allows the solution in a structure of type BVP_SOL to be
        ! extended to a solution on a bigger interval. A typical call has the form
        !       SOLOUT = BVP_EXTEND(SOLIN,ANEW,BNEW,YANEW,YBNEW)
        ! Here SOLIN is the input solution structure and SOLOUT is the output 
        ! structure.  If the interval used to compute SOLIN was [A,B], the new 
        ! guess is for [ANEW,BNEW]. If ANEW < A, the mesh is extended to ANEW and 
        ! Y is extended to the value YANEW there. If ANEW >= A, the arguments ANEW
        ! and YANEW are ignored. The right end of the interval is treated in the 
        ! same way. If SOL%NPAR > 0, SOLIN%PARAMETERS is used in SOLOUT. To supply a 
        ! different guess P for unknown parameters, use the optional argument P.  
        ! To increase MXNSUB, the maximum number of subintervals allowed, use the 
        ! optional argument MAX_NUM_SUBINTERVALS.
        !---------------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT
        !---------------------------------------------------------------------------
        !   Input arguments:
        TYPE(BVP_SOL) :: SOLIN
        DOUBLE PRECISION :: ANEW,YANEW(:),BNEW,YBNEW(:)
        DOUBLE PRECISION, OPTIONAL :: P(:)
        INTEGER, OPTIONAL :: MAX_NUM_SUBINTERVALS

        !   Function output:
        TYPE(BVP_SOL) :: SOLOUT

        !   Local variables:
        INTEGER :: NODE,NPAR ! Number of ODEs, number of parameters.
        INTEGER :: LEFTBC ! Number of left boundary conditions.
        INTEGER :: NPTS ! Number of meshpoints. 
        INTEGER :: MXNSUB ! Maximum number of subintervals allowed.
    
        INTEGER :: IER ! Error flag for array allocation.
        INTEGER :: MODE ! Records type of extension to be performed.
        DOUBLE PRECISION :: A,B ! Problem interval endpoints.

        !   Temporary storage of array info from SOLIN.
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: X,PARAMETERS
        DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: Y
        !--------------------------------------------------------------------------
        !   Assign scalar info from SOLIN structure to local variables.
        NODE = SOLIN%NODE
        NPAR = SOLIN%NPAR
        LEFTBC = SOLIN%LEFTBC
        NPTS = SOLIN%NPTS
        MXNSUB = SOLIN%MXNSUB

        !   Allocate temporary storage for info from SOLIN array fields and assign
        !   these fields to local arrays.
        ALLOCATE(X(NPTS),Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)
        X = SOLIN%X
        Y = SOLIN%Y
        IF (NPAR > 0) THEN
            ALLOCATE(PARAMETERS(NPAR),STAT=IER)
            CALL CHECK_STAT(IER)
            PARAMETERS = SOLIN%PARAMETERS
        END IF

        !   Relevant info from SOLIN is now unpacked. Note that IWORK and WORK 
        !   arrays of SOLIN will no longer be valid after the mesh and solution 
        !   info is extended. Deallocate all array fields of SOLIN.
        DEALLOCATE(SOLIN%X,SOLIN%Y,SOLIN%IWORK,SOLIN%WORK,STAT=IER)
        CALL CHECK_STAT(IER)
        IF (NPAR > 0) THEN
            DEALLOCATE(SOLIN%PARAMETERS,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF


        !   Determine required type of extension and set MODE and NPTS accordingly.
        A = X(1)
        B = X(NPTS)
        MODE = 0
        IF (ANEW < A) THEN
            NPTS = NPTS + 1
            MODE = 1
        END IF
        IF (B < BNEW) THEN
            NPTS = NPTS + 1      
            IF (MODE == 0) THEN
                MODE = 2
            ELSE
                MODE = 3
            END IF    
        END IF

        !   Pack solution info into SOLOUT structure; allocate array fields as needed.
        SOLOUT%INFO = 0
        SOLOUT%NODE = NODE
        SOLOUT%NPAR = NPAR
        SOLOUT%LEFTBC = LEFTBC
        SOLOUT%NPTS = NPTS
    
        IF (PRESENT(MAX_NUM_SUBINTERVALS)) THEN
            SOLOUT%MXNSUB = MAX_NUM_SUBINTERVALS
        ELSE
            SOLOUT%MXNSUB = MXNSUB
        ENDIF

        IF (NPAR > 0) THEN
            ALLOCATE(SOLOUT%PARAMETERS(NPAR),STAT=IER)
            CALL CHECK_STAT(IER)
            SOLOUT%PARAMETERS = PARAMETERS
        END IF
    
        !   If P is present, assign parameter values to SOLOUT field. Verify that
        !   the number of parameters is not changed.
        IF (PRESENT(P)) THEN
            IF (NPAR /= SIZE(P)) THEN
                PRINT *,'Cannot change the number of unknown parameters.'
                STOP
            END IF
            SOLOUT%PARAMETERS = P
        END IF

        !   Allocate arrays for mesh and solution in SOLOUT.
        ALLOCATE(SOLOUT%X(NPTS),SOLOUT%Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)

        !   Based on value of MODE, load mesh and solution fields appropriately.
        SELECT CASE(MODE)
            CASE (0) ! ANEW = A, B = BNEW
                SOLOUT%X = X
                SOLOUT%Y = Y
            CASE (1) ! ANEW < A, B = BNEW
                SOLOUT%X(1) = ANEW
                SOLOUT%X(2:NPTS) = X
                SOLOUT%Y(:,1) = YANEW
                SOLOUT%Y(:,2:NPTS) = Y
            CASE (2) ! ANEW = A, B < BNEW
                SOLOUT%X(1:NPTS-1) = X
                SOLOUT%X(NPTS) = BNEW
                SOLOUT%Y(:,1:NPTS-1) = Y
                SOLOUT%Y(:,NPTS) = YBNEW
            CASE (3) ! ANEW < A, B < BNEW
                SOLOUT%X(1) = ANEW
                SOLOUT%X(2:NPTS-1) = X
                SOLOUT%X(NPTS) = BNEW
                SOLOUT%Y(:,1) = YANEW
                SOLOUT%Y(:,2:NPTS-1) = Y
                SOLOUT%Y(:,NPTS) = YBNEW
        END SELECT

        !   SOLOUT structure is now loaded. Deallocate temporary arrays.
        DEALLOCATE(X,Y,STAT=IER)
        CALL CHECK_STAT(IER)
        IF (NPAR > 0) THEN
            DEALLOCATE(PARAMETERS,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF

    END FUNCTION EXTEND_1 
  

    FUNCTION EXTEND_2(SOLIN,ANEW,BNEW,ORDER,P,MAX_NUM_SUBINTERVALS)              &
    RESULT(SOLOUT)   
        ! This function allows the solution in a structure of type BVP_SOL to be
        ! extended to a solution on a bigger interval. A typical call has the form
        !       SOLOUT = BVP_EXTEND(SOLIN,ANEW,BNEW,ORDER)
        ! Here SOLIN is the input solution structure and SOLOUT is the output 
        ! structure.  If the interval used to compute SOLIN was [A,B], the new 
        ! guess is for [ANEW,BNEW]. If ANEW < A, the mesh is extended to ANEW and 
        ! Y is extended to a value YANEW there. If ANEW >= A, no action is taken
        ! at the left end.  The value YANEW is computed by extrapolation.  The 
        ! optional integer ORDER indicates the order of the polynomial used.  
        ! If ORDER = 1, linear extrapolation is done and otherwise, constant 
        ! extrapolation (ORDER = 0). The right end of the interval is treated in 
        ! the same way. If SOL%NPAR > 0, SOLIN%PARAMETERS is used in SOLOUT. To supply 
        ! a different guess P for unknown parameters, use the optional argument P.  
        ! To increase MXNSUB, the maximum number of subintervals allowed, use the 
        ! optional argument MAX_NUM_SUBINTERVALS.
        !---------------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT,BVP_EVAL
        !---------------------------------------------------------------------------
        !   Input arguments:
        TYPE(BVP_SOL) :: SOLIN
        DOUBLE PRECISION :: ANEW,BNEW
        INTEGER, OPTIONAL :: ORDER
        DOUBLE PRECISION, OPTIONAL :: P(:)
        INTEGER, OPTIONAL :: MAX_NUM_SUBINTERVALS

        !   Function output:
        TYPE(BVP_SOL) :: SOLOUT

        !   Local variables:
        INTEGER :: NODE,NPAR ! Number of ODEs, number of parameters.
        INTEGER :: LEFTBC ! Number of left boundary conditions.
        INTEGER :: NPTS ! Number of meshpoints. 
        INTEGER :: MXNSUB ! Maximum number of subintervals allowed.

        INTEGER :: NPTSIN ! Number of points in input mesh.
        DOUBLE PRECISION, DIMENSION(SOLIN%NODE) :: YANEW,YBNEW ! Solution values at
                                                               ! new endpoints.
        DOUBLE PRECISION, DIMENSION(SOLIN%NODE) :: VTMP1,VTMP2 ! Work arrays.
     
        INTEGER :: DEGREE,IER ! Degree of approximation for extending the solution,
                              ! Error flag for array allocation.
        INTEGER :: MODE ! Records type of extension to be performed.
        DOUBLE PRECISION :: A,B ! Problem interval endpoints.

        !   Temporary storage of array info from SOLIN.
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: X,PARAMETERS
        DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: Y
        !----------------------------------------------------------------------------------

        !   Assign scalar info from SOLIN structure to local variables.
        NODE = SOLIN%NODE
        NPAR = SOLIN%NPAR
        LEFTBC = SOLIN%LEFTBC
        NPTS = SOLIN%NPTS
        MXNSUB = SOLIN%MXNSUB

        !   Allocate temporary storage for info from SOLIN array fields and assign
        !   these fields to local arrays.
        ALLOCATE(X(NPTS),Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)
        X = SOLIN%X
        Y = SOLIN%Y
        IF (NPAR > 0) THEN
            ALLOCATE(PARAMETERS(NPAR),STAT=IER)
            CALL CHECK_STAT(IER)
            PARAMETERS = SOLIN%PARAMETERS
        END IF

        !   Determine required type of extension and set MODE and NPTS accordingly.
        !   Save a copy of the input NPTS value for later use.
        NPTSIN = NPTS
        A = SOLIN%X(1)
        B = SOLIN%X(NPTS)
    
        MODE = 0
        IF (ANEW < A) THEN
            NPTS = NPTS + 1
            MODE = 1
        END IF
        IF (B < BNEW) THEN
            NPTS = NPTS + 1      
            IF (MODE == 0) THEN
                MODE = 2
            ELSE
                MODE = 3
            END IF    
        END IF

        !  Set DEGREE based on value of ORDER. Default is constant extension but
        !  set to linear if ORDER is 1. Uses SOLIN.
        DEGREE = 0
        IF (PRESENT(ORDER)) THEN
            IF (ORDER == 1) DEGREE = 1
        END IF

        IF (DEGREE == 0) THEN
            YANEW = SOLIN%Y(:,1)
            YBNEW = SOLIN%Y(:,NPTSIN)
        ELSE
            !     Linear extrapolation  
            IF (ANEW < A) THEN
                CALL BVP_EVAL(SOLIN,A,VTMP1,VTMP2)
                YANEW = VTMP1 + (ANEW - A)*VTMP2  
            END IF    
            IF (B < BNEW) THEN
                CALL BVP_EVAL(SOLIN,B,VTMP1,VTMP2)
                YBNEW = VTMP1 + (BNEW - B)*VTMP2
            END IF
        END IF
        !   In either case, YANEW and YBNEW are now set appropriately and
        !   SOLIN is no longer needed.

        !   Relevant info from SOLIN is now unpacked. Note that IWORK and WORK 
        !   arrays of SOLIN will no longer be valid after the mesh and solution 
        !   info is extended. Deallocate all array fields of SOLIN.
        DEALLOCATE(SOLIN%X,SOLIN%Y,SOLIN%IWORK,SOLIN%WORK,STAT=IER)
        CALL CHECK_STAT(IER)
        IF (NPAR > 0) THEN
            DEALLOCATE(SOLIN%PARAMETERS,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF


        !   Pack solution info into SOLOUT structure; allocate array fields as needed.
        SOLOUT%INFO = 0
        SOLOUT%NODE = NODE
        SOLOUT%NPAR = NPAR
        SOLOUT%LEFTBC = LEFTBC
        SOLOUT%NPTS = NPTS

        IF (PRESENT(MAX_NUM_SUBINTERVALS)) THEN
            SOLOUT%MXNSUB = MAX_NUM_SUBINTERVALS
        ELSE
            SOLOUT%MXNSUB = MXNSUB
        ENDIF

        IF (NPAR > 0) THEN
            ALLOCATE(SOLOUT%PARAMETERS(NPAR),STAT=IER)
            CALL CHECK_STAT(IER)
            SOLOUT%PARAMETERS = PARAMETERS
        END IF
    
        !   If P is present, assign parameter values to SOLOUT field. Verify that
        !   the number of parameters is not changed.
        IF (PRESENT(P)) THEN
            IF (NPAR /= SIZE(P)) THEN
                PRINT *,'Cannot change the number of unknown parameters.'
                STOP
            END IF
            SOLOUT%PARAMETERS = P
        END IF

        !   Allocate arrays for mesh and solution in SOLOUT.     
        ALLOCATE(SOLOUT%X(NPTS),SOLOUT%Y(NODE,NPTS),STAT=IER)
        CALL CHECK_STAT(IER)

        !   Based on value of MODE, load mesh and solution fields appropriately.
        SELECT CASE(MODE)
            CASE (0) ! ANEW = A, B = BNEW
                SOLOUT%X = X
                SOLOUT%Y = Y
            CASE (1) ! ANEW < A, B = BNEW
                SOLOUT%X(1) = ANEW
                SOLOUT%X(2:NPTS) = X
                SOLOUT%Y(:,1) = YANEW
                SOLOUT%Y(:,2:NPTS) = Y
            CASE (2) ! ANEW = A, B < BNEW
                SOLOUT%X(1:NPTS-1) = X
                SOLOUT%X(NPTS) = BNEW
                SOLOUT%Y(:,1:NPTS-1) = Y
                SOLOUT%Y(:,NPTS) = YBNEW
            CASE (3) ! ANEW < A, B < BNEW
                SOLOUT%X(1) = ANEW
                SOLOUT%X(2:NPTS-1) = X
                SOLOUT%X(NPTS) = BNEW
                SOLOUT%Y(:,1) = YANEW
                SOLOUT%Y(:,2:NPTS-1) = Y
                SOLOUT%Y(:,NPTS) = YBNEW
        END SELECT
    
        !   SOLOUT structure is now loaded. Deallocate temporary arrays.
        DEALLOCATE(X,Y,STAT=IER)
        CALL CHECK_STAT(IER)
        IF (NPAR > 0) THEN
            DEALLOCATE(PARAMETERS,STAT=IER)
            CALL CHECK_STAT(IER)
        END IF
    
    END FUNCTION EXTEND_2 

    !=====================end of generic BVP_EXTEND==========================
  


    SUBROUTINE BVP_TERMINATE(SOL)
        ! BVP_TERMINATE deallocates the allocateable array fields of SOL, namely
        ! X, Y, PARAMETRERS, IWORK, and WORK.
        !--------------------------------------------------------------------------
        !     CALLED BY: User's program
        !     CALLS: CHECK_STAT
        !--------------------------------------------------------------------------
        !   Input/output argument:
        TYPE(BVP_SOL) :: SOL

        !   Local variables:    
        INTEGER :: IER
        !----------------------------------------------------------------------------
        DEALLOCATE(SOL%X,SOL%Y,SOL%IWORK,SOL%WORK,STAT=IER)
        CALL CHECK_STAT(IER)

        IF (SOL%NPAR > 0) THEN
            DEALLOCATE(SOL%PARAMETERS,STAT=IER)
            CALL CHECK_STAT(IER) 
        END IF

    END SUBROUTINE BVP_TERMINATE

    END MODULE BVP_M

