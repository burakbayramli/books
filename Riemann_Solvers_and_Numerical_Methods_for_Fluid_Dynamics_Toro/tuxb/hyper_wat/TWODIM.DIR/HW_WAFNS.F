* 
*----------------------------------------------------------------------*
*                                                                      *
*     Split Finite Volume WAF scheme for the time-dependent            *
C     two dimensional shallow water equations                          *
*                                                                      * 
C     Name of program: hwwafns.f                                       *                                  *
*                                                                      *
C     Purpose: to solve the time-dependent two-dimensional             *
C              shallow water equations on a non-Cartesian              *
C              domain with vertical left and right boundaries.         *
C              The Weigthed Average Flux (WAF) method is used          *
C              in conjunction with the HLLC approximate Riemann        *
C              solver and a second-order dimensional splitting         *
C              scheme. A choice of six limiter functions is            *
C              available. The program is set up to solve two           *
C              types of problems, namely:                              *
*                                                                      *
C              CIRCULAR DAMBREAK.                                      *
C              This problem assumes a circular dam that breaks         *
C              instantaneoulsy. The initial conditions are setup       *
C              by calling routine CIRDAM. The computational domain     *
C              is rectangular and is defined by calling the            *
C              routine RECBOU.                                         *
*                                                                      * 
C              BORE REFLECTION.                                        *
C              This problem computes the bore reflection from a        *
C              wall placed at an angle to the bore propagation         *
C              direction. The initial conditions are setup by          *
C              calling the routine WEDGE.The computational domain      *
C              is defined by calling the routine WEDBOU                *   
*                                                                      *
C     Input  file: cirdam.ini (initial data for circular dam)          * 
C     Input  file: wedge.ini  (initial data for bore reflection)       *
C     Output file: gnuxx.out  (xslice through middle for gnuplot)      *
C     Output file: gnuxy.out  (full 2D results for gnuplot)            *
C     Output file: tecxy.out  (full 2D results for tecplot)            *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 19th August 2000                                  * 
*                                                                      * 
C     Theory is found in Refs. 1 and 2 and original references         *
C     therein                                                          * 
*                                                                      *
C     1. Toro, E. F., "Riemann Solvers and Numerical                   * 
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997                           * 
C                      Second Edition, 1999                            *
*                                                                      *
C     2. Toro, E. F., "Front-Capturing Methods for Free-Surface        *
C                      Shallow Flows"                                  *
C                      John Wiley and Sons (due 2000)                  *
C                                                                      *
C     This program is part of HYPER-WAT,                               * 
C     a sub-library of                                                 * 
*                                                                      *
C     3. Toro, E. F., "NUMERICA: A Library of Source Codes for         *
C                      Teaching, Research and Applications"            *     
C                      NUMERITEK LTD, 1999,                            *
C                      Website: www.numeritek.com                      *
*                                                                      *
*     *************************                                        *
C     *  The Code at a Glance *                                        *
*     *************************                                        *
*                                                                      * 
C     CALL CIRDAM (Defines the problem for circular dam)               * 
C          CALL RECBOU (Defines boundaries of rectangular domain)      *
*                                                                      * 
C     OR                                                               *
*                                                                      * 
C     CALL WEDGE (Defines the problem for bore reflection)             * 
C          CALL WEDBOU (Defines boundaries domain)                     *
*                                                                      * 
C     CALL MESHER (Generates mesh)                                     *  
*                                                                      * 
C-----Time stepping begins                                             *  
*                                                                      * 
C         CALL CFLCON (CFL condition)                                  *  
C         CALL SWEEPS (Apply dimensional splitting)                    * 
C              CALL ONEDIM (Dimensional sweeps)                        *
C                   CALL BCONDI (Boundary conditions)                  *
C                        CALL ROTAFW (Forward rotation of data)        *
C                        CALL ROTABK (Backward rotation of data)       *
C                   CALL ROTAFW (Forward rotation of data)             *
C                   CALL CONFLX (Local flux evaluation)                *  
C                   CALL ESTIME (Speed estimates)                      *  
C                   CALL SUPERA (or other flux limiter)                * 
C                   CALL ROTABK (Backward rotation of flux)            *  
C         CALL OUTPUT (Output results)                                 * 
C                                                                      *        
C-----Time stepping ends                                               *   
*                                                                      * 
*----------------------------------------------------------------------*
*
C     Driver program
*
      IMPLICIT NONE
*
C     Declaration of variables
*     
      INTEGER  N, NFREQU, NTMAXI, PROBLEM, IRESET 
*
      REAL     CFLCOE, DT, TIME, TIMEOU, TIMETO, TIMERESET                      
*
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, TIMEOU
*
C     The problem is defined
*
      WRITE(6,*)
      WRITE(6,*)'Input value to select problem:'
      WRITE(6,*)
      WRITE(6,*)'1 for circular dambreak'
      WRITE(6,*)'2 for bore reflection'
      WRITE(6,*)
*
      READ(5,*)PROBLEM
*
      IF(PROBLEM.EQ.1)THEN
         WRITE(6,*)
         WRITE(6,*)'============================='
         WRITE(6,*)'Circular dambreak; input data' 
         WRITE(6,*)'============================='
         WRITE(6,*)
         CALL CIRDAM
      ELSE
         WRITE(6,*)
         WRITE(6,*)'==========================='
         WRITE(6,*)'Bore reflection; input data' 
         WRITE(6,*)'==========================='
         WRITE(6,*)
         CALL WEDGE
      ENDIF
*
C     Mesh is generated
*
      CALL MESHER 
*
      TIME      = 0.0
      TIMETO    = 1.0E-07
      TIMERESET = 0.2
      IRESET    = 0
* 
C     Time stepping starts
*
      WRITE(6,*)
      WRITE(6,*)'---------------------------------------------'
      WRITE(6,*)'   Time step N        TIME             '
      WRITE(6,*)'---------------------------------------------'
*
      DO 10 N = 1, NTMAXI 
*
C        Reset initial condition to deal with "start up error"
C        that occurs when the initial condition is given 
C        by an exact shock obeying the Rankine-Hugoniot 
C        conditions
*
         IF(TIME.GE.TIMERESET.AND.IRESET.EQ.0.AND.PROBLEM.NE.1)THEN
            IRESET = 1
            CALL RESET
         ENDIF
*
C        CFL condition is applied to compute time step DT
*
         CALL CFLCON(CFLCOE, TIME, TIMEOU, DT)
*
         TIME = TIME + DT
*
C        Solver is called
*
         CALL SWEEPS(DT)
*
         IF(MOD(N,NFREQU).EQ.0)THEN
            WRITE(6,20)N, TIME 
         ENDIF
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Solution written to files at time = TIMEOU
*
            CALL OUTPUT
*
            WRITE(6,*)'---------------------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'---------------------------------------------'
*
            GOTO 30
         ENDIF
*
 20      FORMAT(I12,6X,2(F12.7, 4X))

 10   CONTINUE
*
      WRITE(6,*)'Maximum number of time steps NTMAXI reached'
*
 30   CONTINUE
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE CIRDAM 
*
C     Purpose: to read initial data for two-dimensional circular dam
C              break test problems. The domain is divided into 
C              the inner and outer sections of a circle. Initial 
C              values for depth and velocity components have constant 
C              values in each of these regions. The initial conditions
C              are setup by assigning values to the cells cut by the
C              circle that are proportional to their areas lying 
C              inside and outside the circle.
* 
C     Input variables
*
C     DOMLEX    : Domain length in x-direction
C     ICELLS    : Number of computing cells in x-direction
C     DOMLEY    : Domain length in y-direction
C     JCELLS    : Number of computing cells in y-direction
C     RADIUS    : Radius
C     GRAVIT    : Acceleration due to gravity
C     TIMEOU    : Output time 
C     DINS      : Initial depth      inside  circular dam
C     UINS      : Initial x-velocity inside  circular dam 
C     VINS      : Initial y-velocity inside  circular dam
C     DOUT      : Initial depth      outside circular dam
C     UOUT      : Initial x-velocity outside circular dam
C     VOUT      : Initial y-velocity outside circular dam
C     BCXLEF    : Boundary condition on the left
C     BCXRIG    : Boundary condition on the right
C     BCYBOT    : Boundary condition on the bottom
C     BCYTOP    : Boundary condition on the top
C     CFLCOE    : Courant number coefficient
C     NFREQU    : Output frequency to screen  
C     NTMAXI    : Maximum number of time steps
C     LIMITE    : Limiter function
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
      INTEGER BCXLEF,BCXRIG,BCYBOT,BCYTOP,I,IBCS,ICELLS,IDIM, 
     &        ILIM,J,JBCS,JCELLS,JDIM,JLIM,LIMITE,NFREQU,NTMAXI,L 
*  
      REAL    GRAVIT, D, U, V, C, DINS, UINS, VINS, UOUT, VOUT,               
     &        CFLCOE, DOMLEX, DOMLEY, DOUT, DX, DY, RADIUS, 
     &        TIMEOU, XC, YC, R, XV, YV, RMINU, RPLUS, RABSO
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed
C     to be the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM, JDIM),U(IDIM, JDIM),V(IDIM, JDIM),C(IDIM, JDIM), 
     &          ILIM(2,-1:IDIM+2), IBCS(2,-1:IDIM+2), JLIM(2,-1:IDIM+2),
     &          JBCS(2,-1:IDIM+2), R(4), XV(4), YV(4)
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, TIMEOU
      COMMON /GRAVSS/ GRAVIT
      COMMON /TVDCON/ LIMITE
*
C     Initial data is read in
*
      OPEN(UNIT = 1,FILE = 'cirdam.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEX    
      READ(1,*)ICELLS     
      READ(1,*)DOMLEY    
      READ(1,*)JCELLS     
      READ(1,*)RADIUS    
      READ(1,*)GRAVIT     
      READ(1,*)TIMEOU    
      READ(1,*)DINS    
      READ(1,*)UINS     
      READ(1,*)VINS      
      READ(1,*)DOUT    
      READ(1,*)UOUT    
      READ(1,*)VOUT     
      READ(1,*)BCXLEF 
      READ(1,*)BCXRIG
      READ(1,*)BCYBOT 
      READ(1,*)BCYTOP
      READ(1,*)CFLCOE    
      READ(1,*)NFREQU  
      READ(1,*)NTMAXI   
      READ(1,*)LIMITE  
*
      CLOSE(1)
*
C     Input data is echoed to screen
*
      WRITE(6,*)'DOMLEX = ', DOMLEX    
      WRITE(6,*)'ICELLS = ', ICELLS     
      WRITE(6,*)'DOMLEY = ', DOMLEY    
      WRITE(6,*)'JCELLS = ', JCELLS     
      WRITE(6,*)'RADIUS = ', RADIUS     
      WRITE(6,*)'GRAVIT = ', GRAVIT    
      WRITE(6,*)'TIMEOU = ', TIMEOU    
      WRITE(6,*)'DINS   = ', DINS    
      WRITE(6,*)'UINS   = ', UINS     
      WRITE(6,*)'VINS   = ', VINS       
      WRITE(6,*)'DOUT   = ', DOUT    
      WRITE(6,*)'UOUT   = ', UOUT    
      WRITE(6,*)'VOUT   = ', VOUT     
      WRITE(6,*)'BCXLEF = ', BCXLEF 
      WRITE(6,*)'BCXRIG = ', BCXRIG
      WRITE(6,*)'BCYBOT = ', BCYBOT 
      WRITE(6,*)'BCYTOP = ', BCYTOP
      WRITE(6,*)'CFLCOE = ', CFLCOE    
      WRITE(6,*)'NFREQU = ', NFREQU  
      WRITE(6,*)'NTMAXI = ', NTMAXI   
      WRITE(6,*)'LIMITE = ', LIMITE  
* 
C     Define mesh size in the x and y-directions
*
      DX = DOMLEX/REAL(ICELLS)
      DY = DOMLEY/REAL(JCELLS)
*
C     Define rectangular domain to be used by mesh generator
*
      CALL RECBOU(DOMLEX, DOMLEY)
*
      XC = 0.5*DOMLEX
      YC = 0.5*DOMLEY
*
C     Setup initial conditions
*
      DO 10 J = 1, JCELLS
         YV(1) = (J-1)*DY
         YV(2) = (J-1)*DY
         YV(3) = J*DY
         YV(4) = J*DY
*
         DO 20 I = 1, ICELLS
            XV(1) = (I-1)*DX
            XV(2) = I*DX
            XV(3) = I*DX
            XV(4) = (I-1)*DX
*
            RMINU = 0.0
            RPLUS = 0.0
            RABSO = 0.0
*
            DO 30 L = 1,4
               R(L) = SQRT((XV(L)-XC)**2 + (YV(L)-YC)**2) - RADIUS
               IF(R(L).LE.0.0)RMINU = RMINU + R(L)
               IF(R(L).GE.0.0)RPLUS = RPLUS + R(L)
               RABSO = RABSO + ABS(R(L))
 30         CONTINUE
*
C           Assign initial values
*
            D(I, J) = (ABS(RMINU)*DINS + RPLUS*DOUT)/RABSO
            U(I, J) = (ABS(RMINU)*UINS + RPLUS*UOUT)/RABSO
            V(I, J) = (ABS(RMINU)*VINS + RPLUS*VOUT)/RABSO
*
C           Compute celerity
*
            C(I, J) = SQRT(GRAVIT*D(I,J))
*
 20      CONTINUE
 10   CONTINUE
*
      DO 40 J = 1, JCELLS
*
C        Set limits in the x-direction  
*
         ILIM(1,J) = 1
         ILIM(2,J) = ICELLS
*
C        Set boundary conditions in the x-direction  
*
         IBCS(1,J) = BCXLEF
         IBCS(2,J) = BCXRIG 
*
 40   CONTINUE
*
      DO 50 I = 1, ICELLS
*
C        Set limits in y-direction  
*
         JLIM(1,I) = 1
         JLIM(2,I) = JCELLS
*
C        Set boundary conditions in the y-direction  
*
         JBCS(1,I) = BCYBOT
         JBCS(2,I) = BCYTOP  
* 
 50   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE WEDGE
*
C     Purpose: to read initial data for bore reflection problem 
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
      INTEGER BCXLEF, BCXRIG, BCYBOT, BCYTOP, I, IBCS, ICELLS, IDIM, 
     &        ILIM, J, JBCS, JCELLS, JDIM, JLIM, LIMITE, NFREQU, 
     &        NTMAXI                  
*
      REAL    GRAVIT, DAHEAD, UAHEAD, VAHEAD, CAHEAD, FRAHEAD, DBEHIND, 
     &        UBEHIND, VBEHIND, CBEHIND, C, CFLCOE, D, DOMLEX, DX, 
     &        SHOCKSP, SHOFRO, SHOPOS, TIMEOU, U, V, XP, WEDAPE, 
     &        WEDANG             
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM,JDIM), U(IDIM,JDIM), V(IDIM,JDIM), C(IDIM,JDIM), 
     &          ILIM(2,-1:IDIM+2), IBCS(2,-1:IDIM+2), 
     &          JLIM(2,-1:IDIM+2), JBCS(2,-1:IDIM+2)
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, TIMEOU
      COMMON /GRAVSS/ GRAVIT
      COMMON /TVDCON/ LIMITE
*
C     Definition of input variables
*
C     ICELLS : Number of computing cells in I-direction
C     JCELLS : Number of computing cells in J-direction
C     DOMLEX : Domain length in I-direction
C     WEDAPE : Wedge appex
C     WEDANG : Wedge angle
C     SHOFRO : Bore Froude number
C     SHOPOS : Bore initial position 
C     TIMEOU : Output time 
C     BCXLEF : Boundary condition on the left
C     BCXRIG : Boundary condition on the right
C     BCYBOT : Boundary condition on the bottom
C     BCYTOP : Boundary condition on the top
C     CFLCOE : Courant number coefficient
C     NFREQU : Output frequency to screen  
C     NTMAXI : Maximum number of time steps
C     GRAVIT : Acceleration due to gravity
C     DAHEAD : Initial density ahead of bore
C     UAHEAD : Initial u-velocity ahead of bore
C     VAHEAD : Initial v-velocity ahead of bore 
C     LIMITE : Limiter function
*
C     Initial data is read in
*
      OPEN(UNIT = 1,FILE = 'wedge.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)ICELLS    
      READ(1,*)JCELLS     
      READ(1,*)DOMLEX     
      READ(1,*)WEDAPE    
      READ(1,*)WEDANG     
      READ(1,*)SHOFRO     
      READ(1,*)SHOPOS     
      READ(1,*)TIMEOU  
      READ(1,*)BCXLEF 
      READ(1,*)BCXRIG
      READ(1,*)BCYBOT 
      READ(1,*)BCYTOP
      READ(1,*)CFLCOE   
      READ(1,*)NFREQU    
      READ(1,*)NTMAXI    
      READ(1,*)GRAVIT    
      READ(1,*)DAHEAD    
      READ(1,*)UAHEAD    
      READ(1,*)VAHEAD     
      READ(1,*)LIMITE   
*
      CLOSE(1)
*
C     Echoe input data to screen
*
      WRITE(6,*)'ICELLS =',ICELLS    
      WRITE(6,*)'JCELLS =',JCELLS     
      WRITE(6,*)'DOMLEX =',DOMLEX     
      WRITE(6,*)'WEDAPE =',WEDAPE    
      WRITE(6,*)'WEDANG =',WEDANG     
      WRITE(6,*)'SHOFRO =',SHOFRO     
      WRITE(6,*)'SHOPOS =',SHOPOS     
      WRITE(6,*)'TIMEOU =',TIMEOU  
      WRITE(6,*)'BCXLEF =',BCXLEF 
      WRITE(6,*)'BCXRIG =',BCXRIG
      WRITE(6,*)'BCYBOT =',BCYBOT 
      WRITE(6,*)'BCYTOP =',BCYTOP
      WRITE(6,*)'CFLCOE =',CFLCOE   
      WRITE(6,*)'NFREQU =',NFREQU    
      WRITE(6,*)'NTMAXI =',NTMAXI    
      WRITE(6,*)'GRAVIT =',GRAVIT    
      WRITE(6,*)'DAHEAD =',DAHEAD    
      WRITE(6,*)'UAHEAD =',UAHEAD    
      WRITE(6,*)'VAHEAD =',VAHEAD     
      WRITE(6,*)'LIMITE =',LIMITE   
*
C     Compute I-mesh spacing DX (constant)
*
      DX = DOMLEX/REAL(ICELLS)
*
C     Left and right boundaries are assumed vertical.
C     Bottom and top boundary profiles are defined.
C     Routine WEDBOU needs wedge data WEDAPE and WEDHEI
*   
      CALL WEDBOU(DOMLEX, WEDAPE, WEDANG)
*
C     Initial conditions are set up
*
C     For given state ahead and bore Froude number, 
C     the state behind the bore is calculated
*
      CAHEAD  = SQRT(GRAVIT*DAHEAD)
      FRAHEAD = UAHEAD/CAHEAD
      DBEHIND = 0.5*DAHEAD*(-1.0+SQRT(1.0 + 8.0*(SHOFRO - FRAHEAD)**2))
      UBEHIND = UAHEAD+CAHEAD*(1.0 - DAHEAD/DBEHIND)*(SHOFRO - FRAHEAD)
      SHOCKSP = SHOFRO*CAHEAD
      VBEHIND = 0.0
      CBEHIND = SQRT(GRAVIT*DBEHIND)
*
      WRITE(6,*)
      WRITE(6,*)'  Sate behind shock'
      WRITE(6,*)
      WRITE(6,*)'SHOFRO  =              ',SHOFRO
      WRITE(6,*)'SHOCKSP =              ',SHOCKSP
      WRITE(6,*)'DBEHIND =              ',DBEHIND
      WRITE(6,*)'UBEHIND =              ',UBEHIND
      WRITE(6,*)'CBEHIND =              ',CBEHIND
      WRITE(6,*)'UBEHIND - CBEHIND =    ',UBEHIND-CBEHIND
      WRITE(6,*)
*
      DO 10 J = 1, JCELLS
         DO 20 I = 1, ICELLS
            XP = (REAL(I) - 0.5)*DX
            IF(XP.LE.SHOPOS)THEN
               D(I,J) = DBEHIND
               U(I,J) = UBEHIND
               V(I,J) = VBEHIND
            ELSE
               D(I,J) = DAHEAD
               U(I,J) = UAHEAD
               V(I,J) = VAHEAD
            ENDIF
*
C           Compute celerity
*
            C(I,J) = SQRT(GRAVIT*D(I,J))
*
 20      CONTINUE
 10   CONTINUE
*
      DO 30 J = 1, JCELLS
*	      
C        Set limits in I-direction
*
         ILIM(1, J) = 1
         ILIM(2, J) = ICELLS
*	      
C        Set boundary conditions in I-direction  
*
         IBCS(1, J) = BCXLEF
         IBCS(2, J) = BCXRIG
*
 30   CONTINUE
*
      DO 40 I = 1, ICELLS
*	      
C        Set limits in J-direction
*
         JLIM(1, I) = 1
         JLIM(2, I) = JCELLS
*	      
C        Set boundary conditions in J-direction 
*
         JBCS(1, I) = BCYBOT
         JBCS(2, I) = BCYTOP  
* 
 40   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RESET
*
C     Purpose: to re-set initial data after a reset time given by  
C              the user. This is necessary to deal with the "start-up 
C              error". The exact shock derived from the Rankine-Hugoniot 
C              conditions is replaced by a numerical shock.            
*
      IMPLICIT NONE
*
C     Declaration of variables
*          
       INTEGER  I, ICELLS, IDIM, ISHIFT, ISHOCK, J, JCELLS, JDIM, JM       
*
       REAL     C, D, DX, GRAD, GRADMAX, GRAVIT, U, V         
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM,JDIM), U(IDIM,JDIM), V(IDIM,JDIM), C(IDIM,JDIM)      
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /GRAVSS/ GRAVIT
      DATA ISHIFT /8/
*
      JM      = JCELLS/2
      GRADMAX = 0.0
*
      DO 10 I = ICELLS - 1, 1, -1
         GRAD = ABS(D(I+1,JM) - D(I,JM))
         IF(GRAD.GT.GRADMAX)THEN
            GRADMAX = GRAD
            ISHOCK  = I
         ENDIF
 10   CONTINUE
*
C     Reset ishock
*
      ISHOCK = MAX(ISHOCK - ISHIFT, 1)
*
      WRITE(6,*)'Shock position at resetting time',ISHOCK

C     Reset all values behind ishock position
*
      DO 20 J = 1,JCELLS
         DO 30 I = ISHOCK,1,-1
            D(I,J) = D(ISHOCK ,J)
            U(I,J) = U(ISHOCK ,J)
            C(I,J) = SQRT(GRAVIT*D(I,J))
 30      CONTINUE
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SWEEPS(DT) 
*
C     Purpose: to apply sweeps in the I and J directions in the 
C              solution of the two-dimensional shallow water 
C              equations. Dimensional splitting scheme used is 
C              second-order accurate in time 
*
      IMPLICIT NONE
*
C     Declaration of variables
*         
      INTEGER I, IBCS, ILIM, J, JBCS, JDIM, JLIM, IDIM, MLEF, MBCL,          
     &        MRIG, MBCR, ICELLS, JCELLS   
*
      REAL    C, CO, D, DO, DT, DTS, DX, U, UO, V, VO,
     &        AO, AREA2, SCOSE, SINTL, SLENG, SSENO, XCOSE,
     &        XINTL, XLENG, XSENO, YCOSE, YINTL, YLENG, YSENO              
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),C(IDIM,JDIM),
     &          AREA2(IDIM, JDIM),
     &          XINTL(0:IDIM, 0:JDIM), XLENG(0:IDIM, 0:JDIM),
     &          XCOSE(0:IDIM, 0:JDIM), XSENO(0:IDIM, 0:JDIM),
     &          YINTL(0:IDIM, 0:JDIM), YLENG(0:IDIM, 0:JDIM), 
     &          YCOSE(0:IDIM, 0:JDIM), YSENO(0:IDIM, 0:JDIM),
     &          DO(-1:IDIM+2), UO(-1:IDIM+2), VO(-1:IDIM+2),
     &          CO(-1:IDIM+2), AO(-1:IDIM+2), SINTL(-1:IDIM+2), 
     &          SLENG(-1:IDIM+2), SCOSE(-1:IDIM+2), SSENO(-1:IDIM+2),
     &          ILIM(2,-1:IDIM+2), IBCS(2,-1:IDIM+2),
     &          JLIM(2,-1:IDIM+2), JBCS(2,-1:IDIM+2)
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /XYINTL/ XINTL, YINTL  
      COMMON /GEOM2D/ AREA2,XLENG,XCOSE,XSENO,YLENG,YCOSE, YSENO 
*
C     I-sweep is carried out for half a time step
*
      DTS = 0.5*DT
*
      DO 10 J = 1, JCELLS
*
C        Left and right limits in I-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 20 I = MLEF-1, MRIG
*
C           Initial data and geometrical data are stored 
C           in one-dimensional arrays
*
            IF(I.GE.MLEF)THEN
               DO(I) = D(I, J)
               UO(I) = U(I, J)
               VO(I) = V(I, J)
               CO(I) = C(I, J)
               AO(I) = AREA2(I, J)
            ENDIF
            SINTL(I) = XINTL(I, J)
            SLENG(I) = XLENG(I, J)
            SCOSE(I) = XCOSE(I, J)
            SSENO(I) = XSENO(I, J)
*
 20     CONTINUE
*
C       Solver in the I-direction is called.    
*
        CALL RIEMAN(MLEF, MRIG, MBCL, MBCR, DTS, DO, UO, VO, CO,
     &              AO, SINTL, SLENG, SCOSE, SSENO)
*
C       Store one-dimensional solution back in two-dimensional array
*
        DO 30 I = MLEF, MRIG
*
           D(I, J) = DO(I)
           U(I, J) = UO(I)
           V(I, J) = VO(I)
           C(I, J) = CO(I)
*
 30   CONTINUE
*
 10   CONTINUE
*
C     J-sweep is carried out for a complete time step
*
      DTS = DT
*
      DO 40 I = 1, ICELLS 
*
C        Left and right limits in J-sweep are set
*
         MLEF = JLIM(1, I)
         MRIG = JLIM(2, I)
*
C        Boundary conditions are set
*
         MBCL = JBCS(1, I)
         MBCR = JBCS(2, I)
*
         DO 50 J = MLEF-1, MRIG
*
C           Initial data and geometrical data are stored 
C           in one-dimensional arrays
*
            IF(J.GE.MLEF)THEN
               DO(J) = D(I, J)
               UO(J) = U(I, J)
               VO(J) = V(I, J)
               CO(J) = C(I, J) 
               AO(J) = AREA2(I, J)
            ENDIF
            SINTL(J) = YINTL(I, J)
            SLENG(J) = YLENG(I, J)
            SCOSE(J) = YCOSE(I, J)
            SSENO(J) = YSENO(I, J)
*
 50      CONTINUE
*
C        Solver in the J-direction is called 
*
         CALL RIEMAN(MLEF, MRIG, MBCL, MBCR, DTS, DO, UO, VO, CO,
     &               AO, SINTL, SLENG, SCOSE, SSENO)
*
C        Store one-dimensional solution back in two-dimensional arrays
* 
         DO 60 J = MLEF, MRIG 
*
            D(I, J) = DO(J)
            U(I, J) = UO(J)
            V(I, J) = VO(J)
            C(I, J) = CO(J)
*
 60      CONTINUE
 40   CONTINUE
*
C     I-sweep is carried out for half a time step
*
      DTS = 0.5*DT
*
      DO 70 J = 1, JCELLS
*
C        Left and right limits in I-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 80 I = MLEF-1, MRIG
*
C           Initial data and geometrical data are stored 
C           in one-dimensional arrays
*
            IF(I.GE.MLEF)THEN
               DO(I) = D(I, J)
               UO(I) = U(I, J)
               VO(I) = V(I, J)
               CO(I) = C(I, J)
               AO(I) = AREA2(I, J)
            ENDIF
            SINTL(I) = XINTL(I, J)
            SLENG(I) = XLENG(I, J)
            SCOSE(I) = XCOSE(I, J)
            SSENO(I) = XSENO(I, J)
*
 80     CONTINUE
*
C       Solver in the I-direction is called   
*
        CALL RIEMAN(MLEF, MRIG, MBCL, MBCR, DTS, DO, UO, VO, CO,
     &              AO, SINTL, SLENG, SCOSE, SSENO)
*
C       Store one-dimensional solution back in two-dimensional array
*
        DO 90 I = MLEF, MRIG
*
           D(I, J) = DO(I)
           U(I, J) = UO(I)
           V(I, J) = VO(I)
           C(I, J) = CO(I)
*
 90   CONTINUE
*
 70   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RIEMAN(MLEF, MRIG, MBCL, MBCR, DTS, D, U, V, C, 
     &                  AO, SINTL, SLENG, SCOSE, SSENO)
*
C     Purpose: to compute the TVD WAF flux with the HLLC Riemann 
C              solver and evolve the solution by a time DTS. See
C              Chaps. 10, 14 and 16 of Ref. 1; see also Chaps.
C              8 to 12 of Ref. 2      
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER LIMITE, I, IDIM, IUPW, K, MLEF, MRIG, MBCL, MBCR
*
      REAL    D, U, V, C, CS, AS, US, DL, UL, VL, CL, DR, UR, VR, CR, 
     &        QL, QR, DS, GRAVIT, CN, DTS, DTODS, DLOC, DUPW, FDAL, 
     &        FDAR, FLX, FS, RATIO, SL, SR, TOLLIM, WAFLIM, WJUMP, 
     &        WSPEE, WL, WM, WR, AO, CDL, CDR, FDL, FDR, SCOSE,
     &        SINTL, SLENG, SSENO                 
* 
      PARAMETER (IDIM = 500)
*  
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2), C(-1:IDIM+2),       
     &          AO(-1:IDIM+2), SINTL(-1:IDIM+2), SLENG(-1:IDIM+2), 
     &          SCOSE(-1:IDIM+2),SSENO(-1:IDIM+2), CS(3,-1:IDIM+2),
     &          FDAL(3,-1:IDIM+2),FDAR(3,-1:IDIM+2), FLX(3,0:IDIM), 
     &          FS(3,-1:IDIM+2), WSPEE(3,-1:IDIM+2),WJUMP(3,-1:IDIM+2),
     &          CN(3), CDL(3), CDR(3),FDL(3),FDR(3),WAFLIM(3)                 
*
      COMMON /GRAVSS/ GRAVIT 
      COMMON /TVDCON/ LIMITE
*
      DATA TOLLIM /1.0E-06/
*
C     Apply boundary conditions
*
      CALL BCONDI(MLEF, MRIG, MBCL, MBCR, D, U, V, C, SCOSE, SSENO)
*   
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 10 I = MLEF - 2, MRIG + 1 
*
         DL = D(I)
         UL = U(I)
         VL = V(I)
         CL = C(I)
*
         DR = D(I + 1)
         UR = U(I + 1)
         VR = V(I + 1)
         CR = C(I + 1)
*
C        Rotate left state (inside cell)
*
         CALL ROTAFW(UL, VL, SCOSE(I), SSENO(I))
*
C        Compute first two components of conserved variables CDL 
C        and fluxes FDL on rotated left data state
*
         CALL CONFLX(DL, UL, CDL, FDL)
*
C        Rotate right state (inside cell)
*
         CALL ROTAFW(UR, VR, SCOSE(I), SSENO(I))
*
C        Compute first two components of conserved variables CDR 
C        and fluxes FDR on rotated right data state
*
         CALL CONFLX(DR, UR, CDR, FDR)
*
C        -------------------------------------------------------------
C        HLL Approximate Riemann Solver starts
C        ------------------------------------------------------------- 
*
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solvers
* 
C        First compute Two-Rarefaction solution
*
         AS = 0.5*(CL + CR) + 0.5*(UL - UR)
         DS = AS*AS/GRAVIT
*
         IF(DS.LE.MIN(DL,DR))THEN
*
C           Compute Two-Rarefaction solution for particle velocity
*
            US = 0.5*(UL + UR) + CL - CR
         ELSE
*
C           Compute Two-Shock solution for depth and particle
C           velocity
*
            QL = SQRT(0.5*GRAVIT*(DS + DL)/(DS*DL))           
		  QR = SQRT(0.5*GRAVIT*(DS + DR)/(DS*DR))
            DS = (DL*QL + DR*QR + UL - UR)/(QL + QR)
            US = 0.5*(UL + UR) + 0.5*((DS-DR)*QR - (DS-DL)*QL)
         ENDIF
*
C        Compute wave speeds
* 
         IF(DS.LE.DL)THEN
            SL = UL - CL
         ELSE
            SL = UL - CL*SQRT(0.5*DS*(DS + DL))/DL
         ENDIF
*
         IF(DS.LE.DR)THEN
            SR = UR + CR
         ELSE
            SR = UR + CR*SQRT(0.5*DS*(DS + DR))/DR
         ENDIF
*
C        Compute HLL intercell flux in the STAR REGION for first two
C        flux components
*
         DO 20 K = 1, 2
            FS(K,I)=(SR*FDL(K)-SL*FDR(K)+SL*SR*(CDR(K)-CDL(K)))/(SR-SL)
 20      CONTINUE
*
C        -------------------------------------------------------------
C        HLL Approximate Riemann Solver ends
C        ------------------------------------------------------------- 
*
C        Store wave speeds, needed for TVD WAF flux. 
C        Note order of waves
*
         WSPEE(1,I) = SL 
         WSPEE(2,I) = SR
         WSPEE(3,I) = US
*
C        Store wave jumps, needed for TVD WAF flux
*
         WJUMP(1,I) = DS - DL
         WJUMP(2,I) = DR - DS
         WJUMP(3,I) = VR - VL
* 
C        Store first two components of fluxes on rotated data
*
         DO 30 K = 1, 2
            FDAL(K, I)= FDL(K)
            FDAR(K, I)= FDR(K)
 30      CONTINUE
*
 10   CONTINUE
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux starts
C     -----------------------------------------------------------------
*
      DO 40 I = MLEF - 1, MRIG  
*
         DTODS = DTS/SINTL(I)
*
C        Apply TVD condition
*  
         DO 50 K = 1, 3
*
C           Compute Courant numbers for each wave
*
            CN(K) = WSPEE(K, I)*DTODS
*
C           Identify upwind direction
*
            IF(CN(K).GE.0.0)THEN
*
C              Wave k is positive and upwind direction is on the left
*
               IUPW = -1
            ELSE
*
C              Wave k is negative and upwind direction is on the right
*
               IUPW =  1
            ENDIF
*
C           Retrieve local and upwind wave jumps
*
            DLOC = WJUMP(K, I)
            DUPW = WJUMP(K, I + IUPW)
*
C           Modify small jumps preserving sign
*
            IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0, DUPW)
            IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0, DLOC)
*
C           Compute RATIO of upwind jump to local jump
*
            RATIO = DUPW/DLOC
*
C           Select limiter function WAFLIM 
*
C           LIMITE = 1, Godunov's Method
C           LIMITE = 2, Second Order Method (oscillatory)
C           LIMITE = 3, Upwind TVD, with SUPERBEE type limiter 
C           LIMITE = 4, Upwind TVD, with VAN LEER type limiter 
C           LIMITE = 5, Upwind TVD, with VAN ALBADA type limiter 
C           LIMITE = 6, Upwind TVD, with MINMOD type limiter 
*
            IF(LIMITE.EQ.1)WAFLIM(K) = 1.0
            IF(LIMITE.EQ.2)WAFLIM(K) = ABS(CN(K)) 
            IF(LIMITE.EQ.3)CALL SUPERA(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE.EQ.4)CALL VANLEE(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE.EQ.5)CALL VANALB(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE.EQ.6)CALL MINAAA(RATIO, ABS(CN(K)), WAFLIM(K)) 
*
            WAFLIM(K) = WAFLIM(K)*SIGN(1.0, CN(K))
*
 50      CONTINUE 
*
C        Compute weights for first two components of WAF 
C        intercell flux
*
         WL = 0.5*(1.0 + WAFLIM(1))
         WM = 0.5*(WAFLIM(2) - WAFLIM(1))
         WR = 0.5*(1.0 - WAFLIM(2))
*
C        Compute first two components of WAF intercell flux (I,I+1) 
C        and store them in position I
*
         DO 60  K = 1, 2
*
            FLX(K,I) = WL*FDAL(K,I) + WM*FS(K,I) + WR*FDAR(K,I)
*
C           Multiply flux by side length
*
            FLX(K,I) = SLENG(I)*FLX(K,I) 
*
 60      CONTINUE
*
C        Compute weights for third flux component of WAF 
C        intercell flux
*
         WL = 0.5*(1.0 + WAFLIM(3))
         WR = 0.5*(1.0 - WAFLIM(3))
*
         UL = U(I)
         VL = V(I)
         UR = U(I+1)
         VR = V(I+1)
*
         CALL ROTAFW(UL, VL, SCOSE(I), SSENO(I))
         CALL ROTAFW(UR, VR, SCOSE(I), SSENO(I))
*
C        Compute third flux component FLX(3,I)
*
         FLX(3,I) = WL*FLX(1,I)*VL + WR*FLX(1,I)*VR 
*
C        Rotate flux components FLX(2,I), FLX(3,I) back
*
         CALL ROTABK(FLX(2,I), FLX(3,I), SCOSE(I), SSENO(I))
*
 40   CONTINUE
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux ends
C     -----------------------------------------------------------------
*
      DO 70 I = MLEF, MRIG
*
C        Compute conserved variables on data within domain
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
         CS(3,I) = D(I)*V(I)
* 
 70   CONTINUE
*   	
      DO 80 I = MLEF, MRIG
*
C        Update conserved variables
* 
         DTODS = DTS/AO(I)
*
         DO 90 K = 1, 3
*
            CS(K,I) = CS(K,I) - DTODS*(FLX(K,I) - FLX(K,I-1))
*
 90      CONTINUE
*
C        Compute physical variables
*
         D(I) = CS(1,I)
         U(I) = CS(2,I)/D(I)
         V(I) = CS(3,I)/D(I)
         C(I) = SQRT(GRAVIT*D(I))
*
 80   CONTINUE
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, TIME, TIMEOU, DT)
* 
C     Purpose: to apply CFL condition to compute time step DT
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
*      
      INTEGER  I, IDIM, J, JDIM, ICELLS, JCELLS  
*
      REAL     C, CFLCOE, D, DT, DTL, DX, SLX, SLY, SPX, SPY, TIME, 
     &         TIMEOU, U, V, XINTL, YINTL  
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),C(IDIM,JDIM),
     &          XINTL(0:IDIM, 0:JDIM), YINTL(0:IDIM, 0:JDIM) 
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /XYINTL/ XINTL, YINTL
      COMMON /MESHXY/ DX, ICELLS, JCELLS
*
      DT = 1.0E+10
*
      DO 10 I = 1, ICELLS
         DO 20 J = 1, JCELLS
*
C           Find characteristic speeds in each direction
*
            SPX = C(I,J) + ABS(U(I,J))
            SPY = C(I,J) + ABS(V(I,J))
*
C           Find characteristic lengths in each direction
*
            SLX = MIN(XINTL(I-1,J), XINTL(I,J))
            SLY = MIN(YINTL(I,J-1), YINTL(I,J))
*
C           Find local time DTL for cell (i, j) 
*
            DTL = MIN(SLX/SPX, SLY/SPY)
* 
            IF(DTL.LT.DT)DT = DTL 
* 
 20      CONTINUE 
 10   CONTINUE
*
C     Scale time step DT by CFL coefficient
*
      DT = CFLCOE*DT
*
C     Reduce size of DT if TIMEOU is exceeded
*
      IF((TIME + DT).GT.TIMEOU)THEN
         DT = TIMEOU - TIME
      ENDIF
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE OUTPUT
*
C     Purpose: to print out solution at TIMEOU time to files: 
C              gnuxx.out (x-slice along middle, use GNUPLOT)
C              gnuxy.out (full 2D solution, use GNUPLOT)
C              tecxy.out (full 2d solution, use TECPLOT)
*                          
      IMPLICIT NONE
*
C     Declaration of variables
* 
      INTEGER I, IDIM, ICELLS, J, JCELLS, JDIM, JH 
* 
      REAL    C, D, DX, U, V, XPOS, XV, YV, XCM, YCM, VEL              
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),C(IDIM,JDIM),
     &          XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1)  
*
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /SOLUTI/ D, U, V, C
      COMMON /VERTEX/ XV, YV
*
C     Print numerical results for GNUPLOT package
*
      OPEN(UNIT = 1, FILE = 'gnuxx.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 2, FILE = 'gnuxy.out', STATUS = 'UNKNOWN') 
*
      JH = JCELLS/2
*
C     Print out solution along a J = JH (constant) slice
*
      DO 10 I = 1, ICELLS
         XPOS = (REAL(I) - 0.5)*DX  
         VEL  = SQRT(U(I,JH)**2 + V(I,JH)**2)
         WRITE(1,20)XPOS, D(I,JH), U(I,JH), V(I,JH), VEL
 10   CONTINUE 
*  
C     Print out solution along a I = IH (constant) slice
*  
      CLOSE(1)
* 
      WRITE(2,30) '#', ICELLS, JCELLS
*
      DO 40 J = 1, JCELLS
         DO 50 I = 1, ICELLS
*
C           Compute coordinates of centre of mass
*
            XCM = 0.25*(XV(I-1,J-1)+XV(I,J-1)+XV(I,J)+XV(I-1,J))
            YCM = 0.25*(YV(I-1,J-1)+YV(I,J-1)+YV(I,J)+YV(I-1,J))
*
            VEL = SQRT(U(I,J)**2 + V(I,J)**2)
*
            WRITE(2,60)XCM, YCM, D(I, J), U(I, J), V(I,J), VEL
*
 50      CONTINUE
         WRITE(2,*)  
 40    CONTINUE
*    
      CLOSE(2)
*
C     Print numerical results for TECPLOT package
*
      OPEN(UNIT = 1, FILE = 'tecxy.dat', STATUS = 'UNKNOWN')
*
      WRITE(1,*)'TITLE="circular" '
      WRITE(1,*)'VARIABLES="X" "Y" "D" "U" "V" "VEL"'
      WRITE(1,*)'ZONE',',I=',ICELLS, ',J=',JCELLS,',F="POINT"'
*
      DO 70 J = 1, JCELLS
         DO 80 I = 1, ICELLS
*
C           Compute coordinates of centre of mass
*
            XCM = 0.25*(XV(I-1,J-1)+XV(I,J-1)+XV(I,J)+XV(I-1,J))
            YCM = 0.25*(YV(I-1,J-1)+YV(I,J-1)+YV(I,J)+YV(I-1,J))
*
            VEL = SQRT(U(I,J)**2 + V(I,J)**2)
*
            WRITE(1,*)XCM, YCM, D(I,J), U(I,J), V(I,J), VEL
*
 80      CONTINUE
 70   CONTINUE
*    
      CLOSE(1)
*  
 20   FORMAT(F10.6,2X,4(F12.4,1X))
 30   FORMAT(A1,1X,I6,I6)
 60   FORMAT(2(F10.6,1X),2X,4(F12.4,1X))
*  
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(MLEF,MRIG,MBCL,MBCR,D,U,V,C,SCOSE,SSENO) 
*
C     Purpose: to set boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER MLEF, MBCL, MRIG, MBCR, IDIM
*
      REAL    D, U, V, C, SCOSE, SSENO
*
      PARAMETER (IDIM = 500)
* 
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),V(-1:IDIM+2),C(-1:IDIM+2), 
     &          SCOSE(-1:IDIM+2), SSENO(-1:IDIM+2)             
*
*     --------------------------------------
C     Boundary conditions for left boundary
*     --------------------------------------
*
      D(MLEF - 2) = D(MLEF + 1)
      V(MLEF - 2) = V(MLEF + 1)
      C(MLEF - 2) = C(MLEF + 1)
*
      D(MLEF - 1) = D(MLEF)
      V(MLEF - 1) = V(MLEF)
      C(MLEF - 1) = C(MLEF)
*
C     Set orientation of fictitious boundary as that of the
C     real boundary on the left
*
      SCOSE(MLEF - 2) = SCOSE(MLEF - 1)
      SSENO(MLEF - 2) = SSENO(MLEF - 1)
*
C     Rotate forward states MLEF and MLEF + 1 
*
      CALL ROTAFW(U(MLEF),  V(MLEF),  SCOSE(MLEF-1),SSENO(MLEF-1))
      CALL ROTAFW(U(MLEF+1),V(MLEF+1),SCOSE(MLEF-1),SSENO(MLEF-1))
*
      IF(MBCL.EQ.0)THEN
*
C        Transmissive boundary conditions at left end
*  
         U(MLEF - 2) = U(MLEF + 1)   
* 
         U(MLEF - 1) = U(MLEF)
*     
      ELSE
*
C        Reflective boundary conditions at left end
*
         U(MLEF - 2) = -U(MLEF + 1)  
*
         U(MLEF - 1) = -U(MLEF) 
*
      ENDIF  
*
C     Rotate back so that all data can be looped 
*
      CALL ROTABK(U(MLEF-2),V(MLEF-2),SCOSE(MLEF-1),SSENO(MLEF-1))
      CALL ROTABK(U(MLEF-1),V(MLEF-1),SCOSE(MLEF-1),SSENO(MLEF-1))
*
C     Rotate back to recover original data within domain 
*
      CALL ROTABK(U(MLEF),  V(MLEF),  SCOSE(MLEF-1),SSENO(MLEF-1))
      CALL ROTABK(U(MLEF+1),V(MLEF+1),SCOSE(MLEF-1),SSENO(MLEF-1))
*
*     --------------------------------------
C     Boundary conditions for right boundary
*     --------------------------------------
*
      D(MRIG + 2) = D(MRIG - 1)
      V(MRIG + 2) = V(MRIG - 1)
      C(MRIG + 2) = C(MRIG - 1)
*
      D(MRIG + 1) = D(MRIG)
      V(MRIG + 1) = V(MRIG)	
      C(MRIG + 1) = C(MRIG)
*
C     Set orientation of fictitious boundary as that of the
C     real boundary on the right
*
      SCOSE(MRIG + 1) = SCOSE(MRIG)
      SSENO(MRIG + 1) = SSENO(MRIG)
*
C     Rotate forward states MRIG - 1 and MRIG 
*
      CALL ROTAFW(U(MRIG-1), V(MRIG-1), SCOSE(MRIG),SSENO(MRIG))
      CALL ROTAFW(U(MRIG),   V(MRIG),   SCOSE(MRIG),SSENO(MRIG))
*
      IF(MBCR.EQ.0)THEN
*
C	 Transmissive boundary conditions on the right
*
	 U(MRIG + 2) = U(MRIG - 1)
	 U(MRIG + 1) = U(MRIG)
*
       ELSE
*
C	 Reflective boundary conditions on the right
*  
	 U(MRIG + 2) = -U(MRIG - 1)
	 U(MRIG + 1) = -U(MRIG)
*
      ENDIF
*
C     Rotate back so that all data can be looped
*
      CALL ROTABK(U(MRIG + 2),V(MRIG + 2),SCOSE(MRIG),SSENO(MRIG))
      CALL ROTABK(U(MRIG + 1),V(MRIG + 1),SCOSE(MRIG),SSENO(MRIG))
*
C     Rotate back to recover original data within domain
*
      CALL ROTABK(U(MRIG - 1),V(MRIG - 1),SCOSE(MRIG),SSENO(MRIG))
      CALL ROTABK(U(MRIG),    V(MRIG),    SCOSE(MRIG),SSENO(MRIG))
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE ROTAFW(P, Q, COSE, SENO)
*
C     Purpose: to rotate a vector according to side orientation
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   COSE, P, PX, Q, QX, SENO 
*
      PX =  COSE*P + SENO*Q
      QX = -SENO*P + COSE*Q
      P  =  PX
      Q  =  QX
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE ROTABK(P, Q, COSE, SENO)
*
C     Purpose: to rotate a vector back to physical space
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   COSE, P, PX, Q, QX, SENO 
*
      PX = COSE*P - SENO*Q
      QX = SENO*P + COSE*Q
      P  = PX
      Q  = QX
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE CONFLX(D, U, CS, FX)
*
C     Purpose: to calculate first two components of conserved 
C              variables CS and fluxes FX
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  GRAVIT, D, U, CS, FX 
*
      DIMENSION CS(3), FX(3)
      COMMON /GRAVSS/ GRAVIT 
*
C     Calculate conserved variables
*
      CS(1) = D
      CS(2) = D*U
*
C     Calculate fluxes
*
      FX(1) = D*U
      FX(2) = FX(1)*U + 0.5*GRAVIT*D*D
*
      END
* 
*----------------------------------------------------------------------*
*  
      SUBROUTINE SUPERA(R, C, A)
*
C     Purpose: to evaluate a WAF limiter A based on the 
C              SUPERB flux limiter B
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
      REAL   A, B, C, R  
*
      B = MAX(0.0, MIN(2.0*R, 1.0), MIN(R, 2.0)) 
*
C     Transform to WAF limiter
*
      A = 1.0 - (1.0 - C)*B
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VANLEE(R, C, A)
*
C     Purpose: to evaluate a WAF limiter A based on the 
C               van Leer flux limiter B
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
      REAL   A, B, C, R  
*
      IF(R.LE.0.0)THEN
         B = 0.0
      ELSE
         B = 2.0*R/(1.0 + R)
      ENDIF
*
C     Transform to WAF limiter
*
      A = 1.0 - (1.0 - C)*B
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VANALB(R, C, A)
*
C     Purpose: to evaluate a WAF limiter A based on the 
C               van Albada flux limiter B
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
      REAL   A, B, C, R  
*
      B = MAX(0.0, R*(1.0 + R)/(1.0 + R*R)) 
*
C     Transform to WAF limiter
*
      A = 1.0 - (1.0 - C)*B
*
      END 
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MINAAA(R, C, A)
*
C     Purpose: to evaluate a WAF limiter A based on the 
C              MINMOD flux limiter B
*
      IMPLICIT NONE
*
C     Declaration of variables
*  
      REAL   A, B, C, R  
*
      B = MAX(0.0, MIN(R, 1.0)) 
*
C     Transform to WAF limiter
*
      A = 1.0 - (1.0 - C)*B
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MESHER 
*
C     Purpose: to generate mesh for a two-dimensional non-Cartesian 
C              domain with vertical left and right boundaries and 
C              prescribed 'well-behaved' bottom and top boundary 
C              profiles. All mesh parameters are computed and stored.
C              The case of a rectangular domain is allowed.         
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, ICELLS, IDIM, J, JCELLS, JDIM  
*
      REAL    ALPHAL, ALPHAR, AREA2, AREPOL, DX, DELX, DELY, XA, 
     &        XCM, XCOSE, XINTL, XLENG, XSENO, XV, YA, YCM,
     &        YCOSE, YINTL, YLENG, YSENO, YV, ARGUM         
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION AREA2(IDIM, JDIM),
     &          XINTL(0:IDIM, 0:JDIM), XLENG(0:IDIM, 0:JDIM),                 
     &          XCOSE(0:IDIM, 0:JDIM), XSENO(0:IDIM, 0:JDIM),
     &          YINTL(0:IDIM, 0:JDIM), YLENG(0:IDIM, 0:JDIM),
     &          YCOSE(0:IDIM, 0:JDIM), YSENO(0:IDIM, 0:JDIM), 
     &          XV(-1:IDIM+1, -1:JDIM+1),YV(-1:IDIM+1, -1:JDIM+1),   
     &          XA(10), YA(10) 
*
      COMMON /XYINTL/ XINTL, YINTL 
      COMMON /GEOM2D/ AREA2,XLENG,XCOSE,XSENO,YLENG,YCOSE,YSENO 
*
      COMMON /MESHXY/ DX, ICELLS, JCELLS    
      COMMON /VERTEX/ XV, YV
*
C     Vertices of quadrilateral cells are found
*
      CALL VERTIC 
*
C     Compute areas
*
      DO 10 I = 1, ICELLS
          DO 20 J = 1 ,JCELLS
*
C            Store coordinates of all 4 vertices
*
             XA(1) = XV(I-1, J-1)
             XA(2) = XV(I,   J-1)
             XA(3) = XV(I,   J  )
             XA(4) = XV(I-1, J  )
*
             YA(1) = YV(I-1, J-1)
             YA(2) = YV(I,   J-1)
             YA(3) = YV(I,   J  )
             YA(4) = YV(I-1, J  )    
*
C            Calculate area of general quadrilateral
*
             CALL AREAPO(4, XA, YA, AREPOL)  
*
C            Store area
*       
             AREA2(I, J) = AREPOL
 20      CONTINUE
 10   CONTINUE
*
C     Compute side information for I-sweep. In order to use the 
C     same intercell side information for two consecutive cells we
C     adopt the convention that the right side has the correct
C     orientation but the left side requires a minus sign in
C     the updating one-dimensional formula. The same convention  
C     applies to the J-sweep
*
      DO 30 J = 1, JCELLS
         DO 40 I = 0, ICELLS
            DELX = XV(I, J) - XV(I, J-1)
            DELY = YV(I, J) - YV(I, J-1)
*
C           Compute length of sides for  
*
            ARGUM = DELX*DELX + DELY*DELY 
*
            IF(abs(ARGUM).gt.100.0)THEN
               WRITE(6,*)'PROGRAM STOPPED',XV(I,J),YV(I,J)
               STOP
            ENDIF
*
            XLENG(I, J) = SQRT(ARGUM)  
*
C           Compute orientation of sides 
*       
            XCOSE(I, J) =  DELY/XLENG(I, J)
            XSENO(I, J) = -DELX/XLENG(I, J)
*
C           Compute coordinates (XCM,  YCM) for centre of mass
*
            XCM = 0.25*(XV(I-1,J-1)+XV(I,J-1)+XV(I,J)+XV(I-1,J))
            YCM = 0.25*(YV(I-1,J-1)+YV(I,J-1)+YV(I,J)+YV(I-1,J)) 
*
C           Compute integration length on the left
*
            CALL INTLEN(XV(I,J-1), YV(I,J-1), XV(I, J), YV(I, J),
     &                  XLENG(I, J), XCM, YCM, ALPHAL)
*
C           Compute integration length on the right
*
            XCM = 0.25*(XV(I,J-1)+XV(I+1,J-1)+XV(I+1,J)+XV(I,J))
            YCM = 0.25*(YV(I,J-1)+YV(I+1,J-1)+YV(I+1,J)+YV(I,J)) 
*
C           Compute integration length on the right
*
            CALL INTLEN(XV(I,J-1), YV(I,J-1), XV(I, J), YV(I, J),
     &                  XLENG(I, J), XCM, YCM, ALPHAR)
* 
C           Compute integration length for the intercell position
*
            XINTL(I, J) = 2.0*MIN(ALPHAL, ALPHAR)
*
 40      CONTINUE
 30   CONTINUE 
*
C     Compute side information for J-sweep
*
      DO 50 I = 1, ICELLS
         DO 60 J = 0, JCELLS
            DELX = XV(I-1, J) - XV(I, J)
            DELY = YV(I-1, J) - YV(I, J)
*
C           Compute length of sides  
*
*
            ARGUM = DELX*DELX + DELY*DELY 
*
            IF(abs(ARGUM).gt.100.0)THEN
               WRITE(6,*)'PROGRAM STOPPED'
	         WRITE(6,*)I,J,ARGUM,XV(I-1,J),YV(I-1,J),XV(I,J),YV(I,J)
               STOP
            ENDIF
*
            YLENG(I, J) = SQRT(ARGUM) 
*
C            YLENG(I, J) = SQRT(DELX*DELX + DELY*DELY)  
*
C           Compute orientation of sides  
*       
            YCOSE(I, J) =  DELY/YLENG(I, J)
            YSENO(I, J) = -DELX/YLENG(I, J)
*
C           Compute coordinates (XCM,  YCM) for centre of mass
*
            XCM = 0.25*(XV(I,J-1)+XV(I,J)+XV(I-1,J)+XV(I-1,J-1))
            YCM = 0.25*(YV(I,J-1)+YV(I,J)+YV(I-1,J)+YV(I-1,J-1)) 
*
C           Compute integration length on the left
*           
            CALL INTLEN(XV(I,J), YV(I,J), XV(I-1, J), YV(I-1, J),
     &                  YLENG(I, J), XCM, YCM, ALPHAL)
*
C           Compute integration length on the right
*
            XCM = 0.25*(XV(I,J)+XV(I,J+1)+XV(I-1,J+1)+XV(I-1,J))
            YCM = 0.25*(YV(I,J)+YV(I,J+1)+YV(I-1,J+1)+YV(I-1,J)) 
*
C           Compute integration length on the right
*
            CALL INTLEN(XV(I,J), YV(I,J), XV(I-1, J), YV(I-1, J),
     &                  YLENG(I, J), XCM, YCM, ALPHAR)
*  
C           Compute integration length for the intercell position
* 
            YINTL(I, J) = 2.0*MIN(ALPHAL, ALPHAR)
*
 60      CONTINUE
 50   CONTINUE
*
      END
* 
C----------------------------------------------------------------------C
*
      SUBROUTINE VERTIC 
*
C     Purpose: to generate mesh for 2D domain with vertical left 
C              and right boundaries and prescribed bottom and top
C              boundaries (XB, YB) and (XT, YT). Coordinates of 
C              quadrilateral cell vertices XV(,) and YV(,) are found  
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, ICELLS, IDIM, J, JCELLS, JDIM, K, KBMAX, KTMAX 
* 
      REAL     DELY, DX, SLOPE, X1, X2, XB, XP, XT, XV,              
     &         Y1, Y2, YB, YBOT, YT, YTOP, YV, TOLERA
*     
      PARAMETER (IDIM = 500, JDIM = 500)
*	
      DIMENSION XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1),  
     &          XB(IDIM), YB(IDIM), XT(IDIM), YT(IDIM)
*
      COMMON /MESHXY/ DX, ICELLS, JCELLS  
      COMMON /VERTEX/ XV, YV
      COMMON /YBOUND/ XB, YB, XT, YT, KBMAX, KTMAX
      DATA TOLERA /1.0E-07/
*
      DO 10 I = 0, ICELLS
*
         XP = REAL(I)*DX
*
C        Loop over BOTTOM boundary to locate grid position XP
*
         DO 20 K = 1, KBMAX - 1
            X1 = XB(K)
            Y1 = YB(K)
            X2 = XB(K+1)
            Y2 = YB(K+1)
*
            IF(ABS(XP-X1).LE.TOLERA.OR.ABS(XP-X2).LE.TOLERA.OR.
     &         (X1.LE.XP.AND.XP.LE.X2))GOTO 30
*
 20      CONTINUE
*
C        Interpolate to find bottom position at grid position XP
*
 30      SLOPE = (Y2 - Y1)/(X2 - X1)
         YBOT  = Y1 + SLOPE*(XP - X1)
*
C        Loop over TOP boundary to locate grid position XP
*
         DO 40 K = 1, KTMAX - 1
            X1 = XT(K)
            Y1 = YT(K)
            X2 = XT(K+1)
            Y2 = YT(K+1)
*
            IF(ABS(XP-X1).LE.TOLERA.OR.ABS(XP-X2).LE.TOLERA.OR.
     &         (X1.LE.XP.AND.XP.LE.X2))GOTO 50
*
 40      CONTINUE
*
C        Interpolate to find top position at grid position XP
*
 50      SLOPE = (Y2 - Y1)/(X2 - X1)
         YTOP  = Y1 + SLOPE*(XP - X1)
*
         DELY = (YTOP - YBOT)/REAL(JCELLS)
*
         DO 60 J = 0, JCELLS
            XV(I,J) = XP 
            YV(I,J) = YBOT + REAL(J)*DELY 
 60      CONTINUE
*
 10   CONTINUE
*
C     Apply 'boundary conditions' to vertices
*
      DO 70 J = 0, JCELLS
         XV(-1, J)       = 2.0*XV(0, J) - XV(1, J)
         YV(-1, J)       = YV(1, J)
         XV(ICELLS+1, J) = 2.0*XV(ICELLS, J) - XV(ICELLS-1, J)
         YV(ICELLS+1, J) = YV(ICELLS-1, J)
 70   CONTINUE
*
      DO 80 I = 0, ICELLS
         XV(I, -1)       = XV(I, 1)
         YV(I, -1)       = 2.0*YV(1, 0) - YV(I, 1)
         XV(I, JCELLS+1) = XV(I, JCELLS-1)
         YV(I, JCELLS+1) = 2.0*YV(I, JCELLS) - YV(I, JCELLS-1)
 80   CONTINUE
*                  
      
      END
C----------------------------------------------------------------------C 
*
      SUBROUTINE RECBOU(DOMLEX, DOMLEY)
*
C     Purpose: to set bottom and top boundaries of 2D domain              
C              for assumed vertical left and right boundaries.
C              Here the scheme is applied to the specific case
C              of a rectangular domain to study a circular 
C              dambreak problem
*
C     XB()  : x-coordinate of bottom boundary (x-dependent)
C     YB()  : y-position   of bottom boundary (x-dependent)
C     XT()  : x-coordinate of top    boundary (x-dependent)
C     YT()  : y-position   of top    boundary (x-dependent)
C     KBMAX : Number of points defining bottom boundary 
C     KTMAX : Number of points defining top boundary 
*  
      IMPLICIT NONE
*  
      INTEGER  IDIM, KBMAX, KTMAX              
* 
      REAL     DOMLEX, DOMLEY, XB, XT, YB, YT                     
*            
      PARAMETER (IDIM = 500)
*
      DIMENSION XB(IDIM), YB(IDIM), XT(IDIM), YT(IDIM)
*
      COMMON /YBOUND/ XB, YB, XT, YT, KBMAX, KTMAX
*
      KBMAX  = 2     
      KTMAX  = 2     
*
C     ----------------Define bottom boundary---------------
*
      XB(1) = 0.0     
      YB(1) = 0.0     
*
C     Define end of wedge profile  
*
      XB(2) = DOMLEX 
      YB(2) = 0.0   
*
C     ----------------Define top boundary------------------
*
      XT(1) = 0.0      
      YT(1) = DOMLEY    
*
      XT(2) = DOMLEX   
      YT(2) = DOMLEY     
*
      END
*
C----------------------------------------------------------------------C 
*
      SUBROUTINE WEDBOU(DOMLEX, WEDAPE, WEDANG)
*
C     Purpose: to set bottom and top boundaries of 2D domain              
C              for assumed vertical left and right boundaries.
C              Here the scheme is applied to the specific case
C              of a wedge at an angle to an oncoming single
C              shock wave
*
C     XB()  : x-coordinate of bottom boundary (x-dependent)
C     YB()  : y-position   of bottom boundary (x-dependent)
C     XT()  : x-coordinate of top    boundary (x-dependent)
C     YT()  : y-position   of top    boundary (x-dependent)
C     KBMAX : Number of points defining bottom boundary 
C     KTMAX : Number of points defining top boundary 
*  
      IMPLICIT NONE
*  
      INTEGER  IDIM, KBMAX, KTMAX              
* 
      REAL     DOMLEX, WEDANG, WEDAPE, WEDHEI, PIE, 
     &         XB, XT, YB, YT, YTOP                      
*            
      PARAMETER (IDIM = 500)
*
      DIMENSION XB(IDIM), YB(IDIM), XT(IDIM), YT(IDIM)
*
      COMMON /YBOUND/ XB, YB, XT, YT, KBMAX, KTMAX
*
C     Calculate PI
*
      PIE = 4.0*ATAN(1.0)
C
*     Express wegde angle in radians
*
      WEDANG = WEDANG*PIE/180.0
*
      WEDHEI = (DOMLEX - WEDAPE)*TAN(WEDANG)
      YTOP   = 2.0*WEDHEI
*
      KBMAX  = 3     
      KTMAX  = 2     
*
C     ----------------Define bottom boundary---------------
*
      XB(1) = 0.0     
      YB(1) = 0.0     
*
C     Define appex of wedge
*
      XB(2) = WEDAPE  
      YB(2) = 0.0     
*
C     Define end of wedge profile  
*
      XB(3) = DOMLEX  
      YB(3) = WEDHEI   
*
C     ----------------Define top boundary------------------
*
      XT(1) = 0.0      
      YT(1) = YTOP     
*
      XT(2) = DOMLEX   
      YT(2) = YTOP     
*
      END
*
C----------------------------------------------------------------------C
*
      SUBROUTINE  AREAPO(N, X, Y, AREPOL)  
*
C     Purpose: to compute area of convex general polygon of N sides 
*     
      IMPLICIT NONE                
*   
      DIMENSION X(10), Y(10) 
*      
C     Declaration of variables
*
      INTEGER I, N
*
      REAL    AREPOL, X, Y
*
      X(N+1) = X(1)
      Y(N+1) = Y(1)
*
      AREPOL = 0.0
*
      DO 10 I = 1, N
          AREPOL = AREPOL + X(I)*Y(I+1) - X(I+1)*Y(I)
 10   CONTINUE
*
      AREPOL = 0.5*AREPOL
*
      END
*
C-----------------------------------------------------------------------C
*
      SUBROUTINE INTLEN(XA, YA, XB, YB, SLEN, XP, YP, ALPHA)
*    
C     Purpose: to compute integration length, which is distance
C              between a side (XA,YA)-(XB,YB) and a point (XP, YP), 
C              the centre of mass of the adjacent cell
*
      IMPLICIT NONE                
*         
C     Declaration of variables
*
      REAL  XA, YA, XB, YB, SLEN, XP, YP, ALPHA
*
      ALPHA = ABS((XB - XA)*(YP - YA) - (YB - YA)*(XP - XA))/SLEN
*
      END
*
C-----------------------------------------------------------------------C
*  
 
