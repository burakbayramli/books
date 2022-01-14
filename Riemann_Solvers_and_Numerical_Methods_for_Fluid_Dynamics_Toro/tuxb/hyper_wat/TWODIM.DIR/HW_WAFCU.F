*
*----------------------------------------------------------------------* 
*                                                                      *
*     Unsplit Finite Volume WAF-type scheme for the time-dependent     *
C     two dimensional shallow water equations                          *
*                                                                      * 
C     Purpose: to solve the time-dependent two-dimensional shallow     *
C              water equations for an on a Cartesian domain using      *
C              the unsplit WAF finite volume method of Billett and     *
C              Toro, in conjunction with the HLLC approximate          *
C              Riemann solver. A selection of 6 limiter functions      *
C              is available. The program is set up to solve a          *
C              CIRCULAR DAMBREAK problem, which assumes a circular     *
C              dam that breaks instantaneoulsy. The initial            *
C              conditions are setup by calling the routine CIRDAM.     *
C              The computational domain is rectangular.                *
*                                                                      * 
*                                                                      *  
C     Name of program: hwwafcu.f                                       * 
*                                                                      * 
C     Input  file: cirdam.ini (initial data)                           *
C     Output file: gnuxx.out (slice in x-direction for GNUPLOT use)    *
C     Output file: gnuxy.out (2D results for GNUPLOT use)              *
C     Output file: tecxy.out (2D results for TECPLOT use)              *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 19th August 2000                                  * 
*                                                                      * 
C     Theory is found in Refs. 1, and 2 and in original references     *
C     therein                                                          * 
*                                                                      * 
C     1. Toro, E. F., "Riemann Solvers and Numerical                   * 
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997                           * 
C                      Second Edition, 1999                            *
*                                                                      *
C     2. Toro, E. F., "Front-Capturing Methods for                     *
C                      Free-Surface Shallow Flows                      *
C                      John Wiley and Sons (due 2000)                  *
*                                                                      *     
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
C     CALL CIRDAM (Defines the problem)                                *                       
C     CALL MESHER (Generates mesh)                                     *  
*                                                                      * 
C-----Time stepping begins                                             *  
*                                                                      * 
C         CALL CFLCON (CFL condition)                                  *  
C         CALL FLUXES (Fluxes in x and y directions)                   * 
C              CALL ONEDIM (Dimensional sweeps)                        *
C                   CALL BCONDI (Boundary conditions)                  *  
C                   CALL CONFLX (Local flux evaluation)                *  
C                   CALL ESTIME (Speed estimates)                      *  
C                   CALL SUPERA (or other limiter)                     *  
C         CALL UPDATE (Simultaneous update of 2D solution)             *  
C         CALL OUTPUT (Output results)                                 * 
*                                                                      * 
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
      REAL     CFLCOE, DT, TIME, TIMEOU, TIMETO            
*      
      INTEGER  N, NFREQU, NTMAXI              
*
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, TIMEOU
*
      DATA TIMETO /1.0E-07/
*
C     Define the problem to be solved
*
      CALL CIRDAM
* 
C     Mesh is generated
*
      CALL MESHER 
*
C     Time stepping starts
*
      TIME = 0.0
* 
      WRITE(6,*)'---------------------------------------------'
      WRITE(6,*)'   Time step N        TIME             '
      WRITE(6,*)'---------------------------------------------'
*
      DO 10 N = 1, NTMAXI 
*
C        CFL condition is applied to compute time step DT
*
         CALL CFLCON(CFLCOE, TIME, TIMEOU, DT)
*
         TIME = TIME + DT
*
C        Solver is called
*
         CALL FLUXES(DT) 
*
         CALL UPDATE(DT) 
*
         IF(MOD(N,NFREQU).EQ.0)THEN
            WRITE(6,20)N, TIME 
         ENDIF
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO.OR.N.GE.NTMAXI)THEN
*
C           Solution written out at time = TIMEOU
*
            CALL OUTPUT
*
            WRITE(6,*)'---------------------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'---------------------------------------------'
*
            GOTO 30
*
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
C     DIAMEF    : Domain-scaled factor for diameter (eg. 1/2)
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
      REAL    GRAVIT, D, U, V, C, UC, DINS, UINS, VINS, UOUT, VOUT,               
     &        CFLCOE, DOMLEX, DOMLEY, DOUT, DX, DY, RADIUS, 
     &        TIMEOU, XC, YC, R, XV, YV, RMINU, RPLUS, RABSO
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed
C     to be the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM, JDIM),U(IDIM, JDIM),V(IDIM, JDIM),C(IDIM,JDIM), 
     &          UC(3,IDIM, JDIM), ILIM(2,-1:IDIM+2), IBCS(2,-1:IDIM+2), 
     &          JLIM(2,-1:IDIM+2),JBCS(2,-1:IDIM+2),R(4),XV(4),YV(4)
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /CONSER/ UC
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
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
      WRITE(6,*)
      WRITE(6,*)'============================='
      WRITE(6,*)'Circular dambreak; input data' 
      WRITE(6,*)'============================='
      WRITE(6,*)
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
            DO 30 L = 1, 4
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
C     Compute conserved variables on data 
*
      DO 60 I = 1, ICELLS
         DO 70 J = 1, JCELLS
*
            UC(1,I,J) = D(I,J)
            UC(2,I,J) = D(I,J)*U(I,J)
            UC(3,I,J) = D(I,J)*V(I,J)
* 
 70      CONTINUE
 60   CONTINUE
*
      END  
*
*----------------------------------------------------------------------*
*
      SUBROUTINE FLUXES(DT)
*
C     Purpose: To evaluate the intercell fluxes according to the 
C              WAF Finite Volume scheme of Billett and Toro. These
C              are to be used in simultaneous updating conservative
C              formula of soubroutine UPDATE
*
      IMPLICIT NONE
*
C     Declaration of variables
*   
      INTEGER I, IBCS, ILIM, J, JBCS, JDIM, JLIM, IDIM, K, MLEF, MBCL, 
     &        MRIG, MBCR, ICELLS, JCELLS, ORDER             
*
      REAL    C, CN, CO, D, DN, DO, DT, DTS, DX, DY, FFLX, GFLX, SFLX, 
     &        U, UN, UO, V, VN, VO   
*          
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),C(IDIM,JDIM),    
     &          DN(IDIM,JDIM),UN(IDIM,JDIM),VN(IDIM,JDIM),
     &          CN(IDIM,JDIM),DO(-1:IDIM+2),UO(-1:IDIM+2),  
     &          VO(-1:IDIM+2),CO(-1:IDIM+2),ILIM(2,-1:IDIM+2),
     &          IBCS(2,-1:IDIM+2),JLIM(2,-1:IDIM+2),JBCS(2,-1:IDIM+2),      
     &          FFLX(3,0:IDIM,0:JDIM),GFLX(3,0:IDIM,0:JDIM),
     &          SFLX(3,0:IDIM)   
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /WALLSS/ MLEF, MRIG, MBCL, MBCR
      COMMON /FLUX2D/ FFLX, GFLX
*
C     Y-sweep is carried out for a time t = 0.5*DT  using the Godunov 
C     first-order upwind method, to obtain intermediate 2D solution.
C     This is stored in arrays DN, UN, VN, CN
*
      ORDER = 1
      DTS   = 0.5*DT
*
      DO 10 I = 1, ICELLS 
*
C        Left and right limits in y-sweep are set
*
         MLEF = JLIM(1, I)
         MRIG = JLIM(2, I)
*
C        Boundary conditions for y-sweep are set
*
         MBCL = JBCS(1, I)
         MBCR = JBCS(2, I)
*
         DO 20 J = MLEF, MRIG
*
C           Initial data are stored in one-dimensional arrays
*
            DO(J) = D(I, J)
            UO(J) = U(I, J)
            VO(J) = V(I, J)
            CO(J) = C(I, J) 
 20      CONTINUE
*
C        Solver in the y-direction is called
C        Note order of velocity components in argument list   
*
         CALL RIEMAN(ORDER,MLEF,MRIG,MBCL,MBCR,DTS,DY,DO,VO,UO,CO,SFLX)
*
C        Store intermadiate solution back in two-dimensional arrays
* 
         DO 30 J = MLEF, MRIG 
*
            DN(I, J) = DO(J)
            UN(I, J) = UO(J)
            VN(I, J) = VO(J)
            CN(I, J) = CO(J)
*
 30      CONTINUE
 10   CONTINUE
*
C     X-sweep is carried out for a full time step DT to
C     obtain a WAF-type flux, which is stored into FFLX(,,)
*
      ORDER = 2
      DTS   = DT
*
      DO 40 J = 1, JCELLS
*
C        Left and right limits in x-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions for x-sweep are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 50 I = MLEF, MRIG
*
C           Intermediate data are stored in one-dimensional arrays
*
            DO(I) = DN(I,J)
            UO(I) = UN(I,J)
            VO(I) = VN(I,J)
            CO(I) = CN(I,J)
*
 50      CONTINUE
*
C        Solver in the x-direction is called  
C        Note order of velocity components in argument list    
*
         CALL RIEMAN(ORDER,MLEF,MRIG,MBCL,MBCR,DTS,DX,DO,UO,VO,CO,SFLX)
*
C        Store flux SFLX into x-intercell fluxes FFLX
*
         DO 60 I = MLEF-1, MRIG
            DO 70 K = 1,3
               FFLX(K,I,J) = SFLX(K,I)
 70         CONTINUE
 60      CONTINUE
 40   CONTINUE
*
C     X-sweep is carried out for a time t = 0.5*DT  using the Godunov 
C     first-order upwind method, to obtain intermediate 2D solution. 
C     This is stored in arrays DN, UN, VN, CN
*
      ORDER = 1
      DTS   = 0.5*DT
*
      DO 80 J = 1, JCELLS
*
C        Left and right limits in x-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions for x-sweep are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 90 I = MLEF, MRIG
*
C           Initial data are stored in one-dimensional arrays
*
            DO(I) = D(I, J)
            UO(I) = U(I, J)
            VO(I) = V(I, J)
            CO(I) = C(I, J)
*
 90      CONTINUE
*
C        Solver in the x-direction is called
C        Note order of velocity components in argument list   
*
         CALL RIEMAN(ORDER,MLEF,MRIG,MBCL,MBCR,DTS,DX,DO,UO,VO,CO,SFLX)
*
C        Store intermediate solution back in two-dimensional arrays
*
         DO 100 I = MLEF, MRIG
*
            DN(I,J) = DO(I)
            UN(I,J) = UO(I)
            VN(I,J) = VO(I)
            CN(I,J) = CO(I)
*
 100     CONTINUE
*
 80   CONTINUE
*
C     Y-sweep is carried out for a full time step DT to
C     obtain a WAF-type flux, which is stored into GFLX(,,)
*
      ORDER = 2
      DTS   = DT
*
      DO 110 I = 1, ICELLS 
*
C        Left and right limits in y-sweep are set
*
         MLEF = JLIM(1, I)
         MRIG = JLIM(2, I)
*
C        Boundary conditions for y-sweep are set
*
         MBCL = JBCS(1, I)
         MBCR = JBCS(2, I)
*
         DO 120 J = MLEF, MRIG
*
C           Intermediate data are stored in one-dimensional arrays
*
            DO(J) = DN(I,J)
            UO(J) = UN(I,J)
            VO(J) = VN(I,J)
            CO(J) = CN(I,J) 
 120     CONTINUE
*
C        Solver in the y-direction is called
C        Note order of velocity components in argument list   
*
         CALL RIEMAN(ORDER,MLEF,MRIG,MBCL,MBCR,DTS,DY,DO,VO,UO,CO,SFLX) 
*
C        Store flux SFLX into y-intercell fluxes GFLX. Note
C        order exchange
*
         DO 130 J = MLEF-1, MRIG           
            GFLX(1,I,J) = SFLX(1,J)
            GFLX(2,I,J) = SFLX(3,J)
            GFLX(3,I,J) = SFLX(2,J)
 130     CONTINUE
*
 110  CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE UPDATE(DT)
*
C     Purpose: to apply unsplit finite volume conservative formula
C              to update the solution UC to the next time level.
C              Physical variables are evaluated from the conserved
C              variables UC
*
      IMPLICIT NONE
*
C     Declaration of variables
*          
      INTEGER I, ICELLS, IDIM, J, JCELLS, JDIM, K  
* 
      REAL    GRAVIT, D, U, V, C, UC, DELFX, DELFY, DT, DTODX, DTODY,
     &        DX, DY, FFLX, GFLX   
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),C(IDIM,JDIM), 
     &          UC(3,IDIM,JDIM),FFLX(3,0:IDIM,0:JDIM), 
     &          GFLX(3,0:IDIM,0:JDIM)   
*
      COMMON /GRAVSS/ GRAVIT
      COMMON /SOLUTI/ D, U, V, C
      COMMON /CONSER/ UC
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
      COMMON /FLUX2D/ FFLX, GFLX
*   	
      DTODX = DT/DX
      DTODY = DT/DY
*
      DO 10 I = 1, ICELLS
*
         DO 20 J = 1, JCELLS
*
C           Update conserved variables UC(K,I,J)
*
            DO 30 K = 1, 3
*
               DELFX = DTODX*(FFLX(K,I,J) - FFLX(K,I-1,J))
               DELFY = DTODY*(GFLX(K,I,J) - GFLX(K,I,J-1))
*
               UC(K,I,J) = UC(K,I,J) - DELFX - DELFY
*
 30         CONTINUE
*
C           Compute physical variables
*
            D(I,J) = UC(1,I,J)
            U(I,J) = UC(2,I,J)/D(I,J)
            V(I,J) = UC(3,I,J)/D(I,J)
            C(I,J) = SQRT(GRAVIT*D(I,J))
 20      CONTINUE
 10   CONTINUE
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, TIME, TIMEOU, DT)
* 
C     Purpose: to apply CFL condition to compute time step DT.
C              Linearised stability limit is 1.
*
      IMPLICIT NONE
*
C     Declaration of variables
*       
      INTEGER  I, IDIM, J, JDIM, ICELLS, JCELLS  
*
      REAL     C, CFLCOE, D, DT, DTL, DX, DY, SPX, SPY, TIME, TIMEOU, 
     &         U, V 

      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM, JDIM),U(IDIM, JDIM),V(IDIM, JDIM),C(IDIM, JDIM) 
*
      COMMON /SOLUTI/ D, U, V, C
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
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
C           Find local time DTL for cell (i, j) 
*
            DTL = MIN(DX/SPX, DY/SPY)
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
C              gnuxy.out (full 2D solution,     use GNUPLOT)
C              tecxy.out (full 2D solution,     use TECPLOT)
*                          
      IMPLICIT NONE
*
C     Declaration of variables
* 
      INTEGER I, IDIM, ICELLS, J, JCELLS, JDIM, JH 
* 
      REAL    C, D, DX, DY, U, V, XPOS, XV, YV, XCM, YCM, VEL              
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),C(IDIM,JDIM),
     &          XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1)  
*
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
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
      WRITE(1,*)'TITLE="Results" '
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
      SUBROUTINE MESHER
*
C     Purpose: to generate mesh for 2D rectangular domain (trivial).
C              Coordinates XV(,), YV(,) of cell vertices are 
C              calculated and stored to be used in OUTPUT routine 
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, ICELLS, IDIM, J, JCELLS, JDIM 
* 
      REAL     DX, DY, XP, YP, XV, YV 
*     
      PARAMETER (IDIM = 500, JDIM = 500)
*	
      DIMENSION XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1) 
*
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS  
      COMMON /VERTEX/ XV, YV
*
      DO 10 I = 0, ICELLS
         XP = REAL(I)*DX 
         DO 20 J = 0, JCELLS
            YP = REAL(J)*DY
            XV(I,J) = XP 
            YV(I,J) = YP
 20      CONTINUE
 10   CONTINUE
* 
      END
*
C-----------------------------------------------------------------------C
*
      SUBROUTINE BCONDI(MLEF, MRIG, MBCL, MBCR, D, U, V, C) 
*
C     Purpose: to set boundary conditions
*   
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER MLEF, MBCL, MRIG, MBCR, IDIM
*
      REAL    D, U, V, C 
*
      PARAMETER (IDIM = 500)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2), C(-1:IDIM+2)
*
C     Set boundary conditions on the left/bottom
*
      D(MLEF - 2) = D(MLEF + 1)
      V(MLEF - 2) = V(MLEF + 1)
      C(MLEF - 2) = C(MLEF + 1)
*
      D(MLEF - 1) = D(MLEF)
      V(MLEF - 1) = V(MLEF)
      C(MLEF - 1) = C(MLEF)
*
      IF(MBCL.EQ.0)THEN
*
C        Transmissive boundary conditions  
*
         U(MLEF - 2) = U(MLEF + 1)
         U(MLEF - 1) = U(MLEF)
*
      ELSE
*
C        Reflective boundary conditions at left end
*
         U(MLEF - 2) = -U(MLEF + 1)
         U(MLEF - 1) = -U(MLEF)
*
      ENDIF  
*
C     Set boundary conditions on the right/top
*
      D(MRIG + 2) = D(MRIG - 1)
      V(MRIG + 2) = V(MRIG - 1)
      C(MRIG + 2) = C(MRIG - 1)
*
      D(MRIG + 1) = D(MRIG)
      V(MRIG + 1) = V(MRIG)
      C(MRIG + 1) = C(MRIG)
*
      IF(MBCR.EQ.0)THEN
*
C        Transmissive boundary conditions  
*
         U(MRIG + 2) = U(MRIG - 1)
         U(MRIG + 1) = U(MRIG)
*
      ELSE
*
C        Reflective boundary conditions 
*
         U(MRIG + 2) = -U(MRIG - 1)
         U(MRIG + 1) = -U(MRIG)
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RIEMAN(ORDER,MLEF,MRIG,MBCL,MBCR,DTS,DSP,D,U,V,C,FLX)
*
C     Purpose: to compute the TVD WAF flux with the HLLC Riemann 
C              solver and evolve the solution by a time DTS. See
C              Chaps. 10, 14 and 16 of Ref. 1 and Chaps. 8 to 12 
C              of Ref. 2     
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER LIMITE, I, IDIM, IUPW, K, MLEF, MRIG, MBCL, MBCR,ORDER
*
      REAL    D, U, V, C, CS, AS, US, DL, UL, VL, CL, DR, UR, VR, CR, 
     &        QL, QR, DS, GRAVIT, CN, DTS, DSP, DTODS, DLOC, DUPW, 
     &        FDAL, FDAR, FLX, FS, RATIO, SL, SR, TOLLIM, WAFLIM,  
     &        WJUMP, WSPEE, WL, WM, WR, CDL, CDR, FDL, FDR               
* 
      PARAMETER (IDIM = 500)
*  
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2), C(-1:IDIM+2),       
     &          FDAL(3,-1:IDIM+2), FDAR(3,-1:IDIM+2), FLX(3,0:IDIM),
     &          FS(3,-1:IDIM+2), WSPEE(3,-1:IDIM+2), WJUMP(3,-1:IDIM+2), 
     &          CN(3), CDL(3), CDR(3), FDL(3), FDR(3), CS(3), WAFLIM(3)                        
*
      COMMON /GRAVSS/ GRAVIT 
      COMMON /TVDCON/ LIMITE
*
      DATA TOLLIM /1.0E-06/
*
C     Apply boundary conditions
*
      CALL BCONDI(MLEF, MRIG, MBCL, MBCR, D, U, V, C)
*   
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 10 I = MLEF - 2 , MRIG + 1
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
C        Compute first two components of conserved variables CDL 
C        and fluxes FDL on left data state
*
         CALL CONFLX(DL, UL, CDL, FDL)
*
C        Compute first two components of conserved variables CDR 
C        and fluxes FDR on right data state
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
C        First compute Two-Rarefaction solution for depth
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
C        Store wave speeds, needed for TVD WAF flux
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
            FDAL(K, I) = FDL(K)
            FDAR(K, I) = FDR(K)
 30      CONTINUE
*
 10   CONTINUE
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux starts
C     -----------------------------------------------------------------
*
      DTODS = DTS/DSP
*
      DO 40 I = MLEF - 1, MRIG  
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
C              Wave k is positive and upwind direction is on the Left
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
            IF(ORDER.EQ.1)WAFLIM(K) = 1.0
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
C        and store it in I
*
         DO 60  K = 1, 2
*
            FLX(K,I) = WL*FDAL(K,I) + WM*FS(K,I) + WR*FDAR(K,I)
* 
 60      CONTINUE
*
C        Compute weights for third component of WAF 
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
C        Compute third flux component FLX(3,I)
*
         FLX(3,I) = WL*FLX(1,I)*VL + WR*FLX(1,I)*VR 
*
 40   CONTINUE
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux ends
C     -----------------------------------------------------------------
*
      IF(ORDER.EQ.1)THEN
*   	
         DO 70 I = MLEF, MRIG
*
C           Compute conserved variables on data within domain
*
            CS(1) = D(I)
            CS(2) = D(I)*U(I)
            CS(3) = D(I)*V(I)
* 
C           Update conserved variables
* 
            DO 80 K = 1, 3
*
               CS(K) = CS(K) - DTODS*(FLX(K,I) - FLX(K,I-1))
*
 80         CONTINUE
*
C           Compute physical variables
*
            D(I) = CS(1)
            U(I) = CS(2)/D(I)
            V(I) = CS(3)/D(I)
*
            C(I) = SQRT(GRAVIT*D(I))
*
 70      CONTINUE
*
      ENDIF
*
      END
* 
*----------------------------------------------------------------------*
*
      SUBROUTINE CONFLX(D, U, CS, FX)
*
C     Purpose: to calculate first two components conserved 
C     variables CS and fluxes FX on data
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  GRAVIT, D, U, CS, FX 
*
      DIMENSION CS(3), FX(3)
*
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
