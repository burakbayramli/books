*
*----------------------------------------------------------------------*
*                                                                      *
C     Weighted Average Flux (WAF) Method (RCM) for the time-dependent  *
C     one dimensional in-homogenoeus Shallow Water Equations           *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional             *
C              Shallow Water Equations with geometric source           *
C              terms due to cylindrical symmetry by the Weighted       *
C              Average Flux (WAF) Method using the HLL approximate     *
C              Riemann solver, with choice of six limiter              *
C              functions. The source terms are treated by the          *
C              standard splitting scheme.                              *
*                                                                      *
C     Program name: hw_wafi.f                                          *
*                                                                      *
C     Input  file: wafi.ini (initial data)                             *
C     Output file: wafi.out (numerical results)                        *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 17 August 2000                                    *
*                                                                      *
C     REFERENCES                                                       *
*                                                                      *
C     1. Toro, E. F., "Shock-Capturing Methods for                     *
C                      Free-Surface Shallow Flows"                     *
C                      John Wiley and Sons 2000                        *   
*                                                                      *
C     2. Toro, E. F., "Riemann Solvers and Numerical                   *
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997                           *
C                      Second Edition, 1999                            * 
C     This program is part of HYPER-WAT,                               * 
C     a sub-library of                                                 * 
*                                                                      *
C     3. Toro, E. F., "NUMERICA: A Library of Source Codes for         *
C                      Teaching, Research and Applications"            *     
C                      NUMERITEK LTD, 1999,                            *
C                      Website: www.numeritek.com                      *
*                                                                      *
*----------------------------------------------------------------------*
*
C     Driver program
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "wafi.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA 
*
C     Time marching procedure
*
      WRITE(6,*)'---------------------------------------------'
      WRITE(6,*)'   Time step N        TIME           TIMEOU'
      WRITE(6,*)'---------------------------------------------'
*
      DO 10 N = 1, NTMAXI
*
C        Boundary conditions are set
*
         CALL BCONDI(CELLS)
*
C        Courant-Friedrichs-Lewy (CFL) condition imposed
*
         CALL CFLCON(CFLCOE, CELLS, N, TIME, TIMEOU)
*
C        Numerical fluxes are calculated using the WAF method
*
         CALL WAFFLU(CELLS)
*
C        Solution is updated using fluxes in conservative formula
*
         CALL UPDATE(CELLS)
*
C        Account for geometric source terms due to cylindrical symmetry
*
         CALL GEOSOU(CELLS)
*
         IF(MOD(N,NFREQU).EQ.0)WRITE(6,20)N, TIME, TIMEOU
*
C        Check output time
*
         TIMDIF = ABS(TIME - TIMEOU)
*
         IF(TIMDIF.LE.TIMTOL)THEN
*
C           Solution is written to "wafi.out' at specified time
*
            CALL OUTPUT(CELLS)
*
            WRITE(6,*)'---------------------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'---------------------------------------------'
*
            GOTO 30
         ENDIF
*
 10   CONTINUE
*
 20   FORMAT(I12,6X,2(F12.7, 4X))
 30   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE READER
*
C     Purpose: to read initial parameters of the problem
*
C     Input variables
*
C     DOMLEN : Channel length
C     GATE1  : Position of gate 1
C     CELLS  : Number of computing cells
C     GRAVIT : Acceleration due to gravity
C     TIMEOU : Output time
C     DLINIT : Initial depth on left section of channel
C     ULINIT : Initial velocity on left section of channel
C     DMINIT : Initial depth on middle section of channel
C     UMINIT : Initial velocity on middle section of channel
C     DRINIT : Initial depth on right section of channel
C     URINIT : Initial velocity on right section of channel
C     GATE2  : Position of gate 2
C     CFLCOE : Courant number coefficient
C     IBCLEF : Type of left boundary conditions
C     IBCRIG : Type of right boundary conditions
C     NFREQU : Output frequency to screen
C     NTMAXI : Maximum number of time steps
C     LIMITE : Limiter function
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, CELLS, LIMITE, NFREQU, NTMAXI
*
      REAL    CFLCOE, DOMLEN, GATE1, GATE2, GRAVIT, TIMEOU, 
     &        DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT 
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, GATE1, GATE2
      COMMON /ACCELE/ GRAVIT
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
      COMMON /INISTA/ DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT
      COMMON /TVDCON/ LIMITE
*
      OPEN(UNIT = 1, FILE = 'wafi.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)GATE1
      READ(1,*)CELLS
      READ(1,*)GRAVIT
      READ(1,*)TIMEOU
      READ(1,*)DLINIT
      READ(1,*)ULINIT
      READ(1,*)DMINIT
      READ(1,*)UMINIT
      READ(1,*)DRINIT
      READ(1,*)URINIT
      READ(1,*)GATE2
      READ(1,*)CFLCOE
      READ(1,*)IBCLEF
      READ(1,*)IBCRIG
      READ(1,*)NFREQU
      READ(1,*)NTMAXI
      READ(1,*)LIMITE
*
      CLOSE(1)
*
C     Input data is echoed to screen
*
      WRITE(6,*)
      WRITE(6,*)'Input data echoed to screen'
      WRITE(6,*)
      WRITE(6,*)'DOMLEN = ',DOMLEN
      WRITE(6,*)'GATE1  = ',GATE1
      WRITE(6,*)'CELLS  = ',CELLS
      WRITE(6,*)'GRAVIT = ',GRAVIT
      WRITE(6,*)'TIMEOU = ',TIMEOU
      WRITE(6,*)'DLINIT = ',DLINIT
      WRITE(6,*)'ULINIT = ',ULINIT
      WRITE(6,*)'DMINIT = ',DMINIT
      WRITE(6,*)'UMINIT = ',UMINIT
      WRITE(6,*)'DRINIT = ',DRINIT
      WRITE(6,*)'URINIT = ',URINIT
      WRITE(6,*)'GATE2  = ',GATE2
      WRITE(6,*)'CFLCOE = ',CFLCOE
      WRITE(6,*)'IBCLEF = ',IBCLEF
      WRITE(6,*)'IBCRIG = ',IBCRIG
      WRITE(6,*)'NFREQU = ',NFREQU
      WRITE(6,*)'NTMAXI = ',NTMAXI
      WRITE(6,*)'LIMITE = ',LIMITE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INITIA 
*
C     Purpose: to set initial conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM, NFREQU, NTMAXI            
*
      REAL    D, DOMLEN, DT, DX, GATE1, GATE2, XPOS, CFLCOE, TIMEOU,
     &        U, DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT 
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /DOMAIN/ DOMLEN, GATE1, GATE2
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
      COMMON /INISTA/ DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT
      COMMON /PRIMIT/ D, U
      COMMON /MESHPA/ DT, DX
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Set initial data in channel of length DOMLEN, which is divided
C     into 3 sections by gates at positions GATE1 and GATE2
*
      DO 10 I = 1, CELLS
*
         XPOS = (REAL(I) - 0.5)*DX
*
         IF(XPOS.LE.GATE1)THEN
*
C           Set initial values in left section of domaim
*
            D(I) = DLINIT
            U(I) = ULINIT
         ENDIF
*
         IF(XPOS.GT.GATE1.AND.XPOS.LE.GATE2)THEN
*
C           Set initial values in middle section of domaim
*
            D(I) = DMINIT
            U(I) = UMINIT
         ENDIF

         IF(XPOS.GT.GATE2)THEN
*
C           Set initial values in right section of domaim
*
            D(I) = DRINIT
            U(I) = URINIT
         ENDIF
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(CELLS)
*
C     Purpose: to set boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, CELLS, IDIM
*
      REAL    D, U
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /BOUNDA/ IBCLEF, IBCRIG
*
      IF(IBCLEF.EQ.0)THEN
*
C        Transmissive boundary conditions on the left
*
         D(-1) =  D(2)
         U(-1) = -U(2)
*
         D(0)  =  D(1)
         U(0)  =  U(1)
*
      ELSE
*
C        Reflective boundary conditions on the left
*
         D(-1) =  D(2)
         U(-1) = -U(2)
*
         D(0)  =  D(1)
         U(0)  = -U(1)
*
      ENDIF
*
      IF(IBCRIG.EQ.0)THEN
*
C        Transmissive boundary conditions on the right
*
         D(CELLS + 1) = D(CELLS)
         U(CELLS + 1) = U(CELLS)
*
         D(CELLS + 2) = D(CELLS - 1)
         U(CELLS + 2) = U(CELLS - 1)
C
      ELSE
*
C        Reflective boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) = -U(CELLS)
*
         D(CELLS + 2) =  D(CELLS - 1)
         U(CELLS + 2) = -U(CELLS - 1)
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE,CELLS,N,TIME,TIMEOU)
*
C     Purpose: to apply the CFL condition to find a stable time
C              step size DT
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM, N
*
      REAL    C, CFLCOE, D, DT, DX, GRAVIT, SMAX, SPELOC, TIME,
     &        TIMEOU, U
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), C(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /ACCELE/ GRAVIT
      COMMON /CELERI/ C
      COMMON /MESHPA/ DT, DX
*
      SMAX = 0.0
*
C     Find estimate for maximum velocity SMAX present in data
*
      DO 10 I = -1, CELLS + 2
*
C        Compute celerity
*
         C(I)   = SQRT(GRAVIT*D(I))
*
         SPELOC = ABS(U(I)) + C(I)
         IF(SPELOC.GT.SMAX)SMAX = SPELOC
 10   CONTINUE
*
C     Compute time step DT 
*
      DT = CFLCOE*DX/SMAX
*
C     For early times DT is reduced to compensate for approximate
C     calculation of SMAX
*
      IF(N.LE.5)DT = 0.2*DT
*
C     Check size of DT to avoid exceeding output time
*
      IF((TIME + DT).GT.TIMEOU)THEN
*
C        Recompute DT
*
         DT = TIMEOU - TIME
      ENDIF
*
C     Find current time
*
      TIME = TIME + DT
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE OUTPUT(CELLS)
*
C     Purpose: to output the solution at a specified time TIMEOU
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    D, DT, DX, U, XPOS 
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /MESHPA/ DT, DX
*
      OPEN(UNIT = 1, FILE = 'wafi.out', STATUS = 'UNKNOWN')
*
      XPOS = -0.5*DX
      DO 10 I  = CELLS, 1, -1
         XPOS  =  XPOS + DX
         WRITE(1,30)XPOS, D(I), -U(I)
 10   CONTINUE
*
      DO 20 I   = 1, CELLS
         XPOS  =  XPOS + DX
         WRITE(1,30)XPOS, D(I), U(I)
 20   CONTINUE
*
      CLOSE(1)
*
 30   FORMAT(3(F14.6,2X)) 
*
      CLOSE(1)
*
      END
*
*----------------------------------------------------------------------*
*
       SUBROUTINE UPDATE(CELLS)
*
C      Purpose: to update the solution using conservative formula
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  CELLS, I, IDIM, K             
*
      REAL     CS, D, DT, DTODX, DX, FLUX, TOLDEP, U           
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), CS(2,-1:IDIM+2), 
     &          FLUX(2,-1:IDIM+2) 
*
      COMMON /PRIMIT/ D, U
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FLUX
      COMMON /MESHPA/ DT, DX
      DATA TOLDEP /1.0E-06/
*
C     Update conserved variables to new time level
*
      DTODX = DT/DX
*
       DO 10 I = 1, CELLS
*
          DO 20 K    = 1, 2
             CS(K,I) = CS(K,I) - DTODX*(FLUX(K,I) - FLUX(K,I-1))
 20       CONTINUE
*
C         Compute physical variables
* 
          D(I) = CS(1,I)
*
          IF(D(I).GT.TOLDEP)THEN
             U(I) = CS(2,I)/D(I)
          ELSE
             U(I) = 0.0
          ENDIF
*
 10    CONTINUE
* 
       END
*
*
*----------------------------------------------------------------------*
*
      SUBROUTINE GEOSOU(CELLS)
*
C     Purpose: to account for influence of geometric source terms
C              due to cylindrical symmetry. Use simple first-order
C              splitting scheme
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    CS, D, DT, DX, RAD, U
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), CS(2,-1:IDIM+2)  
*
      COMMON /PRIMIT/ D, U
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
*
      DO 10 I = 1, CELLS
*
         RAD = (REAL(I) - 0.5)*DX
*
C        Correct conserved variables 
*
         CS(1,I) = CS(1,I) - (DT/RAD)*D(I)*U(I)
         CS(2,I) = CS(2,I) - (DT/RAD)*D(I)*U(I)*U(I)
*
C        Recompute physical variables
*
         D(I) = CS(1,I)
         U(I) = CS(2,I)/CS(1,I)
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 
       SUBROUTINE WAFFLU(CELLS)
*
C      Purpose: to compute the WAF intercell flux using the 
C               HLL Riemann solver
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER   CELLS, I, IDIM, IUPW, K, LIMITE                
*
      REAL      C, CL, CN, CR, CS, D, DL, DLOC, DR,         
     &          DS, DT, DTODX, DUPW, DX, FD, FLUX, FHLL, FS, GRAVIT,                
     &          RATIO, SL, SR, TOLDEP, TOLLIM, U, UL, UR, WAFLIM,       
     &          WJ, WL, WM, WR, WS         
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION C(-1:IDIM+2), D(-1:IDIM+2), U(-1:IDIM+2),
     &          CN(2), WAFLIM(2), CS(2,-1:IDIM+2), FD(2,-1:IDIM+2),
     &          FLUX(2,-1:IDIM+2), FS(2,-1:IDIM+2), WJ(2,-1:IDIM+2),
     &          WS(2,-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /ACCELE/ GRAVIT
      COMMON /CELERI/ C
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FLUX
      COMMON /MESHPA/ DT, DX
      COMMON /TVDCON/ LIMITE
*
      DATA TOLLIM, TOLDEP /1.0E-6, 1.0E-06/
*
      DO 10 I = -1, CELLS + 2
*
C        Compute fluxes on data
*
         FD(1,I) = D(I)*U(I)
         FD(2,I) = D(I)*U(I)*U(I) + 0.5*GRAVIT*D(I)*D(I)
*
C        Compute conserved variables on data
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
*
 10   CONTINUE
* 
      DTODX = DT/DX
*
      DO 20 I = -1, CELLS + 1
*
C        Define Left and Right data for Riemann problem
*
         DL = D(I)
         UL = U(I)
         CL = C(I)
*
         DR = D(I+1)
         UR = U(I+1)
         CR = C(I+1)
*
C        Compute wave speed estimates SL, SR. Dry bed case     
* 
         IF(DR.LT.TOLDEP.AND.DL.GT.TOLDEP)THEN
*	
C           Dry bed on the right
*
            DS = DR
            SL = UL - CL
            SR = UL + 2.0*CL
            GOTO 9999
         ENDIF
*
         IF(DL.LT.TOLDEP.AND.DR.GT.TOLDEP)THEN
*	
C           Dry bed on the left
*
            DS = DL
            SL = UR - 2.0*CR
            SR = UR + CR
            GOTO 9999
         ENDIF
*	
         IF(DL.LT.TOLDEP.AND.DR.LT.TOLDEP)THEN
*	
C           Dry bed on the left and right
*
            DS = 0.0
            SL = -TOLDEP
            SR = +TOLDEP
            GOTO 9999
         ENDIF
*
C        Compute DS is star region according to Two-
C        Rarefaction approximation
*
         DS = (0.5*(CL + CR) + 0.25*(UL - UR))**2/GRAVIT
*
C        Compute wave speed estimates SL, SR. Wet bed case     
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
 9999    CONTINUE
*
C        Compute fluxes in STAR region according to HLL
*
         DO 30 K  = 1, 2
            FHLL = SR*FD(K,I)-SL*FD(K,I+1)+SL*SR*(CS(K,I+1)-CS(K,I))
            FS(K,I) = FHLL/(SR - SL)
 30      CONTINUE
*
C        Compute wave jumps in depth for TVD condition
*
         WJ(1,I) = DS - DL
         WJ(2,I) = DR - DS
*
C        Store wave speeds
*
         WS(1,I) = SL
         WS(2,I) = SR
*
 20   CONTINUE
*
C      Compute intercell fluxes according to the WAF method
* 
       DO 40 I = 0, CELLS
*
C        Apply TVD condition
*
         DO 50 K = 1,2
*
C           Compute Courant numbers for each wave
*
            CN(K) = WS(K,I)*DTODX
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
C           Compute wave jumps
*
            DLOC = WJ(K,I)
            DUPW = WJ(K,I+IUPW)
*
C           Modify small jumps preserving sign
*
            IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0,DUPW)
            IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0,DLOC)
*
C           Compute RATIO of upwind to local changes
*
            RATIO = DUPW/DLOC
*
C           Select limiter function WAFLIM
*
C           LIMITE = 1, Godunov's Method
C           LIMITE = 2, Upwind Second Order Method (non-monotone)
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
C        Compute weights for TVD WAF flux evaluation
*
         WL = 0.5*(1.0       + WAFLIM(1))
         WM = 0.5*(WAFLIM(2) - WAFLIM(1))
         WR = 0.5*(1.0       - WAFLIM(2))
*
         DO 60 K = 1, 2
            FLUX(K,I) = WL*FD(K,I) + WM*FS(K,I) + WR*FD(K,I+1)
 60      CONTINUE
*
 40   CONTINUE
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