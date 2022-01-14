*
*----------------------------------------------------------------------*
*                                                                      *
C     WAF Scheme for the Linearised Equations                          *
C     of Gas Dynamics                                                  *
*                                                                      *
C     Purpose: to solve the time-dependent, linearised equations       *
C              of Gas Dynamics in one space dimension using the        *
C              Weighted Average Flux (WAF) method in conjunction       *
C              with the exact Riemann solver and with six limiter      *
C              functions. The Godunov and Lax-Wendroff schemes         *
C              are obtained as special cases                           *
*                                                                      *
C     Input  file: gawaf.ini                                           *
C     Output file: gawaf.out                                           *
*                                                                      *
C     Name of program: HL-GAWAF                                        *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Ref. 1 and in original references therein     *
*                                                                      *
C     1. Toro, E. F., "Riemann Solvers and Numerical                   *
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997                           *
C                      Second Edition, 1999                            *
*                                                                      *
C     This program is part of                                          *
*                                                                      *
C     NUMERICA                                                         *
C     A Library of Source Codes for Teaching,                          *
C     Research and Applications,                                       *
C     by E. F. Toro                                                    *
C     Published by NUMERITEK LTD, 1999                                 *
C     Website: www.numeritek.com                                       *
*                                                                      *
*----------------------------------------------------------------------*
*
C     Driver program
*
      IMPLICIT NONE
*
C     Declaration of variables:
*
      INTEGER BCONDL, BCONDR, CELLS, LIMITE, N, NFREQ, NTMAXI
*
      REAL    CBAR, DBAR, CFLCOE, DIAPH, DOMLEN, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ BCONDL, BCONDR, CELLS, CFLCOE, DIAPH,
     &                DOMLEN, LIMITE, NFREQ, NTMAXI, TIMEOU
      COMMON /MATRIX/ CBAR, DBAR
*
      DATA TIME, TIMETO/0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "gawaf.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(DOMLEN, DIAPH, CELLS)
*
C     Eigenstructure of the system is computed
*
      CALL EIGENS(CBAR)
*
C     Time marching procedure
*
      WRITE(6,*)'Time step N         TIME'
      WRITE(6,*)'--------------------------------'
*
      DO 10 N = 1, NTMAXI
*
C        Boundary conditions are set
*
         CALL BCONDI(BCONDL, BCONDR, CELLS)
*
C        Courant-Friedrichs-Lewy (CFL) condition imposed.
C        For constant speeds, as here, this routine could
C        be outside the time-marching loop
*
         CALL CFLCON(CFLCOE, TIME, TIMEOU)
*
C        Intercell numerical fluxes are computed
*
         CALL WAFFLX(LIMITE, CELLS)
*
C        Solution is updated according to conservative formula
*
         CALL UPDATE(CELLS)
*
         IF(MOD(N,NFREQ).EQ.0)WRITE(6,20)N, TIME
*
C        Check output time
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Numerical solution written to "gawaf.out' at time TIMEOU
*
            CALL OUTPUT(CELLS)
*
            WRITE(6,*)'---------------------------------------'
            WRITE(6,*)'Number of time steps = ',N
            WRITE(6,*)'---------------------------------------'
*
            GOTO 30
         ENDIF
*
 10   CONTINUE
*
 20   FORMAT(I12,6X, F12.7)
 30   CONTINUE
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE READER
*
C     Purpose: to read initial parameters of the problem
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  BCONDL, BCONDR, CELLS, LIMITE, NFREQ, NTMAXI
*
      REAL     CBAR, DBAR, CFLCOE, DIAPH, DOMLEN, TIMEOU,
     &         DLINIT, ULINIT, DRINIT, URINIT
*
      COMMON /DATAIN/ BCONDL, BCONDR, CELLS, CFLCOE, DIAPH,
     &                DOMLEN, LIMITE, NFREQ, NTMAXI, TIMEOU
      COMMON /INICON/ DLINIT, ULINIT, DRINIT, URINIT
      COMMON /MATRIX/ CBAR, DBAR
*
C     Description of variables
*
C     DOMLEN   : Domain length
C     CFLCOE   : Courant number coefficient
C     LIMITE   : Choice of flux limiter
C     CELLS    : Number of cells in domain
C     BCONDL   : Boundary conditions parameter, left
C     BCONDR   : Boundary conditions parameter, right
C     NFREQ    : Output frequency to screen
C     NTMAXI   : Maximum number of time steps
C     TIMEOU   : Output time
C     DIAPH    : Position of initial discontinuity
C     CBAR     : Constant sound speed
C     DBAR     : Constant density
C     DLINIT   : Left  depth
C     ULINIT   : Left  velocity
C     DRINIT   : Right depth
C     URINIT   : Right right velocity
*
      OPEN(UNIT = 1, FILE = 'gawaf.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)CFLCOE
      READ(1,*)LIMITE
      READ(1,*)CELLS
      READ(1,*)BCONDL
      READ(1,*)BCONDR
      READ(1,*)NFREQ
      READ(1,*)NTMAXI
      READ(1,*)TIMEOU
      READ(1,*)DIAPH
      READ(1,*)CBAR
      READ(1,*)DBAR
      READ(1,*)DLINIT
      READ(1,*)ULINIT
      READ(1,*)DRINIT
      READ(1,*)URINIT
*
      CLOSE(1)
*
      WRITE(6,*)'--------------------------------'
      WRITE(6,*)'Data read in is echoed to screen'
      WRITE(6,*)'--------------------------------'
      WRITE(6,*)'DOMLEN  = ',DOMLEN
      WRITE(6,*)'CFLCOE  = ',CFLCOE
      WRITE(6,*)'LIMITE  = ',LIMITE
      WRITE(6,*)'CELLS   = ',CELLS
      WRITE(6,*)'BCONDL  = ',BCONDL
      WRITE(6,*)'BCONDR  = ',BCONDR
      WRITE(6,*)'NFREQ   = ',NFREQ
      WRITE(6,*)'NTMAXI  = ',NTMAXI
      WRITE(6,*)'TIMEOU  = ',TIMEOU
      WRITE(6,*)'DIAPH   = ',DIAPH
      WRITE(6,*)'CBAR    = ',CBAR
      WRITE(6,*)'DBAR    = ',DBAR
      WRITE(6,*)'DLINIT  = ',DLINIT
      WRITE(6,*)'ULINIT  = ',ULINIT
      WRITE(6,*)'DRINIT  = ',DRINIT
      WRITE(6,*)'URINIT  = ',URINIT
      WRITE(6,*)'--------------------------------'
*
 10   FORMAT(1X, F10.4)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INITIA(DOMLEN, DIAPH, CELLS)
*
C     Purpose: to set initial conditions for solution U and initialise
C              other variables. Initial data is that for a Riemann
C              problem
*
C     Description of variables
*
C     CELLS      : Number of cells
C     IDIM       : Array dimension parameter
C     DX         : Spatial mesh size
C     FLUX       : Array for intercell fluxes
C     U          : Array for numerical solution
C     XPOS       : Position along x-axis
C     DLINIT     : Left density
C     ULINIT     : Left velocity
C     DRINIT     : Right density
C     URINIT     : Right velocity
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, K, CELLS, IDIM
*
      REAL    DIAPH, DOMLEN, DX, FLUX, U, XPOS,
     &        DLINIT, ULINIT, DRINIT, URINIT
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(2, -1:IDIM+2), U(2, -1:IDIM+2)
*
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
      COMMON /INICON/ DLINIT, ULINIT, DRINIT, URINIT
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Initialise arrays
*
      DO 10 I  = -1, IDIM+2
         DO 20 K = 1, 2
            FLUX(K, I) = 0.0
            U(K, I)    = 0.0
 20      CONTINUE
 10   CONTINUE
*
C     Set initial conditions (Riemann problem)
*
      DO 30 I = 1, CELLS
*
         XPOS = REAL(I)*DX
*
         IF(XPOS.LE.DIAPH)THEN
            U(1, I) = DLINIT
            U(2, I) = ULINIT
         ELSE
            U(1, I) = DRINIT
            U(2, I) = URINIT
         ENDIF
*
 30   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE EIGENS(CBAR)
*
C     Purpose: to compute eigenvalues (trivial)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  CBAR, LAMBD1, LAMBD2
*
      COMMON /SPEEDS/ LAMBD1, LAMBD2
*
C     Set eigenvalues LAMBD1 and LAMBD2
*
      LAMBD1 = - CBAR
      LAMBD2 = + CBAR
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(BCONDL, BCONDR, CELLS)
*
C     Purpose: to apply boundary conditions.
C              For BCONDL/R = 0 apply transmissive boundary conditions
C              Otherwise        apply reflective boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER BCONDL, BCONDR, CELLS, IDIM
*
      REAL    U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(2, -1:IDIM+2)
*
      COMMON /SOLUTI/ U
*
C     Left boundary
*
      IF(BCONDL.EQ.0)THEN
*
C        Apply transmissive boundary conditions
*
         U(1, -1) = U(1, 2)
         U(2, -1) = U(2, 2)
*
         U(1, 0)  = U(1, 1)
         U(2, 0)  = U(2, 1)

      ELSE
*
C        Apply reflective boundary conditions
*
         U(1, -1) =  U(1, 2)
         U(2, -1) = -U(2, 2)
*
         U(1, 0)  =  U(1, 1)
         U(2, 0)  = -U(2, 1)
      ENDIF
*
C     Right boundary
*
      IF(BCONDR.EQ.0)THEN
*
C        Apply transmissive boundary conditions
*
         U(1, CELLS + 1) = U(1, CELLS)
         U(2, CELLS + 1) = U(2, CELLS)
*
         U(1, CELLS + 2) = U(1, CELLS - 1)
         U(2, CELLS + 2) = U(2, CELLS - 1)
      ELSE
*
C        Apply reflective boundary conditions
*
         U(1, CELLS + 1) =  U(1, CELLS)
         U(2, CELLS + 1) = -U(2, CELLS)
*
         U(1, CELLS + 2) =  U(1, CELLS - 1)
         U(2, CELLS + 2) = -U(2, CELLS - 1)
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, TIME, TIMEOU)
*
C     Purpose: to apply the CFL condition to compute a stable
C              time step DT
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  CFLCOE, DT, DX, LAMBD1, LAMBD2, SMAX, TIME, TIMEOU
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /SPEEDS/ LAMBD1, LAMBD2
*
      SMAX = MAX(ABS(LAMBD1), ABS(LAMBD2))
      DT   = CFLCOE*DX/SMAX
*
C     Check size of DT to avoid exceeding output time
*
      IF((TIME+DT).GT.TIMEOU)THEN
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
      SUBROUTINE UPDATE(CELLS)
*
C     Purpose: to update the solution to a new time level using the
C              explicit conservative formula. System is regarded as
C              a conservative system with flux F = A U
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IDIM, K, CELLS
*
      REAL    DT, DTODX, DX, FLUX, U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(2, -1:IDIM+2), FLUX(2, -1:IDIM+2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
      DTODX = DT/DX
*
      DO 10 I = 1, CELLS
*
         DO 20 K = 1, 2
            U(K, I) = U(K, I) + DTODX*(FLUX(K, I-1) - FLUX(K, I))
 20      CONTINUE
*
 10   CONTINUE
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
      INTEGER CELLS, I, IDIM
*
      REAL    DX, U, XPOS
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(2, -1:IDIM+2)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
      OPEN(UNIT = 1, FILE = 'gawaf.out', STATUS = 'UNKNOWN')
*
      DO 10 I = 1, CELLS
*
         XPOS = (REAL(I) - 0.5)*DX
         WRITE(1,20)XPOS, U(1, I),  U(2, I)
*
 10   CONTINUE
*
      CLOSE(1)
*
 20   FORMAT(3(4X, F10.5))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE WAFFLX(LIMITE, CELLS)
*
C     Purpose: to compute the WAF flux with the exact
C              Riemann solver, to be used in explicit
C              conservative formula, subroutine UPDATE
*
C     Description of main variables
*
C      FD     :   Array of fluxes on data
C      FLUX   :   Array of intercell fluxes
C      FS     :   Array of fluxes in the Star Region
C      WAFLIM :   WAF limiter function
C      WJ     :   Array of wave jumps for TVD condition
C      WS     :   Array of wave speeds for TVD condition
C      WL     :   Flux weight for left state
C      WM     :   Flux weight for star state
C      WR     :   Flux weight for right state
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  CELLS, I, IDIM, IUPW, K, LIMITE
*
      REAL     CBAR, DBAR, CN, DL, DLOC, DR, DS, DT, DTODX, DUPW,
     &         DX, FD, FLUX, FS, LAMBD1, LAMBD2, RATIO, TOLLIM,
     &         U, UL, UR, US, WAFLIM, WJ, WL, WM, WR, WS
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(2, -1:IDIM+2), U(2, -1:IDIM+2), FD(2, -1:IDIM+2),
     &          WS(2, -1:IDIM+2), WJ(2, -1:IDIM+2), FS(2, -1:IDIM+2),
     &          CN(2), WAFLIM(2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
      COMMON /MATRIX/ CBAR, DBAR
      COMMON /SPEEDS/ LAMBD1, LAMBD2
*
      DATA TOLLIM/1.0E-06/
*
      DTODX = DT/DX
*
C     Compute fluxes on data
*
      DO 10 I = -1, CELLS + 2
*
         FD(1,I) = U(2,I)*DBAR
         FD(2,I) = U(1,I)*CBAR*CBAR/DBAR
*
 10   CONTINUE
*
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 20 I = -1, CELLS + 1
*
         DL = U(1, I)
         UL = U(2, I)
*
         DR = U(1, I+1)
         UR = U(2, I+1)
*
C        Solve Riemann problem exactly for state (DS, US)
*
         CALL RPRWAF(DL, UL, DR, UR, DS, US)
*
C        Compute intercell flux at state (DS, US)
*
         FS(1,I) = US*DBAR
         FS(2,I) = DS*CBAR*CBAR/DBAR
*
C        Store wave speeds for TVD condition
*
         WS(1,I) = LAMBD1
         WS(2,I) = LAMBD2
*
C        Store wave density jumps for TVD condition
*
         WJ(1,I) = DS        - U(1,I)
         WJ(2,I) = U(1, I+1) - DS
*
 20   CONTINUE
*
C     Compute the WAF intercell flux
*
      DO 30 I = 0, CELLS
*
C        Apply TVD condition
*
         DO 40 K = 1, 2
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
 40      CONTINUE
*
C        Compute weights for WAF flux evaluation
*
         WL = 0.5*(1.0       + WAFLIM(1))
         WM = 0.5*(WAFLIM(2) - WAFLIM(1))
         WR = 0.5*(1.0       - WAFLIM(2))
*
         DO 50 K = 1, 2
            FLUX(K,I) = WL*FD(K,I) + WM*FS(K,I)+ WR*FD(K,I+1)
 50      CONTINUE
*
 30   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RPRWAF(DL, UL, DR, UR, DS, US)
*
C     Purpose: to compute the solution of the Riemann problem for
C              the linearised equations of Gas Dynamics to obtain
C              the Star Values DS, US
*
      IMPLICIT NONE
*
      REAL  CBAR, DBAR, HA, DL, UL, DR, UR, DS, US
*

      COMMON /MATRIX/ CBAR, DBAR
*
      HA = DBAR/CBAR
*
      DS = 0.5*(DL + DR) + 0.5*HA*(UL - UR)
      US = 0.5*(UL + UR) + 0.5*(DL - DR)/HA
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
      A  = 1.0 - (1.0 - C)*B
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
      A  = 1.0 - (1.0 - C)*B
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
      A  = 1.0 - (1.0 - C)*B
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
      A  = 1.0 - (1.0 - C)*B
*
      END
*
*----------------------------------------------------------------------*
*


