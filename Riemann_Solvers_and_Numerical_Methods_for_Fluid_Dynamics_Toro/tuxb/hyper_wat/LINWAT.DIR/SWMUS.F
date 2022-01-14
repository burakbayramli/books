*
*----------------------------------------------------------------------*
*                                                                      *
C     MUSCL-Hancock Scheme for the Linearised Shallow Water Equations  *
*                                                                      *
C     Purpose: to solve the time-dependent, linearised shallow         *
C              water equations in one space dimension using the        *
C              MUSCL-Hancock scheme in conjunction with the exact      *
C              Riemann solver and with seven slope limiter             *
C              functions. The Godunov and Fromm schemes are            *
C              obtained as special cases                               *
*                                                                      *
C     Input  file: swmus.ini                                           *
C     Output file: swmus.out                                           *
*                                                                      *
C     Name of program: HL-SWMUS                                        *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in references 1, 2 and original references       *
C     therein                                                          *
*                                                                      *
C     1. Toro, E. F., "Riemann Solvers and Numerical                   *
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997                           *
C                      Second Edition, 1999                            *
*                                                                      *
C     2. Toro, E. F., "Front-Capturing Methods                         *
C                      for Free-Surface Shallow Flows"                 *
C                      John Wiley and Sons, 2000                       *
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
      REAL    CFLCOE, DIAPH, DOMLEN, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ BCONDL, BCONDR, CELLS, CFLCOE, DIAPH,
     &                DOMLEN, LIMITE, NFREQ, NTMAXI, TIMEOU
*
      DATA TIME, TIMETO /0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "swmus.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(DOMLEN, DIAPH, CELLS)
*
C     Eigenstructure of the system is computed
*
      CALL EIGENS
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
         CALL MUSFLX(LIMITE, CELLS)
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
C           Numerical solution written to "swmus.out' at time TIMEOU
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
*
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
      REAL     UBAR, HBAR, GRAVIT, CFLCOE, DIAPH, DOMLEN, TIMEOU,
     &         DLINIT, ULINIT, DRINIT, URINIT
*
      COMMON /DATAIN/ BCONDL, BCONDR, CELLS, CFLCOE, DIAPH,
     &                DOMLEN, LIMITE, NFREQ, NTMAXI, TIMEOU
      COMMON /MATRIX/ UBAR, HBAR, GRAVIT
      COMMON /INICON/ DLINIT, ULINIT, DRINIT, URINIT
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
C     UBAR     : Constant velocity
C     HBAR     : Constant depth
C     GRAVIT   : Acceleration due to gravity
C     DLINIT   : Left  depth
C     ULINIT   : Left  velocity
C     DRINIT   : Right depth
C     URINIT   : Right right velocity
*
      OPEN(UNIT = 1, FILE = 'swmus.ini', STATUS = 'UNKNOWN')
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
      READ(1,*)UBAR
      READ(1,*)HBAR
      READ(1,*)GRAVIT
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
      WRITE(6,*)'UBAR    = ',UBAR
      WRITE(6,*)'HBAR    = ',HBAR
      WRITE(6,*)'GRAVIT  = ',GRAVIT
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
C     Purpose: to set initial conditions for solution U and
C              initialise other variables. Initial data is as
C              for Riemann problem
*
C     Description of variables
*
C     CELLS      : Number of cells
C     IDIM       : Array dimension parameter
C     DX         : Spatial mesh size
C     FLUX       : Array for intercell fluxes
C     U          : Array for numerical solution
C     XPOS       : Position along x-axis
C     DLINIT     : Left depth
C     ULINIT     : Left velocity
C     DRINIT     : Right depth
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
         XPOS = (REAL(I) - 0.5)*DX
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
      SUBROUTINE EIGENS
*
C     Purpose: to compute eigenvalues and celerity
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  CBAR, HBAR, UBAR, LAMBD1, LAMBD2, GRAVIT
*
      COMMON /SPEEDS/ LAMBD1, LAMBD2, CBAR
      COMMON /MATRIX/ UBAR, HBAR, GRAVIT
*
C     Calculate constant celerity CBAR
*
      CBAR = SQRT(GRAVIT*HBAR)
*
C     Calculate eigenvalues LAMBD1 and LAMBD2
*
      LAMBD1 = UBAR - CBAR
      LAMBD2 = UBAR + CBAR
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
      REAL CBAR, CFLCOE, DT, DX, LAMBD1, LAMBD2, SMAX, TIME, TIMEOU
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /SPEEDS/ LAMBD1, LAMBD2, CBAR
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
      OPEN(UNIT = 1, FILE = 'swmus.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE MUSFLX(LIMITE, CELLS)
*
C     Purpose: to compute intercell flux according to the
C              MUSCL-Hancock method, to be used in explicit
C              conservative formula, subroutine UPDATE
*
C     Variables:
*
C     EL            Evolved left extrapolated values
C     ER            Evolved right extrapolated values
C     PIL           Left extrapolated values
C     PIR           Right extrapolated values
C     FIL           Flux on left extrapolated values
C     FIR           Flux on right extrapolated values
C     FLUX          Intercell flux
C     LIMITE        Choice of slope limiter
C     OMEGA         Parameter in data reconstruction slope
C     RATIO         Ratio of upwind to local changes
C     TOL           Small positive tolerance
C     DS            Value of U(1,I) in Godunov state
C     US            Value of U(2,I) in Godunov state
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, IDIM, K, LIMITE
*
      REAL    UBAR, HBAR, GRAVIT, DELFLUX, DELTA, DLOC,
     &        DT, DTODX, DUPW, DX, EL, ER, FIL, FIR, FLUX,
     &        OMEGA, PIL, PIR, RATIO, TOL, U, DL, UL, DR, UR, DS, US
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(2, -1:IDIM+2), U(2, -1:IDIM+2), EL(2, -1:IDIM+2),
     &          ER(2, -1:IDIM+2), PIL(2), PIR(2), FIL(2), FIR(2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
      COMMON /MATRIX/ UBAR, HBAR, GRAVIT
*
      DATA OMEGA, TOL /0.0, 1.0E-06/
*
      DTODX = DT/DX
*
C     Reconstruct the solution in each cell I, I = 0, CELLS + 1,
C     and evolve extrapolated values to the boundary
*
      DO 10 I = 0, CELLS + 1
*
         DO 20 K = 1, 2
*
            DUPW  = U(K, I) - U(K, I - 1)
            DLOC  = U(K, I + 1) - U(K, I)
            DELTA = 0.5*(1.0 + OMEGA)*DUPW + 0.5*(1.0 - OMEGA)*DLOC
*
C           Small changes are reset, preserving their sign
*
            IF(ABS(DUPW).LE.TOL)DUPW = TOL*SIGN(1.0,DUPW)
            IF(ABS(DLOC).LE.TOL)DLOC = TOL*SIGN(1.0,DLOC)
*
C           Ratio of changes is computed
*
            RATIO = DUPW/DLOC
*
C           Compute slope limiter functions. The subroutines carry
C           DELTA, multiply it by the slope limiter and  return
C           a limited DELTA to be used in the boundary extrapolation
C           step
*
C           Slope limiters used are:
*
C           LIMITE = 1, Godunov's first order upwind method
C           LIMITE = 2, Fromm second order method (oscillatory)
C           LIMITE = 3, upwind TVD, with SUPERBEE type limiter
C           LIMITE = 4, upwind TVD, with VAN LEER type limiter
C           LIMITE = 5, upwind TVD, with VAN ALBADA type limiter
C           LIMITE = 6, upwind TVD, with MINMOD type limiter
C           LIMITE = 7, upwind TVD, with MINMAX type limiter
*
            IF(LIMITE.EQ.1)DELTA = 0.0
            IF(LIMITE.EQ.2)DELTA = DELTA
            IF(LIMITE.EQ.3)CALL SBSLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.4)CALL VLSLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.5)CALL VASLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.6)CALL MISLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.7)CALL MINMAX(DUPW, DLOC, DELTA)
*
C           Boundary extrapolated values
C           PIL(K) and PIP(K) are computed
*
            PIL(K) = U(K, I) - 0.5*DELTA
            PIR(K) = U(K, I) + 0.5*DELTA
*
 20      CONTINUE
*
C        Numerically, the system is treated as a conservative
C        system. Fluxes F are computed as the product of the
C        constant coefficient matrix A and the vector U
C        of unknowns, F = A U
*
C        Compute boundary extrapolated fluxes
*
         FIL(1) = UBAR*PIL(1)   + HBAR*PIL(2)
         FIL(2) = GRAVIT*PIL(1) + UBAR*PIL(2)

         FIR(1) = UBAR*PIR(1)   + HBAR*PIR(2)
         FIR(2) = GRAVIT*PIR(1) + UBAR*PIR(2)
*
C        Evolve boundary extrapolated values for conserved
C        variables in each cell i
*
         DO 30 K = 1, 2
            DELFLUX = 0.5*DTODX*(FIL(K) - FIR(K))
            EL(K,I) = PIL(K) + DELFLUX
            ER(K,I) = PIR(K) + DELFLUX
 30      CONTINUE
*
 10   CONTINUE
*
C     Compute intercell flux FLUX(I), I = 0, CELLS, according
C     to the Godunov first-order upwind method
*
      DO 40 I = 0, CELLS
*
C        Data for local Riemann problem
*
         DL = ER(1, I)
         UL = ER(2, I)
*
         DR = EL(1, I + 1)
         UR = EL(2, I + 1)
*
C        Solve Riemann problem exactly for state (DS, US)
*
         CALL RIEMAN(HBAR, DL, UL, DR, UR, DS, US)
*
C        Compute intercell flux at state (DS, US)
*
         FLUX(1, I) = UBAR*DS   + HBAR*US
         FLUX(2, I) = GRAVIT*DS + UBAR*US
*
 40   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RIEMAN(HBAR, DL, UL, DR, UR, DS, US)
*
C     Purpose: to compute the solution of the Riemann problem for
C              the linearised  shallow water equations along the
C              t-axis, to find the Godunov state (DS, US)
*
      IMPLICIT NONE
*
      REAL  CBAR, HBAR, HA, DL, UL, DR, UR, DS, US, LAMBD1, LAMBD2
*
      COMMON /SPEEDS/ LAMBD1, LAMBD2, CBAR
*
      IF(LAMBD2.LE.0.0)THEN
*
C       Supercritical flow to the left. Sought state is right state
*
        DS = DR
        US = UR
*
      ENDIF
*
      IF(LAMBD1.LE.0.0.AND.LAMBD2.GE.0.0)THEN
*
C       Subcritical flow. Sought state is "star" state
*
        HA = HBAR/CBAR
*
        DS = 0.5*(DL + DR) + 0.5*HA*(UL - UR)
        US = 0.5*(UL + UR) + 0.5*(DL - DR)/HA
*
      ENDIF

      IF(LAMBD1.GE.0.0)THEN
*
C       Supercritical flow to the right. Sought state is left state
*
        DS = DL
        US = UL
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SBSLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a SUPERBEE type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI             = 0.0
      IF(R.GE.0.0)PHI = 2.0*R
      IF(R.GE.0.5)PHI = 1.0
*
      IF(R.GE.1.0)THEN
         DENOR = 1.0 - OMEGA + (1.0 + OMEGA)*R
         PHIR  = 2.0/DENOR
         PHI   = MIN(PHIR, R)
         PHI   = MIN(PHI, 2.0)
      ENDIF
*
      DELTA = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VLSLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a VAN LEER type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI = 0.0
*
      IF(R.GE.0.0)THEN
         DENOR = 1.0 - OMEGA + (1.0 + OMEGA)*R
         PHIR  = 2.0/DENOR
         PHI   = 2.0*R/(1.0 + R)
         PHI   = MIN(PHI, PHIR)
      ENDIF
*
      DELTA    = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VASLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a VAN ALBADA type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI = 0.0
*
      IF(R.GE.0.0)THEN
         DENOR = 1.0 - OMEGA + (1.0 + OMEGA)*R
         PHIR  = 2.0/DENOR
         PHI   = R*(1.0 + R)/(1.0 + R*R)
         PHI   = MIN(PHI, PHIR)
      ENDIF
*
      DELTA    = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MISLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a MINMOD type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI             = 0.0
      IF(R.GE.0.0)PHI = R
*
      IF(R.GE.1.0)THEN
         DENOR = 2.0*(1.0 - OMEGA + (1.0 + OMEGA)*R)
         PHIR  = 4.0/DENOR
         PHI   = MIN(1.0, PHIR)
      ENDIF
*
      DELTA    = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MINMAX(DUPW, DLOC, DELTA)
*
C     Purpose: to compute a MINMAX type slope limiter DELTA.
C              This is the most diffusive of all limiters
C              for centred schemes
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  BETAL, BETAR, DELTA, DLOC, DUPW, SIGNO
*
      BETAL = 1.0
      BETAR = 1.0
      SIGNO = 0.5*(SIGN(1.0,DUPW) + SIGN(1.0,DLOC))
      DELTA = SIGNO*(MIN(BETAL*ABS(DUPW),BETAR*ABS(DLOC)))
*
      END
*
*----------------------------------------------------------------------*
*

