*
*----------------------------------------------------------------------*
*                                                                      *
C      WAF scheme for the inviscid Burgers's equation                  *
*                                                                      *
C      Name of program: HL-BUWAF                                       *
*                                                                      *
C     Purpose: to solve the inviscid Burgers equation using the        *
C              Weighthed Average Flux (WAF) scheme in conjunction      *
C              with the exact Riemann solver and six limiter           *
C              functions                                               *
*                                                                      *
C     Input  file: buwaf.ini                                           *
C     output file: buwaf.out                                           *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Chaps. 5, 13 and 14 of Reference 1            *
C     and in original references therein                               *
*                                                                      *
C     1. Toro, E. F., "Riemann Solvers and Numerical                   *
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag,                                *
C                      Second Edition, 1999                            *
*                                                                      *
C     This program is part of                                          *
*                                                                      *
C     NUMERICA                                                         *
C     A Library of Source Codes for Teaching,                          *
C     Research and Applications,                                       *
C     by E. F. Toro                                                    *
C     Published by NUMERITEK LTD,                                      *
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
      INTEGER CELLS, ITEST, LIMITE, N, NFREQ, NTMAXI
*
      REAL    CFLCOE, DOMLEN, DT, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ CELLS, CFLCOE, DOMLEN, ITEST, LIMITE, NFREQ,
     &                NTMAXI, TIMEOU
*
      COMMON /DELTAT/ DT
*
      DATA TIMETO /1.0E-07/
*
C     Parameters of problem are read in from file "buwaf.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(DOMLEN, ITEST, CELLS)
*
      WRITE(6,*)'-----------------------------------'
      WRITE(6,*)'   Time step N        TIME         '
      WRITE(6,*)'-----------------------------------'
*
C     Time marching procedure
*
      TIME = 0.0
*
      DO 10 N = 1, NTMAXI
*
C        Boundary conditions are set
*
         CALL BCONDI(CELLS)
*
C        Courant-Friedrichs-Lewy (CFL) condition imposed
*
         CALL CFLCON(CFLCOE, CELLS, TIME, TIMEOU)
*
         TIME = TIME + DT
*
C        Intercell numerical fluxes are computed
*
         CALL WAFFLX(LIMITE, CELLS)
*
C        Solution is updated according to
C        conservative formula
*
         CALL UPDATE(CELLS)
*
         IF(MOD(N,NFREQ).EQ.0)WRITE(6,20)N, TIME
*
C        Check output time
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Solution is written to "buwaf.out' at
C           specified time TIMEOU
*
            CALL OUTPUT(CELLS)
*
            WRITE(6,*)'-----------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
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
      INTEGER  CELLS, ITEST, LIMITE, NFREQ, NTMAXI
*
      REAL     CFLCOE, DOMLEN, TIMEOU
*
      COMMON /DATAIN/ CELLS, CFLCOE, DOMLEN, ITEST, LIMITE, NFREQ,
     &                NTMAXI, TIMEOU
*
C     Input variables
*
C     CFLCOE   : Courant number coefficient
C     DOMLEN   : Domain length
C     ITEST    : Test problem
C     CELLS    : Number of cells in domain
C     LIMITE   : Limiter function
C     NFREQ    : Output frequency to screen
C     NTMAXI   : Maximum number of time steps
C     TIMEOU   : Output time
*
      OPEN(UNIT = 1,FILE = 'buwaf.ini',STATUS = 'UNKNOWN')
*
      READ(1,*)CFLCOE
      READ(1,*)DOMLEN
      READ(1,*)ITEST
      READ(1,*)CELLS
      READ(1,*)LIMITE
      READ(1,*)NFREQ
      READ(1,*)NTMAXI
      READ(1,*)TIMEOU
*
      CLOSE(1)
*
      WRITE(6,*)'--------------------------------'
      WRITE(6,*)'Data read in is echoed to screen'
      WRITE(6,*)'--------------------------------'
      WRITE(6,*)'CFLCOE = ',CFLCOE
      WRITE(6,*)'DOMLEN = ',DOMLEN
      WRITE(6,*)'ITEST  = ',ITEST
      WRITE(6,*)'CELLS  = ',CELLS
      WRITE(6,*)'LIMITE = ',LIMITE
      WRITE(6,*)'NFREQ  = ',NFREQ
      WRITE(6,*)'NTMAXI = ',NTMAXI
      WRITE(6,*)'TIMEOU = ',TIMEOU
      WRITE(6,*)'--------------------------------'
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INITIA(DOMLEN, ITEST, CELLS)
*
C     Purpose: to set initial conditions for solution U
C              and initialise other variables. There are
C              two choices of initial conditions,
C              determined by ITEST
*
C     Main variables:
*
C     DX            Spatial mesh  size
C     I             Variable in do loop
C     ITEST         Defines test problem
C     FLUX          Array for intercell fluxes
C     U             Array for numerical solution
C     XPOS          Position along x-axis
C     XRIGHT        Left diaphragm
C     XMIDDL        Middle diaphragm
C     XRIGHT        Right diaphragm
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, ITEST, IDIM
*
      REAL    DOMLEN, DX, FLUX, U, XLEFT, XPOS, XMIDDL,
     &        XRIGHT
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM + 2), U(-1:IDIM + 2)
*
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Initialise arrays
*
      DO 10 I = -1, IDIM + 2
         FLUX(I) = 0.0
         U(I)    = 0.0
 10   CONTINUE
*
      IF(ITEST.EQ.1)THEN
*
C        Test 1: smooth profile
*
         XPOS    = -1.0
*
         DO 20 I = 1,  CELLS
            XPOS = XPOS + 2.0/REAL(CELLS)
            U(I) = EXP(-8.0*XPOS*XPOS)
 20      CONTINUE
*
      ELSE
*
C        Test 2: square wave
*
         XLEFT  = 0.1*DOMLEN
         XMIDDL = 0.5*DOMLEN
         XRIGHT = 0.9*DOMLEN
*
         DO 30 I = 1, CELLS
*
            XPOS = (REAL(I) - 0.5)*DX
*
            IF(XPOS.LT.XLEFT)THEN
               U(I) = -1.0
            ENDIF
*
            IF(XPOS.GE.XLEFT.AND.XPOS.LE.XMIDDL)THEN
               U(I) = 1.0
            ENDIF
*
            IF(XPOS.GT.XMIDDL.AND.XPOS.LE.XRIGHT)THEN
               U(I) = 0.0
            ENDIF
*
            IF(XPOS.GT.XRIGHT)THEN
               U(I) = -1.0
            ENDIF
*
 30      CONTINUE
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(CELLS)
*
C     Purpose: to apply boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, IDIM
*
      REAL    U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(-1:IDIM + 2)
*
      COMMON /SOLUTI/ U
*
C     Left boundary, periodic boundary condition
*
      U(-1) = U(CELLS - 1)
      U(0)  = U(CELLS)
*
C     Right boundary, periodic boundary condition
*
      U(CELLS + 1) =  U(1)
      U(CELLS + 2) =  U(2)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, CELLS, TIME, TIMEOU)
*
C     Purpose: to apply the CFL condition to compute a
C              stable time step DT
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  CELLS, I, IDIM
*
      REAL     CFLCOE, DT, DX, SMAX, TIME, TIMEOU, U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(-1:IDIM + 2)
*
      COMMON /SOLUTI/ U
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
*
      SMAX = -1.0E+06
*
C     Find maximum characteristic speed
*
      DO 10 I = -1, CELLS + 2
         IF(ABS(U(I)).GT.SMAX)SMAX = ABS(U(I))
 10   CONTINUE
*
      DT = CFLCOE*DX/SMAX
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
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE UPDATE(CELLS)
*
C     Purpose: to update the solution to a new time level
C              using the explicit conservative formula
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    DT, DX, DTODX, FLUX, U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(-1:IDIM + 2), FLUX(-1:IDIM + 2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
      DTODX = DT/DX
*
      DO 10 I = 1, CELLS
         U(I) = U(I) + DTODX*(FLUX(I-1) - FLUX(I))
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE OUTPUT(CELLS)
*
C     Purpose: to output the solution at a specified time
C              TIMEOU
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    DX, U, XPOS
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(-1:IDIM + 2)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
      OPEN(UNIT = 1,FILE = 'buwaf.out',STATUS = 'UNKNOWN')
*
      DO 10 I = 1, CELLS
*
C        Find position of cell centre
*
         XPOS = (REAL(I) - 0.5)*DX
         WRITE(1,20)XPOS, U(I)
*
 10   CONTINUE
*
      CLOSE(1)
*
 20   FORMAT(2(4X, F10.5))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE WAFFLX(LIMITE, CELLS)
*
C     Purpose: to compute intercell fluxes according to the WAF
C              scheme (TVD upwind), to be used in explicit
C              conservative formula
*
C     Main variables:
*
C     LIMITE        Choice of slope limiter
C     CN            Local Courant number
C     DLOC          Local wave jump
C     DUPW          Upwind wave jump
C     RATIO         Ratio of upwind to local jumps
C     S             Speed arising from local Riemann problem
C     URPL          State resulting from local Riemann problem (left)
C     URPR          State resulting from local Riemann problem (right)
C     WAFLIM        WAF limiter function
C     WJ            Array to store wave jumps from local Riemann problem
C     WS            Array to store wave speed from local Riemann problem
C     WL            WAF weight for left contribution
C     WR            WAF weight for right contribution
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, IDIM, IUPW, LIMITE
*
      REAL    CN,  DLOC, DT, DTODX, DUPW, DX, FLUX, FSL, FSR,
     &        RATIO, S, TOLLIM, U, UL, UR, URPL, URPR, WAFLIM,
     &        WJ, WL, WR, WS
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2), FSL(-1:IDIM+2),
     &          FSR(-1:IDIM+2), WS(-1:IDIM+2), WJ(-1:IDIM+2)

      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
      DATA TOLLIM /1.0E-06/
*
      DO 10 I = -1, CELLS + 1
*
         UL = U(I)
         UR = U(I + 1)
*
         CALL RIEMAN(UL, UR, URPL, URPR, S)
*
C        Calculate fluxes on states URPL, URPR
*
         FSL(I) = 0.5*URPL*URPL
         FSR(I) = 0.5*URPR*URPR
*
C        Store wave speed and wave jump for TVD condition
*
         WS(I) = S
*
         WJ(I) = URPR - URPL
*
 10   CONTINUE
*
C     Compute the WAF intercell flux
*
      DTODX = DT/DX
*
      DO 20 I = 0, CELLS
*
C        Apply TVD condition
*
         CN = WS(I)*DTODX
*
C        Identify upwind direction
*
         IF(CN.GE.0.0)THEN
*
C           Wave is positive and upwind direction is on the left
*
            IUPW = -1
         ELSE
*
C           Wave is negative and upwind direction is on the right
*
            IUPW =  1
         ENDIF
*
C        Compute wave jump
*
         DLOC = WJ(I)
         DUPW = WJ(I+IUPW)
*
C        Modify small jumps preserving their sign
*
         IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0,DUPW)
         IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0,DLOC)
*
C        Compute RATIO of upwind to local changes
*
         RATIO = DUPW/DLOC
*
C        Select limiter function WAFLIM
*
C        LIMITE = 1, Godunov's Method
C        LIMITE = 2, Second Order Method (oscillatory)
C        LIMITE = 3, Upwind TVD, with SUPERBEE type limiter
C        LIMITE = 4, Upwind TVD, with VAN LEER type limiter
C        LIMITE = 5, Upwind TVD, with VAN ALBADA type limiter
C        LIMITE = 6, Upwind TVD, with MINMOD type limiter
*
         IF(LIMITE.EQ.1)WAFLIM = 1.0
         IF(LIMITE.EQ.2)WAFLIM = ABS(CN)
         IF(LIMITE.EQ.3)CALL SUPERA(RATIO, ABS(CN), WAFLIM)
         IF(LIMITE.EQ.4)CALL VANLEE(RATIO, ABS(CN), WAFLIM)
         IF(LIMITE.EQ.5)CALL VANALB(RATIO, ABS(CN), WAFLIM)
         IF(LIMITE.EQ.6)CALL MINAAA(RATIO, ABS(CN), WAFLIM)
*
         WAFLIM = WAFLIM*SIGN(1.0, CN)
*
C        Compute weights for WAF flux evaluation
*
         WL = 0.5*(1.0 + WAFLIM)
         WR = 0.5*(1.0 - WAFLIM)
*
         FLUX(I) = WL*FSL(I) + WR*FSR(I)
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RIEMAN(UL, UR, URPL, URPR, S)
*
C     Purpose: to solve the Riemann problem for the inviscid
C              Burgers equation exactly and to find states
C              for computing the WAF flux
*
C     Variables:
*
C     UL         Left data state
C     UR         Right data state
C     S          Wave speed (single)
C     URPL       Left state for flux exaluation
C     URPR       Right state for flux exaluation
*
      IMPLICIT NONE
*
      REAL   S, UL, UR, URPL, URPR
*
C     States for flux evaluation are set to default values
*
      URPL = UL
      URPR = UR
*
      IF(UL.GT.UR)THEN
*
C        Solution is a shock wave. Compute shock speed S
*
         S = 0.5*(UL + UR)
*
      ELSE
*
C        Solution is a rarefaction wave. There are 3 cases:
*
         IF(UL.GE.0.0)THEN
*
C           Right supersonic rarefaction
*
            S = MAX(UL, UR)
         ENDIF
*
         IF(UR.LE.0.0)THEN
*
C           Left supersonic rarefaction
*
            S = MIN(UL, UR)
         ENDIF
*
         IF(UL.LE.0.0.AND.UR.GE.0.0)THEN
*
C           Transonic rarefaction
*
            IF(ABS(UL).LT.ABS(UR))THEN
               S    = UR
               URPL = 0.0
               URPR = UR
            ELSE
               S    = UL
               URPL = UL
               URPR = 0.0
            ENDIF
         ENDIF
      ENDIF
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

