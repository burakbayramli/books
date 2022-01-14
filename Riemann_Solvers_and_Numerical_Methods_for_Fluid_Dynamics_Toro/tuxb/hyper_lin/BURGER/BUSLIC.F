*
*----------------------------------------------------------------------*
*                                                                      *
C     SLIC method for the inviscid Burgers's equation                  *
*                                                                      *
C      Name of program: HL-BUSLIC                                      *
*                                                                      *
C     Purpose: to solve the inviscid Burgers equation using the        *
C              Slope Limiter Centred (SLIC) scheme with a Choice       *
C              of seven slope limiter functions                        *
*                                                                      *
C     Input  file: buslic.ini                                          *
C     output file: buslic.out                                          *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Chaps. 7 and 13 of Reference 1                *
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
C     Parameters of problem are read in from file "buslic.ini"
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
         CALL FLUXES(LIMITE, CELLS)
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
C           Solution is written to "buslic.out' at
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
      OPEN(UNIT = 1,FILE = 'buslic.ini',STATUS = 'UNKNOWN')
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
      OPEN(UNIT = 1,FILE = 'buslic.out',STATUS = 'UNKNOWN')
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
      SUBROUTINE FLUXES(LIMITE, CELLS)
*
C     Purpose: to compute intercell fluxes according to the SLIC
C              method (TVD Centred), to be used in explicit
C              conservative formula
*
C     Main variables:
*
C     EL            Left extrapolated values
C     ER            Right extrapolated values
C     I             Variable for do loop
C     LIMITE        Choice of slope limiter
C     IDIM          Dimension of arrays
C     OMEGA         Parameter in data reconstruction
C     RATIO         Ratio of upwind to local changes
C     TOL           Small positive tolerance
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, LIMITE, IDIM
*
      REAL    DELTA,  DELFLUX, DLOC, DT, DUPW, EL, ER, DX, FL, FR,
     &        FLUX, FLULF, FLURI, OMEGA, RATIO, TOL, U, URI, UXL, UXR
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2), EL(-1:IDIM+2),
     &          ER(-1:IDIM+2)

      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
      DATA OMEGA, TOL /0.0, 1.0E-06/
*
C     Reconstruct the solution in each cell I, I = 0, CELLS + 1,
C     and evolve extrapolated values to the boundary
*
      DO 10 I = 0, CELLS + 1
*
         DUPW  = U(I) - U(I-1)
         DLOC  = U(I+1) - U(I)
         DELTA = 0.5*(1.0 + OMEGA)*DUPW + 0.5*(1.0 - OMEGA)*DLOC
*
C        Small changes are reset, preserving their sign
*
         IF(ABS(DUPW).LE.TOL)DUPW = TOL*SIGN(1.0,DUPW)
         IF(ABS(DLOC).LE.TOL)DLOC = TOL*SIGN(1.0,DLOC)
*
C        Ratio of changes is computed
*
         RATIO = DUPW/DLOC
*
C        Compute slope limiter functions. The subroutines carry
C        DELTA, multiply it by the slope limiter and  return
C        a limited DELTA to be used in the boundary extrapolation
C        step
*
C        Slope limiters used are:
*
C        LIMITE = 1, Godunov's first order upwind method
C        LIMITE = 2, Fromm second order method (oscillatory)
C        LIMITE = 3, upwind TVD, with SUPERBEE type limiter
C        LIMITE = 4, upwind TVD, with VAN LEER type limiter
C        LIMITE = 5, upwind TVD, with VAN ALBADA type limiter
C        LIMITE = 6, upwind TVD, with MINMOD type limiter
C        LIMITE = 7, upwind TVD, with MINMAX type limiter
*
         IF(LIMITE.EQ.1)DELTA = 0.0
         IF(LIMITE.EQ.2)DELTA = DELTA
         IF(LIMITE.EQ.3)CALL SBSLIC(RATIO, OMEGA, DELTA)
         IF(LIMITE.EQ.4)CALL VLSLIC(RATIO, OMEGA, DELTA)
         IF(LIMITE.EQ.5)CALL VASLIC(RATIO, OMEGA, DELTA)
         IF(LIMITE.EQ.6)CALL MISLIC(RATIO, OMEGA, DELTA)
         IF(LIMITE.EQ.7)CALL MINMAX(DUPW, DLOC, DELTA)
C
C        Boundary extrapolated values UXL and UXR are computed
C
         UXL = U(I) - 0.5*DELTA
         UXR = U(I) + 0.5*DELTA
*
C        UXL and  UXR are evolved by a half time step and stored
C        into EL(I) and ER(I), where I denotes i+1/2.
*
         DELFLUX = 0.5*(DT/DX)*0.5*(UXL*UXL - UXR*UXR)
         EL(I)   = UXL +  DELFLUX
         ER(I)   = UXR +  DELFLUX
*
 10   CONTINUE
*
C     Compute intercell flux FLUX(I), I = 0, CELLS, according
C     to the FORCE method
*
      DO 20 I = 0, CELLS
*
C        Compute Lax-Friedrichs flux
*
         FL    = 0.5*ER(I)*ER(I)
         FR    = 0.5*EL(I+1)*EL(I+1)
*
         FLULF = 0.5*(FL + FR) + 0.5*(DX/DT)*(ER(I) - EL(I+1))
*
C        Compute Richtmyer flux
*
         URI   = 0.5*(ER(I) + EL(I+1)) + 0.5*(DT/DX)*(FL - FR)
*
         FLURI = 0.5*URI*URI
*
C        Compute FORCE flux by averaging the Lax-Friedrichs and
C        Richtmyer fluxes
*
         FLUX(I) = 0.5*(FLULF + FLURI)
*
 20   CONTINUE
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

