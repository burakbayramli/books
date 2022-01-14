*
*----------------------------------------------------------------------*
*                                                                      *
C     MUSCL-Hancock scheme for the linear advection equation           *
*                                                                      *
C     Name of program: HL-LAMUS                                        *
*                                                                      *
C     Purpose: to solve the linear advection equation with constant    *
C              coefficient by the MUSCL-Hancock TVD method using       *
C              seven  slope limiter functions. The Godunov and         *
C              Fromm schemes are included as special cases             *
*                                                                      *
C     Input  file: lamus.ini                                           *
C     output file: lamus.out                                           *
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
C                      Springer-Verlag, 1997                           *
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
      INTEGER ITEST, LIMITE, CELLS, N, NFREQ, NTMAXI
*
      REAL    CFLCOE, DOMLEN, SPEEDA, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, LIMITE, CELLS, NFREQ,
     &                NTMAXI, SPEEDA, TIMEOU
*
      DATA TIME, TIMETO /0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "lamus.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(DOMLEN, ITEST, CELLS)
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
         CALL BCONDI(CELLS)
*
C        Courant-Friedrichs-Lewy (CFL) condition imposed
*
         CALL CFLCON(CFLCOE, SPEEDA, TIME, TIMEOU)
*
C        Intercell numerical fluxes are computed
*
         CALL MUSFLX(LIMITE, CELLS, SPEEDA)
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
C           Numerical solution written to "lamus.out' at time TIMEOU
*
            CALL OUTPUT(CELLS)
*
C           Exact solution written to "exact.out' at time TIMEOU
*
            CALL EXASOL(DOMLEN, CELLS, SPEEDA, TIME)
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
C     Input variables
*
C     CFLCOE   : Courant number coefficient
C     DOMLEN   : Domain length
C     ITEST    : Test problem
C     LIMITE   : Choice of flux limiter
C     CELLS    : Number of cells in domain
C     NFREQ    : Output frequency to screen
C     NTMAXI   : Maximum number of time steps
C     SPEEDA   : Speed in PDE
C     TIMEOU   : Output time
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  ITEST, LIMITE, CELLS, NFREQ, NTMAXI
*
      REAL     CFLCOE, DOMLEN, SPEEDA, TIMEOU
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, LIMITE, CELLS, NFREQ,
     &                NTMAXI, SPEEDA, TIMEOU
*
      OPEN(UNIT = 1, FILE = 'lamus.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)CFLCOE
      READ(1,*)DOMLEN
      READ(1,*)ITEST
      READ(1,*)LIMITE
      READ(1,*)CELLS
      READ(1,*)NFREQ
      READ(1,*)NTMAXI
      READ(1,*)SPEEDA
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
      WRITE(6,*)'LIMITE = ',LIMITE
      WRITE(6,*)'CELLS  = ',CELLS
      WRITE(6,*)'NFREQ  = ',NFREQ
      WRITE(6,*)'NTMAXI = ',NTMAXI
      WRITE(6,*)'SPEEDA = ',SPEEDA
      WRITE(6,*)'TIMEOU = ',TIMEOU
      WRITE(6,*)'--------------------------------'
*
 10   FORMAT(1X, F10.4)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INITIA(DOMLEN, ITEST, CELLS)
*
C     Purpose: to set initial conditions for solution U and
C              initialise other variables. There are two
C              choices of initial conditions
*
C     Variables:
*
C     FLUX          Array for intercell fluxes
C     U             Array for numerical solution
C     ITEST         Defines test problem
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, ITEST, CELLS, IDIM
*
      REAL    DOMLEN, DX, FLUX, U, UEXACT, XLEFT, XPOS, XRIGHT
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2), UEXACT(-1:IDIM+2)
*
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
      COMMON /EXACTS/ UEXACT
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Initialise arrays
*
      DO 10 I = -1, IDIM+2
         FLUX(I)   = 0.0
         U(I)      = 0.0
         UEXACT(I) = 0.0
 10   CONTINUE
*
      IF(ITEST.EQ.1)THEN
*
C        Test 1: smooth profile
*
         XPOS    = -1.0
         DO 20 I = 1, CELLS
            XPOS = XPOS + 2.0/REAL(CELLS)
            U(I) = EXP(-8.0*XPOS*XPOS)
 20      CONTINUE
*
      ELSE
*
C        Test 2: square wave
*
         XLEFT  = 0.3*DOMLEN
         XRIGHT = 0.7*DOMLEN
*
         DO 30 I = 1, CELLS
*
            XPOS = (REAL(I) - 0.5)*DX
            IF(XPOS.LE.XLEFT.OR.XPOS.GT.XRIGHT)THEN
               U(I) = 0.0
            ELSE
               U(I) = 1.0
            ENDIF
*
 30      CONTINUE
*
      ENDIF
*
C     Store exact solution
*
      DO 40 I = 1,  CELLS
         UEXACT(I) = U(I)
 40   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(CELLS)
*
C     Purpose: to apply periodic boundary conditions
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
      DIMENSION U(-1:IDIM+2)
*
      COMMON /SOLUTI/ U
*
C     Left boundary
*
      U(-1) = U(CELLS-1)
      U(0)  = U(CELLS)
*
C     Right boundary
*
      U(CELLS+1) = U(1)
      U(CELLS+2) = U(2)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, SPEEDA, TIME, TIMEOU)
*
C     Purpose: to apply the CFL condition to compute a stable
C              time step DT based on maximum wave speed SMAX
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL CFLCOE, DT, DX, SMAX, SPEEDA, TIME, TIMEOU
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
*
      SMAX = ABS(SPEEDA)
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
C               explicit conservative formula
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
      DIMENSION U(-1:IDIM+2), FLUX(-1:IDIM+2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
      DTODX = DT/DX
*
      DO 10 I = 1, CELLS
         U(I) = U(I) + DTODX *(FLUX(I-1) - FLUX(I))
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
      INTEGER I, CELLS, IDIM
*
      REAL    DX, U, XPOS
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(-1:IDIM+2)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
      OPEN(UNIT = 1, FILE = 'lamus.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE EXASOL(DOMLEN, CELLS, SPEEDA, TIME)
*
C     Purpose: to output the exact solution at a specified time TIME
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM, M, MS, SIGNO
*
      REAL    DOMLEN, DX, SPEEDA, TIME, TRAVEL, TMIN, TMAX,
     &        TLEFT, TRIGH, UEXACT, XE, XPOS
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION UEXACT(-1:IDIM+2)
*
      COMMON /EXACTS/ UEXACT
      COMMON /DELTAX/ DX
*
      OPEN(UNIT = 1, FILE = 'exact.out', STATUS = 'UNKNOWN')
*
      TRAVEL = TIME*SPEEDA
      SIGNO  = INT(SIGN(1.0, SPEEDA))
      M      = SIGNO + INT(TRAVEL/DOMLEN)
      TMAX   = M*DOMLEN
      TMIN   = (M - SIGNO)*DOMLEN
*
C     Profile is translated to domain [0, DOMLEN]
*
      MS = 0
*
      DO 10 I = 1, CELLS
*
C        Shift position of cells to lie within [0, DOMLEN]
*
         XPOS = REAL(I)*DX + TRAVEL
         IF(SPEEDA.GT.0.0)THEN
            IF(MS.EQ.0.AND.XPOS.GE.TMAX)THEN
               MS = I
            ENDIF
         ELSE
            IF(MS.EQ.0.AND.XPOS.LE.TMAX)THEN
               MS = I
            ENDIF
         ENDIF
*
 10   CONTINUE
*
      IF(SIGNO.GE.0)THEN
         TLEFT = TMIN
         TRIGH = TMAX
      ELSE
         TLEFT = TMAX
         TRIGH = TMIN
      ENDIF
*
      DO 30 I = MS, CELLS
         XE = (REAL(I))*DX + TRAVEL - TRIGH
         WRITE(1,50)XE, UEXACT(I)
 30   CONTINUE
*
      DO 40 I = 1, MS - 1
         XE = (REAL(I))*DX + TRAVEL - TLEFT
         WRITE(1,50)XE, UEXACT(I)
 40   CONTINUE
*
      CLOSE(1)
*
 50   FORMAT(2(4X, F10.5))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MUSFLX(LIMITE, CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the MUSCL-
C              Hancock method, to be used in explicit conservative
C              formula
*
C     Variables:
*
C     EL            Left extrapolated values
C     ER            Right extrapolated values
C     LIMITE        Choice of slope limiter
C     OMEGA         Parameter in data reconstruction
C     RATIO         Ratio of upwind to local jumps
C     S             Sign of speed a
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, LIMITE, IDIM
*
      REAL    DELTA, DLOC, DT, DUPW, EL, ER, DX, FLUX, FONE,
     &        FTWO, OMEGA, RATIO, S, SPEEDA, TOL, U, UXL, UXR
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2), EL(-1:IDIM+2),
     &          ER(-1:IDIM+2)
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
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
*
C        Boundary extrapolated values UXL and UXR are computed
*
         UXL = U(I) - 0.5*DELTA
         UXR = U(I) + 0.5*DELTA
*
C        UXL and  UXR are evolved by a half time step and stored
C        into EL(I) and ER(I), where I denotes i+1/2.
*
         EL(I) = UXL + 0.5*(DT/DX)*SPEEDA*(UXL - UXR)
         ER(I) = UXR + 0.5*(DT/DX)*SPEEDA*(UXL - UXR)
*
 10   CONTINUE
*
C     Compute intercell flux FLUX(I), I = 0, CELLS, according
C     to the Godunov first-order upwind method
*
      DO 20 I = 0, CELLS
*
         S    = SIGN(1.0, SPEEDA)
*
         FONE = 0.5*(1.0 + S)*SPEEDA*ER(I)
         FTWO = 0.5*(1.0 - S)*SPEEDA*EL(I+1)
*
C        Store intercell flux
*
         FLUX(I) = FONE + FTWO
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

