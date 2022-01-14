*
*----------------------------------------------------------------------*
*                                                                      *
C     WAF scheme for the linear advection equation                     *
*                                                                      *
C     Name of program: HL-LAWAF                                        *
*                                                                      *
C     Purpose: to solve the linear advection equation with constant    *
C              coefficient by the Weighted Average Flux (WAF) method   *
C              using six limiter functions. The Godunov and            *
C              Lax-Wendroff schemes are included as special cases.     *
*                                                                      *
C     Input  file: lawaf.ini                                           *
C     output file: lawaf.out                                           *
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
      INTEGER CELLS, ITEST, LIMITE, N, NFREQ, NTMAXI
*
      REAL    CFLCOE, DOMLEN, SPEEDA, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, LIMITE, CELLS, NFREQ,
     &                NTMAXI, SPEEDA, TIMEOU
*
      DATA TIME, TIMETO /0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "lawaf.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(DOMLEN, ITEST, CELLS)
*
C     Time marching procedure
*
      WRITE(6,*)'-----------------------------------'
      WRITE(6,*)'   Time step N        TIME         '
      WRITE(6,*)'-----------------------------------'
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
         CALL WAFFLX(LIMITE, CELLS, SPEEDA)
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
C           Solution is written to "lawaf.out' at specified time
*
            CALL OUTPUT(CELLS)
*
C           Exact solution written to "exact.out' at time TIMEOU
*
            CALL EXASOL(DOMLEN, CELLS, SPEEDA, TIME)
*
            WRITE(6,*)'---------------------------------------------'
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
      OPEN(UNIT = 1, FILE = 'lawaf.ini', STATUS = 'UNKNOWN')
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
      OPEN(UNIT = 1, FILE = 'lawaf.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE WAFFLX(LIMITE, CELLS,SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Weighted Average Flux (WAF) method, to be used
C              in explicit conservative formula
*
C     Variables:
*
C     CN            Local CFL number
C     DLOC          Local jump in data
C     DUPW          Upwind jump in data
C     FL            Left flux in Riemann problem
C     FR            Right flux in Riemann problem
C     IUPW          Variable for wave direction
C     LIMITE        Choice of flux limiter
C     RATIO         Ratio of upwind to local jumps
C     TOLLIM        Small positive tolerance
C     UL            Left state in Riemann problem
C     UR            Right state in Riemann problem
C     WAFLIM        WAF limiter
C     WL            Weight for left state
C     WR            Weight for right state
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IUPW, LIMITE, CELLS, IDIM
*
      REAL    CN, DLOC, DT, DUPW, DX, FL, FR, FLUX, RATIO,
     &        SPEEDA, TOLLIM, U, UL, UR, WAFLIM, WL, WR
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
      DATA TOLLIM /1.0E-06/
*
C     Compute intercell flux FLUX(I), I = 0, CELLS+1
C     Solution of Riemann problem RP(I, I+1) is stored in FLUX(I)
*
      DO 10 I = 0, CELLS
*
C        Define states UL (Left) and UR (Right) for local
C        Riemann problem  RP(UL, UR)
*
         UL = U(I)
         UR = U(I+1)
*
C        Solve the Riemann problem RP(UL, UR) (trivial in this case)
*
         CN = SPEEDA*DT/DX
*
C        Compute local jump DLOC. This is the change across
C        the wave of speed SPEEDA.
*
         DLOC = U(I+1) - U(I)
*
C        Identify the upwind direction
*
         IF(SPEEDA.GT.0.0)THEN
*
C           Information travels from the left
*
            IUPW = -1
*
         ELSE
*
C           Information travels from the right
*
            IUPW = +1
*
         ENDIF
*
C        Compute the upwind jump DUPW. This depends
C        on the direction of the wave in the solution of the
C        Riemann problem RP(UL, UR), namely the sign of SPEEDA
*
         DUPW = U(I+1+IUPW) - U(I+IUPW)
*
C        Modify small changes
*
         IF(ABS(DUPW).LE.TOLLIM)DUPW = TOLLIM*SIGN(1.0,DUPW)
         IF(ABS(DLOC).LE.TOLLIM)DLOC = TOLLIM*SIGN(1.0,DLOC)
*
C        Compute RATIO of upwind to local changes
*
         RATIO = DUPW/DLOC
*
C        Apply TVD condition to find WAF limiter function WAFLIM
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
C        Compute weights for TVD WAF method
*
         WL = 0.5*(1.0 + WAFLIM)
         WR = 0.5*(1.0 - WAFLIM)
*
C        Compute WAF Flux FLUX(I) corresponding to RP(I,I+1)
*
         FL      = SPEEDA*UL
         FR      = SPEEDA*UR
         FLUX(I) = WL*FL + WR*FR
*
 10   CONTINUE
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

