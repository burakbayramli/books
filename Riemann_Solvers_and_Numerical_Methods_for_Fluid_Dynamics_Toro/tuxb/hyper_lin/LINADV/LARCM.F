*
*----------------------------------------------------------------------*
*                                                                      *
C     Random Choice Method (RCM) for the linear advection equation     *
*                                                                      *
C     Name of program: HL-LARCM                                        *
*                                                                      *
C     Purpose: to solve the linear advection equation with constant    *
C              coefficient by the Random Choice Method (RCM).          *
C              The van der Corput pseudo-random sequences are used     *
C              for sampling local exact solutions of the Riemann       *
C              problem                                                 *
*                                                                      *
C     Input  file: larcm.ini                                           *
C     output file: larcm.out                                           *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Chap. 7 of Reference 1                        *
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
      INTEGER ITEST, CELLS, N, NFREQ, NTMAXI
*
      REAL    CFLCOE, DOMLEN, SPEEDA, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, CELLS, NFREQ,
     &                NTMAXI, SPEEDA, TIMEOU
*
      DATA TIME, TIMETO /0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "larcm.ini"
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
C        Solution is updated according to RCM
*
         CALL RCMUPD(N, SPEEDA, CELLS)
*
         IF(MOD(N,NFREQ).EQ.0)WRITE(6,20)N, TIME
*
C        Check output time
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Numerical solution written to "larcm.out' at time TIMEOU
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
 30   CONTINUE
*
 20   FORMAT(I12,6X, F12.7)
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
      INTEGER  ITEST, CELLS, NFREQ, NTMAXI
*
      REAL     CFLCOE, DOMLEN, SPEEDA, TIMEOU
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, CELLS, NFREQ,
     &                 NTMAXI, SPEEDA, TIMEOU
*
      OPEN(UNIT = 1, FILE = 'larcm.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)CFLCOE
      READ(1,*)DOMLEN
      READ(1,*)ITEST
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
      REAL    DOMLEN, DX, U, UEXACT, XLEFT, XPOS, XRIGHT
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION  U(-1:IDIM+2), UEXACT(-1:IDIM+2)
*
      COMMON /DELTAX/ DX
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
      U(0)  = U(CELLS)
*
C     Right boundary
*
      U(CELLS+1) = U(1)
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
      OPEN(UNIT = 1, FILE = 'larcm.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE RCMUPD(N, SPEEDA, CELLS)
*
C     Purpose: to update the solution to a new time level using the
C              Random Choice Method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM, N
*
      REAL    DT, DX, SL, SR, SPEEDA, THETA, U, URCM, US
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(-1:IDIM+2), URCM(-1:IDIM+2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
      CALL RANDOM(THETA, N)
*
      SL = THETA*DX/DT
      SR = (THETA - 1.0)*DX/DT
*
      DO 10 I = 1, CELLS
*
         IF(THETA.LT.0.5)THEN
            CALL SAMPLE(SL, SPEEDA, U(I-1), U(I), US)
         ELSE
            CALL SAMPLE(SR, SPEEDA, U(I), U(I+1), US)
         ENDIF
*
C        Store sampled solution
*
         URCM(I) = US
*
 10   CONTINUE
*
C     Reset initial conditions
*
      DO 20 I = 1, CELLS
         U(I) = URCM(I)
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMPLE(S, SPEEDA, UL, UR, US)
*
C     Purpose: to random sample the solution of the Riemann
C              problem with data (UL, UR)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    S, SPEEDA, UL, UR, US
*
      IF(S.LE.SPEEDA)THEN
         US = UL
      ELSE
         US = UR
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RANDOM(THETA, N)
*
C     Purpose: to generate a sequence of van der Corput pseudo
C              random numbers to be used with the Random Choice
C              Method. Parameters K1, K2 may be chosen by user,
C              with K1 > K2 and relatively prime
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, J, K1, K2, L, N, NN
*
      REAL     THETA
*
      DATA K1, K2 /5, 3/
*
      THETA = 0.0
      I     = 0
*
 10   NN    = N/(K1**I)
*
      IF(NN.LT.1)GOTO 20
*
      L     = MOD(NN, K1)
      J     = MOD(K2*L, K1)
      THETA = THETA + REAL(J)/(K1**(I + 1))
      I     = I + 1
*
      GOTO 10
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*


