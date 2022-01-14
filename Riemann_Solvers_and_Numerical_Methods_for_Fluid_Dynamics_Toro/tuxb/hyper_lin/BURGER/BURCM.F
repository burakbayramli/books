*
*----------------------------------------------------------------------*
*                                                                      *
C     Random Choice Method for the inviscid Burgers's                  *
C                  equation                                            *
*                                                                      *
C     Name of program: HL-BURCM                                        *
*                                                                      *
C     Purpose: to solve the inviscid Burgers equation                  *
C              using the Random Choice Method (RCM) in                 *
C              conjunction with the exact Riemann solver               *
C              and van der Corput sequences                            *
*                                                                      *
C     Input  file: burcm.ini                                           *
C     output file: burcm.out                                           *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Section 5.3.3 of Chapter 5 and                *
C     Sects. 7.2 and 7.5 of Chapter 7, Reference 1. See                *
C     also original references quoted in Ref. 1                        *
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
      INTEGER ITEST, CELLS, N, NFREQ, NTMAXI
*
      REAL    CFLCOE, DOMLEN, DT, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, CELLS,
     &                NFREQ, NTMAXI, TIMEOU
      COMMON /DELTAT/ DT
*
      DATA TIMETO /1.0E-07/
*
C     Parameters of problem are read in from  file "burcm.ini"
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
C        Solution is updated according to
C        the Random Choice Method
*
         CALL UPDATE(N, CELLS)
*
         IF(MOD(N,NFREQ).EQ.0)WRITE(6,20)N, TIME
*
C        Check output time
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Solution is written to "burcm.out' at
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
      INTEGER  ITEST, CELLS, NFREQ, NTMAXI
*
      REAL     CFLCOE, DOMLEN, TIMEOU
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, ITEST, CELLS, NFREQ,
     &                NTMAXI, TIMEOU
*
      OPEN(UNIT = 1,FILE = 'burcm.ini',STATUS = 'UNKNOWN')
*
C     Input variables
*
C     CFLCOE   : Courant number coefficient
C     DOMLEN   : Domain length
C     ITEST    : Test problem
C     CELLS    : Number of cells in domain
C     NFREQ    : Output frequency to screen
C     NTMAXI   : Maximum number of time steps
C     TIMEOU   : Output time
*
      READ(1,*)CFLCOE
      READ(1,*)DOMLEN
      READ(1,*)ITEST
      READ(1,*)CELLS
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
C     Main variables
*
C     DX            Spatial mesh  size
C     I             Variable in do loop
C     ITEST         Defines test problem
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
      INTEGER I, ITEST, CELLS, IDIM
*
      REAL    DOMLEN, DX, U, XLEFT, XPOS, XMIDDL, XRIGHT
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(0:IDIM + 1)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Initialise array
*
      DO 10 I = 0, IDIM + 1
         U(I) = 0.0
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
C        Test 2: square waves
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
      DIMENSION U(0:IDIM + 1)
*
      COMMON /SOLUTI/ U
*
C     Left boundary, periodic boundary condition
*
      U(0)  = U(CELLS)
*
C     Right boundary, periodic boundary condition
*
      U(CELLS + 1) =  U(1)
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
      INTEGER  I, CELLS, IDIM
*
      REAL     CFLCOE, DT, DX, SMAX, TIME, TIMEOU, U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(0:IDIM + 1)
*
      COMMON /SOLUTI/ U
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
*
      SMAX = -1.0E+06
*
C     Find maximum characteristic speed
*
      DO 10 I = 0, CELLS + 1
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
      SUBROUTINE UPDATE(N, CELLS)
*
C     Purpose: to update the solution to a new time level
C              using the explicit conservative formula
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, IDIM, N
*
      REAL    DT, DX, SL, SR, THETA, U, URCM, US
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(0:IDIM + 1), URCM(0:IDIM + 1)
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
            CALL SAMPLE(SL, U(I-1), U(I), US)
         ELSE
            CALL SAMPLE(SR, U(I), U(I+1), US)
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
      DIMENSION U(0:IDIM + 1)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
      OPEN(UNIT = 1,FILE = 'burcm.out',STATUS = 'UNKNOWN')
*
      DO 10 I = 1, CELLS
*
C        Find position of cell centre
*
         XPOS = (REAL(I) - 0.5)*DX
         WRITE(1, 20)XPOS, U(I)
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
      SUBROUTINE SAMPLE(SXT, UL, UR, US)
*
C     Purpose: to random sample the solution of the Riemann
C              problem with data (UL, UR)
*
C     Variables:
*
C     UL         Left data state
C     UR         Right data state
C     S          Shock speed
C     SXT        Sampling speed THETA DX/DT
C     US         Sampled state
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    S, SXT, UL, UR, US
*
      IF(UL.GT.UR)THEN
*
C        Solution is a shock wave
C        Compute shock speed S
*
         S = 0.5*(UL + UR)
*
         IF(SXT.LE.S)THEN
*
C           Sampled state is left data state
*
            US = UL
         ELSE
*
C           Sampled state is right data state
*
            US = UR
         ENDIF
*
      ELSE
*
C        Solution is a rarefaction wave.
C        There are 3 cases:
*
         IF(SXT.LE.UL)THEN
*
C           Sampled state is left data state
*
            US = UL
         ENDIF
*
         IF(SXT.GE.UR)THEN
*
C           Sampled state is right data state
*
            US = UR
         ENDIF
*
         IF(UL.LE.SXT.AND.SXT.LE.UR)THEN
*
C           Sampled state is state x/t inside rarefaction
*
            US = SXT
         ENDIF
*
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

