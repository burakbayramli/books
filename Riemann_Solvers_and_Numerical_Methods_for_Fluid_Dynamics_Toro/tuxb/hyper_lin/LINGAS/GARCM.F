*
*----------------------------------------------------------------------*
*                                                                      *
C     Random Choice Method (RCM) for the Linearised Equations          *
C     of Gas Dynamics                                                  *
*                                                                      *
C     Purpose: to solve the time-dependent, linearised equations       *
C              of Gas Dynamics in one space dimension using the        *
C              Random Choice Method on a non-staggered grid in         *
C              conjunction with van der Corput pseudo-random           *
C              sequences                                               *
*                                                                      *
C     Input  file: garcm.ini                                           *
C     Output file: garcm.out                                           *
*                                                                      *
C     Name of program: HL-GARCM                                        *
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
      INTEGER BCONDL, BCONDR, CELLS, N, NFREQ, NTMAXI
*
      REAL    CBAR, DBAR, CFLCOE, DIAPH, DOMLEN, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ BCONDL, BCONDR, CELLS, CFLCOE, DIAPH,
     &                DOMLEN, NFREQ, NTMAXI, TIMEOU
      COMMON /MATRIX/ CBAR, DBAR
*
      DATA TIME, TIMETO /0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "garcm.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(DOMLEN, DIAPH, CELLS)
*
C     Wave speeds (constant) are computed
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
C        Solution is updated via the Random Choice Method
*
         CALL RCMMET(CELLS, N)
*
         IF(MOD(N,NFREQ).EQ.0)WRITE(6,20)N, TIME
*
C        Check output time
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Numerical solution written to "garcm.out' at time TIMEOU
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
      INTEGER  BCONDL, BCONDR, CELLS, NFREQ, NTMAXI
*
      REAL     CBAR, DBAR, CFLCOE, DIAPH, DOMLEN, TIMEOU,
     &         DLINIT, ULINIT, DRINIT, URINIT
*
      COMMON /DATAIN/ BCONDL, BCONDR, CELLS, CFLCOE, DIAPH,
     &                DOMLEN, NFREQ, NTMAXI, TIMEOU
      COMMON /INICON/ DLINIT, ULINIT, DRINIT, URINIT
      COMMON /MATRIX/ CBAR, DBAR
*
C     Description of variables
*
C     DOMLEN   : Domain length
C     CFLCOE   : Courant number coefficient
C     CELLS    : Number of cells in domain
C     BCONDL   : Boundary conditions parameter, left
C     BCONDR   : Boundary conditions parameter, right
C     NFREQ    : Output frequency to screen
C     NTMAXI   : Maximum number of time steps
C     TIMEOU   : Output time
C     DIAPH    : Position of initial discontinuity
C     CBAR     : Constant sound speed
C     DBAR     : Constant density
C     DLINIT   : Left density
C     ULINIT   : Left velocity
C     DRINIT   : Right density
C     URINIT   : Right velocity
*
      OPEN(UNIT = 1, FILE = 'garcm.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)CFLCOE
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
C     Purpose: to set initial conditions for solution U and
C              initialise other variables. Initial conditions
C              are as for a Riemann problem
*
C     Description of variables
*
C     CELLS      : Number of cells
C     IDIM       : Array dimension parameter
C     DX         : Spatial mesh size
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
      REAL    DIAPH, DOMLEN, DX, U, XPOS, DLINIT, ULINIT,
     &        DRINIT, URINIT
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(2, 0:IDIM+1)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
      COMMON /INICON/ DLINIT, ULINIT, DRINIT, URINIT
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Initialise array
*
      DO 10 I  = 0, IDIM+1
         DO 20 K = 1, 2
            U(K, I) = 0.0
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
C     Calculate eigenvalues LAMBD1 and LAMBD2
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
      DIMENSION U(2, 0:IDIM+1)
*
      COMMON /SOLUTI/ U
*
C     Left boundary
*
      IF(BCONDL.EQ.0)THEN
*
C        Apply transmissive boundary conditions
*
         U(1, 0)  = U(1, 1)
         U(2, 0)  = U(2, 1)

      ELSE
*
C        Apply reflective boundary conditions
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
      ELSE
*
C        Apply reflective boundary conditions
*
         U(1, CELLS + 1) =  U(1, CELLS)
         U(2, CELLS + 1) = -U(2, CELLS)
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, TIME, TIMEOU)
*
C     Purpose: to apply the CFL condition to compute a stable
C              time step DT.
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL CFLCOE, DT, DX, LAMBD1, LAMBD2, SMAX, TIME, TIMEOU
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
      DIMENSION U(2, 0:IDIM+1)
*
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
*
      OPEN(UNIT = 1, FILE = 'garcm.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE RCMMET(CELLS, N)

C     Purpose: to compute solution by the Random Choice Method
C              on non-staggered grid using van der Corput sequences
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  CELLS, I, IDIM, N
*
      REAL     CBAR, DBAR, DL, DR, DSAM, DSTAR, DT, DTODX, DX,
     &         LAMBD1, LAMBD2, RANNUM, SPEEDL, SPEEDR, U, UL, UR,
     &         USAM, USTAR
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION U(2, 0:IDIM+1)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /SOLUTI/ U
      COMMON /MATRIX/ CBAR, DBAR
      COMMON /SPEEDS/ LAMBD1, LAMBD2
      COMMON /STATES/ DL, UL, DR, UR
*
C     Van de Corput pseudo-random number RANNUM is generated
C     at each time level N
*
      CALL RANDOM(RANNUM, N)
*
      DTODX  = DT/DX
      SPEEDL = RANNUM/DTODX
      SPEEDR = (RANNUM - 1.0)/DTODX
*
C     Sweep along x-axis
*
      DO 10 I = 1, CELLS
*
         IF(I.EQ.1)THEN
*
C           Solve Riemann problem at left boundary
*
            DL = U(1, I-1)
            UL = U(2, I-1)
*
            DR = U(1, I)
            UR = U(2, I)
*
            CALL RPSTAR(CBAR, DBAR, DSTAR, USTAR)
*
         ENDIF
*
         IF(RANNUM.LE.0.5)THEN
*
C           Sample solution of left Riemann problem
*
            CALL SAMPLE(SPEEDL,LAMBD1,LAMBD2,DSTAR,USTAR,DSAM,USAM)
*
         ENDIF
*
C        Solve Riemann problem RP(i,i+1)
*
         DL = U(1, I)
         UL = U(2, I)
*
         DR = U(1, I+1)
         UR = U(2, I+1)
*
         CALL RPSTAR(CBAR, DBAR, DSTAR, USTAR)
*
         IF(RANNUM.GT.0.5)THEN
*
C           Sample solution of right Riemann problem
*
            CALL SAMPLE(SPEEDR,LAMBD1,LAMBD2,DSTAR,USTAR,DSAM,USAM)
*
         ENDIF
*
C        Assign sampled values DSAM, USAM to cell i
*
         U(1, I) = DSAM
         U(2, I) = USAM
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RPSTAR(CBAR, DBAR, DSTAR, USTAR)
*
C     Purpose: to compute the solution of the Riemann problem in
C              the STAR REGION for the linearised equations of
C              Gas Dynamics
*
      IMPLICIT NONE
*
      REAL  CBAR, DBAR, HA, DSTAR, USTAR, DL, UL, DR, UR
*
      COMMON /STATES/ DL, UL, DR, UR
*
      HA = DBAR/CBAR
*
      DSTAR = 0.5*(DL + DR) + 0.5*HA*(UL - UR)
      USTAR = 0.5*(UL + UR) + 0.5*(DL - DR)/HA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMPLE(S, LAMBD1, LAMBD2, DSTAR, USTAR, DSAM, USAM)
*
C     Purpose: to sample the solution of the Riemann problem
C              for the linearised equations of Gas Dynamics
*
      IMPLICIT NONE
*
      REAL  LAMBD1, LAMBD2, S,
     &      DLINIT, ULINIT, DRINIT, URINIT, DSTAR, USTAR, DSAM, USAM
*
      COMMON /STATES/ DLINIT, ULINIT, DRINIT, URINIT
*
      IF(S.LE.LAMBD1)THEN
*
C        Sample point lies to the left of the left wave
*
         DSAM = DLINIT
         USAM = ULINIT
*
      ENDIF
*
      IF(S.GE.LAMBD1.AND.S.LE.LAMBD2)THEN
*
C        Sample point lies in the STAR REGION
*
         DSAM = DSTAR
         USAM = USTAR
*
      ENDIF

      IF(S.GE.LAMBD2)THEN
*
C        Sample point lies to the righ of the right wave
*
         DSAM = DRINIT
         USAM = URINIT
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


