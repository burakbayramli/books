*
*----------------------------------------------------------------------*
*                                                                      *
C     Exact Riemann Solver                                             *
*                                                                      *
C     Purpose: to solve exactly the Riemann problem for                *
C              the linearised shallow water equations                  *
*                                                                      *
C     Input  file: swrpex.ini                                          *
C     Output file: swrpex.out                                          *
*                                                                      *
C     Name of program: HL-SWRPEX                                       *
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
C                      Springer-Verlag, 1997.                          *
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
C     Declaration of variables
*
      INTEGER CELLS, I
*
      REAL    UBAR, HBAR, GRAVIT, DIAPH, DOMLEN, DX, S, TIMEOU,
     &        XPOS, DLINIT, ULINIT, DRINIT, URINIT, USTAR, DSTAR,
     &        DSAM, USAM, LAMBD1, LAMBD2, CBAR
*
      COMMON /MATRIX/ UBAR, HBAR, GRAVIT
      COMMON /SPEEDS/ LAMBD1, LAMBD2, CBAR
      COMMON /STATES/ DLINIT, ULINIT, DRINIT, URINIT
*
C     Description of variables
*
C    DOMLEN     : Domain length
C    CELLS      : Number of cells in domain
C    TIMEOU     : Output time
C    DIAPH      : Position of initial discontinuity
C    UBAR       : Constant velocity
C    HBAR       : Constant velocity
C    GRAVIT     : Acceleration due to gravity
C    DLINIT     : Left  state for depth
C    ULINIT     : Left  state for velocity
C    DRINIT     : Right state for depth
C    URINIT     : Right state for velocity
*
      OPEN(UNIT = 1, FILE = 'swrpex.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)CELLS
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
      WRITE(6,*)'DOMLEN  = ', DOMLEN
      WRITE(6,*)'CELLS   = ', CELLS
      WRITE(6,*)'TIMEOU  = ', TIMEOU
      WRITE(6,*)'DIAPH   = ', DIAPH
      WRITE(6,*)'UBAR    = ', UBAR
      WRITE(6,*)'HBAR    = ', HBAR
      WRITE(6,*)'GRAVIT  = ', GRAVIT
      WRITE(6,*)'DLINIT  = ', DLINIT
      WRITE(6,*)'ULINIT  = ', ULINIT
      WRITE(6,*)'DRINIT  = ', DRINIT
      WRITE(6,*)'URINIT  = ', URINIT
      WRITE(6,*)'--------------------------------'
*
      CALL EIGENS
*
      CALL RIEMAN(HBAR, CBAR, DSTAR, USTAR)
*
      WRITE(6,*)
      WRITE(6,*)DSTAR, USTAR
      WRITE(6,*)
*
      DX = DOMLEN/REAL(CELLS)
*
C     Complete solution at time TIMEOU is found
*
      OPEN(UNIT = 1,FILE = 'swrpex.out',STATUS = 'UNKNOWN')
*
      DO 10 I = 1, CELLS
*
         XPOS = (REAL(I) - 0.5)*DX
         S    = (XPOS - DIAPH)/TIMEOU
*
C        Solution at point (X,T) = ( XPOS - DIAPH, TIMEOU) is found
*
         CALL SAMPLE(S, LAMBD1, LAMBD2, DSTAR, USTAR, DSAM, USAM)
*
C        Exact solution profiles are written to exact.out
*
         WRITE(1, 20)XPOS, DSAM, USAM
*
 10   CONTINUE
*
      CLOSE(1)
*
 20   FORMAT(5(F14.6, 2X))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE EIGENS
*
C     Purpose: to compute the eigenvalues of the system
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  UBAR, HBAR, GRAVIT, CBAR, LAMBD1, LAMBD2
*
      COMMON /MATRIX/ UBAR, HBAR, GRAVIT
      COMMON /SPEEDS/ LAMBD1, LAMBD2, CBAR
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
      SUBROUTINE RIEMAN(HBAR, CBAR, DSTAR, USTAR)
*
C     Purpose: to compute the solution of the Riemann problem in
C              the STAR REGION for the linearised shallow water
C              equations
*
      IMPLICIT NONE
*
      REAL  HBAR, CBAR, HA, DSTAR, USTAR, DL, UL, DR, UR
*
      COMMON /STATES/ DL, UL, DR, UR
*
      HA = HBAR/CBAR
*
      DSTAR = 0.5*(DL + DR) + 0.5*HA*(UL - UR)
      USTAR = 0.5*(UL + UR) + 0.5*(DL - DR)/HA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE  SAMPLE(S, LAMBD1, LAMBD2, DSTAR, USTAR, DSAM, USAM)
*
C     Purpose: to sample the solution of the Riemann problem
C              for linearised shallow water equations
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

