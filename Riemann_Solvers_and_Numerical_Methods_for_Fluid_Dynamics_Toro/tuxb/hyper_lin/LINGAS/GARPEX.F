*
*----------------------------------------------------------------------*
*                                                                      *
C     Exact Riemann Solver                                             *
*                                                                      *
C     Purpose: to solve exactly the Riemann problem for                *
C              the linearised equations of gas dynamics                *
*                                                                      *
C     Input  file: garpex.ini                                          *
C     Output file: garpex.out                                          *
*                                                                      *
C     Name of program: HL-GARPEX                                       *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in reference  and original references            *
C     therein                                                          *
*                                                                      *
C     1. Toro, E. F., "Riemann Solvers and Numerical                   *
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997.                          *
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
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I
*
      REAL    CBAR, DBAR, DIAPH, DOMLEN, DX, S, TIMEOU,
     &        XPOS, DLINIT, ULINIT, DRINIT, URINIT, USTAR, DSTAR,
     &        DSAM, USAM, LAMBD1, LAMBD2
*
      COMMON /STATES/ DLINIT, ULINIT, DRINIT, URINIT
*
C     Description of input variables
*
C     DOMLEN     : Domain length
C     CELLS      : Number of points for evaluating excat solution
C     TIMEOU     : Output time
C     DIAPH      : Position of initial discontinuity
C     CBAR       : Constant sound speed
C     DBAR       : Constant density
C     DLINIT     : Left density
C     ULINIT     : Left velocity
C     DRINIT     : Right density
C     URINIT     : Right velocity
*
      OPEN(UNIT = 1, FILE = 'garpex.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)CELLS
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
      WRITE(6,*)'DOMLEN  = ', DOMLEN
      WRITE(6,*)'CELLS   = ', CELLS
      WRITE(6,*)'TIMEOU  = ', TIMEOU
      WRITE(6,*)'DIAPH   = ', DIAPH
      WRITE(6,*)'CBAR    = ', CBAR
      WRITE(6,*)'DBAR    = ', DBAR
      WRITE(6,*)'DLINIT  = ', DLINIT
      WRITE(6,*)'ULINIT  = ', ULINIT
      WRITE(6,*)'DRINIT  = ', DRINIT
      WRITE(6,*)'URINIT  = ', URINIT
      WRITE(6,*)'--------------------------------'
*
      CALL EIGENS(CBAR, LAMBD1, LAMBD2)
*
      CALL RIEMAN(CBAR, DBAR, DSTAR, USTAR)
*
      WRITE(6,*)
      WRITE(6,*)DSTAR, USTAR
      WRITE(6,*)
*
      DX = DOMLEN/REAL(CELLS)
*
C     Complete solution at time TIMEOU is found
*
      OPEN(UNIT = 1,FILE = 'garpex.out',STATUS = 'UNKNOWN')
*
      DO 10 I = 1, CELLS
*
         XPOS = REAL(I)*DX
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
      SUBROUTINE EIGENS(CBAR, LAMBD1, LAMBD2)
*
C     Purpose: to compute the eigenvalues of the system (trivial)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   CBAR, LAMBD1, LAMBD2
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
      SUBROUTINE RIEMAN(CBAR, DBAR, DSTAR, USTAR)
*
C     Purpose: to compute the solution of the Riemann problem in
C              the STAR REGION for the linearised shallow water
C              equations
*
      IMPLICIT NONE
*
      REAL  CBAR, DBAR, DSTAR, USTAR, HA, DL, UL, DR, UR
*
      COMMON /STATES/ DL, UL, DR, UR
*
      HA = DBAR/CBAR
*
      DSTAR = 0.5*(DL + DR) + 0.5*(UL - UR)*HA
      USTAR = 0.5*(UL + UR) + 0.5*(DL - DR)/HA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE  SAMPLE(S, LAMBD1, LAMBD2, DSTAR, USTAR, DSAM, USAM)
*
C     Purpose: to sample the solution of the Riemann problem
C              for linearised equations of gas dynamics
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

