*
*----------------------------------------------------------------------*
*                                                                      *
C     A selection of 8 schemes for the linear advection equation       *
*                                                                      *
C     Name of program: HL-LAGEN                                        *
*                                                                      *
C     Purpose: to solve the linear advection equation with constant    *
C              coefficient by a selection of 8 schemes, namely:        *
*                                                                      *
C              The Godunov first-order upwind scheme                   *
C              The Toro-Billett first-order upwind scheme              *
C              The Lax-Friedrichs scheme (first-order centred)         *
C              The FORCE scheme (first-order centred)                  *
C              The Godunov first-order centred scheme                  *
C              The Lax-Wendroff scheme (second order, oscillatory)     *
C              The Fromm scheme  (second order, oscillatory)           *
C              The Warming-Beam scheme (second order, oscillatory)     *
*                                                                      *
C     Input  file: lagen.ini                                           *
C     output file: lagen.out                                           *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Chaps. 5, 7 and 13 of Reference 1             *
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
      INTEGER ITEST, IFLUX, CELLS, N, NFREQ, NTMAXI
*
      REAL    CFLCOE, DOMLEN, SPEEDA, TIME, TIMEOU, TIMETO
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, IFLUX, ITEST, CELLS, NFREQ,
     &                NTMAXI, SPEEDA, TIMEOU
*
      DATA TIME, TIMETO /0.0, 1.0E-07/
*
C     Parameters of problem are read in from file "lagen.ini"
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
C        Intercell numerical fluxes are computed.
C        There are 8 choices available
*
         IF(IFLUX.EQ.1)CALL GODUPW(CELLS, SPEEDA)
         IF(IFLUX.EQ.2)CALL TORBIL(CELLS, SPEEDA)
         IF(IFLUX.EQ.3)CALL LAXFRI(CELLS, SPEEDA)
         IF(IFLUX.EQ.4)CALL FORCEF(CELLS, SPEEDA)
         IF(IFLUX.EQ.5)CALL GODCEN(CELLS, SPEEDA)
         IF(IFLUX.EQ.6)CALL RICHTM(CELLS, SPEEDA)
         IF(IFLUX.EQ.7)CALL FROMMF(CELLS, SPEEDA)
         IF(IFLUX.EQ.8)CALL WARBEA(CELLS, SPEEDA)
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
C           Solution is written to "lagen.out' at specified time
*
            CALL OUTPUT(CELLS)
*
C           Exact solution written to "lagen.out' at time TIMEOU
*
            CALL EXASOL(DOMLEN, CELLS, SPEEDA, TIME)
*
            WRITE(6,*)'-----------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'-----------------------------------'
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
C     Purpose: Read initial parameters of the problem
*
C     CFLCOE   : Courant number coefficient
C     DOMLEN   : Domain length
C     IFLUX    : Choice of intercell flux
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
      INTEGER  IFLUX, ITEST, CELLS, NFREQ, NTMAXI
*
      REAL     CFLCOE, DOMLEN, SPEEDA, TIMEOU
*
      COMMON /DATAIN/ CFLCOE, DOMLEN, IFLUX, ITEST, CELLS, NFREQ,
     &                 NTMAXI, SPEEDA, TIMEOU
*
      OPEN(UNIT = 1, FILE = 'lagen.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)CFLCOE
      READ(1,*)DOMLEN
      READ(1,*)IFLUX
      READ(1,*)ITEST
      READ(1,*)CELLS
      READ(1,*)NFREQ
      READ(1,*)NTMAXI
      READ(1,*)SPEEDA
      READ(1,*)TIMEOU
*
      CLOSE(1)
*
      WRITE(6,*)'-----------------------------------'
      WRITE(6,*)'Data read in is echoed to screen'
      WRITE(6,*)'-----------------------------------'
      WRITE(6,*)'CFLCOE  = ',CFLCOE
      WRITE(6,*)'DOMLEN  = ',DOMLEN
      WRITE(6,*)'IFLUX   = ',IFLUX
      WRITE(6,*)'ITEST   = ',ITEST
      WRITE(6,*)'CELLS   = ',CELLS
      WRITE(6,*)'NFREQ   = ',NFREQ
      WRITE(6,*)'NTMAXI  = ',NTMAXI
      WRITE(6,*)'SPEEDA  = ',SPEEDA
      WRITE(6,*)'TIMEOU = ',TIMEOU
      WRITE(6,*)'-----------------------------------'
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
      DO 10 I    = -1, IDIM + 2
         U(I)    = 0.0
         FLUX(I) = 0.0
 10   CONTINUE
*
      IF(ITEST.EQ.1)THEN
*
C        Test 1: smooth profile
*
         XPOS    = -1.0
         DO 20 I = 1,  CELLS
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
*     Store exact solution
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
C              time step DT according to the maximum wave speed SMAX
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
      REAL    DT, DX, DTOX, FLUX, U
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
      DTOX = DT/DX
*
      DO 10 I = 1, CELLS
         U(I) = U(I) + DTOX*(FLUX(I-1) - FLUX(I))
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
      OPEN(UNIT = 1, FILE = 'lagen.out', STATUS = 'UNKNOWN')
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
C----------------------------------------------------------------------C
C-----Numerical  flux  is  computed. There  are  8  choices  ----------C
C----------------------------------------------------------------------C
*
      SUBROUTINE GODUPW(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Godunov first order upwind method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    FONE, FTWO, FLUX, S, SPEEDA, U
*
      PARAMETER (IDIM = 1000)
*
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
C     Compute intercell flux FLUX(I), I = 0, CELLS
*
      DO 10 I = 0, CELLS
*
         S = SIGN(1.0, SPEEDA)
*
         FONE = 0.5*(1.0 + S)*SPEEDA*U(I)
         FTWO = 0.5*(1.0 - S)*SPEEDA*U(I+1)
*
C        Store intercell flux
*
       FLUX(I) = FONE + FTWO
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE TORBIL(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Toro-Billett first order CFL-2 scheme
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    CFLNUM, DT, DX, FONE, FTWO, FLUX, S, SPEEDA, U
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
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 13.22, Chapter 13, Ref. 1
*
*
      DO 10 I = 0, CELLS
*
         CFLNUM = SPEEDA*DT/DX
*
C        Identify the upwind direction
*
         S = SIGN(1.0, SPEEDA)
*
         IF(ABS(CFLNUM).LE.1.0)THEN
*
C           Conventional Godunov upwind flux
*
            FONE = 0.5*(1.0 + S)*SPEEDA*U(I)
            FTWO = 0.5*(1.0 - S)*SPEEDA*U(I+1)
         ELSE
            IF(SPEEDA.GE.0.0)THEN
*
C              Information travels from the left
*
               FONE = ((CFLNUM - 1)/CFLNUM)*SPEEDA*U(I-1)
                 FTWO = (1.0/CFLNUM)*SPEEDA*U(I)
*
            ELSE
*
C              Information travels from the right
*
             FONE = -(1.0/CFLNUM)*SPEEDA*U(I+1)
               FTWO = ((CFLNUM + 1)/CFLNUM)*SPEEDA*U(I+2)
            ENDIF
         ENDIF
*
C       Store intercell flux
*
        FLUX(I) = FONE + FTWO
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE LAXFRI(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Lax-Friedrichs method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    DT, DX, FONE, FTWO, FLUX, SPEEDA, U
*
      PARAMETER (IDIM = 1000)
      DIMENSION FLUX(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /DELTAT/ DT
      COMMON /DELTAX/ DX
      COMMON /FLUXFS/ FLUX
      COMMON /SOLUTI/ U
*
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 5.77, Chapter 5, Ref. 1
*
      DO 10 I = 0, CELLS
*
         FONE = 0.5*(SPEEDA*U(I) + SPEEDA*U(I+1))
         FTWO = 0.5*(DX/DT)*(U(I) - U(I+1))
*
C        Store intercell flux
*
       FLUX(I) = FONE + FTWO
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE FORCEF(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              FORCE method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    DT, DX, FONE, FTWO, FLUX, FLULF, FLURI, SPEEDA, U
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
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 7.32, Chapter 7, Ref. 1
*
      DO 10 I = 0, CELLS
*
C        Compute Lax-Friedrichs flux
*
         FONE = 0.5*(SPEEDA*U(I) + SPEEDA*U(I+1))
         FTWO = 0.5*(DX/DT)*(U(I) - U(I+1))
*
       FLULF = FONE + FTWO
*
C        Compute Richtmyer flux
*
         FONE  = 0.5*(U(I) + U(I+1))
         FTWO  = 0.5*(DT/DX)*(SPEEDA*U(I) - SPEEDA*U(I+1))
*
         FLURI = FONE + FTWO
*
C        Compute FORCE flux
*
       FLUX(I) = 0.5*(FLULF + FLURI)
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE GODCEN(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Godunov first-order centred method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    CFLNUM, DT, DX, FONE, FTWO, FLUX, SPEEDA, U
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
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 13.73, Chapter 13, Ref. 1
*
      DO 10 I = 0, CELLS
*
C        Calculate CFL number
*
         CFLNUM = SPEEDA*DT/DX
*
C        Compute the Godunov centred flux
*
         FONE = 0.5*(1.0 + 2.0*CFLNUM)*SPEEDA*U(I)
         FTWO = 0.5*(1.0 - 2.0*CFLNUM)*SPEEDA*U(I+1)
*
C        Store intercell flux
*
       FLUX(I) = FONE + FTWO
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RICHTM(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Richtmyer method, or two step Lax-Wendroff method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    DT, DX, FONE, FTWO, FLUX, SPEEDA, U
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
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 5.79, Chapter 5, Ref. 1
*
      DO 10 I = 0, CELLS
*
         FONE = 0.5*(U(I) + U(I+1))
         FTWO = 0.5*(DT/DX)*(SPEEDA*U(I) - SPEEDA*U(I+1))
*
C        Compute intercell flux
*
         FLUX(I) = SPEEDA*(FONE + FTWO)
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE FROMMF(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C              Fromm method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    CFLNUM, DT, DX, FONE, FTWO, FLUX, S, SPEEDA, U
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
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 13.35, Chapter 13, Ref. 1
*
      DO 10 I = 0, CELLS
*
C        Calculate CFL number
*
         CFLNUM = SPEEDA*DT/DX
*
C        Identify the upwind direction
*
         S    = SIGN(1.0, SPEEDA)
*
         FONE = SPEEDA*(U(I) + 0.25*(1.0 - CFLNUM)*(U(I+1) - U(I-1)))
*
         FTWO = SPEEDA*(U(I+1) - 0.25*(1.0 + CFLNUM)*(U(I+2) - U(I)))
*
C        Compute intercell flux
*
         FLUX(I) = 0.5*(1.0 + S)*FONE + 0.5*(1.0 - S)*FTWO
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE WARBEA(CELLS, SPEEDA)
*
C     Purpose: to compute intercell fluxes according to the
C               Warming-Beam method
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    CFLNUM, DT, DX, FONE, FTWO, FLUX, SPEEDA, U
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
C     Compute intercell flux FLUX(I), I = 0, CELLS.
C     See Eq. 13.21, Chapter 13, Ref. 1
*
      DO 10 I = 0, CELLS
*
C        Calculate CFL number
*
         CFLNUM = SPEEDA*DT/DX
*
         IF(SPEEDA.GT.0.0)THEN
            FONE = -0.5*(1.0 - CFLNUM)*SPEEDA*U(I-1)
            FTWO =  0.5*(3.0 - CFLNUM)*SPEEDA*U(I)
         ELSE
            FONE =  0.5*(3.0 + CFLNUM)*SPEEDA*U(I+1)
            FTWO = -0.5*(1.0 + CFLNUM)*SPEEDA*U(I+2)
         ENDIF
*
C        Store intercell flux
*
       FLUX(I) =  FONE + FTWO
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
