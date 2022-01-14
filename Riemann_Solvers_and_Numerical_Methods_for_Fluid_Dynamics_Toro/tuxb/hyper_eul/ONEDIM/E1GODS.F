*
*----------------------------------------------------------------------*
*                                                                      *
*     First-Order Godunov schemes for the time-dependent               *
C     one-dimensional Euler equations                                  *
*                                                                      *
C     Program name: HE-E1GODS                                          *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas by Godunov methods with      *
C              several state Rieman solvers, namely:                   *
*                                                                      *
C              The exact Riemann solver                                *
C              The Two-Shock Riemann solver                            *
C              The adaptive Riemann solver                             *
C              The PRVS Riemann solver                                 *
*                                                                      *
C     Input  file: e1gods.ini (initial data)                           *
C     Output file: e1gods.out (numerical results)                      *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Ref. 1, Chaps. 4, 6 and 9, and in original    *
C     references therein                                               *
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
C     Declaration of variables
*
      INTEGER INTFLX, CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, PSCALE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, INTFLX, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1gods.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA(CELLS)
*
C     Time marching procedure
*
      WRITE(6,*)'---------------------------------------------'
      WRITE(6,*)'   Time step N        TIME           TIMEOU'
      WRITE(6,*)'---------------------------------------------'
*
      DO 10 N = 1, NTMAXI
*
C        Boundary conditions are set
*
         CALL BCONDI(CELLS)
*
C        Courant-Friedrichs-Lewy (CFL) condition imposed
*
         CALL CFLCON(CFLCOE, CELLS, N, TIME, TIMEOU)
*
C        Intercell numerical fluxes are computed.
C        Four choices are available, depending on value of INTFLX
*
         CALL RPGODU(CELLS, INTFLX)
*
C        Solution is updated according to conservative formula
*
         CALL UPDATE(CELLS)
*
         IF(MOD(N,NFREQU).EQ.0)WRITE(6,20)N, TIME, TIMEOU
*
C        Check output time
*
         TIMDIF = ABS(TIME - TIMEOU)
*
         IF(TIMDIF.LE.TIMTOL)THEN
*
C           Solution is written to "e1gods.out' at specified time
*
            CALL OUTPUT(CELLS, PSCALE)
*
            WRITE(6,*)'---------------------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'---------------------------------------------'
*
            GOTO 30
         ENDIF
*
 10   CONTINUE
 30   CONTINUE
 20   FORMAT(I12,6X,2(F12.7, 4X))
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
C     DOMLEN    : Domain length
C     DIAPH1    : Position of diaphragm 1
C     CELLS     : Number of computing cells
C     GAMMA     : Ratio of specific heats
C     TIMEOU    : Output time
C     DLINIT    : Initial density  on left section of tube
C     ULINIT    : Initial velocity on left section of tube
C     PLINIT    : Initial pressure on left section of tube
C     DMINIT    : Initial density  on middle section of tube
C     UMINIT    : Initial velocity on middle section of tube
C     PMINIT    : Initial pressure on middle section of tube
C     DRINIT    : Initial density  on right section of tube
C     URINIT    : Initial velocity on right section of tube
C     PRINIT    : Initial pressure on right section of tube
C     DIAPH2    : Position of diaphragm 2
C     CFLCOE    : Courant number coefficient
C     IBCLEF    : Type of left boundary conditions
C     IBCRIG    : Type of right boundary conditions
C     NFREQU    : Output frequency to screen
C     NTMAXI    : Maximum number of time steps
C     PSCALE    : Pressure scaling factor
C     INTFLX    : Choice of intercell flux
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER INTFLX, IBCLEF, IBCRIG, CELLS, NFREQU, NTMAXI
*
      REAL    CFLCOE, DOMLEN, DIAPH1, DIAPH2, PSCALE, TIMEOU,
     &        DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT, DRINIT,
     &        URINIT, PRINIT,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /DRIVER/ CFLCOE, INTFLX, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      OPEN(UNIT = 1, FILE = 'e1gods.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)DIAPH1
      READ(1,*)CELLS
      READ(1,*)GAMMA
      READ(1,*)TIMEOU
      READ(1,*)DLINIT
      READ(1,*)ULINIT
      READ(1,*)PLINIT
      READ(1,*)DMINIT
      READ(1,*)UMINIT
      READ(1,*)PMINIT
      READ(1,*)DRINIT
      READ(1,*)URINIT
      READ(1,*)PRINIT
      READ(1,*)DIAPH2
      READ(1,*)CFLCOE
      READ(1,*)IBCLEF
      READ(1,*)IBCRIG
      READ(1,*)NFREQU
      READ(1,*)NTMAXI
      READ(1,*)PSCALE
      READ(1,*)INTFLX
*
      CLOSE(1)
*
C     Input data is echoed to screen
*
      WRITE(6,*)
      WRITE(6,*)'Input data echoed to screen'
      WRITE(6,*)
      WRITE(6,*)'DOMLEN = ',DOMLEN
      WRITE(6,*)'DIAPH1 = ',DIAPH1
      WRITE(6,*)'CELLS  = ',CELLS
      WRITE(6,*)'GAMMA  = ',GAMMA
      WRITE(6,*)'TIMEOU = ',TIMEOU
      WRITE(6,*)'DLINIT = ',DLINIT
      WRITE(6,*)'ULINIT = ',ULINIT
      WRITE(6,*)'PLINIT = ',PLINIT
      WRITE(6,*)'DMINIT = ',DMINIT
      WRITE(6,*)'UMINIT = ',UMINIT
      WRITE(6,*)'PMINIT = ',PMINIT
      WRITE(6,*)'DRINIT = ',DRINIT
      WRITE(6,*)'URINIT = ',URINIT
      WRITE(6,*)'PRINIT = ',PRINIT
      WRITE(6,*)'DIAPH2 = ',DIAPH2
      WRITE(6,*)'CFLCOE = ',CFLCOE
      WRITE(6,*)'IBCLEF = ',IBCLEF
      WRITE(6,*)'IBCRIG = ',IBCRIG
      WRITE(6,*)'NFREQU = ',NFREQU
      WRITE(6,*)'NTMAXI = ',NTMAXI
      WRITE(6,*)'PSCALE = ',PSCALE
      WRITE(6,*)'INTFLX = ',INTFLX
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INITIA(CELLS)
*
C     Purpose: to set initial conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
      REAL    DOMLEN, DIAPH1, DIAPH2, DT, DX, D, U, P, CS,
     &        DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT, DRINIT,
     &        URINIT, PRINIT, XPOS,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),P(-1:IDIM+2),CS(3,-1:IDIM+2)
*
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
*
C     Compute gamma related constants
*
      G1 = (GAMMA - 1.0)/(2.0*GAMMA)
      G2 = (GAMMA + 1.0)/(2.0*GAMMA)
      G3 = 2.0*GAMMA/(GAMMA - 1.0)
      G4 = 2.0/(GAMMA - 1.0)
      G5 = 2.0/(GAMMA + 1.0)
      G6 = (GAMMA - 1.0)/(GAMMA + 1.0)
      G7 = (GAMMA - 1.0)/2.0
      G8 = GAMMA - 1.0
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Set initial data in tube of length DOMLEN, which is divided
C     into 3 sections by diaphragms at positions DIAPH1 and DIAPH2
*
      DO 10 I = 1, CELLS
*
         XPOS = (REAL(I) - 0.5)*DX
*
         IF(XPOS.LE.DIAPH1)THEN
*
C           Set initial values in left section of domaim
*
            D(I) = DLINIT
            U(I) = ULINIT
            P(I) = PLINIT
         ENDIF
*
         IF(XPOS.GT.DIAPH1.AND.XPOS.LE.DIAPH2)THEN
*
C           Set initial values in middle section of domaim
*
            D(I) = DMINIT
            U(I) = UMINIT
            P(I) = PMINIT
         ENDIF

         IF(XPOS.GT.DIAPH2)THEN
*
C           Set initial values in right section of domaim
*
            D(I) = DRINIT
            U(I) = URINIT
            P(I) = PRINIT
         ENDIF
*
C        Compute conserved variables
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
         CS(3,I) = 0.5*CS(2,I)*U(I) + P(I)/G8
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(CELLS)
*
C     Purpose: to set boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, CELLS, IDIM
*
      REAL    D, U, P
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U, P
      COMMON /BOUNDA/ IBCLEF, IBCRIG
*
      IF(IBCLEF.EQ.0)THEN
*
C        Transmissive boundary conditions on the left
*
         D(0) =  D(1)
         U(0) =  U(1)
         P(0) =  P(1)
*
      ELSE
*
C        Reflective boundary conditions on the left
*
         D(0) =  D(1)
         U(0) = -U(1)
         P(0) =  P(1)
*
      ENDIF
*
      IF(IBCRIG.EQ.0)THEN
*
C        Transmissive boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) =  U(CELLS)
         P(CELLS + 1) =  P(CELLS)
*
      ELSE
*
C        Reflective boundary conditions on the right
*
         D(CELLS + 1) = D(CELLS)
         U(CELLS + 1) =-U(CELLS)
         P(CELLS + 1) = P(CELLS)
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, CELLS, N, TIME, TIMEOU)
*
C     Purpose: to apply the CFL condition to find a stable time
C              step size DT
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM, N
*
      REAL    C, CFLCOE, D, DT, DX, P, SMAX, SBEXTD, TIME,
     &        TIMEOU, U,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /MESHPA/ DT, DX
*
      SMAX = 0.0
*
C     Find maximum velocity SMAX present in data
*
      DO 10 I = 0, CELLS + 1
*
C        Compute speed of sound
*
         C(I)   = SQRT(GAMMA*P(I)/D(I))
*
         SBEXTD  = ABS(U(I)) + C(I)
         IF(SBEXTD.GT.SMAX)SMAX = SBEXTD
 10   CONTINUE
*
C     Compute time step DT, for early times reduce its size
*
      DT = CFLCOE*DX/SMAX
*
C     For early times DT is reduced to compensate for approximate
C     calculation of SMAX
*
      IF(N.LE.5)DT = 0.2*DT
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
C     Find current time
*
      TIME = TIME + DT
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE OUTPUT(CELLS, PSCALE)
*
C     Purpose: to output the solution at a specified time TIMEOU
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    D, DT, DX, ENERGI, P, PSCALE, U, XPOS,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),P(-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /MESHPA/ DT, DX
*
      OPEN(UNIT = 1, FILE = 'e1gods.out', STATUS = 'UNKNOWN')
*
      DO 10 I   = 1, CELLS
         XPOS   = (REAL(I) - 0.5)*DX
         ENERGI =  P(I)/D(I)/G8/PSCALE
         WRITE(1,20)XPOS, D(I), U(I), P(I)/PSCALE, ENERGI
 10   CONTINUE
*
      CLOSE(1)
*
 20   FORMAT(5(F14.6,2X))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE UPDATE(CELLS)
*
C     Purpose: to update the solution according to the conservative
C              formula and compute physical variables
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, K, CELLS, IDIM
*
      REAL    DT, DX, DTODX, D, U, P, CS, FI,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FI(3,-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FI
      COMMON /MESHPA/ DT, DX
*
      DTODX = DT/DX
*
      DO 10 I = 1, CELLS
*
         DO 20 K = 1, 3
            CS(K,I) = CS(K,I) + DTODX*(FI(K,I-1) - FI(K,I))
 20      CONTINUE
*
 10   CONTINUE
*
C     Compute physical variables
*
      DO 30 I = 1, CELLS
         D(I) = CS(1,I)
         U(I) = CS(2,I)/D(I)
         P(I) = G8*(CS(3,I) - 0.5*CS(2,I)*U(I))
 30   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RPGODU(CELLS, INTFLX)
*
C     Purpose: to compute an intercell Godunov flux using
C              four possible Riemann solvers:
*
C     INTFLX = 1: The exact Riemann solver
C     INTFLX = 2: The Two-Shock Riemann solver
C     INTFLX = 3: The adaptive Riemann solver
C     INTFLX = 4: PRVS Riemann solver
*
C     Theory is found in Ref. 1, Chaps. 4 and 9
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, INTFLX
*
      REAL     C, CL, CR, D, DL, DR, DSAM, ENERGS, FI, P, PL,
     &         PM, PR, PSAM, U, UL, UM, UR, USAM, XOVERT,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          FI(3,-1:IDIM+2)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /FLUXES/ FI
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
C     Solve Riemann problem (i,i+1) and store quantities in i
*
C     Set value x/t = 0 (along t-axis)
*
      XOVERT = 0.0
*
      DO 10 I = 0, CELLS
*
         DL = D(I)
         UL = U(I)
         PL = P(I)
         CL = C(I)
*
         DR = D(I+ 1)
         UR = U(I + 1)
         PR = P(I + 1)
         CR = C(I + 1)
*
C        Solver Riemann problem exactly for star values
C        of pressure PM and velocity UM
*
         IF(INTFLX.EQ.1)CALL EXACT(PM, UM)
         IF(INTFLX.EQ.2)CALL TWOSHO(PM, UM)
         IF(INTFLX.EQ.3)CALL ADAPTI(PM, UM)
         IF(INTFLX.EQ.4)CALL PVRSOL(PM, UM)
*
C        Sample solution of Riemann problem at x/t = 0
C        to find Godunov state (DSAM, PSAM, USAM)
*
         CALL SAMPLE(PM, UM, XOVERT, DSAM, PSAM, USAM)
*
C        Compute intercell flux at Godunov state
*
         FI(1,I) = DSAM*USAM
         FI(2,I) = DSAM*USAM*USAM + PSAM
         ENERGS  = 0.5*USAM*USAM*DSAM + PSAM/G8
         FI(3,I) = USAM*(ENERGS + PSAM)
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE PVRSOL(PM, UM)
*
C     Purpose: to compute PM and UM  in the Star Region using
C              the PVRS Riemann solver.
*
C              We use exact relations for density and EXACT
C              solution for sonic flow in sampling routine
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        CUP, PM, UM
*

      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
*
C     Compute PM and UM from PVRS Riemann solver
*
      CUP = 0.25*(DL + DR)*(CL + CR)
*
      PM  = 0.5*(PL + PR) + 0.5*(UL - UR)*CUP
*
C     Reset pressure if negative
*
      PM  = MAX(0.0, PM)
*
      UM  = 0.5*(UL + UR) + 0.5*(PL - PR)/CUP
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE TWOSHO(PM, UM)
*
C     Purpose: to compute PM and UM  in the Star Region using
C              the TWO-SHOCK Riemann solver.
*
C              We use exact relations for density and EXACT
C              solution for sonic flow in sampling routine
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        CUP, GEL, GER, PM, PPV, UM
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
*
C     Compute guess pressure from PVRS Riemann solver
*
      CUP  = 0.25*(DL + DR)*(CL + CR)
      PPV  = 0.5*(PL + PR) + 0.5*(UL - UR)*CUP
*
      PPV  = MAX(0.0, PPV)
*
C     Two-Shock Riemann solver with PVRS as estimate
*
      GEL = SQRT((G5/DL)/(G6*PL + PPV))
      GER = SQRT((G5/DR)/(G6*PR + PPV))
      PM  = (GEL*PL + GER*PR - (UR - UL))/(GEL + GER)
      UM  = 0.5*(UL + UR) + 0.5*(GER*(PM - PR) - GEL*(PM - PL))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE ADAPTI(PM, UM)
*
C     Purpose: to compute PM and UM  in the Star Region using
C              the adaptive Riemann solver: PVRS, TRRS, TSRS.
*
C              We use exact relations for density and EXACT
C              solution for sonic flow in sampling routine
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        CUP, GEL, GER, PM, PMAX, PMIN, PPV, PQ,
     &        PTL, PTR, QMAX, QUSER, UM
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
*
      QUSER = 2.0
*
C     Compute guess pressure from PVRS Riemann solver
*
      CUP  = 0.25*(DL + DR)*(CL + CR)
      PPV  = 0.5*(PL + PR) + 0.5*(UL - UR)*CUP
      PPV  = MAX(0.0, PPV)
      PMIN = MIN(PL,  PR)
      PMAX = MAX(PL,  PR)
      QMAX = PMAX/PMIN
*
      IF(QMAX.LE.QUSER.AND.(PMIN.LE.PPV.AND.PPV.LE.PMAX))THEN
*
C        Select PVRS Riemann solver
*
         PM = PPV
         PM = PPV
         UM = 0.5*(UL + UR) + 0.5*(PL - PR)/CUP
*
      ELSE
         IF(PPV.LT.PMIN)THEN
*
C           Select Two-Rarefaction Riemann solver
*
            PQ  = (PL/PR)**G1
            UM  = (PQ*UL/CL + UR/CR + G4*(PQ - 1.0))/(PQ/CL + 1.0/CR)
            PTL = 1.0 + G7*(UL - UM)/CL
            PTR = 1.0 + G7*(UM - UR)/CR
            PM  = 0.5*(PL*PTL**G3 + PR*PTR**G3)
*
         ELSE
*
C           Select Two-Shock Riemann solver with PVRS as estimate
*
            GEL = SQRT((G5/DL)/(G6*PL + PPV))
            GER = SQRT((G5/DR)/(G6*PR + PPV))
            PM  = (GEL*PL + GER*PR - (UR - UL))/(GEL + GER)
            UM  = 0.5*(UL + UR) + 0.5*(GER*(PM - PR) - GEL*(PM - PL))
*
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE EXACT(P, U)
*
      IMPLICIT NONE
*
C     Purpose: to compute PM and UM  in the Star Region using
C              the EXACT Riemann solver
*
C              We use exact relations for density and EXACT
C              solution for sonic flow in sampling routine
*
C     Declaration of variables
*
      INTEGER I, NRITER
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        CHANGE, FL, FLD, FR, FRD, P, POLD, PSTART, TOLPRE,
     &        U, UDIFF
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      DATA TOLPRE, NRITER/1.0E-05, 20/
*
C     Guessed value PSTART is computed
*
      CALL GUESSP(PSTART)
*
      POLD  = PSTART
      UDIFF = UR - UL
*
      DO 10 I = 1, NRITER
*
         CALL PREFUN(FL, FLD, POLD, DL, PL, CL)
         CALL PREFUN(FR, FRD, POLD, DR, PR, CR)
         P      = POLD - (FL + FR + UDIFF)/(FLD + FRD)
         CHANGE = 2.0*ABS((P - POLD)/(P + POLD))
         IF(CHANGE.LE.TOLPRE)GOTO 20
         IF(P.LT.0.0)P = TOLPRE
         POLD  = P
*
 10   CONTINUE
*
      WRITE(6,*)'Divergence in Newton-Raphson iteration'
*
 20   CONTINUE
*
C     Compute velocity in Star region
*
      U = 0.5*(UL + UR + FR - FL)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE GUESSP(PM)
C
C     Purpose: to provide a guessed value for pressure
C              in the Star Region
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        CUP, GEL, GER, PM, PMAX, PMIN, PPV, PQ,
     &        PTL, PTR, QMAX, QUSER, UM
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
*
      QUSER = 2.0
*
C     Compute guess pressure from PVRS Riemann solver
*
      CUP  = 0.25*(DL + DR)*(CL + CR)
      PPV  = 0.5*(PL + PR) + 0.5*(UL - UR)*CUP
      PPV  = MAX(0.0, PPV)
      PMIN = MIN(PL,  PR)
      PMAX = MAX(PL,  PR)
      QMAX = PMAX/PMIN
*
      IF(QMAX.LE.QUSER.AND.(PMIN.LE.PPV.AND.PPV.LE.PMAX))THEN
*
C        Select PVRS Riemann solver
*
         PM = PPV
      ELSE
         IF(PPV.LT.PMIN)THEN
*
C           Select Two-Rarefaction Riemann solver
*
            PQ  = (PL/PR)**G1
            UM  = (PQ*UL/CL + UR/CR + G4*(PQ - 1.0))/(PQ/CL + 1.0/CR)
            PTL = 1.0 + G7*(UL - UM)/CL
            PTR = 1.0 + G7*(UM - UR)/CR
            PM  = 0.5*(PL*PTL**G3 + PR*PTR**G3)
         ELSE
*
C           Select Two-Shock Riemann solver with PVRS as estimate
*
            GEL = SQRT((G5/DL)/(G6*PL + PPV))
            GER = SQRT((G5/DR)/(G6*PR + PPV))
            PM  = (GEL*PL + GER*PR - (UR - UL))/(GEL + GER)
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE PREFUN(F,FD,P,DK,PK,CK)
*
C     Purpose: to evaluate the pressure functions FL and FR
C              in exact Riemann solver and their derivatives
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    AK, BK, CK, DK, F, FD, P, PK, PRATIO, QRT,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      IF(P.LE.PK)THEN
*
C        Rarefaction wave
*
         PRATIO = P/PK
         F      = G4*CK*(PRATIO**G1 - 1.0)
         FD     = (1.0/(DK*CK))*PRATIO**(-G2)
      ELSE
*
C        Shock wave
*
         AK  = G5/DK
         BK  = G6*PK
         QRT = SQRT(AK/(BK + P))
         F   = (P - PK)*QRT
         FD  = (1.0 - 0.5*(P - PK)/(BK + P))*QRT
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMPLE(PM, UM, S, D, P, U)
*
C     Purpose: to sample the solution throughout the wave pattern
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        C, CML, CMR, D, P, PM, PML, PMR,  S,
     &        SHL, SHR, SL, SR, STL, STR, U, UM
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
*
      IF(S.LE.UM)THEN
*
C        Sampling point lies to the left of the contact discontinuity
*
         IF(PM.LE.PL)THEN
*
C           Left rarefaction
*
            SHL = UL - CL
*
            IF(S.LE.SHL)THEN
*
C              Sampled point is left data state
*
               D = DL
               U = UL
               P = PL
            ELSE
               CML = CL*(PM/PL)**G1
               STL = UM - CML
*
               IF(S.GT.STL)THEN
*
C                 Sampled point is Star Left state
*
                  D = DL*(PM/PL)**(1.0/GAMMA)
                  U = UM
                  P = PM
               ELSE
*
C                 Sampled point is inside left fan
*
                  U = G5*(CL + G7*UL + S)
                  C = G5*(CL + G7*(UL - S))
                  D = DL*(C/CL)**G4
                  P = PL*(C/CL)**G3
               ENDIF
            ENDIF
         ELSE
*
C           Left shock
*
            PML = PM/PL
            SL  = UL - CL*SQRT(G2*PML + G1)
*
            IF(S.LE.SL)THEN
*
C              Sampled point is left data state
*
               D = DL
               U = UL
               P = PL

            ELSE
*
C              Sampled point is Star Left state
*
               D = DL*(PML + G6)/(PML*G6 + 1.0)
               U = UM
               P = PM
            ENDIF
         ENDIF
      ELSE
*
C        Sampling point lies to the right of the contact discontinuity
*
         IF(PM.GT.PR)THEN
*
C           Right shock
*
            PMR = PM/PR
            SR  = UR + CR*SQRT(G2*PMR + G1)
*
            IF(S.GE.SR)THEN
*
C              Sampled point is right data state
*
               D = DR
               U = UR
               P = PR
            ELSE
*
C              Sampled point is Star Right state
*
               D = DR*(PMR + G6)/(PMR*G6 + 1.0)
               U = UM
               P = PM
            ENDIF
         ELSE
*
C           Right rarefaction
*
            SHR = UR + CR
*
            IF(S.GE.SHR)THEN
*
C              Sampled point is right data state
*
               D = DR
               U = UR
               P = PR
            ELSE
               CMR = CR*(PM/PR)**G1
               STR = UM + CMR
*
               IF(S.LE.STR)THEN
*
C                 Sampled point is Star Right state
*
                  D = DR*(PM/PR)**(1.0/GAMMA)
                  U = UM
                  P = PM
               ELSE
*
C                 Sampled point is inside left fan
*
                  U = G5*(-CR + G7*UR + S)
                  C = G5*(CR - G7*(UR - S))
                  D = DR*(C/CR)**G4
                  P = PR*(C/CR)**G3
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*

