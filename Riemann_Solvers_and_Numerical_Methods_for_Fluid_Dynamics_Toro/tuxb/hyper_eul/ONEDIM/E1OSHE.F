*
*----------------------------------------------------------------------*
*                                                                      *
C     First-Order Godunov schemes for the one-dimensional              *
C                   Euler equations                                    *
*                                                                      *
C     Program name: HE-E1OSHE                                          *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas by Godunov methods with      *
C              the approximate Rieman solvers of Osher and Solomon     *
C              using two path orderings, namely the                    *
*                                                                      *
C     Original Osher or O-ordering, FLUX = 1, and                      *
C     Physical or P-ordering      , FLUX = 2                           *
*                                                                      *
C     Input  file: e1oshe.ini (input data)                             *
C     Output file: e1oshe.out (numerical results)                      *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Chapter 12 of Reference 1 and in original     *
C     references therein                                               *
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
C     Declaration of variables
*
      INTEGER FLUX, CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, PSCALE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, FLUX, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1oshe.ini"
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
C        Intercell numerical fluxes are computed. Three
C        choices are available
*
         IF(FLUX.EQ.1)CALL OSHERO(CELLS)
         IF(FLUX.EQ.2)CALL OSHERP(CELLS)
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
C           Solution is written to "e1oshe.out' at specified time
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
*
 20   FORMAT(I12,6X,2(F12.7, 4X))
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
C     INTFLX    : Choice of path ordering
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
      OPEN(UNIT = 1, FILE = 'e1oshe.ini', STATUS = 'UNKNOWN')
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
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) = -U(CELLS)
         P(CELLS + 1) =  P(CELLS)
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
      OPEN(UNIT = 1, FILE = 'e1oshe.out', STATUS = 'UNKNOWN')
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
 20    CONTINUE
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
      SUBROUTINE OSHERP(CELLS)
*
C     Purpose: to compute an intercell Godunov flux using
C              the OSHER approximate Riemann solver with
C              PHYSICAL or P-ordering of integration paths.
C              Details to be found in Chapt. 12 of Ref. 1
C              and original referenced therein
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL    C, CML, CMR, CS, D, DML, DMR, DSL, DSR,
     &        FD, FI, FML, FMR, FSL, FSR, P, PM, PSL,
     &        PSR, U, UM, USL, USR,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FD(3,-1:IDIM+2), FI(3,-1:IDIM+2),
     &          FSL(3), FML(3), FMR(3), FSR(3)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FI
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
C     Compute fluxes on data and conserved variables
C     in fictitious cells
*
      DO 10 I = 0, CELLS + 1
*
         IF(I.LT.1.OR.I.GT.CELLS)THEN
            CS(1,I) = D(I)
            CS(2,I) = D(I)*U(I)
            CS(3,I) = 0.5* D(I)*U(I)*U(I) + P(I)/G8
         ENDIF
*
         FD(1,I) = CS(2,I)
         FD(2,I) = CS(2,I)*U(I)   + P(I)
         FD(3,I) = U(I)*(CS(3,I)  + P(I))
*
 10   CONTINUE
*
C     Solve Riemann problem (i,i+1) and store quantities in i
*
      DO 20 I = 0, CELLS
*
         DL = D(I)
         UL = U(I)
         PL = P(I)
         CL = C(I)
*
         DR = D(I + 1)
         UR = U(I + 1)
         PR = P(I + 1)
         CR = C(I + 1)
*
C        Compute intersection points with P-ordering
C        using two-rarefaction approximation
*
         CALL INTERSP(PM, UM, DML, DMR, CML, CMR)
*
C        =============
C        Case A: Table 12.8, column 2
C        =============
*
         IF((UL-CL).GE.0.0.AND.(UR+CR).GE.0.0)THEN
*
C           Case A1
*
            IF(UM.GE.0.0.AND.(UM-CML).GE.0.0)THEN
*
               DO 30 K = 1, 3
                  FI(K, I) = FD(K, I)
 30            CONTINUE
*
            ENDIF
*
C           Case A2
*
            IF(UM.GE.0.0.AND.(UM-CML).LE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               CALL FLUXEVAL(FML, PM, UM, DML)
*
               DO 40 K = 1, 3
                  FI(K, I) = FD(K, I) - FSL(K) + FML(K)
 40            CONTINUE
*
            ENDIF
*
C           Case A3
*
            IF(UM.LE.0.0.AND.(UM+CMR).GE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               DO 50 K = 1, 3
                  FI(K, I) = FD(K, I) - FSL(K) + FMR(K)
 50            CONTINUE
*
            ENDIF
*
C           Case A4
*
            IF(UM.LE.0.0.AND.(UM+CMR).LE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 60 K = 1, 3
                  FI(K, I) = FD(K, I) - FSL(K) + FSR(K)
 60            CONTINUE
*
            ENDIF
*
         ENDIF
*
C        =============
C        Case B: Table 12.8, column 3
C        =============
*
         IF((UL-CL).GE.0.0.AND.(UR+CR).LE.0.0)THEN
*
C           Case B1
*
            IF(UM.GE.0.0.AND.(UM-CML).GE.0.0)THEN
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 70 K = 1, 3
                  FI(K, I) = FD(K, I) - FSR(K) + FD(K, I + 1)
 70            CONTINUE
*
            ENDIF
*
C           Case B2
*
            IF(UM.GE.0.0.AND.(UM-CML).LE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL,PSL, USL, DSL)
*
               CALL FLUXEVAL(FML, PM, UM, DML)
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 80 K = 1, 3
                  FI(K,I) = FD(K,I)-FSL(K)+FML(K)-FSR(K)+FD(K,I+1)
 80            CONTINUE
*
            ENDIF
*
C           Case B3
*
            IF(UM.LE.0.0.AND.(UM+CMR).GE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 90 K = 1, 3
                  FI(K,I) = FD(K,I)-FSL(K)+FMR(K)-FSR(K)+FD(K,I+1)
 90            CONTINUE
*
            ENDIF
*
C           Case B4
*
            IF(UM.LE.0.0.AND.(UM+CMR).LE.0.0)THEN
*
               CALL SONLEF(PSL,USL,DSL)
*
               CALL FLUXEVAL(FSL,PSL,USL,DSL)
*
               DO 100 K = 1, 3
                  FI(K, I) = FD(K, I) - FSL(K) + FD(K, I + 1)
 100           CONTINUE
*
            ENDIF
*
         ENDIF
*
C        =============
C        Case C: Table 12.8, column 4
C        =============
*
         IF((UL-CL).LE.0.0.AND.(UR+CR).GE.0.0)THEN
*
C           Case C1
*
            IF(UM.GE.0.0.AND.(UM-CML).GE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               DO 110 K = 1, 3
                  FI(K, I) = FSL(K)
 110           CONTINUE
*
            ENDIF
*
C           Case C2
*
            IF(UM.GE.0.0.AND.(UM-CML).LE.0.0)THEN
*
               CALL FLUXEVAL(FML, PM, UM, DML)
*
               DO 120 K = 1, 3
                  FI(K, I) = FML(K)
 120           CONTINUE
*
            ENDIF
*
C           Case C3
*
            IF(UM.LE.0.0.AND.(UM+CMR).GE.0.0)THEN
*
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               DO 130 K = 1, 3
                  FI(K, I) = FMR(K)
 130           CONTINUE
*
            ENDIF
*
C           Case C4
*
            IF(UM.LE.0.0.AND.(UM+CMR).LE.0.0)THEN
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 140 K = 1, 3
                  FI(K, I) = FSR(K)
 140           CONTINUE
*
            ENDIF
*
         ENDIF
*
C        =============
C        Case D: Table 12.8, column 5
C        =============
*
         IF((UL-CL).LE.0.0.AND.(UR+CR).LE.0.0)THEN
*
C           Case D1
*
            IF(UM.GE.0.0.AND.(UM-CML).GE.0.0)THEN
*
               CALL SONLEF(PSL, USL, DSL)
*
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 150 K = 1, 3
                  FI(K, I) = FSL(K) - FSR(K) + FD(K, I + 1)
 150           CONTINUE
*
            ENDIF
*
C           Case D2
*
            IF(UM.GE.0.0.AND.(UM-CML).LE.0.0)THEN
*
               CALL FLUXEVAL(FML, PM, UM, DML)
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 160 K = 1, 3
                  FI(K, I) = FML(K) - FSR(K) + FD(K, I + 1)
 160           CONTINUE
*
            ENDIF
*
C           Case D3
*
            IF(UM.LE.0.0.AND.(UM+CMR).GE.0.0)THEN
*
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               CALL SONRIG(PSR, USR, DSR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 170 K = 1, 3
                  FI(K, I) = FMR(K) - FSR(K) + FD(K, I + 1)
 170           CONTINUE
*
            ENDIF
*
C           Case D4
*
            IF(UM.LE.0.0.AND.(UM+CMR).LE.0.0)THEN
*
               DO 180 K = 1, 3
                  FI(K,I) = FD(K, I + 1)
 180           CONTINUE
*
            ENDIF
*
         ENDIF
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INTERSP(PM, UM, DML, DMR, CML, CMR)
*
C     Purpose: to compute intersection points for Osher solver with
C              PHYSICAL or P-ordering of integration paths.
C              Compute: PM, UM, DML, DMR, CML, CMR
C              according to two-rarefaction approximation
*
      IMPLICIT NONE
*
      REAL    CML, CMR, DML, DMR, PM, PQ, PTL, PTR, UM,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PQ  = (PL/PR)**G1
      UM  = (PQ*UL/CL + UR/CR + G4*(PQ - 1.0))/(PQ/CL + 1.0/CR)
      PTL = 1.0 + G7*(UL - UM)/CL
      PTR = 1.0 + G7*(UM - UR)/CR
      PM  = 0.5*(PL*PTL**G3 + PR*PTR**G3)
*
      DML = DL*(PM/PL)**(1.0/GAMMA)
      DMR = DR*(PM/PR)**(1.0/GAMMA)
*
C     Compute sound speeds
*
      CML = CL*(PM/PL)**G1
      CMR = CR*(PM/PR)**G1
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE FLUXEVAL(FX, P, U, D)
*
C     Purpose: to fluxes FX at values P, U, D
*
      IMPLICIT NONE
*
      REAL    D, FX, P, U,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      DIMENSION FX(3)
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      FX(1) = D*U
      FX(2) = D*U*U + P
      FX(3) = U*(0.5*D*U*U + P/G8 + P)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SONLEF(PSL, USL, DSL)
*
C     Purpose: to compute left SONIC state PSL, USL, DSL
*
      IMPLICIT NONE
*
      REAL    CSL, DSL, PSL, USL,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      USL = G6*UL + CL*G5
      CSL = USL
      DSL = DL*(CSL/CL)**G4
      PSL = PL*(DSL/DL)**GAMMA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SONRIG(PSR, USR, DSR)
*
C     Purpose: to compute right SONIC state PSR, USR, DSR
*
      IMPLICIT NONE
*
      REAL    CSR, DSR, PSR, USR,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      USR = G6*UR - CR*G5
      CSR = -USR
      DSR = DR*(CSR/CR)**G4
      PSR = PR*(DSR/DR)**GAMMA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE OSHERO(CELLS)
*
C     Purpose: to compute an intercell Godunov flux using
C              the OSHER approximate Riemann solver with
C              OSHER or O-ordering of integration paths.
C              Details to be found in Chapt. 12 of Ref. 1
C              and original referenced therein
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL    C, CML, CMR, CS, D, DML, DMR, DSL, DSR,
     &        FD, FI, FML, FMR, FSL, FSR, P, PM, PSL,
     &        PSR, U, UM, USL, USR,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FD(3,-1:IDIM+2), FI(3,-1:IDIM+2),
     &          FSL(3), FML(3), FMR(3), FSR(3)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FI
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
C     Compute fluxes on data and conserved variables
C     in fictitious cells
*
      DO 10 I = 0, CELLS + 1
*
         IF(I.LT.1.OR.I.GT.CELLS)THEN
            CS(1,I) = D(I)
            CS(2,I) = D(I)*U(I)
            CS(3,I) = 0.5* D(I)*U(I)*U(I) + P(I)/G8
         ENDIF
*
         FD(1,I) = CS(2,I)
         FD(2,I) = CS(2,I)*U(I)   + P(I)
         FD(3,I) = U(I)*(CS(3,I)  + P(I))
*
 10   CONTINUE
*
C     Solve Riemann problem (i,i+1) and store quantities in i
*
      DO 20 I = 0, CELLS
*
         DL = D(I)
         UL = U(I)
         PL = P(I)
         CL = C(I)
*
         DR = D(I + 1)
         UR = U(I + 1)
         PR = P(I + 1)
         CR = C(I + 1)
*
C        Compute intersection points with O-ordering
C        using two-rarefaction approximation, with O-ordering
*
         CALL INTERSO(PM, UM, DML, DMR, CML, CMR)
*
C        =============
C        Case A: Table 12.10, column 2
C        =============
*
         IF((UL+CL).GE.0.0.AND.(UR-CR).GE.0.0)THEN
*
C           Case A1
*
            IF((UM+CML).LE.0.0)THEN
*
               CALL SONLEO(PSL, USL, DSL)
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 30 K = 1, 3
                  FI(K, I) = FD(K, I) - FSL(K) + FSR(K)
 30            CONTINUE
*
            ENDIF
*
C           Case A2
*
            IF(UM.LE.0.0.AND.(UM+CML).GE.0.0)THEN
*
               CALL FLUXEVAL(FML, PM, UM, DML)
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 40 K = 1, 3
                  FI(K, I) = FD(K, I) - FML(K) + FSR(K)
 40            CONTINUE
*
            ENDIF
*
C           Case A3
*
            IF((UM-CMR).GE.0.0)THEN
*
               DO 50 K = 1, 3
                  FI(K, I) = FD(K, I)
 50            CONTINUE
*
            ENDIF
*
C           Case A4
*
            IF(UM.GE.0.0.AND.(UM-CMR).LE.0.0)THEN
*
               CALL FLUXEVAL(FMR, PM, UM, DMR)
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 60 K = 1, 3
                  FI(K, I) = FD(K, I) - FMR(K) + FSR(K)
 60            CONTINUE
*
            ENDIF
*
         ENDIF
*
C        =============
C        Case B: Table 12.10, column 3
C        =============
*
         IF((UL+CL).GE.0.0.AND.(UR-CR).LE.0.0)THEN
*
C           Case B1
*
            IF((UM+CML).LE.0.0)THEN
*
               CALL SONLEO(PSL,USL,DSL)
               CALL FLUXEVAL(FSL,PSL,USL,DSL)
*
               DO 70 K = 1, 3
                  FI(K, I) = FD(K, I) - FSL(K) + FD(K, I + 1)
 70            CONTINUE
*
            ENDIF
*
C           Case B2
*
            IF(UM.LE.0.0.AND.(UM+CML).GE.0.0)THEN
*
               CALL FLUXEVAL(FML, PM, UM, DML)
*
               DO 80 K = 1, 3
                  FI(K, I) = FD(K, I) - FML(K) + FD(K, I + 1)
 80            CONTINUE
*
            ENDIF
*
C           Case B3
*
            IF((UM-CMR).GE.0.0)THEN
*
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 90 K = 1, 3
                  FI(K, I) = FD(K, I) - FSR(K) + FD(K, I + 1)
 90            CONTINUE
*
            ENDIF
*
C           Case B4
*
            IF(UM.GE.0.0.AND.(UM-CMR).LE.0.0)THEN
*
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               DO 100 K = 1, 3
                  FI(K, I) = FD(K, I) - FMR(K) + FD(K, I + 1)
 100           CONTINUE
*
            ENDIF
*
         ENDIF
*
C        =============
C        Case C: Table 12.10, column 4
C        =============
*
         IF((UL+CL).LE.0.0.AND.(UR-CR).GE.0.0)THEN
*
C           Case C1
*
            IF((UM+CML).LE.0.0)THEN
*
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 110 K = 1, 3
                  FI(K, I) = FSR(K)
 110           CONTINUE
*
            ENDIF
*
C           Case C2
*
            IF(UM.LE.0.0.AND.(UM+CML).GE.0.0)THEN
*
               CALL FLUXEVAL(FML, PM, UM, DML)
               CALL SONLEO(PSL, USL, DSL)
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 120 K = 1, 3
                  FI(K, I) = FSL(K) - FML(K) + FSR(K)
 120           CONTINUE
*
            ENDIF
*
C           Case C3
*
            IF((UM-CMR).GE.0.0)THEN
*
               CALL SONLEO(PSL ,USL, DSL)
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               DO 130 K = 1, 3
                  FI(K, I) = FSL(K)
 130           CONTINUE
*
            ENDIF
*
C           Case C4
*
            IF(UM.GE.0.0.AND.(UM-CMR).LE.0.0)THEN
*
               CALL SONLEO(PSL, USL, DSL)
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 140 K = 1, 3
                  FI(K, I) = FSL(K) - FMR(K) + FSR(K)
 140           CONTINUE
*
            ENDIF
*
         ENDIF
*
C        =============
C        Case D: Table 12.10, column 5
C        =============
*
         IF((UL+CL).LE.0.0.AND.(UR-CR).LE.0.0)THEN
*
C           Case D1
*
            IF((UM+CML).LE.0.0)THEN
*
               DO 150 K = 1, 3
                  FI(K, I) = FD(K, I + 1)
 150           CONTINUE
*
            ENDIF
*
C           Case D2
*
            IF(UM.LE.0.0.AND.(UM+CML).GE.0.0)THEN
*
               CALL FLUXEVAL(FML, PM, UM, DML)
               CALL SONLEO(PSL, USL, DSL)
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
*
               DO 160 K = 1, 3
                  FI(K, I) = FSL(K) - FML(K) + FD(K, I + 1)
 160           CONTINUE
*
            ENDIF
*
C           Case D3
*
            IF((UM-CMR).GE.0.0)THEN
*
               CALL SONLEO(PSL, USL, DSL)
               CALL FLUXEVAL(FSL, PSL, USL, DSL)
               CALL SONRIO(PSR, USR, DSR)
               CALL FLUXEVAL(FSR, PSR, USR, DSR)
*
               DO 170 K = 1, 3
                  FI(K, I) = FSL(K) - FSR(K) + FD(K, I + 1)
 170           CONTINUE
*
            ENDIF
*
C           Case D4
*
            IF(UM.GE.0.0.AND.(UM-CMR).LE.0.0)THEN
*
               CALL SONLEO(PSL, USL, DSL)
               CALL FLUXEVAL(FSL,PSL, USL, DSL)
               CALL FLUXEVAL(FMR, PM, UM, DMR)
*
               DO 180 K = 1, 3
                  FI(K, I) = FSL(K) - FMR(K) + FD(K, I + 1)
 180           CONTINUE
*
            ENDIF
*
         ENDIF
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INTERSO(PM, UM, DML, DMR, CML, CMR)
*
C     Purpose: to compute intersection points for Osher's solver with
C              OSHER or O-ordering of integration paths.
C              Compute: PM, UM, DML, DMR, CML, CMR
C              according to two-rarefaction approximation, but
C              with O-ordering
*
      IMPLICIT NONE
*
      REAL    CML, CMR, DML, DMR, PLG, PM, PRG, UDE,
     &        UM, UNU,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PLG = PL**G1
      PRG = PR**G1
      PM  = ((CL + CR + G7*(UR - UL))/(CL/PLG + CR/PRG))**G3
      UNU = (PLG/PRG)*UL/CL+UR/CR-(PLG/PRG - 1.0)*G4
      UDE = (PLG/PRG)/CL+1.0/CR
      UM  = UNU/UDE
      DML = DL*(PM/PL)**(1.0/GAMMA)
      DMR = DR*(PM/PR)**(1.0/GAMMA)
*
      CML = SQRT(GAMMA*PM/DML)
      CMR = SQRT(GAMMA*PM/DMR)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SONLEO(PSL, USL, DSL)
*
C     Purpose: to compute left SONIC state PSL, USL, DSL
*
      IMPLICIT NONE
*
      REAL    CSL, DSL, PSL, USL,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      USL = G6*UL - CL*G5
      CSL = -USL
      DSL = DL*(CSL/CL)**G4
      PSL = PL*(DSL/DL)**GAMMA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SONRIO(PSR, USR, DSR)
*
C     Purpose: to compute left SONIC state PSL, USL, DSL
*
      IMPLICIT NONE
*
      REAL    CSR, DSR, PSR, USR,
     &        DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      USR = G6*UR + CR*G5
      CSR = USR
      DSR = DR*(CSR/CR)**G4
      PSR = PR*(DSR/DR)**GAMMA
*
      END
*
*----------------------------------------------------------------------*
*

