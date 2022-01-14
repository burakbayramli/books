*
*----------------------------------------------------------------------*
*                                                                      *
C     First-Order Flux Vector Splitting schemes for the                *
C     time-dependent one dimensional Euler equations                   *
*                                                                      *
C     Name of program: HE-E1FVSS                                       *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas by three first-order         *
C              Flux Vector Splitting  methods, namely:                 *
*                                                                      *
C              The  van Leer scheme                                    *
C              The  Steger-Warming  scheme                             *
C              The  Liou-Steffen  scheme                               *
*                                                                      *
C     Input  file: e1fvss.ini (initial data)                           *
C     Output file: e1fvss.out (numerical results)                      *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Ref. 1, Chaps. 6 and 8, and in original       *
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
      INTEGER FLUX, CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, PSCALE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, FLUX, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1fvss.ini"
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
         IF(FLUX.EQ.1)CALL STEWAR(CELLS)
         IF(FLUX.EQ.2)CALL VANLEE(CELLS)
         IF(FLUX.EQ.3)CALL LIOSTE(CELLS)
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
C           Solution is written to "e1fvss.out' at specified time
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
C     INTFLX    : Choice of intecell flux
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
     &        GAMMA, G7, G8
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /DRIVER/ CFLCOE, INTFLX, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G7, G8
*
      OPEN(UNIT = 1, FILE = 'e1fvss.ini', STATUS = 'UNKNOWN')
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
*
      REAL    DOMLEN, DIAPH1, DIAPH2, DT, DX, D, U, P, CS,
     &        DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT, DRINIT,
     &        URINIT, PRINIT, XPOS,
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),P(-1:IDIM+2),CS(3,-1:IDIM+2)
*
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
*
C     Compute gamma related constants
*
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
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
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
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),P(-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /MESHPA/ DT, DX
*
      OPEN(UNIT = 1, FILE = 'e1fvss.out', STATUS = 'UNKNOWN')
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
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FI(3,-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
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
      SUBROUTINE STEWAR(CELLS)
*
C     Purpose: to compute an intercell flux FI(K, I) according
C              to the Steger-Warming method
C              Stability:
C              0 < CFL Coefficient < 1.0
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     C, CO, D, ENER, FI, FLEFT, FRIGH,
     &         HM, HP, MACH, P, S, SM, SP, U,
     &         GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),C(-1:IDIM+2),
     &          FI(3, -1:IDIM + 2), FRIGH(3), FLEFT(3), S(3),
     &          SP(3), SM(3)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /FLUXES/ FI
*
      DO 10 I = 0, CELLS + 1
*
C        Compute eigenvalues on data
*
         S(1) = U(I) - C(I)
         S(2) = U(I)
         S(3) = U(I) + C(I)
*
         IF(S(3).LE.0.0)THEN
*
C           Supersonic flow to the left
*
            FRIGH(1) = 0.0
            FRIGH(2) = 0.0
            FRIGH(3) = 0.0
*
C           Compute the backward flux FLEFT component
*
            ENER     = 0.5*D(I)*U(I)*U(I) + P(I)/G8
*
            FLEFT(1) = D(I)*U(I)
            FLEFT(2) = D(I)*U(I)*U(I) + P(I)
            FLEFT(3) = U(I)*(ENER + P(I))
*
         ENDIF
*
         IF(S(1).GE.0.0)THEN
*
C           Supersonic flow to the right
*
            FLEFT(1) = 0.0
            FLEFT(2) = 0.0
            FLEFT(3) = 0.0
*
C           Compute the forward flux FRIGH component
*
            ENER     = 0.5*D(I)*U(I)*U(I) + P(I)/G8
*
            FRIGH(1) = D(I)*U(I)
            FRIGH(2) = D(I)*U(I)*U(I) + P(I)
            FRIGH(3) = U(I)*(ENER + P(I))
*
         ENDIF
*
         MACH = U(I)/C(I)
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
C           Define Steger-Warming splitting of eigenvalues
*
            DO 20 K  = 1, 3
               SP(K) = 0.5*(S(K) + ABS(S(K)))
               SM(K) = 0.5*(S(K) - ABS(S(K)))
 20         CONTINUE
*
            HM = 0.5*U(I)*U(I) + C(I)*C(I)/G8 - U(I)*C(I)
            HP = 0.5*U(I)*U(I) + C(I)*C(I)/G8 + U(I)* C(I)
            CO  = 0.5*D(I)/GAMMA
*
C           Compute forward flux component FRIGH
*
            FRIGH(1) = CO*(SP(1) + 2.0*G8*SP(2) + SP(3))
            FRIGH(2) = CO*(S(1)*SP(1) + 2.0*G8*S(2)*SP(2) + S(3)*SP(3))
            FRIGH(3) = CO*(HM*SP(1) + G8*S(2)*S(2)*SP(2) + HP*SP(3))
*
C           Compute backward flux component FLEFT
*
            FLEFT(1) = CO*(SM(1) + 2.0*G8*SM(2) + SM(3))
            FLEFT(2) = CO*(S(1)*SM(1) + 2.0*G8*S(2)*SM(2) + S(3)*SM(3))
            FLEFT(3) = CO*(HM*SM(1) + G8*S(2)*S(2)*SM(2) + HP*SM(3))
*
         ENDIF
*
C        Compute intercell fluxes
*
         DO 30 K = 1, 3
*
            IF(I.GE.0.AND.I.LE.CELLS)THEN
               FI(K, I) = FRIGH(K)
            ENDIF
*
            IF(I.GE.1)THEN
               FI(K, I - 1) = FI(K, I - 1) + FLEFT(K)
            ENDIF
*
 30      CONTINUE
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VANLEE(CELLS)
*
C     Purpose: to compute an intercell flux FI(K, I) according
C              to the van Leer method
C              Stability:
C              0 < CFL Coefficient < 0.6
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     C, COE1, COE2, D, ENER, FI, FLEFT, FLEFT2, FMLEFT,
     &         FMRIGH, FRIGH2, FRIGH, MACH, P, U,
     &         GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),C(-1:IDIM+2),
     &          FI(3, -1:IDIM + 2), FRIGH(3), FLEFT(3)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /FLUXES/ FI
*
      DO 10 I = 0, CELLS + 1
*
         IF((U(I) + C(I)).LE.0.0)THEN
*
C           Supersonic flow to the left
*
            FRIGH(1) = 0.0
            FRIGH(2) = 0.0
            FRIGH(3) = 0.0
*
C           Compute the backward flux FLEFT component
*
            ENER     = 0.5*D(I)*U(I)*U(I) + P(I)/G8
*
            FLEFT(1) = D(I)*U(I)
            FLEFT(2) = D(I)*U(I)*U(I) + P(I)
            FLEFT(3) = U(I)*(ENER + P(I))
*
         ENDIF
*
         IF((U(I) - C(I)).GE.0.0)THEN
*
C           Supersonic flow to the right
*
            FLEFT(1) = 0.0
            FLEFT(2) = 0.0
            FLEFT(3) = 0.0
*
C           Compute the forward flux FRIGH component
*
            ENER     = 0.5*D(I)*U(I)*U(I) + P(I)/G8
*
            FRIGH(1) = D(I)*U(I)
            FRIGH(2) = D(I)*U(I)*U(I) + P(I)
            FRIGH(3) = U(I)*(ENER + P(I))
*
         ENDIF
*
         MACH = U(I)/C(I)
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
            FMRIGH = 0.25*D(I)*C(I)*(MACH + 1.0)**2
            FMLEFT = 0.25*D(I)*C(I)*(MACH - 1.0)**2
            FRIGH2 = G7*MACH + 1.0
            FLEFT2 = G7*MACH - 1.0
*
            COE1   = 2.0*C(I)/GAMMA
            COE2   = 2.0*C(I)*C(I)/(GAMMA*GAMMA - 1.0)
*
C           Compute forward flux component FRIGH
*
            FRIGH(1) = FMRIGH
            FRIGH(2) = FMRIGH*COE1*FRIGH2
            FRIGH(3) = FMRIGH*COE2*FRIGH2*FRIGH2
*
C           Compute backward flux component FLEFT
*
            FLEFT(1) = -FMLEFT
            FLEFT(2) = -FMLEFT*COE1*FLEFT2
            FLEFT(3) = -FMLEFT*COE2*FLEFT2*FLEFT2
*
         ENDIF
*
C        Compute intercell fluxes
*
         DO 20 K = 1, 3
*
            IF(I.GE.0.AND.I.LE.CELLS)THEN
               FI(K, I) = FRIGH(K)
            ENDIF
*
            IF(I.GE.1)THEN
               FI(K, I - 1) = FI(K, I - 1) + FLEFT(K)
            ENDIF
*
 20      CONTINUE
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE LIOSTE(CELLS)
*
C     Purpose: to compute an intercell flux FI(K, I) according
C              to the Liou-Steffen method
C              Stability:
C              0 < CFL Coefficient < 1.0
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM
*
      REAL     C, D, FI, HE, MACH, MACHI, MACHL,
     &         MACHR,  P, PRESI, PRESL, PRESR, U,
     &         GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),C(-1:IDIM+2),
     &          FI(3, -1:IDIM + 2), MACHI(-1:IDIM+2), PRESI(-1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /FLUXES/ FI
*
      DO 10 I = 0, CELLS + 1
*
C        Compute local Mach number
*
         MACH = U(I)/C(I)
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
C           Compute Mach number components
*
            MACHR = +0.25*(MACH + 1.0)**2
            MACHL = -0.25*(MACH - 1.0)**2
*
C           Compute pressure components
*
            PRESR = 0.5*P(I)*(1.0 + MACH)
            PRESL = 0.5*P(I)*(1.0 - MACH)
*
         ELSE
*
C           Supersonic flow
*
C           Compute Mach number components
*
            MACHR = 0.5*(MACH + ABS(MACH))
            MACHL = 0.5*(MACH - ABS(MACH))
*
C           Compute pressure components
*
            PRESR = 0.5*P(I)*(MACH + ABS(MACH))/MACH
            PRESL = 0.5*P(I)*(MACH - ABS(MACH))/MACH
*
         ENDIF
*
C        Compute interface Mach number MACHI(I) and pressure PRESI(I)
*
         IF(I.GE.0.AND.I.LE.CELLS)THEN
            MACHI(I) = MACHR
            PRESI(I) = PRESR
         ENDIF
*
         IF(I.GE.1)THEN
            MACHI(I - 1) = MACHI(I - 1) + MACHL
            PRESI(I - 1) = PRESI(I - 1) + PRESL
         ENDIF
*
 10   CONTINUE
*
C     Compute intercell fluxes
*
      DO 20 I = 0, CELLS
*
         IF(MACHI(I).GE.0.0)THEN
*
C           Compute enthalpy HE
*
            HE       = 0.5*U(I)*U(I) + C(I)*C(I)/G8
*
            FI(1, I) = MACHI(I)*D(I)*C(I)
            FI(2, I) = MACHI(I)*D(I)*C(I)*U(I) + PRESI(I)
            FI(3, I) = MACHI(I)*D(I)*C(I)*HE
         ELSE
*
C           Compute enthalpy HE
*
            HE       = 0.5*U(I + 1)*U(I + 1)+C(I + 1)*C(I + 1)/G8
*
            FI(1, I) = MACHI(I)*D(I + 1)*C(I + 1)
            FI(2, I) = MACHI(I)*D(I + 1)*C(I + 1)*U(I + 1) + PRESI(I)
            FI(3, I) = MACHI(I)*D(I + 1)*C(I + 1)*HE
         ENDIF
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*

