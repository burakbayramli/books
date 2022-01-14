*
*----------------------------------------------------------------------*
*                                                                      *
C     Weigthed Average Flux (WAF) scheme for the time-dependent        *
C     one dimensional Euler equations with cylindrical or              *
C     spherical symmetry (geometric source terms)                      *
*                                                                      *
C     Program name: HE-E1WAFE                                          *
C                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas with geometric source        *
C              terms due to cylindrical or spherical symmetry. The     *
C              Weighted Average Flux (WAF) method in conjunction       *
C              with the HLLC approximate Riemann solver is used        *
C              to solve the homogeneous problem (no source terms),     *
C              with a choice of six limiter functions. The source      *
C              terms are treated via time-operator splitting with      *
C              the Ordinary Differential Equations solved by the       *
C              first-order Euler method. The particular choice         *
C              of soure term is determined by the value of ALPHA       *
*                                                                      *
C     Input  file: e1wafe.ini (initial data)                           *
C     Output file: e1wafe.out (numerical results)                      *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Ref. 1, Chaps. 1, 14 and 16, and in           *
C     original references therein                                      *
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
      INTEGER LIMITE, CELLS, N, NFREQU, NTMAXI
*
      REAL    ALPHA, CFLCOE, PSCALE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, LIMITE, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE, ALPHA
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1wafe.ini"
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
C        Intercell numerical fluxes are computed
*
         CALL WAFFLU(LIMITE, CELLS)
*
C        Homogeneous solution is updated according to conservative
C        formula
*
         CALL UPDATE(CELLS)
*
C        Homogeneous solution is corrected by accounting
C        for geometric source terms
*
         CALL SOURCE(CELLS, ALPHA)
*
         IF(MOD(N,NFREQU).EQ.0)WRITE(6,20)N, TIME, TIMEOU
*
C        Check output time
*
         TIMDIF = ABS(TIME - TIMEOU)
*
         IF(TIMDIF.LE.TIMTOL)THEN
*
C           Solution is written to "e1wafe.out' at specified time
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
C     LIMITE    : Choice of flux limiter
C     ALPHA     : Choice of geometric source term
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, CELLS, LIMITE, NFREQU, NTMAXI
*
      REAL    ALPHA, CFLCOE, DOMLEN, DIAPH1, DIAPH2, PSCALE, TIMEOU,
     &        DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT, DRINIT,
     &        URINIT, PRINIT,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /DRIVER/ CFLCOE, LIMITE, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE, ALPHA
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      OPEN(UNIT = 1, FILE = 'e1wafe.ini', STATUS = 'UNKNOWN')
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
      READ(1,*)LIMITE
      READ(1,*)ALPHA
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
      WRITE(6,*)'LIMITE = ',LIMITE
      WRITE(6,*)'ALPHA  = ',ALPHA
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
         D(0)  =  D(1)
         U(0)  =  U(1)
         P(0)  =  P(1)
*
         D(-1) =  D(2)
         U(-1) =  U(2)
         P(-1) =  P(2)
*
      ELSE
*
C        Reflective boundary conditions on the left
*
         D(0)  =  D(1)
         U(0)  = -U(1)
         P(0)  =  P(1)
*
         D(-1) =  D(2)
         U(-1) = -U(2)
         P(-1) =  P(2)
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
         D(CELLS + 2) =  D(CELLS-1)
         U(CELLS + 2) =  U(CELLS-1)
         P(CELLS + 2) =  P(CELLS-1)
*
      ELSE
*
C        Reflective boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) = -U(CELLS)
         P(CELLS + 1) = P(CELLS)
*
         D(CELLS + 2) =  D(CELLS-1)
         U(CELLS + 2) = -U(CELLS-1)
         P(CELLS + 2) =  P(CELLS-1)
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
      DO 10 I = -1, CELLS + 2
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
      OPEN(UNIT = 1, FILE = 'e1wafe.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE WAFFLU(LIMITE, CELLS)
*
C     Purpose: to compute the EAF flux with the HLLC Riemann solver
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IUPW, K, LIMITE, CELLS, IDIM
      REAL    D, U, P, C, CS, DL, UL, PL, CL, DR, UR, PR, CR,
     &        CN, CSL, CSR, DT, DX, DTODX, DLOC, DUPW,
     &        ENEL, ENER, FD, FI, FSL, FSR, RATIO,
     &        SL, SM, SR, TOLLIM, WAFLIM, WJ, WS,
     &        WL, WSL, WSR, WR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FD(3,-1:IDIM+2), FI(3,-1:IDIM+2),
     &          FSL(3,-1:IDIM+2),FSR(3,-1:IDIM+2), WS(3,-1:IDIM+2),
     &          WJ(3,-1:IDIM+2), CN(3), CSL(3), CSR(3), WAFLIM(3)
*
      COMMON /MESHPA/ DT, DX
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FI
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      DATA TOLLIM /1.0E-06/
*
C     Compute fluxes on data and conserved variables
C     in fictitious cells
*
      DO 10 I = -1, CELLS + 2
*
         FD(1,I) = D(I)*U(I)
         FD(2,I) = D(I)*U(I)*U(I) + P(I)
         FD(3,I) = U(I)*(0.5* D(I)*U(I)*U(I) + P(I)/G8  + P(I))
*
         IF(I.LT.1.OR.I.GT.CELLS)THEN
            CS(1,I) = D(I)
            CS(2,I) = D(I)*U(I)
            CS(3,I) = 0.5* D(I)*U(I)*U(I) + P(I)/G8
         ENDIF
*
 10   CONTINUE
*
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 20 I = -1, CELLS + 1
*
         DL = D(I)
         UL = U(I)
         PL = P(I)
         CL = C(I)
*
         DR = D(I+1)
         UR = U(I+1)
         PR = P(I+1)
         CR = C(I+1)
*
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solvers
*
         CALL ESTIME(SL, SM, SR)
*
C        Compute star states U*L and U*R
*
         ENEL = CS(3,I)/DL   + (SM - UL)*(SM + PL/(DL*(SL - UL)))
         ENER = CS(3,I+1)/DR + (SM - UR)*(SM + PR/(DR*(SR - UR)))
*
         CSL(1) = DL*(SL - UL)/(SL - SM)
         CSL(2) = CSL(1)*SM
         CSL(3) = CSL(1)*ENEL
*
         CSR(1) = DR*(SR - UR)/(SR - SM)
         CSR(2) = CSR(1)*SM
         CSR(3) = CSR(1)*ENER
*
C        Compute and store star fluxes F*L and F*R
*
         DO 30 K = 1, 3
            FSL(K,I) = FD(K,I)   + SL*(CSL(K) - CS(K,I))
            FSR(K,I) = FD(K,I+1) + SR*(CSR(K) - CS(K,I+1))
 30      CONTINUE
*
C        Store wave speeds for TVD condition
*
         WS(1,I) = SL
         WS(2,I) = SM
         WS(3,I) = SR
*
C        Store wave density jumps for TVD condition
*
         WJ(1,I) = CSL(1)    - CS(1,I)
         WJ(2,I) = CSR(1)    - CSL(1)
         WJ(3,I) = CS(1,I+1) - CSR(1)
*
 20   CONTINUE
*
C     Compute the WAF intercell flux
*
      DTODX = DT/DX
*
      DO 40 I = 0, CELLS
*
C        Apply TVD condition
*
         DO 50 K = 1,3
*
C           Compute Courant numbers for each wave
*
            CN(K) = WS(K,I)*DTODX
*
C           Identify upwind direction
*
            IF(CN(K).GE.0.0)THEN
*
C              Wave k is positive and upwind direction is on the Left
*
               IUPW = -1
            ELSE
*
C              Wave k is negative and upwind direction is on the right
*
               IUPW =  1
            ENDIF
*
C           Compute wave jumps
*
            DLOC = WJ(K,I)
            DUPW = WJ(K,I+IUPW)
*
C           Modify small jumps preserving sign
*
            IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0,DUPW)
            IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0,DLOC)
*
C           Compute RATIO of upwind to local changes
*
            RATIO = DUPW/DLOC
*
C           Select limiter function WAFLIM
*
C           LIMITE = 1, Godunov's Method
C           LIMITE = 2, Upwind Second Order Method (non-monotone)
C           LIMITE = 3, Upwind TVD, with SUPERBEE type limiter
C           LIMITE = 4, Upwind TVD, with VAN LEER type limiter
C           LIMITE = 5, Upwind TVD, with VAN ALBADA type limiter
C           LIMITE = 6, Upwind TVD, with MINMOD type limiter
*
            IF(LIMITE.EQ.1)WAFLIM(K) = 1.0
            IF(LIMITE.EQ.2)WAFLIM(K) = ABS(CN(K))
            IF(LIMITE.EQ.3)CALL SUPERA(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE.EQ.4)CALL VANLEE(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE.EQ.5)CALL VANALB(RATIO, ABS(CN(K)), WAFLIM(K))
            IF(LIMITE.EQ.6)CALL MINAAA(RATIO, ABS(CN(K)), WAFLIM(K))
*
            WAFLIM(K) = WAFLIM(K)*SIGN(1.0, CN(K))
*
 50      CONTINUE
*
C        Compute weights for WAF flux evaluation
*
         WL  = 0.5*(1.0       + WAFLIM(1))
         WSL = 0.5*(WAFLIM(2) - WAFLIM(1))
         WSR = 0.5*(WAFLIM(3) - WAFLIM(2))
         WR  = 0.5*(1.0       - WAFLIM(3))
*
         DO 60 K = 1, 3
            FI(K,I) = WL*FD(K,I)+WSL*FSL(K,I)+WSR*FSR(K,I)+WR*FD(K,I+1)
 60      CONTINUE
*
 40   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SOURCE(CELLS, ALPHA)
*
C     Purpose: to account for geometric source terms arising in
C              cylindrically and spherically symetric flow. For
C              details see Ref. 1, Sect. 1.6, Chapt. 1. The value
C              of ALPHA determines the nature of the source term:
*
C              ALPHA = 0.0 (no source, plane 1D flow)
C              ALPHA = 1.0 (cylindrically symmetric flow)
C              ALPHA = 2.0 (spherically symetric flow)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, K, CELLS, IDIM
*
      REAL    DT, DX, D, U, P, CS, ALPHA, RADIAL, SRCE,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), SRCE(3)
*
      COMMON /PRIMIT/ D, U, P
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      DO 10 I = 1, CELLS
*
C        Evaluate radial distance from origin
*
         RADIAL = (REAL(I) - 0.5)*DX
*
C        Evaluate source terms
*
         SRCE(1) = CS(2,I)
         SRCE(2) = SRCE(1)*U(I)
         SRCE(3) = U(I)*(CS(3,I) + P(I))
*
C        Correction of conservative variables due to source term
*
         DO 20 K = 1, 3
            CS(K,I) = CS(K,I) - (ALPHA*DT/RADIAL)*SRCE(K)
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
      SUBROUTINE ESTIME(SL, SM, SR)
*
C     Purpose: to compute wave speed estimates for the HLLC Riemann
C              solver using and adaptive approximate-state Riemann
C              solver including the PVRS, TRRS and TSRS solvers
C              See Section 9.5, Chapter 9 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8 ,
     &        CUP, GEL, GER, PM, PMAX, PMIN, PPV, PQ,
     &        PTL, PTR, QMAX, QUSER, SL, SM, SR, UM
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
      PPV  = AMAX1(0.0, PPV)
      PMIN = AMIN1(PL,  PR)
      PMAX = AMAX1(PL,  PR)
      QMAX = PMAX/PMIN
*
      IF(QMAX.LE.QUSER.AND.(PMIN.LE.PPV.AND.PPV.LE.PMAX))THEN
*
C        Select PRVS Riemann solver
*
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
C           Use Two-Shock Riemann solver with PVRS as estimate
*
            GEL = SQRT((G5/DL)/(G6*PL + PPV))
            GER = SQRT((G5/DR)/(G6*PR + PPV))
            PM  = (GEL*PL + GER*PR - (UR - UL))/(GEL + GER)
            UM  = 0.5*(UL + UR) + 0.5*(GER*(PM - PR) - GEL*(PM - PL))
         ENDIF
      ENDIF
*
C     Find speeds
*
      IF(PM.LE.PL)THEN
         SL = UL - CL
      ELSE
         SL = UL - CL*SQRT(1.0 + G2*(PM/PL - 1.0))
      ENDIF
*
      SM = UM

      IF(PM.LE.PR)THEN
         SR = UR + CR
      ELSE
         SR = UR + CR*SQRT(1.0 + G2*(PM/PR - 1.0))
      ENDIF
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
