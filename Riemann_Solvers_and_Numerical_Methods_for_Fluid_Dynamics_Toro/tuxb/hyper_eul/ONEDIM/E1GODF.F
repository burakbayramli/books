*----------------------------------------------------------------------*
*                                                                      *
C     First-Order Godunov schemes for the one-dimensional              *
C                   Euler equations                                    *
*                                                                      *
C     Name of code: HE-E1GODF                                          *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas by Godunov methods with      *
C              several approximate Rieman solvers of the               *
C              approximate flux type, namely                           *
*                                                                      *
C              INTFLX = 1: The Roe Riemann solver                      *
C              INTFLX = 2: The HLLC Riemann solver                     *
C              INTFLX = 3: The HLL Riemann solver                      *
C              INTFLX = 4: The Rusanov "Riemann" solver                *
*                                                                      *
C     Input  file: e1godf.ini (initial data)                           *
C     Output file: e1godf.out (numerical results)                      *
*                                                                      *
C     Revision: 31st May 1999                                          *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Theory is found in Chaps. 4, 6, 9, 10 and 11 of Reference 1      *
C     and in original references therein                               *
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
      INTEGER INTFLX, CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, PSCALE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, INTFLX, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1godf.ini"
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
         IF(INTFLX.EQ.1)CALL ROE(CELLS)
         IF(INTFLX.EQ.2)CALL HLLC(CELLS)
         IF(INTFLX.EQ.3)CALL HLL(CELLS)
         IF(INTFLX.EQ.4)CALL RUSANOV(CELLS)
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
C           Solution is written to "e1godf.out' at specified time
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
*                                                                                                                                               *
*----------------------------------------------------------------------*
*
      SUBROUTINE READER
C
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
      INTEGER INTFLX , IBCLEF, IBCRIG, CELLS, NFREQU, NTMAXI
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
      OPEN(UNIT = 1, FILE = 'e1godf.ini', STATUS = 'UNKNOWN')
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
*                                                                                                                                               *
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
*
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
*                                                                                                                                               *
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
      ELSE
*
C        Reflective boundary conditions on the left
*
         D(0)  =  D(1)
         U(0)  = -U(1)
         P(0)  =  P(1)
*
      ENDIF
*
      IF(IBCRIG.EQ.0)THEN
*
C        Transmissive  boundary conditions on the right
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
*                                                                                                                                               *
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
*                                                                                                                                               *
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
      OPEN(UNIT = 1, FILE = 'e1godf.out', STATUS = 'UNKNOWN')
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
*                                                                                                                                               *
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
*                                                                                                                                               *
*----------------------------------------------------------------------*
*
      SUBROUTINE ROE(CELLS)
*
C     Purpose: to compute an intercell Godunov flux using
C              the ROE approximate Riemann solver with entropy
C              fix according to Harten and Hyman. See Chap. 11
C              of Ref. 1 and original references therein
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     AK, C, CFLM, CL, CM, CMM, CR, CS,
     &         D, DL, DM, DP, DR, DT, DU, DX, EL, ER, EVAL,
     &         FD, FI, HL, HM, HR, P, PL, PR, REVEC, RM,
     &         SIG, SML, SMR, SNEW, TOLSON, U, UL, UM, UMM, UR,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),P(-1:IDIM+2),C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2),FD(3,-1:IDIM+2),FI(3,-1:IDIM+2),
     &          REVEC(3)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /PRIMIT/ D, U, P
      COMMON /SOUNDS/ C
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FI
      COMMON /MESHPA/ DT, DX
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      DATA TOLSON /0.1/
*
C     Parameter TOLSON determines application of entropy fix.
C     For TOLSON non-positive, no entropy fix is applied.
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
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 20 I = 0, CELLS
*
         DL = D(I)
         UL = U(I)
         PL = P(I)
         CL = C(I)
         EL = CS(3, I)
         HL = (EL + PL)/DL
*
         DR = D(I + 1)
         UR = U(I + 1)
         PR = P(I + 1)
         CR = C(I + 1)
         ER = CS(3, I + 1)
         HR = (ER + PR)/DR
*
C        Compute Roe averages
*
         RM = SQRT(DR/DL)
*
         DM = RM*DL
         UM = (UL + RM*UR)/(1.0 + RM)
         HM = (HL + RM*HR)/(1.0 + RM)
         CM = SQRT(G8*(HM - 0.5*UM*UM))
*
C        Compute increments
*
         DU = UR - UL
         DP = PR - PL
*
C        Identify wave pattern
*
         IF(UM.GT.0.0)THEN
*
C           Contact wave goes to the right
*
            EVAL = UM - CM
            SNEW = EVAL
            AK   = (DP - DM*CM*DU)/(2.0*CM*CM)
*
C           Define Courant number CFLM
*
            CFLM = EVAL*DT/DX
*
C           Test for left sonic rarefaction
*
            IF(ABS(CFLM).LT.TOLSON)THEN
*
C              Small left wave speed is identified
*
C              Use Roe's Riemann solver to find particle speed UMM and
C              sound speed CMM in start left state
*
               SIG = 1.0
*
               CALL STARVALS(SIG, DL, UL, EL, AK, UM, CM, HM, UMM, CMM)
*
               SML = UL - CL
               SMR = UMM - CMM
*
               IF(SML.LT.0.0.AND.SMR.GT.0.0)THEN
*
C                 Left wave is a sonic rarefaction, speed is modified
*
                  SNEW = SML*(SMR - EVAL)/(SMR - SML)
*
               ENDIF
            ENDIF
*
C           Compute one-sided intercell flux from left side
*
            IF(SNEW.LT.0.0)THEN
*
C              Compute right eigenvectors
*
               REVEC(1) = 1.0
               REVEC(2) = UM - CM
               REVEC(3) = HM - UM*CM
*
C              Compute one-sided intercell flux
*
               DO 30 K = 1, 3
                  FI(K, I) = FD(K,I) + SNEW*AK*REVEC(K)
 30            CONTINUE

            ELSE
*
C              Compute one-sided intercell flux
*
               DO 40 K = 1, 3
                  FI(K,I) = FD(K,I)
 40            CONTINUE
*
            ENDIF
*
         ELSE
*
C           Contact wave goes to the right
*
            EVAL = UM + CM
            SNEW = EVAL
            AK   = (DP + DM*CM*DU)/(2.0*CM*CM)
*
C           Define Courant number CFLM
*
            CFLM = EVAL*DT/DX
*
C           Test for left sonic rarefaction
*
            IF(ABS(CFLM).LT.TOLSON)THEN
*
C              Small right wave speed is identified
*
C              Use Roe's Riemann solver to find particle speed UMM and
C              sound speed CMM in start right state
*
               SIG = -1.0
*
               CALL STARVALS(SIG, DR, UR, ER, AK, UM, CM, HM, UMM, CMM)
*
               SML = UMM + CMM
               SMR = UR  + CR
*
               IF(SML.LT.0.0.AND.SMR.GT.0.0)THEN
*
C                 Right wave is a sonic rarefaction, speed is modified
*
               SNEW = SMR*(EVAL - SML)/(SMR - SML)
*
            ENDIF
*
         ENDIF
*
C           Compute one-sided intercell flux from right side
*
            IF(SNEW.GT.0.0)THEN
*
C              Compute right eigenvectors
*
               REVEC(1) = 1.0
               REVEC(2) = UM + CM
               REVEC(3) = HM + UM*CM
*
C              Compute one-sided intercell flux
*
               DO 50 K = 1, 3
                  FI(K, I)=FD(K, I+1) - SNEW*AK*REVEC(K)
 50            CONTINUE
*
            ELSE
*
C              Compute one-sided intercell flux
*
               DO 60 K=1,3
                  FI(K, I) = FD(K, I + 1)
 60            CONTINUE
*
            ENDIF
*
         ENDIF
*
 20   CONTINUE
*
      END
*                                                                                                                                               *
*----------------------------------------------------------------------*
*
      SUBROUTINE STARVALS(SIG, DK, UK, EK, AK, UM, CM, HM, UMM, CMM)
*
C     Purpose: to compute particle velocity and sound speed in
C              appropriate Star state, according to Roe's Riemann
C              solver for states, in order to apply entropy fix
C              of Harten and Hyman. See Chap. 11, Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL      AK, CM, CMM, DK, DMK, EK, HM, PM, SIG, UK, UM, UMM,
     &          GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      DMK = DK + SIG*AK
      UMM = (DK*UK  + SIG*AK*(UM - SIG*CM))/DMK
      PM  = G8*(EK + SIG*AK*(HM - SIG*UM*CM) - 0.5*DMK*UMM*UMM)
      CMM = SQRT(GAMMA*PM/DMK)
*
      END
*                                                                                                                                               *
*----------------------------------------------------------------------*
*
      SUBROUTINE HLLC(CELLS)
C
C     Purpose: to compute an intercell Godunov flux using
C              the HLLC approximate Riemann solver. See Chap. 10
C              Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     C, CL, CR, CS, CSL, CSR, D, DL, DR, ENEL, ENER,
     &         FD, FI, P, PL, PR, SL, SM, SR, U, UL, UR,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FD(3,-1:IDIM+2), FI(3,-1:IDIM+2),
     &          CSL(3), CSR(3)
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
C     Solve Riemann problem (i,i+1) and store quantities in I
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
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solvers
*
         CALL ESTIME(SL, SM, SR)
*
         IF(SL.GE.0.0)THEN
*
C           Right-going supersonic flow
*
            DO 30 K = 1, 3
               FI(K, I) = FD(K, I)
 30         CONTINUE
*
         ENDIF
*
         IF(SL.LE.0.0.AND.SR.GE.0.0)THEN
*
C           Subsonic flow
*
            IF(SM.GE.0.0)THEN
*
C              Subsonic flow to the right
*
               ENEL   = CS(3, I)/DL
               ENEL   = ENEL + (SM - UL)*(SM + PL/(DL*(SL - UL)))
               CSL(1) = DL*(SL - UL)/(SL - SM)
               CSL(2) = CSL(1)*SM
               CSL(3) = CSL(1)*ENEL
*
               DO 40 K = 1, 3
                  FI(K, I) = FD(K, I) + SL*(CSL(K) - CS(K, I))
 40            CONTINUE
*
            ELSE
*
C              Subsonic flow to the left
*
               ENER   = CS(3,I+1)/DR
               ENER   = ENER + (SM - UR)*(SM + PR/(DR*(SR - UR)))
               CSR(1) = DR*(SR - UR)/(SR - SM)
               CSR(2) = CSR(1)*SM
               CSR(3) = CSR(1)*ENER
*
               DO 50 K = 1, 3
                  FI(K, I) = FD(K, I + 1) + SR*(CSR(K) - CS(K, I + 1))
 50            CONTINUE
            ENDIF
         ENDIF
*
         IF(SR.LE.0.0)THEN
*
C           Left-going supersonic flow
*
            DO 60 K = 1, 3
               FI(K, I) = FD(K, I + 1)
 60        CONTINUE
*
        ENDIF

 20   CONTINUE
*
      END
*                                                                                                                                               *
*----------------------------------------------------------------------*
*
      SUBROUTINE HLL(CELLS)
*
C     Purpose: to compute an intercell Godunov flux using
C              the HLL approximate Riemann solver. See Chap 10,
C              Ref. 1 and original references therein
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     C, CL, CR, CS, D, DL, DR, FD, FI, HLLFLUX,
     &         P, PL, PR, SL, SM, SR, U, UL, UR,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FD(3,-1:IDIM+2), FI(3,-1:IDIM+2)
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
C     Solve Riemann problem (i,i+1) and store quantities in I
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
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solvers
*
         CALL ESTIME(SL, SM, SR)
*
         IF(SL.GE.0.0)THEN
*
C           Right-going supersonic flow
*
            DO 30 K = 1, 3
               FI(K, I) = FD(K, I)
 30         CONTINUE
*
         ENDIF
*
         IF(SL.LE.0.0.AND.SR.GE.0.0)THEN
*
C           Subsonic flow
*
            DO 40 K = 1, 3
               HLLFLUX = SR*FD(K, I) - SL*FD(K, I + 1)
               HLLFLUX = HLLFLUX + SL*SR*(CS(K, I + 1) - CS(K, I))
               FI(K,I) = HLLFLUX/(SR - SL)
 40         CONTINUE
*
         ENDIF
*
         IF(SR.LE.0.0)THEN
*
C           Left-going supersonic flow
*
            DO 60 K = 1, 3
               FI(K, I) = FD(K, I + 1)
 60         CONTINUE
*
         ENDIF

 20   CONTINUE
*
      END
*                                                                                                                                               *
*----------------------------------------------------------------------*
*
      SUBROUTINE RUSANOV(CELLS)
C
C     Purpose: to compute an intercell Godunov flux using
C              the Rusanov approximate "Riemann solver".
C              See Chap. 10, Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     C, CL, CR, CS, D, DL, DR, FD, FI, RUSFLUX,
     &         P, PL, PR, SL, SM, SR, SPLUS, U, UL, UR,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), FD(3,-1:IDIM+2), FI(3,-1:IDIM+2)
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
C     Solve Riemann problem (i,i+1) and store quantities in I
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
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solvers
*
         CALL ESTIME(SL, SM, SR)
*
         IF(SL.GE.0.0)THEN
*
C           Right-going supersonic flow
*
            DO 30 K = 1, 3
               FI(K, I) = FD(K, I)
 30         CONTINUE
*
         ENDIF
*
         IF(SL.LE.0.0.AND.SR.GE.0.0)THEN
*
C           Subsonic flow
*
C           Compute single wave speed
*
            SPLUS = MAX(ABS(SL), ABS(SR))
*
C           Compute Rusanov flux
*
            DO 40 K = 1, 3
               RUSFLUX = 0.5*(FD(K, I) + FD(K, I + 1))
               FI(K,I) = RUSFLUX + 0.5*SPLUS*(CS(K, I) - CS(K, I + 1))
 40         CONTINUE
*
         ENDIF
*
         IF(SR.LE.0.0)THEN
*
C           Left-going supersonic flow
*
            DO 60 K = 1, 3
               FI(K, I) = FD(K, I + 1)
 60         CONTINUE
*
         ENDIF
*
 20   CONTINUE
*
      END
*                                                                                                                                                *
*----------------------------------------------------------------------*
*
      SUBROUTINE ESTIME(SL, SM, SR)
*
C     Purpose: to compute wave speed estimates for the HLLC Riemann
C              solver using and adaptive approximate-state Riemann
C              solver including the PVRS, TRRS and TSRS solvers.
C              See Chap. 9, Ref. 1
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
      PPV  = MAX(0.0, PPV)
      PMIN = MIN(PL,  PR)
      PMAX = MAX(PL,  PR)
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
*
      IF(PM.LE.PR)THEN
         SR = UR + CR
      ELSE
         SR = UR + CR*SQRT(1.0 + G2*(PM/PR - 1.0))
      ENDIF
*
      END
*                                                                                                                                               *
*----------------------------------------------------------------------*
*


