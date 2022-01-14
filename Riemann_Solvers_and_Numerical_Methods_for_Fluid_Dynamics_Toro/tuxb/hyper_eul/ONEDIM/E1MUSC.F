*
*----------------------------------------------------------------------*
*                                                                      *
C     MUSCL-Hancock scheme for the one-dimensional Euler equations     *
*                                                                      *
C     Program name: HE-E1MUSC                                          *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas using the MUSCL-Hancock      *
C              method in conjunction with the HLL and HLLC             *
C              approximate Riemann solvers. Also included is the       *
C              Rusanov flux. Seven limiter functions are available.    *
C              The  Godunov first-order upwind  method is included     *
C              as a special case                                       *
*                                                                      *
C     Input  file: e1musc.ini (input data)                             *
C     Output file: e1musc.out (numerical results)                      *
*                                                                      *
C     Programer : E. F. Toro                                           *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
                                                                       *
C     Theory is found in Chaps. 10, 13 and 14 of Reference 1           *
                                                                       *
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
                                                                       *
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
      REAL    CFLCOE, PSCALE, TIME, TIMDIF, TIMOUT, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, LIMITE, CELLS, NFREQU, NTMAXI, TIMOUT,
     &                PSCALE
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1musc.ini"
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
      WRITE(6,*)'   Time step N        TIME           TIMOUT'
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
         CALL CFLCON(CFLCOE, CELLS, N, TIME, TIMOUT)
*
C        Intercell numerical fluxes are computed
*
         CALL MUSFLX(LIMITE, CELLS)
*
C        Solution is updated according to conservative formula
*
         CALL UPDATE(CELLS)
*
         IF(MOD(N,NFREQU).EQ.0)WRITE(6,20)N, TIME, TIMOUT
*
C        Check output time
*
         TIMDIF = ABS(TIME - TIMOUT)
*
         IF(TIMDIF.LE.TIMTOL)THEN
*
C           Solution is written to "e1musc.out' at specified time
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
C     INTFLX    : Choice of intercell flux
C     LIMITE    : Choice of slope limiter function
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, INTFLX, LIMITE, CELLS,
     &        NFREQU, NTMAXI
*
      REAL    CFLCOE, DOMLEN, DIAPH1, DIAPH2, PSCALE, TIMEOU,
     &        DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT, DRINIT,
     &        URINIT, PRINIT,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /DRIVER/ CFLCOE, LIMITE, CELLS, NFREQU, NTMAXI, TIMEOU,
     &                PSCALE
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /FXCHOI/ INTFLX
*
      OPEN(UNIT = 1, FILE = 'e1musc.ini', STATUS = 'UNKNOWN')
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
      READ(1,*)LIMITE
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
      WRITE(6,*)'LIMITE = ',LIMITE
*
      CLOSE(1)
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
         D(0) = D(1)
         U(0) = U(1)
         P(0) = P(1)
*
         D(-1) = D(2)
         U(-1) = U(2)
         P(-1) = P(2)
*
      ELSE
*
C        Reflective boundary conditions on the left
*
         D(0) =  D(1)
         U(0) = -U(1)
         P(0) =  P(1)
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
         D(CELLS + 1) = D(CELLS)
         U(CELLS + 1) = U(CELLS)
         P(CELLS + 1) = P(CELLS)
*
         D(CELLS + 2) = D(CELLS - 1)
         U(CELLS + 2) = U(CELLS - 1)
         P(CELLS + 2) = P(CELLS - 1)
*
      ELSE
*
C        Reflective boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) = -U(CELLS)
         P(CELLS + 1) =  P(CELLS)
*
         D(CELLS + 2) =  D(CELLS - 1)
         U(CELLS + 2) = -U(CELLS - 1)
         P(CELLS + 2) =  P(CELLS - 1)
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
         C(I) = SQRT(GAMMA*P(I)/D(I))
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
      OPEN(UNIT = 1, FILE = 'e1musc.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE MUSFLX(LIMITE, CELLS)
*
C     Purpose: to compute the intercell numerical flux for the
C              MUSCL-Hancock method. This contains a MUSCL
C              reconstruction of the data and application of
C              the Godunov flux with the HLL and HLLC approximate
C              Riemann solver to the boundary extrapolated data.
C              Also available is the Rusanov flux. See Chapt. 10
C              of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, INTFLX, K, LIMITE
*
      REAL    BEXT, CS, D, DELFLUX, DELTA, DLOC, DT, DTODX,
     &        DUPW, DX, FIL, FIR, OMEGA, P, PIL, PIR, RATIO,
     &        TOLLIM, U,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), BEXT(2, 3, -1:IDIM+2),
     &          PIL(3), PIR(3), FIL(3), FIR(3)
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
      COMMON /BEXTRA/ BEXT
      COMMON /FXCHOI/ INTFLX
*
      DATA TOLLIM, OMEGA /1.0E-5, 0.0/
*
C     Compute fluxes on data and conserved variables
*
      DO 10 I = -1, CELLS + 2
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
         CS(3,I) = 0.5* D(I)*U(I)*U(I) + P(I)/G8
*
 10   CONTINUE
*
C     Compute intercell flux for each pair (i, i+1).
C     Store flux in FI(k, i)
*
      DTODX = DT/DX
*
      DO 20 I =  0, CELLS + 1
*
C        Apply TVD condition
*
         DO 30 K = 1, 3
*
C           Compute jumps in conserved variables
*
            DUPW  = CS(K, I)     - CS(K, I - 1)
            DLOC  = CS(K, I + 1) - CS(K, I)
*
C           Modify small jumps preserving sign
*
            IF(ABS(DUPW).LE.TOLLIM)DUPW=TOLLIM*SIGN(1.0,DUPW)
            IF(ABS(DLOC).LE.TOLLIM)DLOC=TOLLIM*SIGN(1.0,DLOC)
*
C           Compute slope by OMEGA weigthed average
*
            DELTA = 0.5*(1.0+OMEGA)*DUPW + 0.5*(1.0-OMEGA)*DLOC
            RATIO = DUPW/DLOC
*
C           Compute slope limiter functions. The subroutines carry
C           DELTA, multiply it by the slope limiter and  return
C           a limited DELTA to be used in the boundary extrapolation
C           step.
*
C           Slope limiters used are:
*
C           LIMITE = 1, Godunov's first order upwind method
C           LIMITE = 2, upwind second order method (non-monotone)
C           LIMITE = 3, upwind TVD, with SUPERBEE type limiter
C           LIMITE = 4, upwind TVD, with VAN LEER type limiter
C           LIMITE = 5, upwind TVD, with VAN ALBADA type limiter
C           LIMITE = 6, upwind TVD, with MINMOD type limiter
C           LIMITE = 7, upwind TVD, with MINMAX type limiter
*
            IF(LIMITE.EQ.1)DELTA = 0.0
            IF(LIMITE.EQ.2)DELTA = DELTA
            IF(LIMITE.EQ.3)CALL SBSLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.4)CALL VLSLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.5)CALL VASLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.6)CALL MISLIC(RATIO, OMEGA, DELTA)
            IF(LIMITE.EQ.7)CALL MINMAX(DUPW, DLOC, DELTA)
*
C           Compute boundary extrapolated values for conserved
C           variables in each cell i
*
            PIL(K) = CS(K, I) - 0.5*DELTA
            PIR(K) = CS(K, I) + 0.5*DELTA
*
 30      CONTINUE
*
C        Evolve boundary extrapolated values for conserved
C        variables in each cell i
*
         CALL FLUEVAL(PIL, FIL)
*
         CALL FLUEVAL(PIR, FIR)
*
         DO 40 K = 1, 3
*
            DELFLUX     = 0.5*DTODX*(FIL(K) - FIR(K))
            BEXT(1,K,I) = PIL(K) + DELFLUX
            BEXT(2,K,I) = PIR(K) + DELFLUX
*
 40      CONTINUE
*
 20   CONTINUE
*
C     Compute intercell flux and store in FI(k, i).
C     Three choices are available
*
      IF(INTFLX.EQ.1)CALL HLLCRS(CELLS)
*
      IF(INTFLX.EQ.2)CALL HLLRS(CELLS)
*
      IF(INTFLX.EQ.3)CALL RUSANO(CELLS)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE FLUEVAL(CS, FLUX)
*
C     Purpose: to compute flux vector components FLUX(K) given the
C              components U(K) of the vector of conserved variables
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &       CS, FLUX, D, U, P, E
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      DIMENSION CS(3), FLUX(3)
*
C     Compute physical variables
*
      D = CS(1)
      U = CS(2)/D
      P = G8*(CS(3) - 0.5*D*U*U)
      E = CS(3)
*
C     Compute fluxes
*
      FLUX(1) = D*U
      FLUX(2) = D*U*U + P
      FLUX(3) = U*(E + P)
C
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE HLLCRS(CELLS)
*
C     Purpose: to compute the intercell flux FI(K, I) according
C              to the Godunov method with the HLLC approximate
C              Riemann solver. See Chap. 10 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
       REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &         BEXT, CDL, CDR, CSL, CSR, ENEL, ENER, FDL, FDR,
     &         SL, SM, SR, FI,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION CDL(3), CDR(3), CSL(3), CSR(3), FDL(3), FDR(3),
     &          FI(3, -1:IDIM+2), BEXT(2, 3, -1:IDIM+2)
*
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /FLUXES/ FI
      COMMON /BEXTRA/ BEXT
*
      DO 10 I = 0, CELLS
*
C        At interface (I, I+1) store conserved variables
C        CDL and CDR from boundary extrapolated values BEXT
*
         DO 20 K = 1, 3
            CDL(K) = BEXT(2, K, I)
            CDR(K) = BEXT(1, K, I + 1)
 20      CONTINUE
*
C        Compute physical variables from conserved variables
*
         DL = CDL(1)
         UL = CDL(2)/DL
         PL = G8*(CDL(3) - 0.5*CDL(2)*UL)
         CL = SQRT(GAMMA*PL/DL)
*
         DR = CDR(1)
         UR = CDR(2)/DR
         PR = G8*(CDR(3) - 0.5*CDR(2)*UR)
         CR = SQRT(GAMMA*PR/DR)
*
C        Compute fluxes FDL and FDR at CDL and CDR
*
         CALL FLUEVAL(CDL, FDL)
*
         CALL FLUEVAL(CDR, FDR)
*
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solver
*
         CALL ESTIME(SL, SM, SR)
*
         IF(SL.GE.0.0)THEN
*
C           Right-going supersonic flow
*
            DO 30 K = 1, 3
               FI(K, I) = FDL(K)
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
               ENEL   = CDL(3)/DL  + (SM - UL)*(SM + PL/(DL*(SL - UL)))
               CSL(1) = DL*(SL - UL)/(SL - SM)
               CSL(2) = CSL(1)*SM
               CSL(3) = CSL(1)*ENEL
*
               DO 40 K = 1, 3
                  FI(K, I) = FDL(K) + SL*(CSL(K) - CDL(K))
 40            CONTINUE
*
            ELSE
*
C              Subsonic flow to the left
*
               ENER   = CDR(3)/DR + (SM - UR)*(SM + PR/(DR*(SR - UR)))
               CSR(1) = DR*(SR - UR)/(SR - SM)
               CSR(2) = CSR(1)*SM
               CSR(3) = CSR(1)*ENER
*
               DO 50 K = 1, 3
                  FI(K, I) = FDR(K) + SR*(CSR(K) - CDR(K))
 50            CONTINUE
            ENDIF
         ENDIF
*
         IF(SR.LE.0.0)THEN
*
C           Left-going supersonic flow
*
            DO 60 K = 1, 3
               FI(K, I) = FDR(K)
 60         CONTINUE
*
         ENDIF
*
 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE HLLRS(CELLS)
*
C     Purpose: to compute the intercell flux FI(K, I) according
C              to the Godunov method with the HLL approximate
C              Riemann solver. See Chap. 10 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     BEXT, CL, CR, DL, DR, CDL, CDR, FI, FDL, FDR, HLL,
     &         PL, PR, SL, SM, SR, UL, UR,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION CDL(3), CDR(3), FDL(3), FDR(3), FI(3, -1:IDIM+2),
     &          BEXT(2, 3, -1:IDIM+2)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /FLUXES/ FI
      COMMON /BEXTRA/ BEXT
*
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 10 I = 0, CELLS
*
C        At interface (I, I+1) store conserved variables
C        CDL and CDR from boundary extrapolated values BEXT
*
         DO 20 K = 1, 3
            CDL(K) = BEXT(2, K, I)
            CDR(K) = BEXT(1, K, I + 1)
 20      CONTINUE
*
C        Compute physical variables from conserved variables
*
         DL = CDL(1)
         UL = CDL(2)/DL
         PL = G8*(CDL(3) - 0.5*CDL(2)*UL)
         CL = SQRT(GAMMA*PL/DL)
*
         DR = CDR(1)
         UR = CDR(2)/DR
         PR = G8*(CDR(3) - 0.5*CDR(2)*UR)
         CR = SQRT(GAMMA*PR/DR)
*
C        Compute fluxes FDL and FDR at CDL and CDR
*
         CALL FLUEVAL(CDL, FDL)
*
         CALL FLUEVAL(CDR, FDR)
*
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solver
*
         CALL ESTIME(SL, SM, SR)
*
         IF(SL.GE.0.0)THEN
*
C           Right-going supersonic flow
*
            DO 30 K = 1, 3
               FI(K, I) = FDL(K)
 30         CONTINUE
*
         ENDIF
*
         IF(SL.LE.0.0.AND.SR.GE.0.0)THEN
*
C           Subsonic flow
*
            DO 40 K = 1, 3
               HLL = SR*FDL(K) - SL*FDR(K) + SL*SR*(CDR(K) - CDL(K))
               FI(K, I) = HLL/(SR - SL)
 40         CONTINUE
*
         ENDIF
*
         IF(SR.LE.0.0)THEN
*
C           Left-going supersonic flow
*
            DO 50 K = 1, 3
               FI(K, I) = FDR(K)
 50         CONTINUE
*
         ENDIF

 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RUSANO(CELLS)
*
C     Purpose: to compute the intercell flux FI(K, I) according
C              to the Rusanov method. See Chap. 10 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, K
*
      REAL     BEXT, CL, CR, DL, DR, CDL, CDR, FI, FDL, FDR,
     &         PL, PR, RUSFLUX, SPLUS, SL, SM, SR, UL, UR,
     &         GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION CDL(3), CDR(3), FDL(3), FDR(3), FI(3, -1:IDIM+2),
     &          BEXT(2, 3, -1:IDIM+2)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /FLUXES/ FI
      COMMON /BEXTRA/ BEXT
*
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 10 I = 0, CELLS
*
C        At interface (I, I+1) store conserved variables
C        CDL and CDR from boundary extrapolated values BEXT
*
         DO 20 K = 1, 3
            CDL(K) = BEXT(2, K, I)
            CDR(K) = BEXT(1, K, I + 1)
 20      CONTINUE
*
C        Compute physical variables from conserved variables
*
         DL = CDL(1)
         UL = CDL(2)/DL
         PL = G8*(CDL(3) - 0.5*CDL(2)*UL)
         CL = SQRT(GAMMA*PL/DL)
*
         DR = CDR(1)
         UR = CDR(2)/DR
         PR = G8*(CDR(3) - 0.5*CDR(2)*UR)
         CR = SQRT(GAMMA*PR/DR)
*
C        Compute fluxes FDL and FDR at CDL and CDR
*
         CALL FLUEVAL(CDL, FDL)
*
         CALL FLUEVAL(CDR, FDR)
*
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solver
*
         CALL ESTIME(SL, SM, SR)
*
         IF(SL.GE.0.0)THEN
*
C           Right-going supersonic flow
*
            DO 30 K = 1, 3
               FI(K, I) = FDL(K)
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
               RUSFLUX = 0.5*(FDL(K) + FDR(K))
               FI(K,I) = RUSFLUX + 0.5*SPLUS*(CDL(K) - CDR(K))
 40         CONTINUE
*
         ENDIF
*
         IF(SR.LE.0.0)THEN
*
C           Left-going supersonic flow
**
            DO 50 K = 1, 3
               FI(K, I) = FDR(K)
 50         CONTINUE
*
         ENDIF

 10   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE ESTIME(SL, SM, SR)
*
C     Purpose: to compute wave speed estimates for the HLLC Riemann
C              solver using and adaptive approximate-state Riemann
C              solver including the PVRS, TRRS and TSRS solvers.
C              See Section 9.5, Chapter 9 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    DL, UL, PL, CL, DR, UR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
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
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SBSLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a SUPERBEE type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI             = 0.0
      IF(R.GE.0.0)PHI = 2.0*R
      IF(R.GE.0.5)PHI = 1.0
*
      IF(R.GE.1.0)THEN
         DENOR = 1.0 - OMEGA + (1.0 + OMEGA)*R
         PHIR  = 2.0/DENOR
         PHI   = MIN(PHIR, R)
         PHI   = MIN(PHI, 2.0)
      ENDIF
*
      DELTA = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VLSLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a VAN LEER type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI = 0.0
*
      IF(R.GE.0.0)THEN
         DENOR = 1.0 - OMEGA + (1.0 + OMEGA)*R
         PHIR  = 2.0/DENOR
         PHI   = 2.0*R/(1.0 + R)
         PHI   = MIN(PHI, PHIR)
      ENDIF
*
      DELTA    = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VASLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a VAN ALBADA type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI = 0.0
*
      IF(R.GE.0.0)THEN
         DENOR = 1.0 - OMEGA + (1.0 + OMEGA)*R
         PHIR  = 2.0/DENOR
         PHI   = R*(1.0 + R)/(1.0 + R*R)
         PHI   = MIN(PHI, PHIR)
      ENDIF
*
      DELTA    = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MISLIC(R, OMEGA, DELTA)
*
C     Purpose: to compute a MINMOD type slope limiter DELTA
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  DELTA, DENOR, OMEGA, PHI, PHIR, R
*
      PHI             = 0.0
      IF(R.GE.0.0)PHI = R
*
      IF(R.GE.1.0)THEN
         DENOR = 2.0*(1.0 - OMEGA + (1.0 + OMEGA)*R)
         PHIR  = 4.0/DENOR
         PHI   = MIN(1.0, PHIR)
      ENDIF
*
      DELTA    = PHI*DELTA
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MINMAX(DUPW, DLOC, DELTA)
*
C     Purpose: to compute a MINMAX type slope limiter DELTA.
C              This is the most diffusive of all limiters
C              for centred schemes
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  BETAL, BETAR, DELTA, DLOC, DUPW, SIGNO
*
      BETAL = 1.0
      BETAR = 1.0
      SIGNO = 0.5*(SIGN(1.0,DUPW) + SIGN(1.0,DLOC))
      DELTA = SIGNO*(MIN(BETAL*ABS(DUPW),BETAR*ABS(DLOC)))
*
      END
*
*----------------------------------------------------------------------*
*

