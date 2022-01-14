*
*----------------------------------------------------------------------*
*                                                                      *
C     MUSCL-Hancock scheme for the one-dimensional Euler equations     *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional Euler       *
C              equations for an ideal gas by the MUSCL-Hancock         *
C              method in conjunction with Flux Vector Splitting        *
C              (FVS) schemes. A choice of two splittings is given:     *
C              Steger-Warming and van Leer. Seven limiter functions    *
C              are available                                           *
*                                                                      *
C     Program name: HE-E1MUSV                                          *
*                                                                      *
C     Input  file: e1musv.ini (initial data)                           *
C     Output file: e1musv.out (numerical results)                      *
*                                                                      *
C     Programer : E. F. Toro                                           *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Chapters 8 and 14 of Reference 1 and in       *
C     original references therein                                      *
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
      INTEGER LIMITE, CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, PSCALE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, LIMITE, CELLS, NFREQU, NTMAXI,
     &                TIMEOU, PSCALE
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "e1musv.ini"
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
         CALL FVSFLX(LIMITE, CELLS)
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
C           Solution is written to "e1musv.out' at specified time
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
C     LIMITE    : Limiter function
C     INTFLX    : Choice of intercell flux
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER INTFLX, IBCLEF, IBCRIG, CELLS, LIMITE, NFREQU, NTMAXI
*
      REAL    CFLCOE, DOMLEN, DIAPH1, DIAPH2, PSCALE, TIMEOU,
     &        DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT, DRINIT,
     &        URINIT, PRINIT,
     &        GAMMA, G7, G8
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, DIAPH1, DIAPH2
      COMMON /DRIVER/ CFLCOE, LIMITE, CELLS, NFREQU, NTMAXI,
     &                TIMEOU, PSCALE
      COMMON /INISTA/ DLINIT, ULINIT, PLINIT, DMINIT, UMINIT, PMINIT,
     &                DRINIT, URINIT, PRINIT
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /FXCHOI/ INTFLX
*
      OPEN(UNIT = 1, FILE = 'e1musv.ini', STATUS = 'UNKNOWN')
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
      WRITE(6,*)'LIMITE = ',LIMITE
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
         P(CELLS + 1) =  P(CELLS)
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
      OPEN(UNIT = 1, FILE = 'e1musv.out', STATUS = 'UNKNOWN')
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
      SUBROUTINE FVSFLX(LIMITE, CELLS)
*
C     Purpose: to compute the intercell numerical flux for the
C              MUSCL-Hancock method. This contains a MUSCL
C              reconstruction of the data and application of
C              Flux Vector Splitting Schemes. See Chaps. 8 and
C              14 of Ref. 1 and original references therein
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, CELLS, IDIM, INTFLX, K, LIMITE
*
      REAL    BEXT, CS, D, DELFLUX, DELTA, DLOC, DT, DTODX,
     &        DUPW, DX, FIL, FIR, OMEG, P, PIL, PIR, RATIO,
     &        TOLLIM, U,
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), P(-1:IDIM+2),
     &          CS(3,-1:IDIM+2), BEXT(2, 3, -1:IDIM+2),
     &          PIL(3), PIR(3), FIL(3), FIR(3)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /PRIMIT/ D, U, P
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
      COMMON /BEXTRA/ BEXT
      COMMON /FXCHOI/ INTFLX
*
      DATA TOLLIM, OMEG /1.0E-5, 0.0/
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
            DUPW = CS(K, I)     - CS(K, I - 1)
            DLOC = CS(K, I + 1) - CS(K, I)
*
C           Modify small jumps preserving sign
*
            IF(ABS(DUPW).LE.TOLLIM)DUPW=TOLLIM*SIGN(1.0,DUPW)
            IF(ABS(DLOC).LE.TOLLIM)DLOC=TOLLIM*SIGN(1.0,DLOC)
*
C           Compute slope by OMEG weigthed average
*
            DELTA = 0.5*(1.0+OMEG)*DUPW + 0.5*(1.0-OMEG)*DLOC
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
            IF(LIMITE.EQ.3)CALL SBSLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.4)CALL VLSLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.5)CALL VASLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.6)CALL MISLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.7)CALL MINMAX(DUPW, DLOC, DELTA)
*
C         Compute boundary extrapolated values for conserved
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
C     Two choices are available
*
      IF(INTFLX.EQ.1)CALL STEWAR(CELLS)
*
      IF(INTFLX.EQ.2)CALL VANLEE(CELLS)
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
      REAL   GAMMA, G7, G8, CS, FLUX, D, U, P, E
*
      COMMON /GAMMAS/ GAMMA, G7, G8
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
      SUBROUTINE STEWAR(CELLS)
*
C     Purpose: to compute an intercell flux FI(K, I) according
C              to the Steger-Warming Flux Vector Splitting method.
C              Theory in Chap. 8 of Ref. 1 and in original
C              references therein
*
      IMPLICIT NONE
*
C     Declaration of variables

      INTEGER I, CELLS, IDIM, K

      REAL    BEXT, CDL, CDR, CL, CO, CR, DL, DR, FDL, FDR, FI,
     &        FIM, FIP, HM, HP, MACH, PL, PR, S, SM, SP, UL, UR,
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION BEXT(2, 3, -1:IDIM+2), FI(3, -1:IDIM + 2),
     &          FIM(3, -1:IDIM + 2), FIP(3, -1:IDIM + 2),
     &          CDL(3), FDL(3), CDR(3), FDR(3), S(3),
     &          SP(3), SM(3)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /BEXTRA/ BEXT
      COMMON /FLUXES/ FI
*
      DO 10 I = 0, CELLS + 1
*
C        In cell I retrieve conserved variables CDL and CDR from
C        boundary extrapolated values BEXT
*
         DO 20 K = 1, 3
            CDL(K) = BEXT(1, K, I)
            CDR(K) = BEXT(2, K, I)
 20      CONTINUE
*
*        **************************************************************
C        Find backward flux component FIM from left extrapolated values
*        **************************************************************
*
C        Compute physical variables from conserved variables
*
         DL = CDL(1)
         UL = CDL(2)/DL
         PL = G8*(CDL(3) - 0.5*CDL(2)*UL)
         CL = SQRT(GAMMA*PL/DL)
*
C        Compute eigenvalues
*
         S(1) = UL - CL
         S(2) = UL
         S(3) = UL + CL
*
         IF(S(3).LE.0.0)THEN
*
C           Left-going supersonic flow
*
C           Compute fluxes FDL at CDL
*
            CALL FLUEVAL(CDL, FDL)
*
            FIM(1, I) = FDL(1)
            FIM(2, I) = FDL(2)
            FIM(3, I) = FDL(3)
*
         ENDIF
*
         IF(S(1).GE.0.0)THEN
*
C           Right-going supersonic flow
*
            FIM(1, I) = 0.0
            FIM(2, I) = 0.0
            FIM(3, I) = 0.0
*
         ENDIF
*
         MACH = UL/CL
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
C           Define Steger-Warming splitting of eigenvalues
*
            DO 30 K  = 1, 3
               SM(K) = 0.5*(S(K) - ABS(S(K)))
 30         CONTINUE
*
            HM = 0.5*UL*UL + CL*CL/G8 - UL*CL
            HP = 0.5*UL*UL + CL*CL/G8 + UL*CL
            CO = 0.5*DL/GAMMA
*
C           Compute backward flux component
*
            FIM(1,I) = CO*(SM(1) + 2.0*G8*SM(2) + SM(3))
            FIM(2,I) = CO*(S(1)*SM(1)+2.0*G8*S(2)*SM(2)+S(3)*SM(3))
            FIM(3,I) = CO*(HM*SM(1)+G8*S(2)*S(2)*SM(2)+HP*SM(3))
C
         ENDIF
*
*        **************************************************************
C        Find forward flux component FIP from right extrapolated values
*        **************************************************************
*
C        Compute physical variables from conserved variables
*
         DR = CDR(1)
         UR = CDR(2)/DR
         PR = G8*(CDR(3) - 0.5*CDR(2)*UR)
         CR = SQRT(GAMMA*PR/DR)
*
C        Compute eigenvalues
*
         S(1) = UR - CR
         S(2) = UR
         S(3) = UR + CR
*
         IF(S(3).LE.0.0)THEN
*
            FIP(1, I) = 0.0
            FIP(2, I) = 0.0
            FIP(3, I) = 0.0
*
         ENDIF
*
         IF(S(1).GE.0.0)THEN
*
C           Right-going supersonic flow
*
C           Compute fluxes FDR at CDR
*
            CALL FLUEVAL(CDR, FDR)
*
            FIP(1, I) = FDR(1)
            FIP(2, I) = FDR(2)
            FIP(3, I) = FDR(3)
*
         ENDIF
*
         MACH = UR/CR
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
C           Define Steger-Warming splitting of eigenvalues
*
            DO 40 K  = 1, 3
               SP(K) = 0.5*(S(K) + ABS(S(K)))
 40         CONTINUE
*
            HM = 0.5*UR*UR + CR*CR/G8 - UR*CR
            HP = 0.5*UR*UR + CR*CR/G8 + UR*CR
            CO = 0.5*DR/GAMMA
*
C           Compute forward flux component FRIGH
*
            FIP(1,I) = CO*(SP(1) + 2.0*G8*SP(2) + SP(3))
            FIP(2,I) = CO*(S(1)*SP(1)+2.0*G8*S(2)*SP(2)+S(3)*SP(3))
            FIP(3,I) = CO*(HM*SP(1)+G8*S(2)*S(2)*SP(2)+HP*SP(3))
*
         ENDIF
*
 10   CONTINUE
*
C     Compute intercell flux as summ of forward and backward
C     flux components
*
      DO 50 I = 0, CELLS
         DO 60 K = 1, 3
            FI(K, I) = FIP(K, I) +  FIM(K, I + 1)
 60      CONTINUE
*
 50   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE VANLEE(CELLS)
*
C     Purpose: to compute an intercell flux FI(K, I) according
C              to the VAN LEER Flux Vector Splitting method.
C              Theory in Chap. 8 of Ref. 1 and in original
C              references therein
*
      IMPLICIT NONE
*
C     Declaration of variables

      INTEGER I, CELLS, IDIM, K
*
      REAL    BEXT, CDL, CDR, CL, CR, COE1, COE2, DL, DR, FDL, FDR,
     &        FI, FIM, FIP, FX1, FX2, MACH, PL, PR, UL, UR,
     &        GAMMA, G7, G8
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION BEXT(2, 3, -1:IDIM+2), FI(3, -1:IDIM + 2),
     &          FIM(3, -1:IDIM + 2), FIP(3, -1:IDIM + 2),
     &          CDL(3), FDL(3), CDR(3), FDR(3)
*
      COMMON /GAMMAS/ GAMMA, G7, G8
      COMMON /BEXTRA/ BEXT
      COMMON /FLUXES/ FI
*
      DO 10 I = 0, CELLS + 1
*
C        In cell I retrieve conserved variables CDL and CDR from
C        boundary extrapolated values BEXT
*
         DO 20 K = 1, 3
            CDL(K) = BEXT(1, K, I)
            CDR(K) = BEXT(2, K, I)
 20      CONTINUE
*
*        **************************************************************
C        Find backward flux component FIM from left extrapolated values
*        **************************************************************
*
C        Compute physical variables from conserved variables
*
         DL = CDL(1)
         UL = CDL(2)/DL
         PL = G8*(CDL(3) - 0.5*CDL(2)*UL)
         CL = SQRT(GAMMA*PL/DL)
*
         IF((UL+CL).LE.0.0)THEN
*
C           Left-going supersonic flow
*
C           Compute fluxes FDL at CDL
*
            CALL FLUEVAL(CDL, FDL)
*
            FIM(1, I) = FDL(1)
            FIM(2, I) = FDL(2)
            FIM(3, I) = FDL(3)
*
         ENDIF
*
         IF((UL-CL).GE.0.0)THEN
*
C           Right-going supersonic flow
*
            FIM(1, I) = 0.0
            FIM(2, I) = 0.0
            FIM(3, I) = 0.0
*
         ENDIF
*
         MACH = UL/CL
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
C           Define van Leer splitting
*
            FX1  = 0.25*DL*CL*(MACH - 1.0)**2
            FX2  = G7*MACH - 1.0
C
            COE1 = 2.0*CL/GAMMA
            COE2 = 2.0*CL*CL/(GAMMA*GAMMA - 1.0)
*
            FIM(1, I) = -FX1
            FIM(2, I) = -FX1*COE1*FX2
            FIM(3, I) = -FX1*COE2*FX2*FX2
C
         ENDIF
*
*        **************************************************************
C        Find forward flux component FIP from right extrapolated values
*        **************************************************************
*
C        Compute physical variables from conserved variables
*
         DR = CDR(1)
         UR = CDR(2)/DR
         PR = G8*(CDR(3) - 0.5*CDR(2)*UR)
         CR = SQRT(GAMMA*PR/DR)
*
         IF((UR+CR).LE.0.0)THEN
*
            FIP(1, I) = 0.0
            FIP(2, I) = 0.0
            FIP(3, I) = 0.0
*
         ENDIF
*
         IF((UR-CR).GE.0.0)THEN
*
C           Right-going supersonic flow
*
C           Compute fluxes FDR at CDR
*
            CALL FLUEVAL(CDR, FDR)
*
            FIP(1, I) = FDR(1)
            FIP(2, I) = FDR(2)
            FIP(3, I) = FDR(3)
*
         ENDIF
*
         MACH = UR/CR
*
         IF(ABS(MACH).LE.1.0)THEN
*
C           Subsonic flow
*
C           Define Steger-Warming splitting of eigenvalues
*
C           Subsonic flow
*
C           Define van Leer splitting
*
            FX1  = 0.25*DR*CR*(MACH + 1.0)**2
            FX2  = G7*MACH + 1.0
C
            COE1 = 2.0*CR/GAMMA
            COE2 = 2.0*CR*CR/(GAMMA*GAMMA - 1.0)
*
            FIP(1, I) = FX1
            FIP(2, I) = FX1*COE1*FX2
            FIP(3, I) = FX1*COE2*FX2*FX2
*
         ENDIF
*
 10   CONTINUE
*
C     Compute intercell flux as summ of forward and backward
C     flux components
*
      DO 50 I = 0, CELLS
         DO 60 K = 1, 3
            FI(K, I) = FIP(K, I) + FIM(K, I + 1)
 60      CONTINUE
*
 50   CONTINUE
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

