*
*----------------------------------------------------------------------*
*                                                                      *
*     Weighted Average Flux (WAF) scheme for the time-dependent        *
C     three dimensional Euler equations for ideal gases                *
*                                                                      *
C     Purpose: to solve the three-dimensional Euler equations for      *
C              an ideal gas on a Cartesian domain using the            *
C              Weighted Average Flux (WAF) method in conjunction       *
C              with the HLLC approximate Riemann solver and            *
C              dimensional splitting. A selection of 6 limiter         *
C              functions is available                                  *
*                                                                      *
C     Name of program: HE-E3WAFCS                                      *
*                                                                      *
C     Input  file: e3wafcs.ini (initial data)                          *
C     Output file: e3wcsx.out  (slice in x-direction)                  *
C     Output file: e3wcsy.out  (slice in y-direction)                  *
C     Output file: e3wcsz.out  (slice in z-direction)                  *
C     Output file: e3wcs2.out  (slice through plane)                   *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Ref. 1, Chaps. 10, 14 and 16, and in          *
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
*     *************************                                        *
C     *  The Code at a Glance *                                        *
*     *************************                                        *
*                                                                      *
C     CALL PROBLM (Defines the problem)                                *
C     CALL MESHER (Generates mesh, trivial  here)                      *
*                                                                      *
C-----Time stepping begins                                             *
*                                                                      *
C         CALL CFLCON (CFL condition)                                  *
C         CALL SWEEPS (Dimensional splitting)                          *
C              CALL ONEDIM (Directional solver)                        *
C                   CALL BCONDI (Boundary conditions)                  *
C                   CALL CONFLX (Local flux evaluation)                *
C                   CALL ESTIME (Speed estimates for HLLC solver)      *
C                   CALL SUPERA (or other limiter)                     *
C         CALL OUTPUT (Output results)                                 *
*                                                                      *
C-----Time stepping ends                                               *
*                                                                      *
*----------------------------------------------------------------------*
*
C     Driver program
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL     CFLCOE, DT, PSCALE, TIME, TIMEOU, TIMETO
*
      INTEGER  N, NFREQU, NTMAXI
*
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, PSCALE, TIMEOU
      DATA TIMETO /1.0E-07/
*
C     Define the problem to be solved
*
      CALL PROBLM
*
C     Mesh is generated (trivial here)
*
      CALL MESHER
*
C     Time stepping starts
*
      TIME = 0.0
*
      WRITE(6,*)'---------------------------------------------'
      WRITE(6,*)'   Time step N        TIME             '
      WRITE(6,*)'---------------------------------------------'
*
      DO 10 N = 1, NTMAXI
*
C        CFL condition is applied to compute time step DT
*
         CALL CFLCON(CFLCOE, TIME, TIMEOU, DT)
*
         TIME = TIME + DT
*
C        Solver is called
*
         CALL SWEEPS(DT)

         IF(MOD(N,NFREQU).EQ.0)THEN
            WRITE(6,20)N, TIME
         ENDIF
*
         IF(ABS(TIME - TIMEOU).LE.TIMETO)THEN
*
C           Solution written out at time = TIMEOU, to files
C           "e3wcsx.out", "e3wcsy.out", "e3wcsz.out" and "e3wcs2.out"
*
            CALL OUTPUT(PSCALE)
*
            WRITE(6,*)'---------------------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'---------------------------------------------'
*
            GOTO 30
*
         ENDIF
*
 20      FORMAT(I12,6X,2(F12.7, 4X))

 10   CONTINUE
*
      WRITE(6,*)'Maximum number of time steps NTMAXI reached'
*
 30   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE PROBLM
*
C     Purpose: to read initial data for spherical explosion and
C              implosion test problems. The domain is divided into
C              the inner and outer sections of a sphere. Initial
C              values for density, velocity components and pressure
C              have constant values in each of these regions. For
C              an explosion problem pressure in the inner region
C              is higher than that in the outer region. For an
C              implosion the opposite happens.
*
C     Input variables
*
C     DOMLEX    : Domain length in x-direction
C     ICELLS    : Number of computing cells in x-direction
C     DOMLEY    : Domain length in y-direction
C     JCELLS    : Number of computing cells in y-direction
C     DOMLEZ    : Domain length in z-direction
C     KCELLS    : Number of computing cells in z-direction
C     RADIUS    : Radius of sphere
C     XC        : X-ccordinate of centre of sphere
C     YC        : Y-ccordinate of centre of sphere
C     ZC        : Z-ccordinate of centre of sphere
C     GAMMA     : Ratio of specific heats
C     TIMEOU    : Output time
C     DINS      : Initial density inside circle
C     UINS      : Initial x-velocity inside circle
C     VINS      : Initial y-velocity inside circle
C     WINS      : Initial z-velocity inside circle
C     PINS      : Initial pressure inside circle
C     DOUT      : Initial density  outside circle
C     UOUT      : Initial x-velocity outside circle
C     VOUT      : Initial y-velocity outside circle
C     WOUT      : Initial z-velocity outside circle
C     POUT      : Initial pressure outside circle
C     BCXLEF    : Boundary condition on the left
C     BCXRIG    : Boundary condition on the right
C     BCYBAC    : Boundary condition on the back
C     BCYFRO    : Boundary condition on the front
C     BCZBOT    : Boundary condition on the bottom
C     BCZTOP    : Boundary condition on the top
C     CFLCOE    : Courant number coefficient
C     NFREQU    : Output frequency to screen
C     PSCALE    : Pressure normalising factor for printing
C     NTMAXI    : Maximum number of time steps
C     LIMITE    : Limiter function
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER BCXLEF, BCXRIG, BCYBAC, BCYFRO, BCZBOT, BCZTOP, I,
     &        IBCS, ICELLS, IDIM, ILIM, J, JBCS, JCELLS, JDIM,
     &        JLIM, K, KCELLS, KDIM, KLIM, KBCS, L, LIMITE, NFREQU,
     &        NTMAXI
*
      REAL    GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        D, U, V, W, P, C,
     &        DINS, UINS, VINS, WINS, PINS, UOUT, VOUT, WOUT, POUT,
     &        CFLCOE, DOMLEX, DOMLEY, DOMLEZ, DOUT, DX, DY,
     &        DZ, PSCALE,
     &        RADIUS, TIMEOU, XC,YC, ZC,
     &        RMINU, RPLUS, R, RABSO, XV, YV, ZV
*
      PARAMETER (IDIM = 100, JDIM = 100, KDIM = 100)
*
C     One-dimensional arrays are given in terms of IDIM, assumed
C     to be the largest of the three parameter values IDIM, JDIM
C     and KDIM
*
      DIMENSION D(IDIM,JDIM,KDIM),U(IDIM,JDIM,KDIM),
     &          V(IDIM,JDIM,KDIM),W(IDIM,JDIM,KDIM),
     &          P(IDIM,JDIM,KDIM),C(IDIM,JDIM,KDIM),
     &          ILIM(2,-1:JDIM+2,-1:KDIM+2),
     &          IBCS(2,-1:JDIM+2,-1:KDIM+2),
     &          JLIM(2,-1:IDIM+2,-1:KDIM+2),
     &          JBCS(2,-1:IDIM+2,-1:KDIM+2),
     &          KLIM(2,-1:IDIM+2,-1:JDIM+2),
     &          KBCS(2,-1:IDIM+2,-1:JDIM+2),
     &          R(8), XV(8), YV(8), ZV(8)
*
      COMMON /SOLUTI/ D, U, V, W, P, C
      COMMON /MESHXY/ DX, DY, DZ, ICELLS, JCELLS, KCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS, KLIM, KBCS
*
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, PSCALE, TIMEOU
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /TVDCON/ LIMITE
*
C     Initial data is read in
*
      OPEN(UNIT = 1,FILE = 'e3wafcs.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEX
      READ(1,*)ICELLS
      READ(1,*)DOMLEY
      READ(1,*)JCELLS
      READ(1,*)DOMLEZ
      READ(1,*)KCELLS
      READ(1,*)RADIUS
      READ(1,*)XC
      READ(1,*)YC
      READ(1,*)ZC
      READ(1,*)GAMMA
      READ(1,*)TIMEOU
      READ(1,*)DINS
      READ(1,*)UINS
      READ(1,*)VINS
      READ(1,*)WINS
      READ(1,*)PINS
      READ(1,*)DOUT
      READ(1,*)UOUT
      READ(1,*)VOUT
      READ(1,*)WOUT
      READ(1,*)POUT
      READ(1,*)BCXLEF
      READ(1,*)BCXRIG
      READ(1,*)BCYBAC
      READ(1,*)BCYFRO
      READ(1,*)BCZBOT
      READ(1,*)BCZTOP
      READ(1,*)CFLCOE
      READ(1,*)NFREQU
      READ(1,*)PSCALE
      READ(1,*)NTMAXI
      READ(1,*)LIMITE
*
      CLOSE(1)
*
C     Input data is echoed to screen
*
      WRITE(6,*)'DOMLEX = ', DOMLEX
      WRITE(6,*)'ICELLS = ', ICELLS
      WRITE(6,*)'DOMLEY = ', DOMLEY
      WRITE(6,*)'JCELLS = ', JCELLS
      WRITE(6,*)'DOMLEZ = ', DOMLEZ
      WRITE(6,*)'KCELLS = ', KCELLS
      WRITE(6,*)'RADIUS = ', RADIUS
      WRITE(6,*)'XC     = ', XC
      WRITE(6,*)'YC     = ', YC
      WRITE(6,*)'ZC     = ', ZC
      WRITE(6,*)'GAMMA  = ', GAMMA
      WRITE(6,*)'TIMEOU = ', TIMEOU
      WRITE(6,*)'DINS   = ', DINS
      WRITE(6,*)'UINS   = ', UINS
      WRITE(6,*)'VINS   = ', VINS
      WRITE(6,*)'WINS   = ', WINS
      WRITE(6,*)'PINS   = ', PINS
      WRITE(6,*)'DOUT   = ', DOUT
      WRITE(6,*)'UOUT   = ', UOUT
      WRITE(6,*)'VOUT   = ', VOUT
      WRITE(6,*)'WOUT   = ', WOUT
      WRITE(6,*)'POUT   = ', POUT
      WRITE(6,*)'BCXLEF = ', BCXLEF
      WRITE(6,*)'BCXRIG = ', BCXRIG
      WRITE(6,*)'BCYBAC = ', BCYBAC
      WRITE(6,*)'BCYFRO = ', BCYFRO
      WRITE(6,*)'BCZBOT = ', BCZBOT
      WRITE(6,*)'BCZTOP = ', BCZTOP
      WRITE(6,*)'CFLCOE = ', CFLCOE
      WRITE(6,*)'NFREQU = ', NFREQU
      WRITE(6,*)'PSCALE = ', PSCALE
      WRITE(6,*)'NTMAXI = ', NTMAXI
      WRITE(6,*)'LIMITE = ', LIMITE
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
C     Define mesh size in the x, y and z-directions
*
      DX = DOMLEX/REAL(ICELLS)
      DY = DOMLEY/REAL(JCELLS)
      DZ = DOMLEZ/REAL(KCELLS)
*
C     Setup initial conditions
*
      DO 10 K = 1, KCELLS
*
         ZV(1) = (K-1)*DZ
         ZV(2) = (K-1)*DZ
         ZV(3) = (K-1)*DZ
         ZV(4) = (K-1)*DZ
         ZV(5) = K*DZ
         ZV(6) = K*DZ
         ZV(7) = K*DZ
         ZV(8) = K*DZ
*
         DO 20 J = 1, JCELLS
            YV(1) = (J-1)*DY
            YV(2) = (J-1)*DY
            YV(3) = J*DY
            YV(4) = J*DY
            YV(5) = (J-1)*DY
            YV(6) = (J-1)*DY
            YV(7) = J*DY
            YV(8) = J*DY
*
            DO 30 I = 1, ICELLS
               XV(1) = (I-1)*DX
               XV(2) = I*DX
               XV(3) = I*DX
               XV(4) = (I-1)*DX
               XV(5) = (I-1)*DX
               XV(6) = I*DX
               XV(7) = I*DX
               XV(8) = (I-1)*DX
*
               RMINU = 0.0
               RPLUS = 0.0
               RABSO = 0.0
*
               DO 39 L = 1,8
                  R(L) = SQRT((XV(L)-XC)**2 + (YV(L)-YC)**2 +
     &                       (ZV(L)-ZC)**2) - RADIUS
                  IF(R(L).LE.0.0)RMINU = RMINU + R(L)
                  IF(R(L).GE.0.0)RPLUS = RPLUS + R(L)
                  RABSO = RABSO + ABS(R(L))
 39            CONTINUE
*
C              Assign initial values
*
               D(I, J, K) = (ABS(RMINU)*DINS + RPLUS*DOUT)/RABSO
               U(I, J, K) = (ABS(RMINU)*UINS + RPLUS*UOUT)/RABSO
               V(I, J, K) = (ABS(RMINU)*VINS + RPLUS*VOUT)/RABSO
               W(I, J, K) = (ABS(RMINU)*WINS + RPLUS*WOUT)/RABSO
               P(I, J, K) = (ABS(RMINU)*PINS + RPLUS*POUT)/RABSO
*
C              Compute sound speed
*
               C(I, J, K) = SQRT(GAMMA*P(I,J,K)/D(I,J,K))
*
 30         CONTINUE
 20      CONTINUE
 10   CONTINUE
*
      DO 40 K = 1, KCELLS
         DO 50 J = 1, JCELLS
*
C           Set limits in the x-direction
*
            ILIM(1,J,K) = 1
            ILIM(2,J,K) = ICELLS
*
C           Set boundary conditions in the x-direction
*
            IBCS(1,J,K) = BCXLEF
            IBCS(2,J,K) = BCXRIG
*
 50      CONTINUE
 40   CONTINUE
*
      DO 60 K = 1, KCELLS
         DO 70 I = 1, ICELLS
*
C           Set limits in y-direction
*
            JLIM(1,I,K) = 1
            JLIM(2,I,K) = JCELLS
*
C           Set boundary conditions in the y-direction
*
            JBCS(1,I,K) = BCYBAC
            JBCS(2,I,K) = BCYFRO
*
 70      CONTINUE
 60   CONTINUE
*
      DO 80 J = 1, KCELLS
         DO 90 I = 1, ICELLS
*
C           Set limits in z-direction
*
            KLIM(1,I,J) = 1
            KLIM(2,I,J) = KCELLS
*
C           Set boundary conditions in the z-direction
*
            KBCS(1,I,J) = BCZBOT
            KBCS(2,I,J) = BCZTOP
*
 90      CONTINUE
 80   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SWEEPS(DT)
*
C     Purpose: to apply dimensional splitting in the x, y and
C              z-directions. First-order time accurate splitting
C              used. For each dimensional sweep a single scheme
C              is used; velocity components U, V and W are swapped
C              appropriately. Second order splittings are given
C              in Chap. 16, Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    C, CO, D, DO, DT, DTS, DX, DY, DZ, P, PO, U, UO, V, VO,
     &        W, WO
*
      INTEGER I, IBCS, ILIM, J, JBCS, JDIM, JLIM, IDIM, K, KDIM,
     &        KLIM, KBCS, MLEF, MBCL,
     &        MRIG, MBCR, ICELLS, JCELLS, KCELLS
*
      PARAMETER (IDIM = 100, JDIM = 100, KDIM = 100)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the three parameter values IDIM, JDIM, KDIM
*
      DIMENSION D(IDIM,JDIM,KDIM),U(IDIM,JDIM,KDIM),
     &          V(IDIM,JDIM,KDIM),W(IDIM,JDIM,KDIM),
     &          P(IDIM,JDIM,KDIM),C(IDIM,JDIM,KDIM),
     &          ILIM(2,-1:JDIM+2,-1:KDIM+2),
     &          IBCS(2,-1:JDIM+2,-1:KDIM+2),
     &          JLIM(2,-1:IDIM+2,-1:KDIM+2),
     &          JBCS(2,-1:IDIM+2,-1:KDIM+2),
     &          KLIM(2,-1:IDIM+2,-1:JDIM+2),
     &          KBCS(2,-1:IDIM+2,-1:JDIM+2),
     &          DO(-1:IDIM+2),UO(-1:IDIM+2),VO(-1:IDIM+2),
     &          WO(-1:IDIM+2),PO(-1:IDIM+2),CO(-1:IDIM+2)
*
      COMMON /SOLUTI/ D, U, V, W, P, C
      COMMON /MESHXY/ DX, DY, DZ, ICELLS, JCELLS, KCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS, KLIM, KBCS
      COMMON /WALLSS/ MLEF, MRIG, MBCL, MBCR
*
C     x-sweep is carried out for a complete time step DT
*
      DO 10 K = 1, KCELLS
         DO 20 J = 1, JCELLS
*
C           Left and right limits in x-sweep are set
*
            MLEF = ILIM(1, J, K)
            MRIG = ILIM(2, J, K)
*
C           Boundary conditions for x-sweep are set
*
            MBCL = IBCS(1, J, K)
            MBCR = IBCS(2, J, K)
*
            DO 30 I = MLEF, MRIG
*
C              Initial data are stored in one-dimensional arrays
*
               DO(I) = D(I, J, K)
               UO(I) = U(I, J, K)
               VO(I) = V(I, J, K)
               WO(I) = W(I, J, K)
               PO(I) = P(I, J, K)
               CO(I) = C(I, J, K)
 30         CONTINUE
*
C           Solver in the x-direction is called
C           Note order of velocity components in argument list
*
            CALL ONEDIM(DT,DX,DO,UO,VO,WO,PO,CO)
*
C           Store solution back in three-dimensional array
*
            DO 40 I = MLEF, MRIG
*
               D(I, J, K) = DO(I)
               U(I, J, K) = UO(I)
               V(I, J, K) = VO(I)
               W(I, J, K) = WO(I)
               P(I, J, K) = PO(I)
               C(I, J, K) = CO(I)
 40         CONTINUE
 20      CONTINUE
 10   CONTINUE
*
C     y-sweep is carried out for a complete time step DT
*
      DO 50 K = 1, KCELLS
         DO 60 I = 1, ICELLS
*
C           Left and right limits in y-sweep are set
*
            MLEF = JLIM(1, I, K)
            MRIG = JLIM(2, I, K)
*
C           Boundary conditions for y-sweep are set
*
            MBCL = JBCS(1, I, K)
            MBCR = JBCS(2, I, K)
*
            DO 70 J = MLEF, MRIG
*
C              Initial data are stored in one-dimensional arrays
*
               DO(J) = D(I, J, K)
               UO(J) = U(I, J, K)
               VO(J) = V(I, J, K)
               WO(J) = W(I, J, K)
               PO(J) = P(I, J, K)
               CO(J) = C(I, J, K)
 70         CONTINUE
*
C           Solver in the y-direction is called
C           Note order of velocity components in argument list
*
            CALL ONEDIM(DT,DY,DO,VO,UO,WO,PO,CO)
*
C           Store solution back in three-dimensional arrays
*
            DO 80 J = MLEF, MRIG
*
               D(I, J, K) = DO(J)
               U(I, J, K) = UO(J)
               V(I, J, K) = VO(J)
               W(I, J, K) = WO(J)
               P(I, J, K) = PO(J)
               C(I, J, K) = CO(J)
 80         CONTINUE
 60      CONTINUE
 50   CONTINUE
*
C     Z-sweep is carried out for a complete time step DT
*
      DO 90 J = 1, JCELLS
         DO 100 I = 1, JCELLS
*
C           Left and right limits in z-sweep are set
*
            MLEF = KLIM(1, I, J)
            MRIG = KLIM(2, I, J)
*
C           Boundary conditions for z-sweep are set
*
            MBCL = KBCS(1, I, J)
            MBCR = KBCS(2, I, J)
*
            DO 110 K = MLEF, MRIG
*
C              Initial data are stored in one-dimensional arrays
*
               DO(K) = D(I, J, K)
               UO(K) = U(I, J, K)
               VO(K) = V(I, J, K)
               WO(K) = W(I, J, K)
               PO(K) = P(I, J, K)
               CO(K) = C(I, J, K)
 110        CONTINUE
*
C           Solver in the z-direction is called
C           Note order of velocity components in argument list
*
            CALL ONEDIM(DT,DZ,DO,WO,UO,VO,PO,CO)
*
C           Store solution back in three-dimensional array
*
            DO 120 K = MLEF, MRIG
*
               D(I, J, K) = DO(K)
               U(I, J, K) = UO(K)
               V(I, J, K) = VO(K)
               W(I, J, K) = WO(K)
               P(I, J, K) = PO(K)
               C(I, J, K) = CO(K)
 120        CONTINUE
 100     CONTINUE
 90   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE, TIME, TIMEOU, DT)
*
C     Purpose: to apply CFL condition to compute time step DT
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL     C, CFLCOE, D, DT, DTL, DX, DY, DZ, P, SPX, SPY, SPZ,
     &         TIME, TIMEOU, U, V, W
*
      INTEGER  I, IDIM, J, JDIM, K, KDIM, ICELLS, JCELLS, KCELLS
*
      PARAMETER (IDIM = 100, JDIM = 100, KDIM = 100)
*
      DIMENSION D(IDIM,JDIM,KDIM),U(IDIM,JDIM,KDIM),
     &          V(IDIM,JDIM,KDIM),W(IDIM,JDIM,KDIM),
     &          P(IDIM,JDIM,KDIM),C(IDIM,JDIM,KDIM)
*
      COMMON /SOLUTI/ D, U, V, W, P, C
      COMMON /MESHXY/ DX, DY, DZ, ICELLS, JCELLS, KCELLS
*
      DT = 1.0E+10
*
      DO 10 I = 1, ICELLS
         DO 20 J = 1, JCELLS
            DO 30 K = 1, KCELLS
*
C              Find characteristic speeds in each direction
*
               SPX = C(I,J,K) + ABS(U(I,J,K))
               SPY = C(I,J,K) + ABS(V(I,J,K))
               SPZ = C(I,J,K) + ABS(W(I,J,K))
*
C              Find characteristic lengths in each direction
*
C              Find local time DTL for cell (i, j, k)
*
               DTL  = MIN(DX/SPX, DY/SPY, DZ/SPZ)
*
               IF(DTL.LT.DT)DT = DTL
 30         CONTINUE
 20      CONTINUE
 10   CONTINUE
*
C     Scale time step DT by CFL coefficient
*
      DT = CFLCOE*DT
*
C     Reduce size of DT if TIMEOU is exceeded
*
      IF((TIME + DT).GT.TIMEOU)THEN
         DT = TIMEOU - TIME
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE OUTPUT(PSCALE)
*
C     Purpose: to print out solution at TIMEOU time to files:
C              e3wcsx.out (solution in x-direction through centre)
C              e3wcsy.out (solution in y-direction through centre)
C              e3wcsz.out (solution in z-direction through centre)
C              e3wcs2.out (2D solution through z-constant plane)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER   I, IDIM, ICELLS, IH, J, JCELLS, JDIM, JH, K, KDIM,
     &          KCELLS, KH
*
      REAL      C, D, DX, DY, DZ, P, PSCALE, U, V, W, POS, XV, YV,
     &          ZV, XCM, YCM, ZCM, VEL
*
      PARAMETER (IDIM = 100, JDIM = 100, KDIM = 100)
*
      DIMENSION D(IDIM,JDIM,KDIM),U(IDIM,JDIM,KDIM),
     &          V(IDIM,JDIM,KDIM),W(IDIM,JDIM,KDIM),
     &          P(IDIM,JDIM,KDIM),C(IDIM,JDIM,KDIM),
     &          XV(-1:IDIM+1,-1:JDIM+1,-1:KDIM+1),
     &          YV(-1:IDIM+1,-1:JDIM+1,-1:KDIM+1),
     &          ZV(-1:IDIM+1,-1:JDIM+1,-1:KDIM+1)
*
      COMMON /SOLUTI/ D, U, V, W, P, C
      COMMON /MESHXY/ DX, DY, DZ, ICELLS, JCELLS, KCELLS
      COMMON /VERTEX/ XV, YV, ZV
*
      OPEN(UNIT = 1, FILE = 'e3wcsx.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 2, FILE = 'e3wcsy.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 3, FILE = 'e3wcsz.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 4, FILE = 'e3wcs2.out', STATUS = 'UNKNOWN')
*
      IH = ICELLS/2
      JH = JCELLS/2
      KH = KCELLS/2
*
C     Print out solution along a J = JH, K = KH (constant) slice
C     Use "gnu" script 'gnu1d' to plot slices
*
      DO 10 I = 1, ICELLS
         POS = (REAL(I) - 0.5)*DX
         VEL = SQRT(U(I,JH,KH)**2+V(I,JH,KH)**2+W(I,JH,KH)**2)
         WRITE(1,20)POS, D(I,JH,KH), VEL, P(I,JH,KH)/PSCALE
 10   CONTINUE
*
C     Print out solution along a I = IH, K = KH (constant) slice
*
      DO 30 J = 1, JCELLS
         POS = (REAL(J) - 0.5)*DY
         VEL = SQRT(U(IH,J,KH)**2+V(IH,J,KH)**2+W(IH,J,KH)**2)
         WRITE(2,20)POS, D(IH,J,KH), VEL, P(IH,J,KH)/PSCALE
 30   CONTINUE
*
*
C     Print out solution along a I = IH, J = JH (constant) slice
*
      DO 40 K = 1, KCELLS
         POS = (REAL(K) - 0.5)*DZ
         VEL = SQRT(U(IH,JH,K)**2+V(IH,JH,K)**2+W(IH,JH,K)**2)
         WRITE(3,20)POS, D(IH,JH,K), VEL, P(IH,JH,K)/PSCALE
 40   CONTINUE
*
      CLOSE(1)
      CLOSE(2)
      CLOSE(3)
*
C     Print out two-dimensional numerical solution to file
C     for plotting purposes. A "gnu" script 'gnu2d' is given
C     as an example. The user may apply other more advanced
C     graphics packages, for which the data output format
C     may have to be changed accordingly
*
      WRITE(4,50) '#', ICELLS, JCELLS
*
      K = KH
*
      DO 70 J = 1, JCELLS
         DO 80 I = 1, ICELLS
*
C           Compute coordinates of centre of mass
*
            XCM = 0.125*(XV(I-1,J-1,K-1)+ XV(I,J-1,K-1) +
     &                   XV(I,J,K-1)    + XV(I-1,J,K-1) +
     &                   XV(I-1,J-1,K)  + XV(I,J-1,K)   +
     &                   XV(I,J,K)      + XV(I-1,J,K))
*
            YCM = 0.125*(YV(I-1,J-1,K-1)+ YV(I,J-1,K-1) +
     &                   YV(I,J,K-1)    + YV(I-1,J,K-1) +
     &                   YV(I-1,J-1,K)  + YV(I,J-1,K)   +
     &                   YV(I,J,K)      + YV(I-1,J,K))
*
            ZCM = 0.125*(ZV(I-1,J-1,K-1)+ ZV(I,J-1,K-1) +
     &                   ZV(I,J,K-1)    + ZV(I-1,J,K-1) +
     &                   ZV(I-1,J-1,K)  + ZV(I,J-1,K)   +
     &                   ZV(I,J,K)      + ZV(I-1,J,K))
*
            VEL = SQRT(U(I,J,K)**2+V(I,J,K)**2+W(I,J,K)**2)
*
            WRITE(4,90)XCM, YCM, D(I,J,K), VEL, P(I,J,K)/PSCALE
 80      CONTINUE
         WRITE(4,*)
 70   CONTINUE
*
      CLOSE(4)
*
 20   FORMAT(F10.6,2X,3(F12.4,1X))
 90   FORMAT(2(F10.6,1X),2X,3(F12.4,1X))
 50   FORMAT(A1,1X,I6,I6)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MESHER
*
C     Purpose: to generate mesh for 3D Cartesian domain (trivial).
C              Coordinates XV(,), YV(,) and ZV(,) of cell vertices
C              are calculated and stored to be used in OUTPUT routine
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, ICELLS, IDIM, J, JCELLS, JDIM , K, KDIM, KCELLS
*
      REAL     DX, DY, DZ, XP, YP, ZP, XV, YV, ZV
*
      PARAMETER (IDIM = 100, JDIM = 100, KDIM = 100)
*      
      DIMENSION XV(-1:IDIM+1,-1:JDIM+1,-1:KDIM+1),
     &          YV(-1:IDIM+1, -1:JDIM+1,-1:KDIM+1),
     &          ZV(-1:IDIM+1, -1:JDIM+1,-1:KDIM+1)
*
      COMMON /MESHXY/ DX, DY, DZ, ICELLS, JCELLS, KCELLS
      COMMON /VERTEX/ XV, YV, ZV
*
      DO 10 I = 0, ICELLS
         XP = REAL(I)*DX
         DO 20 J = 0, JCELLS
            YP = REAL(J)*DY
            DO 30 K = 0, KCELLS
               ZP = REAL(K)*DZ
               XV(I,J,K) = XP
               YV(I,J,K) = YP
               ZV(I,J,K) = ZP
 30         CONTINUE
 20      CONTINUE
 10   CONTINUE
*
      END
*
C-----------------------------------------------------------------------C
*
      SUBROUTINE BCONDI(D, U, V, W, P, C)
*
C     Purpose: to set boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER MLEF, MBCL, MRIG, MBCR, IDIM
*
      REAL    D, U, V, W, P, C
*
      PARAMETER (IDIM = 100)

      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2),
     &          W(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2)
*
      COMMON /WALLSS/ MLEF, MRIG, MBCL, MBCR
*
C     Set boundary conditions on the left/bottom
*
      D(MLEF - 2) = D(MLEF + 1)
      V(MLEF - 2) = V(MLEF + 1)
      W(MLEF - 2) = W(MLEF + 1)
      P(MLEF - 2) = P(MLEF + 1)
      C(MLEF - 2) = C(MLEF + 1)
*
      D(MLEF - 1) = D(MLEF)
      V(MLEF - 1) = V(MLEF)
      W(MLEF - 1) = W(MLEF)
      P(MLEF - 1) = P(MLEF)
      C(MLEF - 1) = C(MLEF)
*
      IF(MBCL.EQ.0)THEN
*
C        Transmissive boundary conditions
*
         U(MLEF - 2) = U(MLEF + 1)
         U(MLEF - 1) = U(MLEF)
*
      ELSE
*
C        Reflective boundary conditions at left end
*
         U(MLEF - 2) = -U(MLEF + 1)
         U(MLEF - 1) = -U(MLEF)
*
      ENDIF
*
C     Set boundary conditions on the right/top
*
      D(MRIG + 2) = D(MRIG - 1)
      V(MRIG + 2) = V(MRIG - 1)
      W(MRIG + 2) = W(MRIG - 1)
      P(MRIG + 2) = P(MRIG - 1)
      C(MRIG + 2) = C(MRIG - 1)
*
      D(MRIG + 1) = D(MRIG)
      V(MRIG + 1) = V(MRIG)
      W(MRIG + 1) = W(MRIG)
      P(MRIG + 1) = P(MRIG)
      C(MRIG + 1) = C(MRIG)
*
      IF(MBCR.EQ.0)THEN
*
C        Transmissive boundary conditions
*
         U(MRIG + 2) = U(MRIG - 1)
         U(MRIG + 1) = U(MRIG)
*
      ELSE
*
C        Reflective boundary conditions
*
         U(MRIG + 2) = -U(MRIG - 1)
         U(MRIG + 1) = -U(MRIG)
*
      ENDIF
*
      END
*
*-----------------------------------------------------------------------*
*
      SUBROUTINE ONEDIM(DTS,DS,D,U,V,W,P,C)
*
C     Purpose: to compute the WAF flux with the HLLC Riemann solver.
C              See Chaps. 10, 14, and 16 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IDIM, IUPW, K, LIMITE, MLEF, MRIG, MBCL, MBCR
*
      REAL    D, U, V, W, P, C, CS,
     &        DL, UL, VL, WL, PL, CL, DR, UR, VR, WR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        CN, CSL, CSR, DTS, DS, DTODS, DLOC, DUPW,
     &        ENEL, ENER, FDAL, FDAR, FLX, FSL, FSR, RATIO,
     &        SL, SM, SR, TOLLIM, WAFLIM, WJ, WS, WLL, WSL,
     &            WSR, WRR, CDL, CDR, FDL, FDR
*
      PARAMETER (IDIM = 100)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2),
     &          W(-1:IDIM+2), P(-1:IDIM+2), C(-1:IDIM+2),
     &          CS(5,-1:IDIM+2),FDAL(5,-1:IDIM+2),FDAR(5,-1:IDIM+2),
     &          FLX(5,-1:IDIM+2),FSL(5,-1:IDIM+2),FSR(5,-1:IDIM+2),
     &          WS(5,-1:IDIM+2),WJ(5,-1:IDIM+2),CN(5),CSL(5),CSR(5),
     &          CDL(5),CDR(5),FDL(5),FDR(5),WAFLIM(5)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /TVDCON/ LIMITE
      COMMON /WALLSS/ MLEF, MRIG, MBCL, MBCR
*
      DATA TOLLIM /1.0E-06/
*
C     Apply boundary conditions
*
      CALL BCONDI(D, U, V, W, P, C)
*
      DO 10 I = MLEF, MRIG
*
C        Compute conserved variables on data within domain
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
         ENER    = U(I)*U(I) + V(I)*V(I) + W(I)*W(I)
         CS(3,I) = 0.5*D(I)*ENER + P(I)/G8
         CS(4,I) = D(I)*V(I)
         CS(5,I) = D(I)*W(I)
*
 10   CONTINUE
*         
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 20 I = MLEF - 2, MRIG + 1
*
         DL = D(I)
         UL = U(I)
         VL = V(I)
         WL = W(I)
         PL = P(I)
         CL = C(I)
*
         DR = D(I + 1)
         UR = U(I + 1)
         VR = V(I + 1)
         WR = W(I + 1)
         PR = P(I + 1)
         CR = C(I + 1)
*
C        Evaluate conserved variables and fluxes on data
*
         CALL CONFLX(DL, UL, VL, WL, PL, CDL, FDL)
         CALL CONFLX(DR, UR, VR, WR, PR, CDR, FDR)
*
C     -----------------------------------------------------------------
C     HLLC Approximate Riemann Solver starts
C     -----------------------------------------------------------------
*
C        Calculate estimates for wave speeds using adaptive
C        approximate-state Riemann solvers
*
         CALL ESTIME(SL, SM, SR)
*
C        Compute star states U*L and U*R
*
         ENEL   = CDL(3)/DL + (SM - UL)*(SM + PL/(DL*(SL - UL)))
         ENER   = CDR(3)/DR + (SM - UR)*(SM + PR/(DR*(SR - UR)))
*
         CSL(1) = DL*(SL - UL)/(SL - SM)
         CSL(2) = CSL(1)*SM
         CSL(3) = CSL(1)*ENEL
*
         CSR(1) = DR*(SR - UR)/(SR - SM)
         CSR(2) = CSR(1)*SM
         CSR(3) = CSR(1)*ENER
*
C        Compute the first three components of star fluxes
C        F*L and F*R and store them
*
         DO 30 K = 1, 3
            FSL(K,I) = FDL(K) + SL*(CSL(K) - CDL(K))
            FSR(K,I) = FDR(K) + SR*(CSR(K) - CDR(K))
 30      CONTINUE
*
C     -----------------------------------------------------------------
C     HLLC Approximate Riemann Solver ends
C     -----------------------------------------------------------------
*
C        Store wave speeds for TVD condition
*
         WS(1,I) = SL
         WS(2,I) = SM
         WS(3,I) = SR
         WS(4,I) = SM
         WS(5,I) = SM
*
C        Store wave density jumps for TVD condition
*
         WJ(1,I) = CSL(1) - CDL(1)
         WJ(2,I) = CSR(1) - CSL(1)
         WJ(3,I) = CDR(1) - CSR(1)
         WJ(4,I) = VR     - VL
         WJ(5,I) = WR     - WL
*
C        Store first three components of fluxes evaluated on data
*
         DO 21 K = 1, 3
            FDAL(K, I) = FDL(K)
            FDAR(K, I) = FDR(K)
 21      CONTINUE
*
 20   CONTINUE
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux starts
C     -----------------------------------------------------------------
*
      DTODS = DTS/DS
*
      DO 40 I = MLEF - 1, MRIG
*
C        Apply TVD condition
*
         DO 50 K = 1, 5
*
C           Compute Courant numbers for each wave
*
            CN(K) = WS(K, I)*DTODS
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
C           Retrieve local and upwind wave jumps
*
            DLOC = WJ(K, I)
            DUPW = WJ(K, I + IUPW)
*
C           Modify small jumps preserving sign
*
            IF(ABS(DUPW).LT.TOLLIM)DUPW = TOLLIM*SIGN(1.0, DUPW)
            IF(ABS(DLOC).LT.TOLLIM)DLOC = TOLLIM*SIGN(1.0, DLOC)
*
C           Compute RATIO of upwind jump to local jump
*
            RATIO = DUPW/DLOC
*
C           Select limiter function WAFLIM
*
C           LIMITE = 1, Godunov's Method
C           LIMITE = 2, Second Order Method (oscillatory)
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
         WLL = 0.5*(1.0 + WAFLIM(1))
         WSL = 0.5*(WAFLIM(2) - WAFLIM(1))
         WSR = 0.5*(WAFLIM(3) - WAFLIM(2))
         WRR = 0.5*(1.0 - WAFLIM(3))
*
C        Compute WAF intercell flux (I,I+1) and store in I
*
         DO 80  K = 1,3
            FLX(K,I) = WLL*FDAL(K,I) + WSL*FSL(K,I)  +
     &                 WSR*FSR(K,I)  + WRR*FDAR(K,I)
 80      CONTINUE
*
C        Compute components 4 and 5 of intercell fluxes
*
          WLL      = 0.5*(1.0 + WAFLIM(4))
          WRR      = 0.5*(1.0 - WAFLIM(4))
          FLX(4,I) = WLL*FLX(1,I)*V(I) + WRR*FLX(1,I)*V(I+1)
*
          WLL      = 0.5*(1.0 + WAFLIM(5))
          WRR      = 0.5*(1.0 - WAFLIM(5))
          FLX(5,I) = WLL*FLX(1,I)*W(I) + WRR*FLX(1,I)*W(I+1)
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux ends
C     -----------------------------------------------------------------
*
 40   CONTINUE
*
      DO 90 I = MLEF, MRIG
*
C        Update conserved variables
*
         DO 100 K = 1, 5
*
            CS(K,I) = CS(K,I) - DTODS*(FLX(K,I) - FLX(K,I-1))
*
 100         CONTINUE
*
C        Compute physical variables
*
         D(I) = CS(1,I)
         U(I) = CS(2,I)/D(I)
         V(I) = CS(4,I)/D(I)
         W(I) = CS(5,I)/D(I)
         ENER = 0.5*(CS(2,I)*U(I) + CS(4,I)*V(I) + CS(5,I)*W(I))
         P(I) = G8*(CS(3,I) - ENER)
         C(I) = SQRT(GAMMA*P(I)/D(I))
*
 90   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CONFLX(D, U, V, W, P, CS, FX)
*
C     Purpose: to calculate conserved variables CS
C              and fluxes FX on data
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL     GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &         D, U, V, W, P, CS, FX
*
      DIMENSION CS(5), FX(5)
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
C     Calculate conserved variables
*
      CS(1) = D
      CS(2) = D*U
      CS(3) = 0.5*D*(U*U + V*V + W*W) + P/G8
*
C     Calculate fluxes
*
      FX(1) = D*U
      FX(2) = FX(1)*U + P
      FX(3) = U*(CS(3) + P)
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
C              Theory is found in Section 9.5, Chapter 9 of
C              Reference 1
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
*           Select Two-Rarefaction Riemann solver
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


