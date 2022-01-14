*
*----------------------------------------------------------------------*
*                                                                      *
*     Weighted Average Flux (WAF) scheme for the time-dependent two    *
C     dimensional Euler equations for ideal gases                      *
*                                                                      *
C     Purpose: to solve the two-dimensional Euler equations for an     *
C              ideal gas on a Cartesian quadrilateral domain using     *
C              the TVD WAF method in conjunction with the HLLC         *
C              approximate Riemann solver and dimensional splitting.   *
C              A selection of 6 limiter functions is available         *
*                                                                      *
C     Name of program: HE-E2WAFCS                                      *
*                                                                      *
C     Input  file: e2wafcs.ini (initial data)                          *
C     Output file: e2wafcs1x.out (slice in x-direction)                *
C     Output file: e2wafcs1y.out (slice in y-direction)                *
C     Output file: e2wafcs2d.out (full 2d results)                     *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 15/05/99                                          *
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
C     CALL MESHER (Generates mesh)                                     *
*                                                                      *
C-----Time stepping begins                                             *
*                                                                      *
C         CALL CFLCON (CFL condition)                                  *
C         CALL FLUXES (Fluxes in x and y directions)                   *
C              CALL ONEDIM (Dimensional splitting)                     *
C                   CALL BCONDI (Boundary conditions)                  *
C                   CALL CONFLX (Local flux evaluation)                *
C                   CALL ESTIME (Speed estimates)                      *
C                   CALL SUPERA (or other limiter)                     *
C         CALL OUTPUT (Output results)                                 *
C-----Time stepping end                                                *
*                                                                      *
*----------------------------------------------------------------------*
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
C     Mesh is generated
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
C           Solution written to "e2wafcs.out" at time = TIMEOU
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
 20   FORMAT(I12,6X,2(F12.7, 4X))

 10   CONTINUE
*
      WRITE(6,*)'Maximum number of time steps NTMAXI reached'
*
 30   CONTINUE
*
      END
*----------------------------------------------------------------------*
*
      SUBROUTINE PROBLM
*
C     Purpose: to read initial data for cylindrical explosion and
C              implosion test problems. The domain is divided into
C              the inner and outer sections of a circle. Initial
C              values for density, velocity components and pressure
C              have constant values in each of these regions. For
C              an explosion problem pressure in the inner region
C              is higher than that in the outer region. For an
C              implosion the opposite happens. Initial values in
C              the Cartesian cells cut by the cylinder (circle) are
C              set in proportion to areas inside and outside the
C              cylinder
*
C     Input variables
*
C     DOMLEX    : Domain length in x-direction
C     ICELLS    : Number of computing cells in x-direction
C     DOMLEY    : Domain length in y-direction
C     JCELLS    : Number of computing cells in y-direction
C     RADIUS    : Radius of cylinder
C     XC        : X-coordinate of centre of cylinder
C     YC        : Y-coordinate of centre of cylinder
C     GAMMA     : Ratio of specific heats
C     TIMEOU    : Output time
C     DINS      : Initial density inside circle
C     UINS      : Initial x-velocity inside circle
C     VINS      : Initial y-velocity inside circle
C     PINS      : Initial pressure inside circle
C     DOUT      : Initial density  outside circle
C     UOUT      : Initial x-velocity outside circle
C     VOUT      : Initial y-velocity outside circle
C     POUT      : Initial pressure outside circle
C     BCXLEF    : Boundary condition on the left
C     BCXRIG    : Boundary condition on the right
C     BCYBOT    : Boundary condition on the bottom
C     BCYTOP    : Boundary condition on the top
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
      INTEGER BCXLEF, BCXRIG, BCYBOT, BCYTOP,
     &        I, IBCS, ICELLS, IDIM, ILIM, J, JBCS,
     &        JCELLS, JDIM, JLIM, L, LIMITE, NFREQU, NTMAXI
*
      REAL    GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        D, U, V, P, C,
     &        DINS, UINS, VINS, PINS, UOUT, VOUT, POUT,
     &        CFLCOE, DOMLEX, DOMLEY, DOUT, DX, DY,
     &        ENERGK, PSCALE, RADIUS, TIMEOU, XC, YC, XV,
     &        YV, R, RMINU, RPLUS, RABSO
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed
C     to be the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM, JDIM), U(IDIM, JDIM), V(IDIM, JDIM),
     &          P(IDIM, JDIM), C(IDIM, JDIM),
     &          ILIM(2,-1:IDIM+2), IBCS(2,-1:IDIM+2),
     &          JLIM(2,-1:IDIM+2), JBCS(2,-1:IDIM+2),
     &          XV(4),YV(4), R(4)
*
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, PSCALE, TIMEOU
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /TVDCON/ LIMITE
*
C     Initial data is read in
*
      OPEN(UNIT = 1,FILE = 'e2wafcs.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEX
      READ(1,*)ICELLS
      READ(1,*)DOMLEY
      READ(1,*)JCELLS
      READ(1,*)RADIUS
      READ(1,*)XC
      READ(1,*)YC
      READ(1,*)GAMMA
      READ(1,*)TIMEOU
      READ(1,*)DINS
      READ(1,*)UINS
      READ(1,*)VINS
      READ(1,*)PINS
      READ(1,*)DOUT
      READ(1,*)UOUT
      READ(1,*)VOUT
      READ(1,*)POUT
      READ(1,*)BCXLEF
      READ(1,*)BCXRIG
      READ(1,*)BCYBOT
      READ(1,*)BCYTOP
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
      WRITE(6,*)'RADIUS = ', RADIUS
      WRITE(6,*)'XC     = ', XC
      WRITE(6,*)'YC     = ', YC
      WRITE(6,*)'GAMMA  = ', GAMMA
      WRITE(6,*)'TIMEOU = ', TIMEOU
      WRITE(6,*)'DINS   = ', DINS
      WRITE(6,*)'UINS   = ', UINS
      WRITE(6,*)'VINS   = ', VINS
      WRITE(6,*)'PINS   = ', PINS
      WRITE(6,*)'DOUT   = ', DOUT
      WRITE(6,*)'UOUT   = ', UOUT
      WRITE(6,*)'VOUT   = ', VOUT
      WRITE(6,*)'POUT   = ', POUT
      WRITE(6,*)'BCXLEF = ', BCXLEF
      WRITE(6,*)'BCXRIG = ', BCXRIG
      WRITE(6,*)'BCYBOT = ', BCYBOT
      WRITE(6,*)'BCYTOP = ', BCYTOP
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
C     Define mesh size in the x and y-directions
*
      DX = DOMLEX/REAL(ICELLS)
      DY = DOMLEY/REAL(JCELLS)
*
C     Setup initial conditions
*
      DO 10 J = 1, JCELLS
         YV(1) = (J-1)*DY
         YV(2) = (J-1)*DY
         YV(3) = J*DY
         YV(4) = J*DY
*
         DO 20 I = 1, ICELLS
            XV(1) = (I-1)*DX
            XV(2) = I*DX
            XV(3) = I*DX
            XV(4) = (I-1)*DX
*
            RMINU = 0.0
            RPLUS = 0.0
            RABSO = 0.0
*
            DO 30 L = 1,4
               R(L) = SQRT((XV(L)-XC)**2 + (YV(L)-YC)**2) - RADIUS
               IF(R(L).LE.0.0)RMINU = RMINU + R(L)
                 IF(R(L).GE.0.0)RPLUS = RPLUS + R(L)
               RABSO = RABSO + ABS(R(L))
 30         CONTINUE
*
C           Assign initial values
*
            D(I, J) = (ABS(RMINU)*DINS + RPLUS*DOUT)/RABSO
            U(I, J) = (ABS(RMINU)*UINS + RPLUS*UOUT)/RABSO
            V(I, J) = (ABS(RMINU)*VINS + RPLUS*VOUT)/RABSO
            P(I, J) = (ABS(RMINU)*PINS + RPLUS*POUT)/RABSO
*
C           Compute sound speed
*
            C(I, J) = SQRT(GAMMA*P(I,J)/D(I,J))
*
 20      CONTINUE
 10   CONTINUE
*
      DO 40 J = 1, JCELLS
*
C        Set limits in the x-direction
*
         ILIM(1,J) = 1
         ILIM(2,J) = ICELLS
*
C        Set boundary conditions in the x-direction
*
         IBCS(1,J) = BCXLEF
         IBCS(2,J) = BCXRIG
*
 40   CONTINUE
*
      DO 50 I = 1, ICELLS
*
C        Set limits in y-direction
*
         JLIM(1,I) = 1
         JLIM(2,I) = JCELLS
*
C        Set boundary conditions in the y-direction
*
         JBCS(1,I) = BCYBOT
         JBCS(2,I) = BCYTOP
*
 50   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SWEEPS(DT)
*
C     Purpose: to apply dimensional splitting in the x and
C              y-directions. Second-order time accurate splitting
C              used. For each dimensional sweep a single scheme
C              is used; velocity components U and V are swapped
C              appropriately.
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    C, CO, D, DO, DT, DTS, DX, DY, P, PO, U, UO, V, VO
*
      INTEGER I, IBCS, ILIM, J, JBCS, JDIM, JLIM, IDIM, MLEF, MBCL,
     &        MRIG, MBCR, ICELLS, JCELLS
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),P(IDIM,JDIM),
     &          C(IDIM,JDIM)
      DIMENSION DO(-1:IDIM+2),UO(-1:IDIM+2),VO(-1:IDIM+2),
     &          PO(-1:IDIM+2),CO(-1:IDIM+2)
      DIMENSION ILIM(2,-1:IDIM+2),IBCS(2,-1:IDIM+2),JLIM(2,-1:IDIM+2),
     &          JBCS(2,-1:IDIM+2)
*
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
*
C     x-sweep is carried out for half a time step
*
      DTS = 0.5*DT
*
      DO 10 J = 1, JCELLS
*
C        Left and right limits in x-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions for x-sweep are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 20 I = MLEF, MRIG
*
C           Initial data are stored in one-dimensional arrays
*
            DO(I) = D(I, J)
            UO(I) = U(I, J)
            VO(I) = V(I, J)
            PO(I) = P(I, J)
            CO(I) = C(I, J)
*
 20     CONTINUE
*
C       Solver in the x-direction is called
C       Note order of velocity components in argument list
*
        CALL ONEDIM(MLEF,MRIG,MBCL,MBCR,DTS,DX,DO,UO,VO,PO,CO)
*
C       Store solution back in two-dimensional array
*
        DO 30 I = MLEF, MRIG
*
           D(I, J) = DO(I)
           U(I, J) = UO(I)
           V(I, J) = VO(I)
           P(I, J) = PO(I)
           C(I, J) = CO(I)
*
 30   CONTINUE
*
 10   CONTINUE
*
C     y-sweep is carried out for a complete time step
*
      DTS = DT
*
      DO 40 I = 1, ICELLS
*
C        Left and right limits in y-sweep are set
*
         MLEF = JLIM(1, I)
         MRIG = JLIM(2, I)
*
C        Boundary conditions for y-sweep are set
*
         MBCL = JBCS(1, I)
         MBCR = JBCS(2, I)
*
         DO 50 J = MLEF, MRIG
*
C           Initial data are stored in one-dimensional arrays
*
            DO(J) = D(I, J)
            UO(J) = U(I, J)
            VO(J) = V(I, J)
            PO(J) = P(I, J)
            CO(J) = C(I, J)
 50      CONTINUE
*
C        Solver in the y-direction is called
C        Note order of velocity components in argument list
*
         CALL ONEDIM(MLEF,MRIG,MBCL,MBCR,DTS,DY,DO,VO,UO,PO,CO)
*
C        Store solution back in two-dimensional arrays
*
         DO 60 J = MLEF, MRIG
*
            D(I, J) = DO(J)
            U(I, J) = UO(J)
            V(I, J) = VO(J)
            P(I, J) = PO(J)
            C(I, J) = CO(J)
*
 60      CONTINUE
 40   CONTINUE
*
C     x-sweep is carried out for half a time step
*
      DTS = 0.5*DT
*
      DO 70 J = 1, JCELLS
*
C        Left and right limits in x-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions for x-sweep are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 80 I = MLEF, MRIG
*
C           Initial data are stored in one-dimensional arrays
*
            DO(I) = D(I, J)
            UO(I) = U(I, J)
            VO(I) = V(I, J)
            PO(I) = P(I, J)
            CO(I) = C(I, J)
*
 80     CONTINUE
*
C       Solver in the x-direction is called
C       Note order of velocity components in argument list
*
        CALL ONEDIM(MLEF,MRIG,MBCL,MBCR,DTS,DX,DO,UO,VO,PO,CO)
*
C       Store solution back in two-dimensional array
*
        DO 90 I = MLEF, MRIG
*
           D(I, J) = DO(I)
           U(I, J) = UO(I)
           V(I, J) = VO(I)
           P(I, J) = PO(I)
           C(I, J) = CO(I)
*
 90   CONTINUE
*
 70   CONTINUE
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
      REAL     C, CFLCOE, D, DT, DTL, DX, DY, P, SPX, SPY,
     &         TIME, TIMEOU, U, V
*
      INTEGER  I, IDIM, J, JDIM, ICELLS, JCELLS
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM, JDIM), U(IDIM, JDIM), V(IDIM, JDIM),
     &          P(IDIM, JDIM), C(IDIM, JDIM)
*
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
*
      DT = 1.0E+10
*
      DO 10 I = 1, ICELLS
         DO 20 J = 1, JCELLS
*
C           Find characteristic speeds in each direction
*
            SPX = C(I,J) + ABS(U(I,J))
            SPY = C(I,J) + ABS(V(I,J))
*
C           Find characteristic lengths in each direction
*
C           Find local time DTL for cell (i, j)
*
            DTL = MIN(DX/SPX, DY/SPY)
*
            IF(DTL.LT.DT)DT = DTL
*
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
C              e2wafcs1x.out (solution in x-direction through centre)
C              e2wafcs1y.out (solution in y-direction through centre)
C              e2wafcs2d.out (full 2D solution)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IDIM, ICELLS, IH, J, JCELLS, JDIM, JH
*
      REAL    C, D, DX, DY, P, PSCALE, U, V, POS, XV, YV,
     &        XCM, YCM, VEL
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM, JDIM), U(IDIM, JDIM), V(IDIM, JDIM),
     &          P(IDIM, JDIM), C(IDIM, JDIM),
     &          XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1)
*
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /VERTEX/ XV, YV
*
      OPEN(UNIT = 1, FILE = 'e2wafcs1x.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 2, FILE = 'e2wafcs1y.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 3, FILE = 'e2wafcs2d.out', STATUS = 'UNKNOWN')
*
      IH = ICELLS/2
      JH = JCELLS/2
*
C     Print out solution along a J = JH (constant) slice
C     Use "gnu" script 'gnu1d' to plot slices
*
      DO 10 I = 1, ICELLS
         POS = REAL(I)*DX
         VEL = SQRT(U(I,JH)**2 + V(I,JH)**2)
         WRITE(1,20)POS,D(I,JH),U(I,JH),V(I,JH),P(I,JH)/PSCALE,VEL
 10   CONTINUE
*
C     Print out solution along a I = IH (constant) slice
*
      DO 30 J = 1, JCELLS
         POS = REAL(J)*DY
         VEL = SQRT(U(IH,J)**2 + V(IH,J)**2)
         WRITE(2,20)POS,D(IH,J),U(IH,J),V(IH,J),P(IH,J)/PSCALE,VEL
 30   CONTINUE
*
      CLOSE(1)
      CLOSE(2)
*
C     Print out two-dimensional numerical solution to file
C     for plotting purposes. A "gnu" script 'gnu2d' is given
C     as an example. The user may apply other more advanced
C     graphics packages, for which the data output format
C     may have to be changed accordingly
*
      WRITE(3,40) '#', ICELLS, JCELLS
*
      DO 50 J = 1, JCELLS
         DO 60 I = 1, ICELLS
*
C           Compute coordinates of centre of mass
*
            XCM = 0.25*(XV(I-1,J-1)+XV(I,J-1)+XV(I,J)+XV(I-1,J))
            YCM = 0.25*(YV(I-1,J-1)+YV(I,J-1)+YV(I,J)+YV(I-1,J))
*
            VEL = SQRT(U(I,J)**2 + V(I,J)**2)
*
            WRITE(3,20)XCM,YCM,D(I,J),U(I,J),V(I,J),P(I,J)/PSCALE,VEL
*
 60      CONTINUE
         WRITE(3,*)
 50   CONTINUE
*
      CLOSE(3)
*
 20   FORMAT(2(F10.6,2X),5X,5(F12.4,2X))
 40   FORMAT(A1,1X,I6,I6)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE MESHER
*
C     Purpose: to generate mesh for 2D rectangular domain (trivial).
C              Coordinates XV(,), YV(,) of cell vertices are
C              calculated and stored to be used in OUTPUT routine
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, ICELLS, IDIM, J, JCELLS, JDIM
*
      REAL     DX, DY, XP, YP, XV, YV
*
      PARAMETER (IDIM = 500, JDIM = 500)
*      
      DIMENSION XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1)
*
      COMMON /MESHXY/ DX, DY, ICELLS, JCELLS
      COMMON /VERTEX/ XV, YV
*
      DO 10 I = 0, ICELLS
         XP = REAL(I)*DX
         DO 20 J = 0, JCELLS
            YP = REAL(J)*DY
            XV(I,J) = XP
            YV(I,J) = YP
 20      CONTINUE
 10   CONTINUE
*
      END
*
C-----------------------------------------------------------------------C
*
      SUBROUTINE BCONDI(MLEF, MRIG, MBCL, MBCR, D, U, V, P, C)
*
C     Purpose: to set boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER MLEF, MBCL, MRIG, MBCR, IDIM
*
      REAL    D, U, V, P, C
*
      PARAMETER (IDIM = 500)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2), P(-1:IDIM+2),
     &          C(-1:IDIM+2)
*
C     Set boundary conditions on the left/bottom
*
      D(MLEF - 2) = D(MLEF + 1)
      V(MLEF - 2) = V(MLEF + 1)
      P(MLEF - 2) = P(MLEF + 1)
      C(MLEF - 2) = C(MLEF + 1)
*
      D(MLEF - 1) = D(MLEF)
      V(MLEF - 1) = V(MLEF)
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
      D(MRIG + 2) =  D(MRIG - 1)
      U(MRIG + 2) = -U(MRIG - 1)
      V(MRIG + 2) =  V(MRIG - 1)
      P(MRIG + 2) =  P(MRIG - 1)
      C(MRIG + 2) =  C(MRIG - 1)
*
      D(MRIG + 1) =  D(MRIG)
      U(MRIG + 1) = -U(MRIG)
      V(MRIG + 1) =  V(MRIG)
      P(MRIG + 1) =  P(MRIG)
      C(MRIG + 1) =  C(MRIG)
*
      IF(MBCR.EQ.0)THEN
*
C         Transmissive boundary conditions
*
         U(MRIG + 2) = U(MRIG - 1)
         U(MRIG + 1) = U(MRIG)
*
       ELSE
*
C         Reflective boundary conditions
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
      SUBROUTINE ONEDIM(MLEF,MRIG,MBCL,MBCR,DTS,DS,D,U,V,P,C)
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
      REAL    D, U, V, P, C, CS,
     &        DL, UL, VL, PL, CL, DR, UR, VR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        CN, CSL, CSR, DTS, DS, DTODS, DLOC, DUPW,
     &        ENEL, ENER, FDAL, FDAR, FLX, FSL, FSR, FS24,
     &        FS42, FXSTAR, FSMID, RATIO, SL, SM, SR, TOLLIM,
     &        WAFLIM, WJ, WS, WL, WSL, WSM, WSR, WR,
     &        CDL, CDR, FDL, FDR
*
      PARAMETER (IDIM = 500)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2), P(-1:IDIM+2),
     &          C(-1:IDIM+2)
      DIMENSION CS(4,-1:IDIM+2),FDAL(4,-1:IDIM+2),FDAR(4,-1:IDIM+2),
     &          FLX(4,-1:IDIM+2),FSL(4,-1:IDIM+2),FSR(4,-1:IDIM+2),
     &          FS24(4,-1:IDIM+2),FS42(4,-1:IDIM+2),WS(4,-1:IDIM+2),
     &          WJ(4,-1:IDIM+2),CN(4),CSL(4),CSR(4),CDL(4),CDR(4),
     &          FDL(4),FDR(4), WAFLIM(4), FSMID(4)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /TVDCON/ LIMITE
*
      DATA TOLLIM /1.0E-06/
*
C     Apply boundary conditions
*
      CALL BCONDI(MLEF, MRIG, MBCL, MBCR, D, U, V, P, C)
*
      DO 10 I = MLEF, MRIG
*
C        Compute conserved variables on data within domain
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
         CS(3,I) = D(I)*V(I)
         CS(4,I) = 0.5* D(I)*(U(I)*U(I) + V(I)*V(I)) + P(I)/G8
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
         PL = P(I)
         CL = C(I)
*
         DR = D(I + 1)
         UR = U(I + 1)
         VR = V(I + 1)
         PR = P(I + 1)
         CR = C(I + 1)
*
C        Evaluate conserved variables and fluxes on data
*
         CALL CONFLX(DL, UL, VL, PL, CDL, FDL)
         CALL CONFLX(DR, UR, VR, PR, CDR, FDR)
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
         ENEL   = CDL(4)/DL   + (SM - UL)*(SM + PL/(DL*(SL - UL)))
         ENER   = CDR(4)/DR + (SM - UR)*(SM + PR/(DR*(SR - UR)))
*
         CSL(1) = DL*(SL - UL)/(SL - SM)
         CSL(2) = CSL(1)*SM
         CSL(3) = CSL(1)*VL
         CSL(4) = CSL(1)*ENEL
*
         CSR(1) = DR*(SR - UR)/(SR - SM)
         CSR(2) = CSR(1)*SM
         CSR(3) = CSR(1)*VR
         CSR(4) = CSR(1)*ENER
*
C        Compute and store star fluxes F*L and F*R
*
         DO 30 K = 1, 4
            FSL(K,I) = FDL(K) + SL*(CSL(K) - CDL(K))
            FSR(K,I) = FDR(K) + SR*(CSR(K) - CDR(K))
 30      CONTINUE
*
C     -----------------------------------------------------------------
C     HLLC Approximate Riemann Solver ends
C     -----------------------------------------------------------------
*
C        Compute mixed-state fluxes to resolve shear waves
*
         FS24(1,I) = FSR(1, I)
         FS24(2,I) = FSR(2, I)
         FS24(3,I) = FSR(1, I)*VL
         FS24(4,I) = FSR(4, I) + 0.5*FSR(1, I)*(VL*VL- VR*VR)

         FS42(1,I) = FSL(1, I)
         FS42(2,I) = FSL(2, I)
         FS42(3,I) = FSL(1, I)*VR
         FS42(4,I) = FSL(4, I) + 0.5*FSL(1, I)*(VR*VR- VL*VL)
*
C        Store wave speeds for TVD condition
*
         WS(1,I) = SL
         WS(2,I) = SM
         WS(3,I) = SR
         WS(4,I) = SM
*
C        Store wave density jumps for TVD condition
*
         WJ(1,I) = CSL(1) - CDL(1)
         WJ(2,I) = CSR(1) - CSL(1)
         WJ(3,I) = CDR(1) - CSR(1)
         WJ(4,I) = VR     - VL
*
C        Store fluxes on data
*
         DO 21 K = 1, 4
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
      DO 40 I = MLEF - 1, MRIG
*
         DTODS = DTS/DS
*
C        Apply TVD condition
*
         DO 50 K = 1, 4
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
         WL = 0.5*(1.0 + WAFLIM(1))
*
C        Compute mixed star state to distinguish between
C        shear and contact in the limiting procedure
*
         IF(WAFLIM(2).LE.WAFLIM(4))THEN
*
C            Case: 24
*
            WSL = 0.5*(WAFLIM(2) - WAFLIM(1))
            WSM = 0.5*(WAFLIM(4) - WAFLIM(2))
            WSR = 0.5*(WAFLIM(3) - WAFLIM(4))
*
            DO 60 K = 1, 4
               FSMID(K) = FS24(K,I)
 60         CONTINUE
*
         ELSE
*
C            Case: 42
*        
            DO 70 K = 1, 4
               FSMID(K) = FS42(K,I)
 70         CONTINUE
*
            WSL = 0.5*(WAFLIM(4) - WAFLIM(1))
            WSM = 0.5*(WAFLIM(2) - WAFLIM(4))
            WSR = 0.5*(WAFLIM(3) - WAFLIM(2))
*
         ENDIF
*
         WR = 0.5*(1.0 - WAFLIM(3))
*
C        Compute WAF intercell flux (I,I+1) and store in I
*
         DO 80  K = 1, 4
*
            FXSTAR   = WSL*FSL(K,I) + WSM*FSMID(K) + WSR*FSR(K,I)
            FLX(K,I) = WL*FDAL(K,I) + FXSTAR       + WR*FDAR(K,I)
*
 80      CONTINUE
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
         DO 100 K = 1, 4
*
            CS(K,I) = CS(K,I) - DTODS*(FLX(K,I) - FLX(K,I-1))
*
 100         CONTINUE
*
C        Compute physical variables
*
         D(I) = CS(1,I)
         U(I) = CS(2,I)/D(I)
         V(I) = CS(3,I)/D(I)
         P(I) = G8*(CS(4,I) - 0.5*(CS(2,I)*U(I) + CS(3,I)*V(I)))
         C(I) = SQRT(GAMMA*P(I)/D(I))
*
 90   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CONFLX(D, U, V, P, CS, FX)
*
C     Purpose: to calculate conserved variables CS
C              and fluxes FX on data
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL     GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &         D, U, V, P, CS, FX
*
      DIMENSION CS(4), FX(4)
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
*
C     Calculate conserved variables
*
      CS(1) = D
      CS(2) = D*U
      CS(3) = D*V
      CS(4) = 0.5*D*(U*U + V*V) + P/G8
*
C     Calculate fluxes
*
      FX(1) = D*U
      FX(2) = FX(1)*U + P
      FX(3) = FX(1)*V
      FX(4) = U*(CS(4) + P)
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


