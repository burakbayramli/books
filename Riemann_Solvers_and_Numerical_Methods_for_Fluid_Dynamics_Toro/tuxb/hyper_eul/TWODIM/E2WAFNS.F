*
*----------------------------------------------------------------------*
*                                                                      *
*     Split Finite Volume WAF scheme for the time-dependent            *
C     two dimensional Euler equations for ideal gases                  *
*                                                                      *
C     Name of program: HE-E2WAFNS                                      *
*                                                                      *
C     Purpose: to solve the two-dimensional Euler equations for an     *
C              ideal gas on a non-Cartesian domain with vertical       *
C              left and right boundaries. The TVD Weigthed Average     *
C              Flux (WAF) method is used in conjunction with the       *
C              HLLC approximate Riemann solver and a second-order      *
C              dimensional splitting scheme. A choice of six limiter   *
C              functions is available. The program is set up to solve  *
C              a sample problem consisting of the shock reflection     *
C              from a wedge placed at an angle to the shock            *
C              propagation direction                                   *
*                                                                      *
C     Input  file: e2wafns.ini (initial data)                          *
C     Output file: e2wnsb.out  (slice through bottom boundary)         *
C     Output file: e2wnst.out  (slice through top boundary )           *
C     Output file: e2wns2.out  (full 2d results)                       *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 31st May 1999                                     *
*                                                                      *
C     Theory is found in Ref. 1, Chap. 16, and original                *
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
C     Published by NUMERITEK LTD,                                      *
C     Website: www.numeritek.com                                       *
*                                                                      *
*     *************************                                        *
C     *  The Code at a Glance *                                        *
*     *************************                                        *
*                                                                      *
C     CALL WEDGE (Defines the problem)                                 *
C          CALL WEDBOU (Defines solid boundaries)                      *
C     CALL MESHER (Generates mesh)                                     *
*                                                                      *
C-----Time stepping begins                                             *
*                                                                      *
C         CALL CFLCON (CFL condition)                                  *
C         CALL SWEEPS (Apply dimensional splitting)                    *
C              CALL ONEDIM (Dimensional sweeps)                        *
C                   CALL BCONDI (Boundary conditions)                  *
C                        CALL ROTAFW (Forward rotation of data)        *
C                        CALL ROTABK (Backward rotation of data)       *
C                   CALL ROTAFW (Forward rotation of data)             *
C                   CALL CONFLX (Local flux evaluation)                *
C                   CALL ESTIME (Speed estimates)                      *
C                   CALL SUPERA (or other flux limiter)                *
C                   CALL ROTABK (Backward rotation of flux)            *
C         CALL OUTPUT (Output results)                                 *
C                                                                      *
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
      INTEGER  N, NFREQU, NTMAXI
*
      REAL     CFLCOE, DT, PSCALE, TIME, TIMEOU, TIMETO
*
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, PSCALE, TIMEOU
*
C     The problem is defined
*
      CALL WEDGE
*
C     Mesh is generated
*
      CALL MESHER
*
      TIME   = 0.0
      TIMETO = 1.0E-07
*
C     Time stepping starts
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
C           Solution written to files 'e2wnsb.out, 'e2wnst.out and
C           'e2wnc2.out' at time = TIMEOU
*
            CALL OUTPUT(PSCALE)
*
            WRITE(6,*)'---------------------------------------------'
            WRITE(6,*)'   Number of time steps = ',N
            WRITE(6,*)'---------------------------------------------'
*
            GOTO 30
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
      SUBROUTINE WEDGE
*
C     Purpose: to read initial data for WEDGE Problem
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER BCXLEF, BCXRIG, BCYBOT, BCYTOP, I, IBCS, ICELLS, IDIM,
     &        ILIM, J, JBCS, JCELLS, JDIM, JLIM, LIMITE, NFREQU,
     &        NTMAXI
*
      REAL    GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        DAHEAD, UAHEAD, VAHEAD, PAHEAD,CAHEAD,
     &        DBEHIND, UBEHIND, VBEHIND, PBEHIND, CBEHIND,
     &        C, CFLCOE, D, DDEN, DNUM, DOMLEX, DX, P, PSCALE,
     &        SHOCKSP, SHOMAC, SHOMSQ, SHOPOS, TIMEOU, U, V, XP,
     &        WEDAPE, WEDANG
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM, JDIM), U(IDIM, JDIM), V(IDIM, JDIM),
     &          P(IDIM, JDIM), C(IDIM, JDIM),
     &          ILIM(2,-1:IDIM+2), IBCS(2,-1:IDIM+2),
     &          JLIM(2,-1:IDIM+2), JBCS(2,-1:IDIM+2)
*
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /PROBLE/ CFLCOE, NFREQU, NTMAXI, PSCALE, TIMEOU
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /TVDCON/ LIMITE
*
C     Definition of input variables
*
C     ICELLS : Number of computing cells in I-direction
C     JCELLS : Number of computing cells in J-direction
C     DOMLEX : Domain length in I-direction
C     WEDAPE : Wedge appex
C     WEDANG : Wedge angle
C     SHOMAC : Shock Mach number
C     SHOPOS : Shock initial position
C     TIMEOU : Output time
C     BCXLEF : Boundary condition on the left
C     BCXRIG : Boundary condition on the right
C     BCYBOT : Boundary condition on the bottom
C     BCYTOP : Boundary condition on the top
C     CFLCOE : Courant number coefficient
C     NFREQU : Output frequency to screen
C     NTMAXI : Maximum number of time steps
C     GAMMA  : Ratio of specific heats
C     DAHEAD : Initial density ahead of shock
C     UAHEAD : Initial u-velocity ahead of shock
C     VAHEAD : Initial v-velocity ahead of shock
C     PAHEAD : Initial pressure ahead of shock
C     PSCALE : Pressure scaling factor for printing
C     LIMITE : Limiter function
*
C     Initial data is read in
*
      OPEN(UNIT = 1,FILE = 'e2wafns.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)ICELLS
      READ(1,*)JCELLS
      READ(1,*)DOMLEX
      READ(1,*)WEDAPE
      READ(1,*)WEDANG
      READ(1,*)SHOMAC
      READ(1,*)SHOPOS
      READ(1,*)TIMEOU
      READ(1,*)BCXLEF
      READ(1,*)BCXRIG
      READ(1,*)BCYBOT
      READ(1,*)BCYTOP
      READ(1,*)CFLCOE
      READ(1,*)NFREQU
      READ(1,*)NTMAXI
      READ(1,*)GAMMA
      READ(1,*)DAHEAD
      READ(1,*)UAHEAD
      READ(1,*)VAHEAD
      READ(1,*)PAHEAD
      READ(1,*)PSCALE
      READ(1,*)LIMITE
*
      CLOSE(1)
*
C     Echoe input data to screen
*
      WRITE(6,*)
      WRITE(6,*)'Input data'
      WRITE(6,*)
      WRITE(6,*)'ICELLS =',ICELLS
      WRITE(6,*)'JCELLS =',JCELLS
      WRITE(6,*)'DOMLEX =',DOMLEX
      WRITE(6,*)'WEDAPE =',WEDAPE
      WRITE(6,*)'WEDANG =',WEDANG
      WRITE(6,*)'SHOMAC =',SHOMAC
      WRITE(6,*)'SHOPOS =',SHOPOS
      WRITE(6,*)'TIMEOU =',TIMEOU
      WRITE(6,*)'BCXLEF =',BCXLEF
      WRITE(6,*)'BCXRIG =',BCXRIG
      WRITE(6,*)'BCYBOT =',BCYBOT
      WRITE(6,*)'BCYTOP =',BCYTOP
      WRITE(6,*)'CFLCOE =',CFLCOE
      WRITE(6,*)'NFREQU =',NFREQU
      WRITE(6,*)'NTMAXI =',NTMAXI
      WRITE(6,*)'GAMMA  =',GAMMA
      WRITE(6,*)'DAHEAD =',DAHEAD
      WRITE(6,*)'UAHEAD =',UAHEAD
      WRITE(6,*)'VAHEAD =',VAHEAD
      WRITE(6,*)'PAHEAD =',PAHEAD
      WRITE(6,*)'PSCALE =',PSCALE
      WRITE(6,*)'LIMITE =',LIMITE
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
      G8  = GAMMA - 1.0
*
C     Compute I-mesh spacing DX (constant)
*
      DX = DOMLEX/REAL(ICELLS)
*
C     Left and right boundaries are assumed vertical.
C     Bottom and top boundary profiles are defined.
C     Routine WEDBOU needs wedge data WEDAPE and WEDHEI
*
      CALL WEDBOU(DOMLEX, WEDAPE, WEDANG)
*
C     Initial conditions are set up
*
C     For given state ahead and shock Mach number,
C     the state behind shock is calculated
*
      SHOMSQ  = SHOMAC*SHOMAC
      PBEHIND = PAHEAD*(2.0*GAMMA*SHOMSQ - G8)/(GAMMA + 1.0)
      DNUM    = 1.0 + ((GAMMA + 1.0)/G8)*PBEHIND/PAHEAD
      DDEN    = PBEHIND/PAHEAD + (GAMMA + 1.0)/G8
      DBEHIND = DAHEAD*(DNUM/DDEN)
      CAHEAD  = SQRT(GAMMA*PAHEAD/DAHEAD)
      UBEHIND = (1.0 - (G8*SHOMSQ + 2.0)/((GAMMA + 1.0)*SHOMSQ))
      UBEHIND = CAHEAD*SHOMAC*UBEHIND
      SHOCKSP = SHOMAC*CAHEAD
      VBEHIND = 0.0
      CBEHIND = SQRT(GAMMA*PBEHIND/DBEHIND)
*
      WRITE(6,*)
      WRITE(6,*)'  Sate behind shock'
      WRITE(6,*)
      WRITE(6,*)'SHOMAC  =              ',SHOMAC
      WRITE(6,*)'SHOCKSP =              ',SHOCKSP
      WRITE(6,*)'DBEHIND =              ',DBEHIND
      WRITE(6,*)'UBEHIND =              ',UBEHIND
      WRITE(6,*)'PBEHIND =              ',PBEHIND/PSCALE
      WRITE(6,*)'CBEHIND =              ',CBEHIND
      WRITE(6,*)'UBEHIND - CBEHIND =    ',UBEHIND-CBEHIND
      WRITE(6,*)
*
      DO 10 J = 1, JCELLS
         DO 20 I = 1, ICELLS
            XP = (REAL(I) - 0.5)*DX
            IF(XP.LE.SHOPOS)THEN
               D(I,J) = DBEHIND
               U(I,J) = UBEHIND
               V(I,J) = VBEHIND
               P(I,J) = PBEHIND
            ELSE
               D(I,J) = DAHEAD
               U(I,J) = UAHEAD
               V(I,J) = VAHEAD
               P(I,J) = PAHEAD
            ENDIF
*
C           Compute sound speed
*
            C(I,J) = SQRT(GAMMA*P(I,J)/D(I,J))
*
 20      CONTINUE
 10   CONTINUE
*
      DO 30 J = 1, JCELLS
*
C        Set limits in I-direction
*
         ILIM(1, J) = 1
         ILIM(2, J) = ICELLS
*
C        Set boundary conditions in I-direction
*
         IBCS(1, J) = BCXLEF
         IBCS(2, J) = BCXRIG
*
 30   CONTINUE
*
      DO 40 I = 1, ICELLS
*
C        Set limits in J-direction
*
         JLIM(1, I) = 1
         JLIM(2, I) = JCELLS
*
C        Set boundary conditions in J-direction
*
         JBCS(1, I) = BCYBOT
         JBCS(2, I) = BCYTOP
*
 40   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SWEEPS(DT)
*
C     Purpose: to apply sweeps in the I and J directions in the
C              solution of the two-dimensional Euler equations.
C              Dimensional splitting scheme used is second-order
C              accurate in time
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IBCS, ILIM, J, JBCS, JDIM, JLIM, IDIM, MLEF, MBCL,
     &        MRIG, MBCR, ICELLS, JCELLS
*
      REAL    C, CO, D, DO, DT, DTS, DX, P, PO, U, UO, V, VO,
     &        AO, AREA2, SCOSE, SINTL, SLENG, SSENO, XCOSE,
     &        XINTL, XLENG, XSENO, YCOSE, YINTL, YLENG, YSENO
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
C     One-dimensional arrays are given in terms of IDIM, assumed to be
C     the largest of the two parameter values IDIM, JDIM
*
      DIMENSION D(IDIM,JDIM),U(IDIM,JDIM),V(IDIM,JDIM),P(IDIM,JDIM),
     &          C(IDIM,JDIM), AREA2(IDIM, JDIM),
     &          XINTL(0:IDIM, 0:JDIM), XLENG(0:IDIM, 0:JDIM),
     &          XCOSE(0:IDIM, 0:JDIM), XSENO(0:IDIM, 0:JDIM),
     &          YINTL(0:IDIM, 0:JDIM), YLENG(0:IDIM, 0:JDIM),
     &          YCOSE(0:IDIM, 0:JDIM), YSENO(0:IDIM, 0:JDIM),
     &          DO(-1:IDIM+2),UO(-1:IDIM+2),VO(-1:IDIM+2),
     &          PO(-1:IDIM+2),CO(-1:IDIM+2),
     &          AO(-1:IDIM+2),SINTL(-1:IDIM+2),SLENG(-1:IDIM+2),
     &          SCOSE(-1:IDIM+2),SSENO(-1:IDIM+2),
     &          ILIM(2,-1:IDIM+2),IBCS(2,-1:IDIM+2),JLIM(2,-1:IDIM+2),
     &          JBCS(2,-1:IDIM+2)
*
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /INDICE/ ILIM, IBCS, JLIM, JBCS
      COMMON /XYINTL/ XINTL, YINTL
      COMMON /GEOM2D/ AREA2, XLENG, XCOSE, XSENO, YLENG, YCOSE, YSENO
*
C     I-sweep is carried out for half a time step
*
      DTS = 0.5*DT
*
      DO 10 J = 1, JCELLS
*
C        Left and right limits in I-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 20 I = MLEF-1, MRIG
*
C           Initial data and geometrical data are stored
C           in one-dimensional arrays
*
            IF(I.GE.MLEF)THEN
               DO(I) = D(I, J)
               UO(I) = U(I, J)
               VO(I) = V(I, J)
               PO(I) = P(I, J)
               CO(I) = C(I, J)
               AO(I) = AREA2(I, J)
            ENDIF
            SINTL(I) = XINTL(I, J)
            SLENG(I) = XLENG(I, J)
            SCOSE(I) = XCOSE(I, J)
            SSENO(I) = XSENO(I, J)
*
 20     CONTINUE
*
C       Solver in the I-direction is called.
*
        CALL ONEDIM(MLEF, MRIG, MBCL, MBCR, DTS, DO, UO, VO, PO, CO,
     &              AO, SINTL, SLENG, SCOSE, SSENO)
*
C       Store one-dimensional solution back in two-dimensional array
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
C     J-sweep is carried out for a complete time step
*
      DTS = DT
*
      DO 40 I = 1, ICELLS
*
C        Left and right limits in J-sweep are set
*
         MLEF = JLIM(1, I)
         MRIG = JLIM(2, I)
*
C        Boundary conditions are set
*
         MBCL = JBCS(1, I)
         MBCR = JBCS(2, I)
*
         DO 50 J = MLEF-1, MRIG
*
C           Initial data and geometrical data are stored
C           in one-dimensional arrays
*
            IF(J.GE.MLEF)THEN
               DO(J) = D(I, J)
               UO(J) = U(I, J)
               VO(J) = V(I, J)
               PO(J) = P(I, J)
               CO(J) = C(I, J)
               AO(J) = AREA2(I, J)
            ENDIF
            SINTL(J) = YINTL(I, J)
            SLENG(J) = YLENG(I, J)
            SCOSE(J) = YCOSE(I, J)
            SSENO(J) = YSENO(I, J)
*
 50      CONTINUE
*
C        Solver in the J-direction is called
*
         CALL ONEDIM(MLEF, MRIG, MBCL, MBCR, DTS, DO, UO, VO, PO, CO,
     &               AO, SINTL, SLENG, SCOSE, SSENO)
*
C        Store one-dimensional solution back in two-dimensional arrays
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
C     I-sweep is carried out for half a time step
*
      DTS = 0.5*DT
*
      DO 70 J = 1, JCELLS
*
C        Left and right limits in I-sweep are set
*
         MLEF = ILIM(1, J)
         MRIG = ILIM(2, J)
*
C        Boundary conditions are set
*
         MBCL = IBCS(1, J)
         MBCR = IBCS(2, J)
*
         DO 80 I = MLEF-1, MRIG
*
C           Initial data and geometrical data are stored
C           in one-dimensional arrays
*
            IF(I.GE.MLEF)THEN
               DO(I) = D(I, J)
               UO(I) = U(I, J)
               VO(I) = V(I, J)
               PO(I) = P(I, J)
               CO(I) = C(I, J)
               AO(I) = AREA2(I, J)
            ENDIF
            SINTL(I) = XINTL(I, J)
            SLENG(I) = XLENG(I, J)
            SCOSE(I) = XCOSE(I, J)
            SSENO(I) = XSENO(I, J)
*
 80     CONTINUE
*
C       Solver in the I-direction is called
*
        CALL ONEDIM(MLEF, MRIG, MBCL, MBCR, DTS, DO, UO, VO, PO, CO,
     &              AO, SINTL, SLENG, SCOSE, SSENO)
*
C       Store one-dimensional solution back in two-dimensional array
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
      SUBROUTINE ONEDIM(MLEF, MRIG, MBCL, MBCR, DTS, D, U, V, P, C,
     &                  AO, SINTL, SLENG, SCOSE, SSENO)
*
C     Purpose: to compute the TVD WAF flux with the HLLC Riemann
C              solver and evolve the solution by a time DTS. See
C              Chaps. 10, 14 and 16 of Ref. 1
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER LIMITE, I, IDIM, IUPW, K, MLEF, MRIG, MBCL, MBCR
*
      REAL    D, U, V, P, C, CS,
     &        DL, UL, VL, PL, CL, DR, UR, VR, PR, CR,
     &        GAMMA, G1, G2, G3, G4, G5, G6, G7, G8,
     &        CN, CSL, CSR, DTS, DTODS, DLOC, DUPW, ENEL, ENER, FDAL,
     &        FDAR, FLX, FSL, FSR,RATIO, SL, SM, SR, TOLLIM, WAFLIM,
     &        WJ, WS, WL, WSL, WSM, WSR, WR, AO, CDL, CDR, FDL, FDR,
     &        SCOSE, SINTL, SLENG, SSENO
*
      PARAMETER (IDIM = 500)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), V(-1:IDIM+2), P(-1:IDIM+2),
     &          C(-1:IDIM+2),
     &          AO(-1:IDIM+2), SINTL(-1:IDIM+2), SLENG(-1:IDIM+2),
     &          SCOSE(-1:IDIM+2), SSENO(-1:IDIM+2), CS(4,-1:IDIM+2),
     &          FDAL(4,-1:IDIM+2), FDAR(4,-1:IDIM+2), FLX(4,-1:IDIM+2),
     &          FSL(4,-1:IDIM+2), FSR(4,-1:IDIM+2), WS(4,-1:IDIM+2),
     &          WJ(4,-1:IDIM+2), CN(4), CSL(4), CSR(4), CDL(4),CDR(4),
     &          FDL(4), FDR(4), WAFLIM(4)
*
      COMMON /STATES/ DL, UL, PL, CL, DR, UR, PR, CR
      COMMON /GAMMAS/ GAMMA, G1, G2, G3, G4, G5, G6, G7, G8
      COMMON /TVDCON/ LIMITE
*
      DATA TOLLIM /1.0E-06/
*
C     Apply boundary conditions
*
      CALL BCONDI(MLEF, MRIG, MBCL, MBCR, D, U, V, P, C, SCOSE, SSENO)
*
C     Solve Riemann problem (i,i+1) and store quantities in I
*
      DO 10 I = MLEF - 2, MRIG + 1
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
C        Rotate left state (inside cell)
*
         CALL ROTAFW(UL, VL, SCOSE(I), SSENO(I))
*
C        Compute first three components of conserved variables CDL
C        and fluxes FDL on rotated left data state
*
         CALL CONFLX(DL, UL, VL, PL, CDL, FDL)
*
C        Rotate right state (inside cell)
*
         CALL ROTAFW(UR, VR, SCOSE(I), SSENO(I))
*
C        Compute first three components of conserved variables CDR
C        and fluxes FDR on rotated right data state
*
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
C        Compute first three components of star states U*L and U*R
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
C        Compute first three components of star
C        fluxes F*L and F*R and store them
*
         DO 20 K = 1, 3
            FSL(K,I) = FDL(K) + SL*(CSL(K) - CDL(K))
            FSR(K,I) = FDR(K) + SR*(CSR(K) - CDR(K))
 20      CONTINUE
*
C     -----------------------------------------------------------------
C     HLLC Approximate Riemann Solver ends
C     -----------------------------------------------------------------
*
C        Store wave speeds for TVD condition. Note order
*
         WS(1,I) = SL
         WS(2,I) = SM
         WS(3,I) = SR
         WS(4,I) = SM
*
C        Store wave jumps for TVD condition. For first three components
C        use jumps in density across waves 1, 2 and 3. For fourth
C        component use jumps in tangential velocity component across
C        wave 4
*
         WJ(1,I) = CSL(1) - CDL(1)
         WJ(2,I) = CSR(1) - CSL(1)
         WJ(3,I) = CDR(1) - CSR(1)
         WJ(4,I) = VR     - VL
*
C        Store first three components of fluxes on rotated data
*
         DO 30 K = 1, 3
            FDAL(K, I)= FDL(K)
            FDAR(K, I)= FDR(K)
 30      CONTINUE
*
 10   CONTINUE
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux starts
C     -----------------------------------------------------------------
*
      DO 40 I = MLEF - 1, MRIG
*
         DTODS = DTS/SINTL(I)
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
C        Compute weights for first three components of WAF
C        intercell flux
*
         WL  = 0.5*(1.0 + WAFLIM(1))
         WSL = 0.5*(WAFLIM(2) - WAFLIM(1))
         WSR = 0.5*(WAFLIM(3) - WAFLIM(2))
         WR  = 0.5*(1.0 - WAFLIM(3))
*
C        Compute first three components of WAF intercell flux (I,I+1)
C        and store it in I
*
         DO 60  K = 1, 3
*
            FLX(K,I)=WL*FDAL(K,I)+WSL*FSL(K,I)+WSR*FSR(K,I)+WR*FDAR(K,I)
*
C           Multiply flux by side length
*
            FLX(K,I) = SLENG(I)*FLX(K,I)
 60      CONTINUE
*
C        Compute fourth flux component FLX(4,I)
*
         WL = 0.5*(1.0 + WAFLIM(4))
         WR = 0.5*(1.0 - WAFLIM(4))
*
         UL = U(I)
         VL = V(I)
         UR = U(I+1)
         VR = V(I+1)
*
         CALL ROTAFW(UL, VL, SCOSE(I), SSENO(I))
         CALL ROTAFW(UR, VR, SCOSE(I), SSENO(I))
*
         FLX(4,I) = WL*FLX(1,I)*VL + WR*FLX(1,I)*VR
*
C        Rotate flux components FLX(2,I), FLX(4,I) back
*
         CALL ROTABK(FLX(2,I), FLX(4,I), SCOSE(I), SSENO(I))
*
C     -----------------------------------------------------------------
C     Computation of the TVD WAF intercell flux ends
C     -----------------------------------------------------------------
*
 40   CONTINUE
*
      DO 70 I = MLEF, MRIG
*
C        Compute conserved variables on data within domain
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
         CS(3,I) = 0.5* D(I)*(U(I)*U(I) + V(I)*V(I)) + P(I)/G8
         CS(4,I) = D(I)*V(I)
*
 70   CONTINUE
*
      DO 80 I = MLEF, MRIG
*
C        Update conserved variables
*
         DTODS = DTS/AO(I)
*
         DO 90 K = 1, 4
*
            CS(K,I) = CS(K,I) - DTODS*(FLX(K,I) - FLX(K,I-1))
*
 90      CONTINUE
*
C        Compute physical variables
*
         D(I) = CS(1,I)
         U(I) = CS(2,I)/D(I)
         V(I) = CS(4,I)/D(I)
         P(I) = G8*(CS(3,I) - 0.5*(CS(2,I)*U(I) + CS(4,I)*V(I)))
         C(I) = SQRT(GAMMA*P(I)/D(I))
*
 80   CONTINUE
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
      INTEGER  I, IDIM, J, JDIM, ICELLS, JCELLS
*
      REAL     C, CFLCOE, D, DT, DTL, DX, P, SLX, SLY, SPX, SPY,
     &         TIME, TIMEOU, U, V, XINTL, YINTL
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM, JDIM), U(IDIM, JDIM), V(IDIM, JDIM),
     &          P(IDIM, JDIM), C(IDIM, JDIM),
     &          XINTL(0:IDIM, 0:JDIM), YINTL(0:IDIM, 0:JDIM)
*
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /XYINTL/ XINTL, YINTL
      COMMON /MESHXY/ DX, ICELLS, JCELLS
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
            SLX = MIN(XINTL(I-1,J), XINTL(I,J))
            SLY = MIN(YINTL(I,J-1), YINTL(I,J))
*
C           Find local time DTL for cell (i, j)
*
            DTL = MIN(SLX/SPX, SLY/SPY)
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
C              e2wnsb.out (solution along bottom boundary)
C              e2wnst.out (solution along top boundary)
C              e2wns2.out (full 2d solution)
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, IDIM, ICELLS, J, JCELLS, JDIM
*
      REAL    C, D, DX, P, PSCALE, U, V, XPOS, XV, YV, XCM, YCM, VEL
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION D(IDIM, JDIM), U(IDIM, JDIM), V(IDIM, JDIM),
     &          P(IDIM, JDIM), C(IDIM, JDIM),
     &          XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1)
*
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /SOLUTI/ D, U, V, P, C
      COMMON /VERTEX/ XV, YV
*
      OPEN(UNIT = 1, FILE = 'e2wnsb.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 2, FILE = 'e2wnst.out', STATUS = 'UNKNOWN')
      OPEN(UNIT = 3, FILE = 'e2wns2.out', STATUS = 'UNKNOWN')
*
      DO 10 I = 1, ICELLS
         XPOS = (REAL(I) - 0.5)*DX
         VEL  = SQRT(U(I,1)**2 + V(I,1)**2)
*
         WRITE(1,20)XPOS,D(I,1), VEL, P(I,1)/PSCALE
*
         VEL  = SQRT(U(I,JCELLS)**2 + V(I,JCELLS)**2)
*
         WRITE(2,20)XPOS, D(I,JCELLS), VEL, P(I,JCELLS)/PSCALE
 10   CONTINUE
*
      CLOSE(1)
      CLOSE(2)
*
      WRITE(3,30) '#', ICELLS, JCELLS
*
      DO 40 J = 1, JCELLS
         DO 50 I = 1, ICELLS
*
C           Compute coordinates of centre of mass
*
            XCM = 0.25*(XV(I-1,J-1)+XV(I,J-1)+XV(I,J)+XV(I-1,J))
            YCM = 0.25*(YV(I-1,J-1)+YV(I,J-1)+YV(I,J)+YV(I-1,J))
*
            VEL = SQRT(U(I,J)**2 + V(I,J)**2)
*
            WRITE(3,60)XCM, YCM, D(I, J), VEL, P(I, J)/PSCALE
*
 50      CONTINUE
         WRITE(3,*)
 40   CONTINUE
*
      CLOSE(3)
*
 20   FORMAT(F10.6,2X,3(F12.4,1X))
 60   FORMAT(2(F10.6,1X),2X,3(F12.4,1X))
 30   FORMAT(A1,1X,I6,I6)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE BCONDI(MLEF,MRIG,MBCL,MBCR,D,U,V,P,C,SCOSE,SSENO)
*
C     Purpose: to set boundary conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER MLEF, MBCL, MRIG, MBCR, IDIM
*
      REAL    D, U, V, P, C, SCOSE, SSENO
*
      PARAMETER (IDIM = 500)
*
      DIMENSION D(-1:IDIM+2),U(-1:IDIM+2),V(-1:IDIM+2),P(-1:IDIM+2),
     &          C(-1:IDIM+2), SCOSE(-1:IDIM+2), SSENO(-1:IDIM+2)
*
*     --------------------------------------
C     Boundary conditions for left boundary
*     --------------------------------------
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
C     Set orientation of fictitious boundary as that of the
C     real boundary on the left
*
      SCOSE(MLEF - 2) = SCOSE(MLEF - 1)
      SSENO(MLEF - 2) = SSENO(MLEF - 1)
*
C     Rotate forward states MLEF and MLEF + 1
*
      CALL ROTAFW(U(MLEF),  V(MLEF),  SCOSE(MLEF-1),SSENO(MLEF-1))
      CALL ROTAFW(U(MLEF+1),V(MLEF+1),SCOSE(MLEF-1),SSENO(MLEF-1))
*
      IF(MBCL.EQ.0)THEN
*
C        Transmissive boundary conditions at left end
*
         U(MLEF - 2) = U(MLEF + 1)
*
         U(MLEF - 1) = U(MLEF)
*
      ELSE
*
C        Reflective boundary conditions at left end
*
         U(MLEF - 2) = -U(MLEF + 1)
*
         U(MLEF - 1) = -U(MLEF)
*
      ENDIF
*
C     Rotate back so that all data can be looped
*
      CALL ROTABK(U(MLEF-2),V(MLEF-2),SCOSE(MLEF-1),SSENO(MLEF-1))
      CALL ROTABK(U(MLEF-1),V(MLEF-1),SCOSE(MLEF-1),SSENO(MLEF-1))
*
C     Rotate back to recover original data within domain
*
      CALL ROTABK(U(MLEF),  V(MLEF),  SCOSE(MLEF-1),SSENO(MLEF-1))
      CALL ROTABK(U(MLEF+1),V(MLEF+1),SCOSE(MLEF-1),SSENO(MLEF-1))
*
*     --------------------------------------
C     Boundary conditions for right boundary
*     --------------------------------------
*
      D(MRIG + 2) = D(MRIG - 1)
      V(MRIG + 2) = V(MRIG - 1)
      P(MRIG + 2) = P(MRIG - 1)
      C(MRIG + 2) = C(MRIG - 1)
*
      D(MRIG + 1) = D(MRIG)
      V(MRIG + 1) = V(MRIG)
      P(MRIG + 1) = P(MRIG)
      C(MRIG + 1) = C(MRIG)
*
C     Set orientation of fictitious boundary as that of the
C     real boundary on the right
*
      SCOSE(MRIG + 1) = SCOSE(MRIG)
      SSENO(MRIG + 1) = SSENO(MRIG)
*
C     Rotate forward states MRIG - 1 and MRIG
*
      CALL ROTAFW(U(MRIG-1), V(MRIG-1), SCOSE(MRIG),SSENO(MRIG))
      CALL ROTAFW(U(MRIG),   V(MRIG),   SCOSE(MRIG),SSENO(MRIG))
*
      IF(MBCR.EQ.0)THEN
*
C        Transmissive boundary conditions on the right
*
         U(MRIG + 2) = U(MRIG - 1)
         U(MRIG + 1) = U(MRIG)
*
      ELSE
*
C        Reflective boundary conditions on the right
*
         U(MRIG + 2) = -U(MRIG - 1)
         U(MRIG + 1) = -U(MRIG)
*
      ENDIF
*
C     Rotate back so that all data can be looped
*
      CALL ROTABK(U(MRIG + 2),V(MRIG + 2),SCOSE(MRIG),SSENO(MRIG))
      CALL ROTABK(U(MRIG + 1),V(MRIG + 1),SCOSE(MRIG),SSENO(MRIG))
*
C     Rotate back to recover original data within domain
*
      CALL ROTABK(U(MRIG - 1),V(MRIG - 1),SCOSE(MRIG),SSENO(MRIG))
      CALL ROTABK(U(MRIG),    V(MRIG),    SCOSE(MRIG),SSENO(MRIG))
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
      SUBROUTINE ROTAFW(P, Q, COSE, SENO)
*
C     Purpose: to rotate a vector according to side orientation
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   COSE, P, PX, Q, QX, SENO
*
      PX =  COSE*P + SENO*Q
      QX = -SENO*P + COSE*Q
      P  =  PX
      Q  =  QX
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE ROTABK(P, Q, COSE, SENO)
*
C     Purpose: to rotate a vector back to physical space
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   COSE, P, PX, Q, QX, SENO
*
      PX = COSE*P - SENO*Q
      QX = SENO*P + COSE*Q
      P  = PX
      Q  = QX
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CONFLX(D, U, V, P, CS, FX)
*
C     Purpose: to calculate first three components of conserved
C              variables CS and fluxes FX
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
      CS(3) = 0.5*D*(U*U + V*V) + P/G8
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
      SUBROUTINE MESHER
*
C     Purpose: to generate mesh for a two-dimensional non-Cartesian
C              domain with vertical left and right boundaries and
C              prescribed 'well-behaved' bottom and top boundary
C              profiles. All mesh parameters are computed and stored
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, ICELLS, IDIM, J, JCELLS, JDIM
*
      REAL    ALPHAL, ALPHAR, AREA2, AREPOL, DX, DELX, DELY, XA,
     &        XCM, XCOSE, XINTL, XLENG, XSENO, XV, YA, YCM,
     &        YCOSE, YINTL, YLENG, YSENO, YV
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION AREA2(IDIM, JDIM),
     &          XINTL(0:IDIM, 0:JDIM), XLENG(0:IDIM, 0:JDIM),
     &          XCOSE(0:IDIM, 0:JDIM), XSENO(0:IDIM, 0:JDIM),
     &          YINTL(0:IDIM, 0:JDIM), YLENG(0:IDIM, 0:JDIM),
     &          YCOSE(0:IDIM, 0:JDIM), YSENO(0:IDIM, 0:JDIM),
     &          XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1),
     &          XA(10), YA(10)
*
      COMMON /XYINTL/ XINTL, YINTL
      COMMON /GEOM2D/ AREA2,XLENG,XCOSE,XSENO,YLENG,YCOSE,YSENO
*
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /VERTEX/ XV, YV
*
C     Vertices of quadrilateral cells are found
*
      CALL VERTIC
*
C     Compute areas
*
      DO 10 I = 1, ICELLS
          DO 20 J = 1 ,JCELLS
*
C            Store coordinates of all 4 vertices
*
             XA(1) = XV(I-1, J-1)
             XA(2) = XV(I,   J-1)
             XA(3) = XV(I,   J  )
             XA(4) = XV(I-1, J  )
*
             YA(1) = YV(I-1, J-1)
             YA(2) = YV(I,   J-1)
             YA(3) = YV(I,   J  )
             YA(4) = YV(I-1, J  )
*
C            Calculate area of general quadrilateral
*
             CALL AREAPO(4, XA, YA, AREPOL)
*
C            Store area
*
             AREA2(I, J) = AREPOL
 20      CONTINUE
 10   CONTINUE
*
C     Compute side information for I-sweep. In order to use the
C     same intercell side information for two consecutive cells we
C     adopt the convention that the right side has the correct
C     orientation but the left side requires a minus sign in
C     the updating one-dimensional formula. The same convention
C     applies to the J-sweep
*
      DO 30 J = 1, JCELLS
         DO 40 I = 0, ICELLS
            DELX = XV(I, J) - XV(I, J-1)
            DELY = YV(I, J) - YV(I, J-1)
*
C           Compute length of sides for
*
            XLENG(I, J) = SQRT(DELX*DELX + DELY*DELY)
*
C           Compute orientation of sides
*
            XCOSE(I, J) =  DELY/XLENG(I, J)
            XSENO(I, J) = -DELX/XLENG(I, J)
*
C           Compute coordinates (XCM,  YCM) for centre of mass
*
            XCM = 0.25*(XV(I-1,J-1)+XV(I,J-1)+XV(I,J)+XV(I-1,J))
            YCM = 0.25*(YV(I-1,J-1)+YV(I,J-1)+YV(I,J)+YV(I-1,J))
*
C           Compute integration length on the left
*
            CALL INTLEN(XV(I,J-1), YV(I,J-1), XV(I, J), YV(I, J),
     &                  XLENG(I, J), XCM, YCM, ALPHAL)
*
C           Compute integration length on the right
*
            XCM = 0.25*(XV(I,J-1)+XV(I+1,J-1)+XV(I+1,J)+XV(I,J))
            YCM = 0.25*(YV(I,J-1)+YV(I+1,J-1)+YV(I+1,J)+YV(I,J))
*
C           Compute integration length on the right
*
            CALL INTLEN(XV(I,J-1), YV(I,J-1), XV(I, J), YV(I, J),
     &                  XLENG(I, J), XCM, YCM, ALPHAR)
*
C           Compute integration length for the intercell position
*
            XINTL(I, J) = 2.0*MIN(ALPHAL, ALPHAR)
*
 40     CONTINUE
 30   CONTINUE
*
C     Compute side information for J-sweep
*
      DO 50 I = 1, ICELLS
         DO 60 J = 0, JCELLS
            DELX = XV(I-1, J) - XV(I, J)
            DELY = YV(I-1, J) - YV(I, J)
*
C           Compute length of sides
*
            YLENG(I, J) = SQRT(DELX*DELX + DELY*DELY)
*
C           Compute orientation of sides
*
            YCOSE(I, J) =  DELY/YLENG(I, J)
            YSENO(I, J) = -DELX/YLENG(I, J)
*
C           Compute coordinates (XCM,  YCM) for centre of mass
*
            XCM = 0.25*(XV(I,J-1)+XV(I,J)+XV(I-1,J)+XV(I-1,J-1))
            YCM = 0.25*(YV(I,J-1)+YV(I,J)+YV(I-1,J)+YV(I-1,J-1))
*
C           Compute integration length on the left
*
            CALL INTLEN(XV(I,J), YV(I,J), XV(I-1, J), YV(I-1, J),
     &                  YLENG(I, J), XCM, YCM, ALPHAL)
*
C           Compute integration length on the right
*
            XCM = 0.25*(XV(I,J)+XV(I,J+1)+XV(I-1,J+1)+XV(I-1,J))
            YCM = 0.25*(YV(I,J)+YV(I,J+1)+YV(I-1,J+1)+YV(I-1,J))
*
C           Compute integration length on the right
*
            CALL INTLEN(XV(I,J), YV(I,J), XV(I-1, J), YV(I-1, J),
     &                  YLENG(I, J), XCM, YCM, ALPHAR)
*
C           Compute integration length for the intercell position
*
            YINTL(I, J) = 2.0*MIN(ALPHAL, ALPHAR)
*
 60     CONTINUE
 50   CONTINUE
*
      END
*
C----------------------------------------------------------------------C
*
      SUBROUTINE VERTIC
*
C     Purpose: to generate mesh for 2D domain with vertical left
C              and right boundaries and prescribed bottom and top
C              boundaries (XB, YB) and (XT, YT). Coordinates of
C              quadrilateral cell vertices XV(,) and YV(,) are found
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, ICELLS, IDIM, J, JCELLS, JDIM, K, KBMAX, KTMAX
*
      REAL     DELY, DX, SLOPE, X1, X2, XB, XP, XT, XV,
     &         Y1, Y2, YB, YBOT, YT, YTOP, YV
*
      PARAMETER (IDIM = 500, JDIM = 500)
*
      DIMENSION XV(-1:IDIM+1, -1:JDIM+1), YV(-1:IDIM+1, -1:JDIM+1),
     &          XB(IDIM), YB(IDIM), XT(IDIM), YT(IDIM)
*
      COMMON /MESHXY/ DX, ICELLS, JCELLS
      COMMON /VERTEX/ XV, YV
      COMMON /YBOUND/ XB, YB, XT, YT, KBMAX, KTMAX
*
      DO 10 I = 0, ICELLS
*
         XP = REAL(I)*DX
*
C        Find bottom position at grid position XP
*
         DO 20 K = 1, KBMAX
            IF(XB(K).LE.XP.AND.XB(K+1).GE.XP)GOTO 30
 20      CONTINUE
*
 30      X1 = XB(K)
         Y1 = YB(K)
         X2 = XB(K+1)
         Y2 = YB(K+1)
*
C        Interpolate to find bottom position at grid position XP
*
         SLOPE = (Y2 - Y1)/(X2 - X1)
         YBOT  = Y1 + SLOPE*(XP - X1)
*
C        Find top position at grid position XP
*
         DO 40 K = 1, KTMAX
            IF(XT(K).LE.XP.AND.XT(K+1).GE.XP)GOTO 50
 40      CONTINUE
*
 50      X1 = XT(K)
         Y1 = YT(K)
         X2 = XT(K+1)
         Y2 = YT(K+1)
*
C        Interpolate to find bottom position at grid position XP
*
         SLOPE = (Y2 - Y1)/(X2 - X1)
         YTOP  = Y1 + SLOPE*(XP - X1)
*
         DELY = (YTOP - YBOT)/REAL(JCELLS)
*
         DO 60 J = 0, JCELLS
            XV(I,J) = XP
            YV(I,J) = YBOT + REAL(J)*DELY
 60      CONTINUE
 10   CONTINUE
*
C     Apply 'boundary conditions' to vertices
*
      DO 70 J = 0, JCELLS
         XV(-1, J)       = 2.0*XV(0, J) - XV(1, J)
         YV(-1, J)       = YV(1, J)
         XV(ICELLS+1, J) = 2.0*XV(ICELLS, J) - XV(ICELLS-1, J)
         YV(ICELLS+1, J) = YV(ICELLS-1, J)
 70   CONTINUE
*
      DO 80 I = 0, ICELLS
         XV(I, -1)       = XV(I, 1)
         YV(I, -1)       = 2.0*YV(1, 0) - YV(I, 1)
         XV(I, JCELLS+1) = XV(I, JCELLS-1)
         YV(I, JCELLS+1) = 2.0*YV(I, JCELLS) - YV(I, JCELLS-1)
 80   CONTINUE
*
      END
*
C----------------------------------------------------------------------C
*
      SUBROUTINE WEDBOU(DOMLEX, WEDAPE, WEDANG)
*
C     Purpose: to set bottom and top boundaries of 2D domain
C              for assumed vertical left and right boundaries.
C              Here the scheme is applied to the specific case
C              of a wedge at an angle to an oncoming single
C              shock wave
*
C     XB()  : x-coordinate of bottom boundary (x-dependent)
C     YB()  : y-position   of bottom boundary (x-dependent)
C     XT()  : x-coordinate of top    boundary (x-dependent)
C     YT()  : y-position   of top    boundary (x-dependent)
C     KBMAX : Number of points defining bottom boundary
C     KTMAX : Number of points defining top boundary
*
      IMPLICIT NONE
*
      INTEGER  IDIM, KBMAX, KTMAX
*
      REAL     DOMLEX, WEDANG, WEDAPE, WEDHEI, PIE,
     &         XB, XT, YB, YT, YTOP
*
      PARAMETER (IDIM = 500)
*
      DIMENSION XB(IDIM), YB(IDIM), XT(IDIM), YT(IDIM)
*
      COMMON /YBOUND/ XB, YB, XT, YT, KBMAX, KTMAX
*
C     Calculate PI
*
      PIE = 4.0*ATAN(1.0)
C
*     Express wegde angle in radians
*
      WEDANG = WEDANG*PIE/180.0
*
      WEDHEI = (DOMLEX - WEDAPE)*TAN(WEDANG)
      YTOP   = 2.0*WEDHEI
*
      KBMAX  = 3
      KTMAX  = 2
*
C     ----------------Define bottom boundary---------------
*
      XB(1) = 0.0
      YB(1) = 0.0
*
C     Define appex of wedge
*
      XB(2) = WEDAPE
      YB(2) = 0.0
*
C     Define end of wedge profile
*
      XB(3) = DOMLEX
      YB(3) = WEDHEI
*
C     ----------------Define top boundary------------------
*
      XT(1) = 0.0
      YT(1) = YTOP
*
      XT(2) = DOMLEX
      YT(2) = YTOP
*
      END
*
C----------------------------------------------------------------------C
*
      SUBROUTINE  AREAPO(N, X, Y, AREPOL)
*
C     Purpose: to compute area of convex general polygon of N sides
*
      IMPLICIT NONE
*
      DIMENSION X(10), Y(10)
*
C     Declaration of variables
*
      INTEGER I, N
*
      REAL    AREPOL, X, Y
*
      X(N+1) = X(1)
      Y(N+1) = Y(1)
*
      AREPOL = 0.0
*
      DO 10 I = 1, N
          AREPOL = AREPOL + X(I)*Y(I+1) - X(I+1)*Y(I)
 10   CONTINUE
*
      AREPOL = 0.5*AREPOL
*
      END
*
C-----------------------------------------------------------------------C
*
      SUBROUTINE INTLEN(XA, YA, XB, YB, SLEN, XP, YP, ALPHA)
*
C     Purpose: to compute integration length, which is distance
C              between a side (XA,YA)-(XB,YB) and a point (XP, YP),
C              the centre of mass of the adjacent cell
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL  XA, YA, XB, YB, SLEN, XP, YP, ALPHA
*
      ALPHA = ABS((XB - XA)*(YP - YA) - (YB - YA)*(XP - XA))/SLEN
*
      END
*
C-----------------------------------------------------------------------C
*

