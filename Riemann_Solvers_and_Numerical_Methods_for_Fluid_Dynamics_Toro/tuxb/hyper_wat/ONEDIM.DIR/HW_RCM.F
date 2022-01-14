*
*----------------------------------------------------------------------*
*                                                                      *
C     Random Choice Method (RCM) for the time-dependent                *
C     one dimensional non-linear Shallow Water Equations               *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional             *
C              Shallow Water Equations by the Random Choice            *
C              Method (RCM) on a non-staggered grid, with van der      *
C              Corput sequences for sampling exact solutions of        *
C              local Riemann problems                                  *
*                                                                      *
C     Program name: HW-RCM                                             *
*                                                                      *
C     Input  file: swrcm.ini (initial data)                            *
C     Output file: swrcm.out (numerical results)                       *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 19th August 2000                                  *
*                                                                      *
C     REFERENCES                                                       *
*                                                                      *
C     1. Toro, E. F., "Shock-Capturing Methods for                     *
C                      Free-Surface Shallow Flows"                     *
C                      John Wiley and Sons 2000                        *   
*                                                                      *
C     2. Toro, E. F., "Riemann Solvers and Numerical                   *
C                      Methods for Fluid Dynamics"                     *
C                      Springer-Verlag, 1997                           *
C                      Second Edition, 1999                            *
*                                                                      *                        
C     This program is part of HYPER-WAT,                               *
C     a sub-library of                                                 *
*                                                                      *
C     3. Toro, E. F., "NUMERICA: A Library of Source Codes for         *
C                      Teaching, Research and Applications"            *     
C                      NUMERITEK LTD, 1999,                            *
C                      Website: www.numeritek.com                      *
*                                                                      *
*----------------------------------------------------------------------*
*
C     Driver program
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, N, NFREQU, NTMAXI
*
      REAL    CFLCOE, TIME, TIMDIF, TIMEOU, TIMTOL
*
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
*
      DATA TIME, TIMTOL /0.0, 1.0E-06/
*
C     Parameters of problem are read in from file "rcm.ini"
*
      CALL READER
*
C     Initial conditions are set up
*
      CALL INITIA 
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
C        Numerical solution is found by the Random Choice Method
*
         CALL RCMMET(CELLS, N)
*
         IF(MOD(N,NFREQU).EQ.0)WRITE(6,20)N, TIME, TIMEOU
*
C        Check output time
*
         TIMDIF = ABS(TIME - TIMEOU)
*
         IF(TIMDIF.LE.TIMTOL)THEN
*
C           Solution is written to "rcm.out' at specified time
*
            CALL OUTPUT(CELLS)
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
C     DOMLEN    : Channel length
C     GATE1     : Position of gate 1
C     CELLS     : Number of computing cells
C     GRAVIT    : Acceleration due to gravity
C     TIMEOU    : Output time
C     DLINIT    : Initial depth on left section of channel
C     ULINIT    : Initial velocity on left section of channel
C     DMINIT    : Initial depth on middle section of channel
C     UMINIT    : Initial velocity on middle section of channel
C     DRINIT    : Initial depth on right section of channel
C     URINIT    : Initial velocity on right section of channel
C     GATE2     : Position of gate 2
C     CFLCOE    : Courant number coefficient
C     IBCLEF    : Type of left boundary conditions
C     IBCRIG    : Type of right boundary conditions
C     NFREQU    : Output frequency to screen
C     NTMAXI    : Maximum number of time steps
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, CELLS, NFREQU, NTMAXI
*
      REAL    CFLCOE, DOMLEN, GATE1, GATE2, GRAVIT, TIMEOU, 
     &        DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT 
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, GATE1, GATE2
      COMMON /ACCELE/ GRAVIT
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
      COMMON /INISTA/ DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT
*
      OPEN(UNIT = 1, FILE = 'rcm.ini', STATUS = 'UNKNOWN')
*
      READ(1,*)DOMLEN
      READ(1,*)GATE1
      READ(1,*)CELLS
      READ(1,*)GRAVIT
      READ(1,*)TIMEOU
      READ(1,*)DLINIT
      READ(1,*)ULINIT
      READ(1,*)DMINIT
      READ(1,*)UMINIT
      READ(1,*)DRINIT
      READ(1,*)URINIT
      READ(1,*)GATE2
      READ(1,*)CFLCOE
      READ(1,*)IBCLEF
      READ(1,*)IBCRIG
      READ(1,*)NFREQU
      READ(1,*)NTMAXI
*
      CLOSE(1)
*
C     Input data is echoed to screen
*
      WRITE(6,*)
      WRITE(6,*)'Input data echoed to screen'
      WRITE(6,*)
      WRITE(6,*)'DOMLEN = ',DOMLEN
      WRITE(6,*)'GATE1  = ',GATE1
      WRITE(6,*)'CELLS  = ',CELLS
      WRITE(6,*)'GRAVIT = ',GRAVIT
      WRITE(6,*)'TIMEOU = ',TIMEOU
      WRITE(6,*)'DLINIT = ',DLINIT
      WRITE(6,*)'ULINIT = ',ULINIT
      WRITE(6,*)'DMINIT = ',DMINIT
      WRITE(6,*)'UMINIT = ',UMINIT
      WRITE(6,*)'DRINIT = ',DRINIT
      WRITE(6,*)'URINIT = ',URINIT
      WRITE(6,*)'GATE2  = ',GATE2
      WRITE(6,*)'CFLCOE = ',CFLCOE
      WRITE(6,*)'IBCLEF = ',IBCLEF
      WRITE(6,*)'IBCRIG = ',IBCRIG
      WRITE(6,*)'NFREQU = ',NFREQU
      WRITE(6,*)'NTMAXI = ',NTMAXI
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE INITIA 
*
C     Purpose: to set initial conditions
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM, NFREQU, NTMAXI            
*
      REAL    D, DOMLEN, DT, DX, GATE1, GATE2, XPOS, CFLCOE, TIMEOU,
     &        U, DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT 
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /DOMAIN/ DOMLEN, GATE1, GATE2
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
      COMMON /INISTA/ DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT
      COMMON /PRIMIT/ D, U
      COMMON /MESHPA/ DT, DX
*
C     Calculate mesh size DX
*
      DX = DOMLEN/REAL(CELLS)
*
C     Set initial data in channel of length DOMLEN, which is divided
C     into 3 sections by gates at positions GATE1 and GATE2
*
      DO 10 I = 1, CELLS
*
         XPOS = (REAL(I) - 0.5)*DX
*
         IF(XPOS.LE.GATE1)THEN
*
C           Set initial values in left section of domaim
*
            D(I) = DLINIT
            U(I) = ULINIT
         ENDIF
*
         IF(XPOS.GT.GATE1.AND.XPOS.LE.GATE2)THEN
*
C           Set initial values in middle section of domaim
*
            D(I) = DMINIT
            U(I) = UMINIT
         ENDIF

         IF(XPOS.GT.GATE2)THEN
*
C           Set initial values in right section of domaim
*
            D(I) = DRINIT
            U(I) = URINIT
         ENDIF
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
      REAL    D, U
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /BOUNDA/ IBCLEF, IBCRIG
*
      IF(IBCLEF.EQ.0)THEN
*
C        Transmissive boundary conditions on the left
*
         D(0) =  D(1)
         U(0) =  U(1)
*
      ELSE
*
C        Reflective boundary conditions on the left
*
         D(0) =  D(1)
         U(0) = -U(1)
*
      ENDIF
*
      IF(IBCRIG.EQ.0)THEN
*
C        Transmissive boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) =  U(CELLS)
*
      ELSE
*
C        Reflective boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) = -U(CELLS)
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
      REAL    C, CFLCOE, D, DT, DX, GRAVIT, SMAX, SBEXTD, TIME,
     &        TIMEOU, U
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), C(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /ACCELE/ GRAVIT
      COMMON /CELERI/ C
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
         C(I)   = SQRT(GRAVIT*D(I))
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
      SUBROUTINE OUTPUT(CELLS)
*
C     Purpose: to output the solution at a specified time TIMEOU
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER I, CELLS, IDIM
*
      REAL    D, DT, DX, U, XPOS 
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2)
*
      COMMON /PRIMIT/ D, U
      COMMON /MESHPA/ DT, DX
*
      OPEN(UNIT = 1, FILE = 'rcm.out', STATUS = 'UNKNOWN')
*
      DO 10 I  = 1, CELLS
         XPOS = (REAL(I) - 0.5)*DX
         WRITE(1,20)XPOS, D(I), U(I)
 10   CONTINUE
*
      CLOSE(1)
*
 20   FORMAT(3(F14.6,2X))
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RCMMET(CELLS,N)
*
C     Purpose: to compute solution by the Random Choice Method
C              on non-staggered grid using van der Corput sequences.
C              For details see Chap. 7 of Ref. 2 and original
C              references therein. See also Chap. 9 of Ref. 2.
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER CELLS, I, IBIAS, IDIM, N
*
      REAL    C, CL, CR, D, DL, DR, DSAM, DT, DTODX, DX,
     &        THETA, S, U, UL, UR, USAM, DRCM, URCM
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), C(-1:IDIM+2),
     &          DRCM(-1:IDIM+2), URCM(-1:IDIM+2) 
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /PRIMIT/ D, U 
      COMMON /CELERI/ C
      COMMON /MESHPA/ DT, DX
*
C     Van de Corput pseudo-random number THETA is generated
C     at each time level N
*
      CALL RANDOM(THETA, N)
*
      DTODX  = DT/DX
*
      IF(THETA.LE.0.5)THEN
         IBIAS = 0
      ELSE
         IBIAS = 1
      ENDIF
*
C     Compute solution sampling 'speed'
*
      S = (THETA - REAL(IBIAS))/DTODX
*
*     Sweep along x-axis
*
      DO 10 I = 1, CELLS
*
         DL = D(I-1 + IBIAS)
         UL = U(I-1 + IBIAS)
         CL = C(I-1 + IBIAS)
*
         DR = D(I   + IBIAS)
         UR = U(I   + IBIAS)
         CR = C(I   + IBIAS)
*
C        Sample exact solution of local Riemann problems.
*
         CALL RIEMAN(DSAM, USAM, S)
*   
C        Store sampled values DSAM, USAM to cell i
*
         DRCM(I) = DSAM
         URCM(I) = USAM
*
 10   CONTINUE
*
C     Assing RCM sampled solution to array of unknowns
*
      DO 20 I = 1, CELLS
*
         D(I) =  DRCM(I)  
         U(I) =  URCM(I)
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RIEMAN(DSAM, USAM, S)
*
      IMPLICIT NONE
*
C     Declaration of variables
*   
      REAL    CL, CR, DCRIT, DL, DR, DSAM, S, UL, UR, USAM                  
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
*
C     Use the "depth positivity condition" to identify  
C     type of data and thus of solution and to call 
C     appropriate exact solver
*
      DCRIT = (UR-UL) - 2.0*(CL+CR)
*
      IF(DL.LE.0.0.OR.DR.LE.0.0.OR.DCRIT.GE.0.0)THEN
*
C        Dry bed cases
*
         CALL DRYBED(DSAM, USAM, S)
*
      ELSE
*
C        Wet bed case
*
         CALL WETBED(DSAM, USAM, S) 
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE WETBED(DSAM, USAM, S)
*
C     Purpose: to solve the Riemann problem exactly for
C              the wet-bed case
*
      IMPLICIT NONE
*
C     Declaration of variables
*    
      INTEGER  IT, NITER        
*
      REAL     CHA, CL, CR, CS, D0, DL, DR, DS, DSAM, FL, FLD, FR,
     &          FRD, GRAVIT, S, TOL, UL, UR, US, USAM      
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /ACCELE/ GRAVIT
      COMMON /STARSO/ CS, DS, US
      DATA NITER,TOL/50,1.0E-06/
*
C     Find starting value for iteration
*
      CALL STARTE 
*
C     Store starting value in D0
*
      D0 = DS
*
C     Start iteration
*
      DO 10 IT = 1, NITER
* 
         CALL GEOFUN(FL,FLD,DS,DL,CL)
         CALL GEOFUN(FR,FRD,DS,DR,CR)
         DS  = DS - (FL + FR + UR-UL)/(FLD + FRD)
         CHA = ABS(DS-D0)/(0.5*(DS+D0))
         IF(CHA.LE.TOL)GOTO 20
         IF(DS.LT.0.0)DS = TOL
         D0 = DS
*
 10   CONTINUE
*
      WRITE(6,*)'Number of NITER iterations exceeded, 
     &           STOP'
      WRITE(6,*)IT,NITER
*
      STOP
*
 20   CONTINUE
*
C     Converged solution for depth DS in Star Region. 
C     Compute velocity US and CS in Star Region
*
      US = 0.5*(UL + UR) + 0.5*(FR - FL)
*
      CS = SQRT(GRAVIT*DS)
*
C     Sample solution  
*
      CALL SAMWET(DSAM, USAM, S)
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE GEOFUN(F, FD, D, DK, CK)
*
C     Purpose: to evaluate functions FL, FR and their 
C              derivatives in iterative Riemann solver, 
C              for wet-bed case.
*
      IMPLICIT NONE
*
C     Declaration of variables
* 
      REAL   C, CK, D, DK, F, FD, GES, GRAVIT  
*  
      COMMON /ACCELE/ GRAVIT
*
      IF(D.LE.DK)THEN
*
C        Wave is rarefaction wave (or depression)
*
         C  = SQRT(GRAVIT*D)
         F  = 2.0*(C-CK)
         FD = GRAVIT/C
      ELSE
*
C        Wave is shock wave (or bore)
*
         GES = SQRT(0.5*GRAVIT*(D+DK)/(D*DK))
         F   = (D-DK)*GES
         FD  = GES - 0.25*GRAVIT*(D-DK)/(GES*D*D)
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE STARTE 
*
C     Purpose: to provide starting value for Newton-Raphson 
C              iteration. The Two-Rarefaction Riemann 
C              Solver (TRRS) and Two-Shock Riemann Solver
C              (TSRS) are used adaptively
*
      IMPLICIT NONE
*
C     Declaration of variables
*   
      REAL     CL, CR, CS, DL, DMIN, DR, DS, GEL, GER, GRAVIT,
     &         UL, UR, US  
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /STARSO/ CS, DS, US
      COMMON /ACCELE/ GRAVIT
*
      DMIN = MIN(DL,DR)
*
C     Use Two-Rarefaction (TRRS) solution as starting value
*
      DS = (1.0/GRAVIT)*(0.5*(CL+CR)-0.25*(UR-UL))**2
*
      IF(DS.LE.DMIN)THEN
*
C        Use Two-Rarefaction (TSRS) approximation as 
C        starting value
*
      ELSE
*
C        Use two-shock (TSRS) solution as starting value
C        with DS as computed from TRRS as estimate
*
*
         GEL = SQRT(0.5*GRAVIT*(DS+DL)/(DS*DL))
         GER = SQRT(0.5*GRAVIT*(DS+DR)/(DS*DR))
         DS  = (GEL*DL + GER*DR - (UR-UL))/(GEL + GER)
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMWET(D, U, S)
*
C     Purpose: to sample solution through wave structure at
C              TIMOUT for wet-bed case
*
      IMPLICIT NONE
*
C     Declaration of variables
*    
      REAL    C, CL, CR, CS, D, DL, DR, DS, GRAVIT, QL, QR, S, SHL,
     &        SHR, SL, SR, STL, STR, U, UL, UR, US             
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /STARSO/ CS, DS, US
      COMMON /ACCELE/ GRAVIT
*
      IF(S.LE.US)THEN
********************************************
C        Sample left wave
********************************************
         IF(DS.GE.DL)THEN
*
C           Left shock
*
            QL = SQRT((DS + DL)*DS/(2.0*DL*DL))
            SL = UL - CL*QL
*
            IF(S.LE.SL)THEN
*           
C              Sample point lies to the left of the shock
*
               D = DL
               U = UL
            ELSE
*           
C              Sample point lies to the right of the shock
*
               D = DS
               U = US
            ENDIF
         ELSE
*
C           Left rarefaction
*
            SHL = UL - CL
*
            IF(S.LE.SHL)THEN
*           
C              Sample point lies to the right of the 
C              rarefaction
*
               D = DL
               U = UL
            ELSE
*
               STL = US - CS
*
               IF(S.LE.STL)THEN
*           
C                 Sample point lies inside the rarefaction
*
                  U = (UL + 2.0*CL + 2.0*S)/3.0
                  C = (UL + 2.0*CL - S)/3.0
                  D = C*C/GRAVIT
               ELSE
*           
C                 Sample point lies in the STAR region
*
                  D = DS
                  U = US
               ENDIF
            ENDIF
         ENDIF
*
      ELSE
********************************************
C        Sample right wave
********************************************
*
         IF(DS.GE.DR)THEN
*
C           Right shock
*
            QR = SQRT((DS + DR)*DS/(2.0*DR*DR))
            SR = UR + CR*QR
*
            IF(S.GE.SR)THEN
*
C              Sample point lies to the right of the shock
*
               D = DR
               U = UR
            ELSE
*
C              Sample point lies to the left of the shock
*
               D = DS
               U = US
            ENDIF
*
         ELSE
*
C           Right rarefaction
*
            SHR = UR + CR
*
            IF(S.GE.SHR)THEN
*
C              Sample point lies to the right of the 
C              rarefaction
*
               D = DR
               U = UR
            ELSE
*
               STR = US + CS
*
               IF(S.GE.STR)THEN
*
C                 Sample point lies inside the rarefaction
*
                  U = (UR  - 2.0*CR + 2.0*S)/3.0
                  C = (-UR + 2.0*CR + S)/3.0
                  D = C*C/GRAVIT           
               ELSE
*
C                 Sample point lies in the STAR region
*
                  D = DS
                  U = US
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE DRYBED(DSAM, USAM, S)
*
C     Pupose: to compute the exact solution in the case 
C             in which a portion of dry bed is present
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    CL, CR, DL, DR, DSAM, S, UL, UR, USAM         
*          
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
*
C
      IF(DL.LE.0.0)THEN
*
C        Left state is dry
*
         CALL SAMLEF(DSAM,USAM,S)
      ELSE
         IF(DR.LE.0.0)THEN
*
C           Right state is dry
*
            CALL SAMRIG(DSAM,USAM,S)
         ELSE
*
C           Middle state is dry
*
            CALL SAMMID(DSAM,USAM,S)
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMLEF(D, U, S)
*
C     Purpose: to sample the solution through the wave 
C              structure at time TIMOUT, for the case in
C              which the left state is dry. Solution 
C              consists of single right rarefaction
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   C, CL, CR, D, DL, DR, GRAVIT, S, SHR, STR, U, UL, UR 
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /ACCELE/ GRAVIT
*
      SHR = UR + CR
*
      IF(S.GE.SHR)THEN
*
C        Sampling point lies to the right of the 
C        rarefaction
*
         D = DR
         U = UR
      ELSE
*
         STR = UR-2.0*CR
*
         IF(S.GE.STR)THEN
*
C           Sampling point lies inside the rarefaction
*
            U = ( UR - 2.0*CR + 2.0*S)/3.0
            C = (-UR + 2.0*CR + S)/3.0
            D = C*C/GRAVIT	          
         ELSE
*
C           Sampling point lies in dry-bed state
*
            D = DL
            U = UL
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMMID(D, U, S)
*
C     Purpose: to sample the solution through the wave 
C              structure at time TIMOUT, for the case in 
C              which the middle state is dry. Solution 
C              consists of a left and a right rarefaction 
C              with a dry portion in the the middle
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL    C, CL, CR, D, DL, DR, GRAVIT, S, SHL, SHR, SSL, SSR,
     &        U, UL, UR         
*
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /ACCELE/ GRAVIT
*
C     Compute wave speeds
*
      SHL = UL - CL
      SSL = UL + 2.0*CL
      SSR = UR - 2.0*CR
      SHR = UR + CR
*
      IF(S.LE.SHL)THEN
*
C        Sampling point lies to the left of the left 
C        rarefaction
*
         D = DL
         U = UL
      ENDIF
*
      IF(S.GT.SHL.AND.S.LE.SSL)THEN
*
C        Sampling point lies inside the left rarefaction
*
         U = (UL + 2.0*CL + 2.0*S)/3.0
         C = (UL + 2.0*CL - S)/3.0
         D = C*C/GRAVIT
      ENDIF
*
      IF(S.GT.SSL.AND.S.LE.SSR)THEN
*
C        Sampling point lies inside the middle dry bed region
*
         D = 0.0
         U = 0.0
      ENDIF
*
      IF(S.GT.SSR.AND.S.LE.SHR)THEN
*
C        Sampling point lies inside the right rarefaction
*
         U = ( UR - 2.0*CR + 2.0*S)/3.0
         C = (-UR + 2.0*CR + S)/3.0
         D = C*C/GRAVIT	           
      ENDIF
*
      IF(S.GT.SHR)THEN
*
C        Sampling point lies to the right of the right 
C        rarefaction
*
         D = DR
         U = UR
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE SAMRIG(D, U, S)
*
C     Purpose: to sample the solution through the wave 
C              structure at time TIMOUT, for the case in 
C              which the right state is dry. Solution 
C              consists of single left rarefaction
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      REAL   C,CL,CR,D,DL,DR,GRAVIT,S,SHL,STL,U,UL,UR 
* 
      COMMON /STATES/ CL, DL, UL, CR, DR, UR
      COMMON /ACCELE/ GRAVIT
*
      SHL = UL - CL
*
      IF(S.LE.SHL)THEN
*
C        Sampling point lies to the left of the rarefaction
*
         D = DL
         U = UL
      ELSE
*
         STL = UL + 2.0*CL
*
         IF(S.LE.STL)THEN
*
C           Sampling point lies inside the rarefaction
*
            U = (UL + 2.0*CL + 2.0*S)/3.0
            C = (UL + 2.0*CL - S)/3.0
            D = C*C/GRAVIT
         ELSE
*
C           Sampling point lies in right dry-bed state
*
            D = DR
            U = UR
         ENDIF
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE RANDOM(THETA, N)
*
C     Purpose: to generate a sequence of van der Corput pseudo
C              random numbers to be used with the Random Choice
C              Method. Parameters K1, K2 may be chosen by user,
C              with K1 > K2 and relatively prime
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  I, J, K1, K2, L, N, NN
*
      REAL     THETA
*
      DATA K1, K2 /5, 3/
*
      THETA = 0.0
      I     = 0
*
 10   NN    = N/(K1**I)
*
      IF(NN.LT.1)GOTO 20
*
      L     = MOD(NN, K1)
      J     = MOD(K2*L, K1)
      THETA = THETA + REAL(J)/(K1**(I + 1))
      I     = I + 1
*
      GOTO 10
*
 20   CONTINUE
*
      END
*
*----------------------------------------------------------------------*
*

