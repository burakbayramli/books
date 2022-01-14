*
*----------------------------------------------------------------------*
*                                                                      *
C     MUSCL-Hancock Method for the time-dependent one dimensional      *
C     non-linear Shallow Water Equations for wet bed situations        *
*                                                                      *
C     Purpose: to solve the time-dependent one dimensional             *
C              Shallow Water Equations by the MUSCL-Hancock method     *
C              in conjunction with the HLL approximate Riemann         *
C              solver                                                  *
*                                                                      *
C     Program name: hw_mush.f                                          *
*                                                                      *
C     Input  file: mush.ini (initial data)                             *
C     Output file: mush.out (numerical results)                        *
*                                                                      *
C     Programer: E. F. Toro                                            *
*                                                                      *
C     Last revision: 19 August 2000                                    *
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
C     Parameters of problem are read in from file "mush.ini"
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
C        Numerical fluxes are calculated using the MUSCL-H method
*
         CALL MUSCLH(CELLS)
*
C        Solution is updated using fluxes in conservative formula
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
C           Solution is written to "mush.out' at specified time
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
C     DOMLEN : Channel length
C     GATE1  : Position of gate 1
C     CELLS  : Number of computing cells
C     GRAVIT : Acceleration due to gravity
C     TIMEOU : Output time
C     DLINIT : Initial depth on left section of channel
C     ULINIT : Initial velocity on left section of channel
C     DMINIT : Initial depth on middle section of channel
C     UMINIT : Initial velocity on middle section of channel
C     DRINIT : Initial depth on right section of channel
C     URINIT : Initial velocity on right section of channel
C     GATE2  : Position of gate 2
C     CFLCOE : Courant number coefficient
C     IBCLEF : Type of left boundary conditions
C     IBCRIG : Type of right boundary conditions
C     NFREQU : Output frequency to screen
C     NTMAXI : Maximum number of time steps
C     LIMITE : Limiter function
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER IBCLEF, IBCRIG, CELLS, LIMITE, NFREQU, NTMAXI
*
      REAL    CFLCOE, DOMLEN, GATE1, GATE2, GRAVIT, TIMEOU, 
     &        DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT 
*
      COMMON /BOUNDA/ IBCLEF, IBCRIG
      COMMON /DOMAIN/ DOMLEN, GATE1, GATE2
      COMMON /ACCELE/ GRAVIT
      COMMON /DRIVER/ CFLCOE, CELLS, NFREQU, NTMAXI, TIMEOU
      COMMON /INISTA/ DLINIT, ULINIT, DMINIT, UMINIT, DRINIT, URINIT
      COMMON /TVDCON/ LIMITE
*
      OPEN(UNIT = 1, FILE = 'mush.ini', STATUS = 'UNKNOWN')
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
      READ(1,*)LIMITE
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
      WRITE(6,*)'LIMITE = ',LIMITE
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
C       Transmissive boundary conditions on the left
*
        D(-1) =  D(2)
        U(-1) = -U(2)
*
        D(0)  =  D(1)
        U(0)  =  U(1)
*
      ELSE
*
C       Reflective boundary conditions on the left
*
        D(-1) =  D(2)
        U(-1) = -U(2)
*
        D(0)  =  D(1)
        U(0)  = -U(1)
*
      ENDIF
*
      IF(IBCRIG.EQ.0)THEN
*
C        Transmissive boundary conditions on the right
*
         D(CELLS + 1) = D(CELLS)
         U(CELLS + 1) = U(CELLS)
*
         D(CELLS + 2) = D(CELLS - 1)
         U(CELLS + 2) = U(CELLS - 1)
C
      ELSE
*
C        Reflective boundary conditions on the right
*
         D(CELLS + 1) =  D(CELLS)
         U(CELLS + 1) = -U(CELLS)
*
         D(CELLS + 2) =  D(CELLS - 1)
         U(CELLS + 2) = -U(CELLS - 1)
*
      ENDIF
*
      END
*
*----------------------------------------------------------------------*
*
      SUBROUTINE CFLCON(CFLCOE,CELLS,N,TIME,TIMEOU)
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
      REAL    C, CFLCOE, D, DT, DX, GRAVIT, SMAX, SPELOC, TIME,
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
C     Find estimate for maximum velocity SMAX present in data
*
      DO 10 I = -1, CELLS + 2
*
C        Compute celerity
*
         C(I)    = SQRT(GRAVIT*D(I))
*
         SPELOC  = ABS(U(I)) + C(I)
         IF(SPELOC.GT.SMAX)SMAX = SPELOC
 10   CONTINUE
*
C     Compute time step DT 
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
      OPEN(UNIT = 1, FILE = 'mush.out', STATUS = 'UNKNOWN')
*
      DO 10 I   = 1, CELLS
         XPOS   = (REAL(I) - 0.5)*DX
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
       SUBROUTINE UPDATE(CELLS)
C
C      Purpose: to update the solution using conservative formula
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  CELLS, I, IDIM, K             
*
      REAL     CS, D, DT, DTODX, DX, FLUX, TOLDEP, U           
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION D(-1:IDIM+2), U(-1:IDIM+2), CS(2,-1:IDIM+2), 
     &          FLUX(2,-1:IDIM+2) 
*
      COMMON /PRIMIT/ D, U
      COMMON /CONSER/ CS
      COMMON /FLUXES/ FLUX
      COMMON /MESHPA/ DT, DX
*
      DATA TOLDEP /1.0E-06/
*
C     Update conserved variables to new time level
*
      DTODX = DT/DX
*
       DO 10 I = 1, CELLS
*
          DO 20 K    = 1, 2
             CS(K,I) = CS(K,I) - DTODX*(FLUX(K,I) - FLUX(K,I-1))
 20       CONTINUE
*
C         Compute physical variables
* 
          D(I) = CS(1,I)
*
          IF(D(I).GT.TOLDEP)THEN
             U(I) = CS(2,I)/D(I)
          ELSE
             U(I) = 0.0
          ENDIF
*
 10    CONTINUE
* 
       END
*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*
      SUBROUTINE MUSCLH(CELLS) 
*
C     Purpose: to compute the intercell numerical flux using the SLIC 
C              method This involves a MUSCL reconstruction of the data,
C              time evolution of the boundary extrapolated values and 
C              application of the first-order Godunov method with the 
C              HLL approximate Riemann solver
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER   CELLS, I, IDIM, K, LIMITE                
*
      REAL      BEXT, CS, D, DELFLUX, DELTA, DLOC, DT, DTODX, 
     &          DUPW, DX, FIL, FIR, OMEG, PIL, PIR, RATIO, 
     &          TOLLIM, U       

      PARAMETER (IDIM = 3000)
*
      DIMENSION BEXT(2, 2, -1:IDIM+2),  D(-1:IDIM+2), U(-1:IDIM+2), 
     &          CS(2,-1:IDIM+2), PIL(2), PIR(2), FIL(2), FIR(2)       
*
      COMMON /PRIMIT/ D, U
      COMMON /CONSER/ CS
      COMMON /MESHPA/ DT, DX
      COMMON /TVDCON/ LIMITE
      COMMON /BEXTRA/ BEXT
*
      DATA  OMEG, TOLLIM/0.0, 1.0E-6/
*
      DO 10 I = -1, CELLS + 2
*
C        Compute conserved variables on data
*
         CS(1,I) = D(I)
         CS(2,I) = D(I)*U(I)
C
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
         DO 30 K = 1, 2
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
C           step. Slope limiters used are:
*
C           LIMITE = 1, FORCE Method
C           LIMITE = 2, Upwind Second Order Method (non-monotone)
C           LIMITE = 3, Upwind TVD, with SUPERBEE type limiter 
C           LIMITE = 4, Upwind TVD, with VAN LEER type limiter 
C           LIMITE = 5, Upwind TVD, with VAN ALBADA type limiter 
C           LIMITE = 6, Upwind TVD, with MINMOD type limiter 
C           LIMITE = 7, Upwind TVD, with MINMAX type limiter 
*          
            IF(LIMITE.EQ.1)DELTA = 0.0
            IF(LIMITE.EQ.2)DELTA = DELTA
            IF(LIMITE.EQ.3)CALL SBSLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.4)CALL VLSLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.5)CALL VASLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.6)CALL MISLIC(RATIO, OMEG, DELTA)
            IF(LIMITE.EQ.7)CALL MINMAX(DUPW, DLOC, DELTA)
*	
C	      Compute boundary extrapolated values for conserved
C           variables in each cell i
*
            PIL(K) = CS(K,I) - 0.5*DELTA
            PIR(K) = CS(K,I) + 0.5*DELTA
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
         DO 40 K = 1, 2
*
            DELFLUX     = 0.5*DTODX*(FIL(K) - FIR(K))
            BEXT(1,K,I) = PIL(K) + DELFLUX
            BEXT(2,K,I) = PIR(K) + DELFLUX
*
 40      CONTINUE 
* 
 20   CONTINUE
*
C     The HLL approximate Riemann solver is called. Intercell fluxes 
C     are stored in array FLUX, which is then used in subroutine 
C     UPDATE
*
      CALL HLLRS(CELLS)
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
      REAL   GRAVIT, CS, FLUX, D, U
* 
      DIMENSION CS(2), FLUX(2)
*
      COMMON /ACCELE/ GRAVIT
*
C     Compute physical variables
*
      D = CS(1)
      U = CS(2)/D 
*
C     Compute fluxes
*
      FLUX(1) = D*U
      FLUX(2) = D*U*U + 0.5*GRAVIT*D*D 
*
      END
*
*----------------------------------------------------------------------* 
* 
      SUBROUTINE HLLRS(CELLS)
*
C     Purpose: to compute an intercell flux FLUX(K, I) according
C              to the MUSCL-Hancock scheme using the HLL approximate 
C              Riemann solver
*
      IMPLICIT NONE
*
C     Declaration of variables
*
      INTEGER  CELLS, I, IDIM, K 
*   
      REAL     BEXT, CDL, CDR, CL, CR, DL, DR, DS, FDL, FDR, FLUX, 
     &         FHLL, GRAVIT, SL, SR, TOLDEP, UL, UR                 
*
      PARAMETER (IDIM = 3000)
*
      DIMENSION FLUX(2,-1:IDIM+2), CDL(2), CDR(2), FDL(2), FDR(2), 
     &          BEXT(2, 2, -1:IDIM+2)                           
*
      COMMON /ACCELE/ GRAVIT
      COMMON /FLUXES/ FLUX
      COMMON /BEXTRA/ BEXT
*
      DATA TOLDEP /1.0E-06/
*
      DO 10 I = 0, CELLS 
*
C        At interface (I, I+1) store conserved variables
C        CDL and CDR from boundary extrapolated values BEXT
*
         DO 20 K = 1, 2
            CDL(K) = BEXT(2, K, I)
            CDR(K) = BEXT(1, K, I + 1)
 20      CONTINUE
*
C        Compute physical variables from conserved variables
*
         DL = CDL(1)
         UL = CDL(2)/DL
         CL = SQRT(GRAVIT*DL)
*
         DR = CDR(1)
         UR = CDR(2)/DR
         CR = SQRT(GRAVIT*DR)
*
C        Compute fluxes FDL and FDR at CDL and CDR
*
         CALL FLUEVAL(CDL, FDL)
*
         CALL FLUEVAL(CDR, FDR)  
*
C        Compute wave speed estimates SL and SR. Dry bed case      
*
         IF(DR.LT.TOLDEP.AND.DL.GT.TOLDEP)THEN
*	
C           Dry bed on the right
*
            SL = UL - CL
            SR = UL + 2.0*CL
            GOTO 9999
         ENDIF
*
         IF(DL.LT.TOLDEP.AND.DR.GT.TOLDEP)THEN
*	
C           Dry bed on the left
*
            SL = UR - 2.0*CR
            SR = UR + CR
            GOTO 9999
         ENDIF
*	
         IF(DL.LT.TOLDEP.AND.DR.LT.TOLDEP)THEN
*	
C           Dry bed on the left and right
*
            SL = -TOLDEP
            SR = +TOLDEP
            GOTO 9999
         ENDIF
*
C        Compute DS is star region according to the Two-
C        Rarefaction approximation
C
         DS = (0.5*(CL + CR) + 0.25*(UL - UR))**2/GRAVIT
*
C        Compute wave speed estimates SL and SR. Wet bed case
* 
         IF(DS.LE.DL)THEN
            SL = UL - CL 
         ELSE      
            SL = UL - CL*SQRT(0.5*DS*(DS + DL))/DL
         ENDIF
*
         IF(DS.LE.DR)THEN
            SR = UR + CR 
         ELSE
            SR = UR + CR*SQRT(0.5*DS*(DS + DR))/DR  
         ENDIF
*
 9999    CONTINUE
*
C        Compute Godunov intercell flux at x/t = 0
*
         IF(SL.GE.0.0)THEN
*   
C           Right-going supersonic flow
*
            DO 30 K = 1, 2
               FLUX(K, I) = FDL(K)  
 30         CONTINUE
*
         ENDIF
*
         IF(SL.LE.0.0.AND.SR.GE.0.0)THEN
*
C           Subsonic flow
C           Compute fluxes in STAR region according to HLL
*
            DO 40 K = 1, 2
               FHLL      = SR*FDL(K)-SL*FDR(K)+SL*SR*(CDR(K)-CDL(K))
	         FLUX(K,I) = FHLL/(SR - SL)
 40         CONTINUE
*
         ENDIF
*
         IF(SR.LE.0.0)THEN
*   
C           Left-going supersonic flow
*
            DO 50 K = 1, 2
               FLUX(K, I) = FDR(K)  
 50         CONTINUE
*
         ENDIF
*
 10   CONTINUE
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
 
 