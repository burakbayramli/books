      PROGRAM TO2
***********************************************************************
*     This program calculates takeoff for commercial jet aircraft.    *
*     Both All Engines Operating (AEO) (normal) and One Engine Out    *
*     (OEI) takeoff are calculated.  OEI calculations are for         *
*     Balanced Field Length (BFL).                                    *
*                                                                     *
*     Methodology:  A.R. Krenkel and A. Salzman                       *
*                   "Journal of Aircraft", Vol. 5, No. 5,             *
*                   Sept-Oct 1968, pp. 429-436                        *
*                                                                     *
*     Algorithm:    Sean R. Lynn                                      *
*                   Original Scheme March 19, 1994                    *
*                   Revision of BFL methodology May 11, 1994          *
*                                                                     *
*     The original methodology solved balance of forces equations of  *
*     motions for the aircraft parametrically.  This program uses     *
*     time-step integration of a 4th order Runge-Kutta scheme to solve*
*     the equations.  The Runge-Kutta scheme is semi-adaptive,        *
*     reducing the user-input step size until the error tolerance is  *
*     met.                                                            *
*                                                                     *
*     The program flows as follows:                                   *
*     1) Initialize variables and open i/o devices.                   *
*     2) Read in user aircraft data from TAKEOFF2.IN using subroutine *
*        GETDAT.  Put input data into array DATARY(24)                *
*     3) Set i/o devices, calculate quadratic thrust coefficients     *
*        using QINTRP, calculate the rotation velocity and set the    *
*        per step error tolerance.                                    *
*     4) Echo input data to user-specified i/o device                 *
*     5) Solve for normal takeoff.                                    *
*        a) Incremental output.                                       *
*           i)   V=0 to V=Vr (up to rotation)                         *
*           ii)  V=Vr to V=Vlo (rotation for user-specified time)     *
*           iii) Vlo to Vobs (climb to user-specified obstacle height)*
*        b) Summary output of normal takeoff.                         *
*     6) Solve for OEI BFL.                                           *
*        a) Two initial guesses are made of the critical engine       *
*           failure velocity.                                         *
*        b) Calculate both OEI takeoff and aborted takeoff for both   *
*           guess velocities.  For BFL these two distances should be =*
*        c) Use bisection method to converge on a Vcrit such that     *
*           the BFL definition is solved                              *
*        d) Output summary data of BFL calculation                    *
*                                                                     *
*     Important subroutines:                                          *
*     GETDAT - gets user input data from TAKEOFF2.IN                  *
*     QINTRP - finds the quadratic coefficients for thrust using      *
*              polynomial interpolation                               *
*     LINTRP - performs linear interpolation between 2 points.  Used  *
*              to interpolate between incremental velocities.         *
*     GROUND - the equations of motion for the ground roll segment    *
*     AIR    - the equations of motion for the climb segment          *
*     ADPTRK - reduces user-input time step size to make sure error   *
*              tolerance is met. (This is the semi-adaptive portion)  *
*     RK34   - third and fourth order Runge-Kutta scheme used in      *
*              conjunction with ADPTRK to solve aircraft EOMs			*  
*																	*
*	Mods:															*
*			1. Added a routine to prompt the user for input and     *
*			   output filenames, Leifur Thor Leifsson 03-02-04.     *
*			2. A header with information on the program is written  *
*			   to screen/file/printer,Leifur Thor Leifsson 03-02-04.*
***********************************************************************

      REAL DATARY(24),T0,T1,T2,VLO,RPARM(11),ERR,Y4(4),Y3(4),T,TOUT,
     .     TOLD,Y1OLD,Y2OLD,TEMP1,TEMP2,LINTRP,DYDX(4),VR,VOBS,XR,XLO,
     .     XOBS,TR,TLO,TOBS,XDIF1,XDIF2,TGES,TGES1,TGES2,Y3OLD,XDIF,
     .     XCRIT,VCRIT,TCRIT,TDESC,VDESC,XDESC,BFL,TFL
      INTEGER INFO(1),IOUNIT,IPARM(1),ONETWO,I
      CHARACTER*40 TITLE

      EXTERNAL GROUND, AIR

      OPEN(6,FILE='PRN')
C      OPEN(7,FILE='TAKEOFF2.OUT')
      OPEN(8,FILE='CON')

C     Get aircraft data from the input file
      CALL GETDAT(DATARY,INFO,TITLE)

C     Set the output device
      IOUNIT = INFO(1)

C     Find quadratic thrust coefficients T0,T1,T2
      CALL QINTRP(DATARY(20),DATARY(17),T0,T1,T2)

C     Calculate the rotation velocity VR
      VR = DATARY(13) * SQRT((2.*DATARY(3))/(DATARY(2)*DATARY(4)*
     .      DATARY(5)))

C     Set step-wise tolerance for Runge-Kutta scheme
      ERR = 1.E-6

C     Write out case title, headings and echo input data
      WRITE (IOUNIT,'(20X,A)') TITLE
      WRITE (IOUNIT,'(//)')
      WRITE (IOUNIT,'(A)') '<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CASE INPUT D
     .ATA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
      WRITE (IOUNIT,*)
      WRITE (IOUNIT,100)'Gravitational accel. (g)=',DATARY(1),'(ft/s^2)'
      WRITE(IOUNIT,100)'Air density at takeoff  =',DATARY(2),'(sl/ft^3)'
      WRITE (IOUNIT,100)'Aircraft weight      (W)=',DATARY(3),'(lbs)'
      WRITE (IOUNIT,100)'Aircraft wing area   (S)=',DATARY(4),'(ft/s^2)'
      WRITE (IOUNIT,100)'Max lift coeff   (CLmax)=',DATARY(5),'(nondim)'
      WRITE (IOUNIT,100)'Ground lift coeff(CLgrd)=',DATARY(6),'(nondim)'
      WRITE (IOUNIT,100)'Air lift coeff   (CLair)=',DATARY(7),'(nondim)'
      WRITE (IOUNIT,100)'Ground drag coeff(CDgrd)=',DATARY(8),'(nondim)'
      WRITE (IOUNIT,100)'Air drag coeff   (CDair)=',DATARY(9),'(nondim)'
      WRITE(IOUNIT,100)'Roll frict coeff (MUgrd)=',DATARY(10),'(nondim)'
      WRITE(IOUNIT,100)'Brake frict coeff(MUbrk)=',DATARY(11),'(nondim)'
      WRITE(IOUNIT,100)'Angle of thrust (LAMBDA)=',DATARY(12),'(rad)'
      WRITE(IOUNIT,100)'Stall margin         (k)=',DATARY(13),'(nondim)'
      WRITE(IOUNIT,100)'Descision time    (TIME)=',DATARY(14),'(sec)'
      WRITE(IOUNIT,100)'Obstacle height  (OBSHT)=',DATARY(15),'(ft)'
      WRITE(IOUNIT,100)'EI pwr remaining (PLOSS)=',DATARY(16),'(fract)'
      WRITE(IOUNIT,*)
      WRITE(IOUNIT,110)'THRUST = ',T0,' + ',T1,'*V + ',T2,'*V^2'
      WRITE (IOUNIT,'(//)')

C     Solve for normal takeoff
c     Set variables passed to subroutines: rho,W,S,CL,CD,mu,T0,T1,T2,
c                                          LAMDA,g - used for grd roll
      RPARM(1) = DATARY(2)
      RPARM(2) = DATARY(3)
      RPARM(3) = DATARY(4)
      RPARM(4) = DATARY(6)
      RPARM(5) = DATARY(8)
      RPARM(6) = DATARY(10)
      RPARM(7) = T0
      RPARM(8) = T1
      RPARM(9) = T2
      RPARM(10) = DATARY(12)
      RPARM(11) = DATARY(1)

c     Ground roll
c     Set number of equations for call to ADPTRK and init dist,spd,time
      NEQ = 2
      Y4(1) = 0.
      Y4(2) = 0.
      Y4(3) = 0.
      Y4(4) = 0.
      T = 0.

c     Find solution between V=0 and V=Vr
      WRITE (IOUNIT,'(A)') '<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NORMAL TAKEOFF
     . DATA >>>>>>>>>>>>>>>>>>>>>>>>>'
      WRITE (IOUNIT,'(A)')'Time (s)      X-Dist (ft)      X-Vel (ft/s)
     .   Y-Dist (ft)      Y-Vel (ft/s)'
      WRITE (IOUNIT,120) T,Y4(1),Y4(2),Y4(3),Y4(4)
      DO 10 I = 1,1000
        TOUT = T + DATARY(23)
        TOLD = T
        Y1OLD = Y4(1)
        Y2OLD = Y4(2)
        CALL ADPTRK(GROUND,T,TOUT,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
        IF (Y4(2) .LT. VR) THEN
          WRITE (IOUNIT,120) TOUT,Y4(1),Y4(2),Y4(3),Y4(4)
        ELSE
          TEMP1 = LINTRP(VR,Y2OLD,Y4(2),TOLD,TOUT)
          TEMP2 = LINTRP(VR,Y2OLD,Y4(2),Y1OLD,Y4(1))
          WRITE (IOUNIT,120) TEMP1,TEMP2,VR,Y4(3),Y4(4)
          TR = TEMP1
          XR = TEMP2
          T = TOUT
          GOTO 20
        END IF
        T = TOUT
10    CONTINUE

c     Find solution for rotation phase (ground run for T=TIME seconds)
20    Y4(1) = TEMP2
      Y4(2) = VR
      TEMP2 = TEMP1 + DATARY(24)
      CALL ADPTRK(GROUND,TEMP1,TEMP2,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
      WRITE (IOUNIT,120) TEMP1,Y4(1),Y4(2),Y4(3),Y4(4)
      TLO = TEMP1
      XLO = Y4(1)
      VLO = Y4(2)

c     Climb phase
c     Change vars. passed to subroutines: CL,CD - used for climb
      RPARM(4) = DATARY(7)
      RPARM(5) = DATARY(9)

c     Set number of equations for call to ADPTRK and init dist,spd,time
      NEQ = 4

c     Solve from end of rotation to next even increment
      T = T + DATARY(24)
      CALL ADPTRK(AIR,TEMP2,T,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
      WRITE (IOUNIT,120) T,Y4(1),Y4(2),Y4(3),Y4(4)

c     Solve from even increment to OBSHT
      DO 30 I = 1,1000
        TOUT = T + DATARY(23)
        TOLD = T
        Y1OLD = Y4(1)
        Y2OLD = Y4(2)
        Y3OLD = Y3(3)
        Y4OLD = Y4(4)
        CALL ADPTRK(AIR,T,TOUT,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
        IF (Y4(3) .LT. DATARY(15)) THEN
          WRITE (IOUNIT,120) TOUT,Y4(1),Y4(2),Y4(3),Y4(4)
        ELSE
          T     = LINTRP(DATARY(15),Y3OLD,Y4(3),TOLD,TOUT)
          Y4(1) = LINTRP(DATARY(15),Y3OLD,Y4(3),Y1OLD,Y4(1))
          Y4(2) = LINTRP(DATARY(15),Y3OLD,Y4(3),Y2OLD,Y4(2))
          Y4(4) = LINTRP(DATARY(15),Y3OLD,Y4(3),Y4OLD,Y4(4))
          WRITE (IOUNIT,120) T,Y4(1),Y4(2),DATARY(15),Y4(4)
          TOBS = T
          XOBS = Y4(1)
          VOBS = SQRT(Y4(2)*Y4(2)+Y4(4)*Y4(4))
          GOTO 40
        END IF
        T = TOUT
30    CONTINUE

c     Write normal takeoff data to output device
40    WRITE (IOUNIT,*)
      WRITE (IOUNIT,'(A)') '<<<<<<<<<<<<<<<<<<<<<<<<<< NORMAL TAKEOFF SU
     .MMARY >>>>>>>>>>>>>>>>>>>>>>'
      WRITE (IOUNIT,*)
      WRITE (IOUNIT,100)'Rotation Velocity   (Vr)=',VR,'(ft/s)'
      WRITE (IOUNIT,100)'Liftoff Velocity   (Vlo)=',VLO,'(ft/s)'
      WRITE (IOUNIT,100)'Velocity over obs.(Vobs)=',VOBS,'(ft/s)'
      WRITE (IOUNIT,100)'Rotation Distance   (Xr)=',XR,'(ft)'
      WRITE (IOUNIT,100)'Liftoff Distance   (Xlo)=',XLO,'(ft)'
      WRITE (IOUNIT,100)'Distance to obst. (Xobs)=',XOBS,'(ft)'
      WRITE (IOUNIT,100)'Rotation Time       (Tr)=',TR,'(s)'
      WRITE (IOUNIT,100)'Liftoff Time       (Tlo)=',TLO,'(s)'
      WRITE (IOUNIT,100)'Time to obst.     (Tobs)=',TOBS,'(s)'
      WRITE (IOUNIT,*)
      WRITE (IOUNIT,100)'TOTAL TAKEOFF DIST (Xto)=',XOBS,'(ft)'
      WRITE (IOUNIT,100)'TOTAL TAKEOFF TIME (Tto)=',TOBS,'(s)'
      WRITE (IOUNIT,*)



C     FIND THE BALANCED FIELD LENGTH
c     Find the critical engine failure velocity, Vcrit
      WRITE (8,'(A)') 'PLEASE WAIT, CONVERGING ON BFL!:  USU @ 2MIN'

c     Set the inital Vcrit guess velocities
      TGES1 = .25*TR
      TGES2 = .95*TR

c     Solve for engine out takeoff at both guess times and use the
c     bisection method to iterate and converge on Xbrak = XOEI

c     Iterate to converge on Vcrit
      DO 93 K = 1, 1000
c     Iterate through two guesses for bisection routine
      DO 92 ONETWO = 1,2

      IF (ONETWO .EQ. 1) THEN
        TGES = TGES1
      ELSE
        TGES = TGES2
      END IF

      RPARM(1) = DATARY(2)
      RPARM(2) = DATARY(3)
      RPARM(3) = DATARY(4)
      RPARM(4) = DATARY(6)
      RPARM(5) = DATARY(8)
      RPARM(6) = DATARY(10)
      RPARM(7) = T0
      RPARM(8) = T1
      RPARM(9) = T2
      RPARM(10) = DATARY(12)
      RPARM(11) = DATARY(1)
      NEQ = 2
      Y4(1) = 0.
      Y4(2) = 0.
      Y4(3) = 0.
      Y4(4) = 0.
      T = 0.

C     Solution for OEI ground roll
c     Solution between V=0 and V=Vcrit (Engine Failure)
      CALL ADPTRK(GROUND,T,TGES,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)

      XCRIT = Y4(1)
      VCRIT = Y4(2)
      TCRIT = TGES
      RPARM(7) = T0*DATARY(16)
      RPARM(8) = T1*DATARY(16)
      RPARM(9) = T2*DATARY(16)
      TOLD = TGES

c     Solution from V=Vcrit to V=Vr
      DO 50 I = 1, 1000
      T = TOLD + 0.5
      Y1OLD = Y4(1)
      Y2OLD = Y4(2)
      CALL ADPTRK(GROUND,TOLD,T,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
      IF (Y4(2) .GE. VR) THEN
        TEMP1 = LINTRP(VR,Y2OLD,Y4(2),Y1OLD,Y4(1))
        TEMP2 = VR
        TEMP3 = LINTRP(VR,Y2OLD,Y4(2),TOLD,T)
        GOTO 60
      END IF
      TOLD = T
50    CONTINUE

c     Solution from V=Vr to V=Vlo (Engine Out Rotation)
60    Y4(1) = TEMP1
      Y4(2) = TEMP2
      T = TEMP3
      TOUT = T + DATARY(24)
      CALL ADPTRK(GROUND,T,TOUT,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)

c     Solution for OEI Climb phase
      NEQ = 4
      RPARM(4) = DATARY(7)
      RPARM(5) = DATARY(9)

      DO 70 I = 1, 1000
      TOLD = T
      T = T + .5
      Y1OLD = Y4(1)
      Y3OLD = Y4(3)
      CALL ADPTRK(AIR,TOLD,T,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
      IF (Y4(3) .GE. DATARY(15)) THEN
        XDIF = LINTRP(DATARY(15),Y3OLD,Y4(3),Y1OLD,Y4(1))
        GOTO 80
      END IF
70    CONTINUE

C     Solution for OEI braking
c     Ground run V=Vcrit to V=V1 (Descision time)
80    NEQ = 2
      RPARM(4) = DATARY(6)
      RPARM(5) = DATARY(8)
      Y4(1) = XCRIT
      Y4(2) = VCRIT
      Y4(3) = 0.
      Y4(4) = 0.
      T = TGES
      TDESC = T + DATARY(14)
      CALL ADPTRK(GROUND,T,T+DATARY(14),Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
      XDESC = Y4(1)
      VDESC = Y4(2)

c     Braking Solution V=V1 to V=0.
c     Set thrust to 0. and friction coefficient to braking value
      RPARM(7) = 0.
      RPARM(8) = 0.
      RPARM(9) = 0.
      RPARM(6) = DATARY(11)
      T = TGES + DATARY(14)

      DO 90 I = 1, 1000
      TOLD = T
      T = T + .5
      Y1OLD = Y4(1)
      CALL ADPTRK(GROUND,TOLD,T,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
      IF (Y4(2) .LE. 0.) THEN
        Y4(1) = LINTRP(0.,Y2OLD,Y4(2),Y1OLD,Y4(1))
        TFL = LINTRP(0.,Y2OLD,Y4(2),TOLD,T)
        GOTO 91
      END IF
90    CONTINUE

	

91    IF (ONETWO .EQ. 1) THEN
        XDIF1 = XDIF - Y4(1)
      ELSE
        XDIF2 = XDIF - Y4(1)
      END IF

92    CONTINUE

      IF (ABS(XDIF2) .LE. 1E-2) THEN		
        BFL = Y4(1)						
        GOTO 94
      ELSE
        TEMP1 = .8*LINTRP(0.,XDIF1,XDIF2,TGES1,TGES2)
        TEMP2 = LINTRP(0.,XDIF1,XDIF2,TGES1,TGES2)
      END IF
        TGES1 = TEMP1
        TGES2 = TEMP2
	
	IF (K == 1000) THEN
	  WRITE(IOUNIT,*) 'BFL CALCULATION DID NOT CONVERGE!'
	END IF
93    CONTINUE

c     Write OEI takeoff data to output device
94    WRITE (IOUNIT,*)
      WRITE (IOUNIT,'(A)') '<<<<<<<<<<<<<<<<<<<<<<<<<< OEI TAKEOFF SUMMA
     .RY >>>>>>>>>>>>>>>>>>>>>>'
      WRITE (IOUNIT,*)
      WRITE (IOUNIT,100)'Critical Velocity   (Vcrit) =',VCRIT,'(ft/s)'
      WRITE (IOUNIT,100)'Decision Velocity      (V1) =',VDESC,'(ft/s)'
      WRITE (IOUNIT,100)'Velocity over obs.   (Vobs) =',VOBS,'(ft/s)'
      WRITE (IOUNIT,100)'Critical Distance   (Xcrit) =',XCRIT,'(ft)'
      WRITE (IOUNIT,100)'Decision Distance      (X1) =',XDESC,'(ft)'
      WRITE (IOUNIT,100)'Balanced Field Length (BFL) =',BFL,'(ft)'
      WRITE (IOUNIT,100)'Critical Time       (Tcrit) =',TCRIT,'(s)'
      WRITE (IOUNIT,100)'Decision Time          (T1) =',TDESC,'(s)'
      WRITE (IOUNIT,100)'OEI Takeoff Time     (TBFL) =',TFL,'(s)'
      WRITE (IOUNIT,*)

	PAUSE 'Press ENTER to close program'

100   FORMAT(A30,F15.5,T44,A8)
110   FORMAT(A,3(F10.3,A))
120   FORMAT(F7.3,6X,4(F10.3,7X))
      END


      SUBROUTINE GETDAT(DATARY,IOINFO,TITLE)
C     This subroutine is used to load data from TAKEOFF2.IN
      REAL DATARY(24)
      INTEGER IOINFO(*),I
      CHARACTER*40 TITLE
	CHARACTER*20 INFILE
	CHARACTER*20 OUTFILE

	WRITE(8,3000)
	WRITE(8,3001)
	WRITE(8,3002)
	WRITE(8,3003)

	WRITE(8,1000) 
      READ(5,'(a)') INFILE
      OPEN(UNIT = 10, FILE = INFILE, STATUS = 'OLD')

C      OPEN (10, FILE='TAKEOFF2.IN')

c     Set acceleration due to gravity
      DATARY(1) = 32.174

c     Read in run title information
      READ (10,'(A)') TITLE

c     Read in the rest of the data (see TAKEOFF2.IN for order)
      DO 10 I = 2,16
        READ(10,*) DATARY(I)
10    CONTINUE
      READ (10,*) DATARY(17),DATARY(18),DATARY(19)
      READ (10,*) DATARY(20),DATARY(21),DATARY(22)
      READ (10,*) DATARY(23)
      READ (10,*) DATARY(24)
      READ (10,*) IOINFO(1)

	IF (IOINFO(1) == 7) THEN
		WRITE(8,1010) 
		READ(5,'(a)') OUTFILE
		OPEN(UNIT = 7, FILE = OUTFILE, STATUS = 'REPLACE')
		WRITE(7,3000)
		WRITE(7,3001)
		WRITE(7,3002)
		WRITE(7,3003)
		WRITE(7,3004)
	END IF

1000	FORMAT(/2x,'Enter input filename:'/)
1010	FORMAT(/2x,'Enter output filename:'/)
3000  FORMAT('  **************************************************'/
     1        '    VT Aerospace Aircraft Design Software Series'/
     2		'  **************************************************'/)
3001  FORMAT('                   Program Takeoff'/
     1		/'  This program calculates takeoff for commercial jet'/
     2		'  aircraft. Both All Engines Operating (AEO) (normal)'/
     3		'  and One Engine Out (OEI) takeoff are calculated. OEI'/
     4		'  calculations are for Balanced Field Length (BFL).'/ 
     5		/'  Methodology:  A.R. Krenkel and A. Salzman'/
     6		 '                Journal of Aircraft, Vol. 5, No. 5,'/            
     7         '                Sept-Oct 1968, pp. 429-436.'/)
3002  FORMAT('  Code written by Sean Lynn, March, 1994.'/
     1			'  Modified by Leifur Thor Leifsson, March, 2004.'/)
3003  FORMAT('  The Department of Aerospace and Ocean Engineering,'/
     1		'  Virginia Tech, Blacksburg, VA 24061.'/
     2		'  http://www.aoe.vt.edu, email: whmason@vt.edu')
3004  FORMAT(/'  **************************************************'/)

      CLOSE(10)
      RETURN
      END


      SUBROUTINE QINTRP(V,T,T0,T1,T2)
C     This subroutine uses polynomial interpolation to find the
C     quadratic contants for the engine thrust/velocity curve, given
C     the three dimensional arrays for thrust (T) and velocity (V).
      REAL V(3),T(3),T0,T1,T2,A,B,C

      A = T(1)
      B = (T(2) - T(1))/(V(2)-V(1))
      C = ((T(3)-T(2))/(V(3)-V(2))-(T(2)-T(1))/(V(2)-V(1)))/(V(3)-V(1))

      T0 = T(1) - B*V(1) + C*V(1)*V(2)
      T1 = B - C*(V(1)+V(2))
      T2 = C

      RETURN
      END


      FUNCTION LINTRP(XIN,X1,X2,Y1,Y2)
C     This function finds the linear interpolation between the points
C     [X0,Y0] and [X1,Y1].
      REAL X1,X2,Y1,Y2,LINTRP,XIN

      LINTRP = Y1 + ((Y2-Y1)/(X2-X1))*(XIN-X1)

      RETURN
      END


      SUBROUTINE GROUND(X,Y,IPARM,RPARM,DYDX)
C     These are the equations for ground roll
      REAL X,Y(*),RPARM(*),DYDX(*),CONST,K0,K1,K2,DUMMY
      INTEGER IPARM(*)

      IPARM(1) = 0
      DUMMY = X

C     Equation is of form du/dt = g/W(K0 + K1*u + K2*u*u), where
C     g/W is 1/mass, and K's are constants and u is the velocity
      CONST = COS(RPARM(10)) + RPARM(6)*SIN(RPARM(10))
      K0 = RPARM(7)*CONST - RPARM(6)*RPARM(2)
      K1 = RPARM(8)*CONST
      K2 = RPARM(9)*CONST + .5*RPARM(1)*RPARM(3)*(RPARM(6)*RPARM(4) -
     .     RPARM(5))

      DYDX(1) = Y(2)
      DYDX(2) = RPARM(11)/RPARM(2)*(K0+K1*Y(2)+K2*Y(2)*Y(2))

      RETURN
      END


      SUBROUTINE AIR(X,Y,IPARM,RPARM,DYDX)
C     These are the equations for CLIMB segment
      REAL X,Y(*),RPARM(*),DYDX(*),K11,K12,K13,K21,K22,K23,DUMMY
      INTEGER IPARM(*)

      IPARM(1) = 0
      DUMMY = X

C     Equations are of the form:  du/dt = g/W*[k11+k12*V+k13*V*V]
C                                 dv/dt = g/W*[k21+k22*V+k23*V*V]
C     where, V = sqrt (u*u + v*v) and tan gamma = v/u
      GAMMA = ATAN(Y(4)/Y(2))
      VSQR = Y(4)*Y(4) + Y(2)*Y(2)

      K11 = RPARM(7)*COS(RPARM(10) + GAMMA)
      K12 = RPARM(8)*COS(RPARM(10) + GAMMA)
      K13 = RPARM(9)*COS(RPARM(10) + GAMMA) - .5*RPARM(1)*RPARM(3)*
     .      (RPARM(5)*COS(GAMMA)+RPARM(4)*SIN(GAMMA))
      K21 = RPARM(7)*SIN(RPARM(10) + GAMMA) - RPARM(2)
      K22 = RPARM(8)*SIN(RPARM(10) + GAMMA)
      K23 = RPARM(9)*SIN(RPARM(10) + GAMMA) + .5*RPARM(1)*RPARM(3)*
     .      (RPARM(4)*COS(GAMMA)-RPARM(5)*SIN(GAMMA))

      DYDX(1) = Y(2)
      DYDX(2) = (RPARM(11)/RPARM(2))*(K11 + K12*SQRT(VSQR) + K13*VSQR)
      DYDX(3) = Y(4)
      DYDX(4) = (RPARM(11)/RPARM(2))*(K21 + K22*SQRT(VSQR) + K23*VSQR)

      RETURN
      END



      SUBROUTINE ADPTRK(F,X,XOUT,Y4,Y3,DYDX,NEQ,IPARM,RPARM,ERR)
************************************************************************
*     This subroutine makes subroutine RK34 a semiadaptive differential*
*     equation solver.  Semi adaptive because it checks only to see if *
*     it can reduce the step size, not make it larger.  This is useful *
*     if the calling program needs to know the solution on specific    *
*     intervals.                                                       *
*                                                                      *
*     Since the a posteriori error of RK34 is for the RK3 solutions,   *
*     the use of RK4 values by the user insures that the true error    *
*     is probably less than prescribed by ERR.                         *
*     THIS IS A VERY CONSERVATIVE ROUTINE                              *
*                                                                      *
*     INPUT:                                                           *
*           X     - (REAL) Initial independent variable position       *
*           XOUT  - (REAL) Independent variable position for which     *
*                   solution is desired                                *
*           Y4    - (REAL ARRAY) Initial conditions for dependent vars *
*           NEQ   - (INTEGER) Number of equations in the system        *
*           IPARM - (INTEGER ARRAY) Used to pass constants to the      *
*                   subroutine calculating the system derivatives      *
*           RPARM - (REAL ARRAY) Used to pass constants to the         *
*                   subroutine calculating the sytem derivatives       *
*           ERR   - (REAL) Prescribed error tolerance.  This will be   *
*                   used to decide whether to initiate adaptive proces *        *
*     OUTPUT:                                                          *
*           Y4    - RK4 solutions                                      *
*           Y3    - RK3 solutions for whatever they're worth (ie. RK4  *
*                   values will be more accurate                       *
************************************************************************
      INTEGER NEQ,IPARM(*),I,N
      REAL X,XOUT,Y4(NEQ),Y3(NEQ),DYDX(NEQ),RPARM(*),ERR,APERR,H,XEND,
     .     TEMPY(10)
      EXTERNAL F

C     Ensure that the user input error tolerance is positive
      ERR = ABS(ERR)

C     Save the initial conditions otherwise if APERR is .GT. ERR the
C     initial conditions end up being run through twice
      DO 10 I = 1, NEQ
         TEMPY(I) = Y4(I)
10    CONTINUE

C     Get an initial a posteriori error estimate (APERR)
      CALL RK34 (F,X,XOUT,TEMPY,Y3,DYDX,NEQ,IPARM,RPARM,APERR)

C     Decide whether error is too large and find new step size if it is
      IF (APERR .GT. ERR) THEN
         H = (XOUT - X)/FLOAT(INT(SQRT(SQRT(APERR/ERR)))+1)
         N = INT(SQRT(SQRT(APERR/ERR)))+1

         DO 20 I = 1,N
            XEND = X+ H
            CALL RK34(F,X,XEND,Y4,Y3,DYDX,NEQ,IPARM,RPARM,APERR)
            X = XEND
20       CONTINUE
      ELSE
         DO 30 I = 1,NEQ
            Y4(I) = TEMPY(I)
30       CONTINUE
      END IF

      RETURN
      END


      SUBROUTINE RK34( F, X, XOUT, Y4, Y3, DYDX, NEQ, IPARM, RPARM, ERR)
************************************************************************
*     RK4   - PERFORMS THIRD AND FOURTH ORDER RUNGE-KUTTA INTEGRATION  *
*             OF A SYSTEM OF NEQ FIRST ORDER EQUATIONS OF THE FORM:    *
*                            DY/DX = F(X,Y)                            *
*             Returns an a posteriori error estimate (RK4 - RK3) for   *
*             RK3 for use in adaptive techniques.                      *
*     INPUT VARIABLES                                                  *
*             Form: SUBROUTINE F( X, Y, IPARM, RPARM, DYDX )           *
*             The subroutine F evaluates the NEQ components of the     *
*             system of diferential equuations DY/DX = F(X,Y).         *
*             IPARM and RPARM are INTEGER and REAL arrays used to      *
*             communicate any information between calling program and F*
*             NOTE: THE SUBROUTINE NAME OF THE SUBROUTINE F MUST       *
*             BE DECLARED "EXTERNAL" IN THE CALLING PROGRAM.           *
*     X     - (REAL) The starting value of the independent variable    *
*     XOUT  - (REAL) The point at which the solution is desired        *
*     Y4    - (ARRAY of length NEQ, REAL) The array of initial condit  *
*     NEQ   - (INTEGER) Number of equations in the system              *
*     K     - (ARRAY of Dimension (NEQ,4), REAL) Used to store the     *
*             values of K1,K2,K3,K4,K5 for each diff eq in the system  *
*             K1,K2,K3 are used for RK3 & K1,K2,K4,K5 are used for RK4 *
*     IPARM - (ARRAY of unknown length, INTEGER) Used to input         *
*             parameters to subroutine F                               *
*     RPARM - (ARRAY of unknown length, REAL) Used to input parameters *
*             to subroutine F                                          *
************************************************************************
*     OUTPUT VARIABLES                                                 *
*                                                                      *
*     XOUT  - Same as input                                            *
*     Y     - Solution array at XOUT                                   *
*     DYDX  - Derivative array AT XOUT                                 *
************************************************************************
      INTEGER J, NEQ, IPARM(*)
      REAL X,XOUT,Y4(NEQ),DYDX(NEQ),H,RPARM(*),H2,TEMPX,TEMPY(10),
     .     K(10,5),Y3(NEQ),ERR

C     Calculate the step size
      H = XOUT - X

C     Calculate H/2
      H2 = .5*H

C     Calculate the K values for the functions K(Jth eqn,#of K)
c     Find K1
      CALL F(X,Y4,IPARM,RPARM,DYDX)
      DO 10 J = 1, NEQ
         K(J,1) = DYDX(J)
10    CONTINUE

c     Find K2
      DO 15 J = 1, NEQ
         TEMPY(J) = Y4(J) + H2*K(J,1)
15    CONTINUE
      TEMPX = X + H2
      CALL F(TEMPX,TEMPY,IPARM,RPARM,DYDX)
      DO 20 J = 1, NEQ
         K(J,2) = DYDX(J)
20    CONTINUE

c     Find K3
      DO 25 J = 1, NEQ
         TEMPY(J) = Y4(J) + .75*H*K(J,2)
25    CONTINUE
      TEMPX = X + .75*H
      CALL F(TEMPX,TEMPY,IPARM,RPARM,DYDX)
      DO 30 J = 1, NEQ
         K(J,3) = DYDX(J)
30    CONTINUE

c     Find K4
      DO 35 J = 1, NEQ
         TEMPY(J) = Y4(J) + H2*K(J,2)
35    CONTINUE
      TEMPX = X + H2
      CALL F(TEMPX,TEMPY,IPARM,RPARM,DYDX)
      DO 40 J = 1, NEQ
         K(J,4) = DYDX(J)
40    CONTINUE

c     Find K5
      DO 45 J = 1, NEQ
         TEMPY(J) = Y4(J) + H*K(J,4)
45    CONTINUE
      TEMPX = X + H
      CALL F(TEMPX,TEMPY,IPARM,RPARM,DYDX)
      DO 50 J = 1, NEQ
         K(J,5) = DYDX(J)
50    CONTINUE

C     Calculate the function values at XOUT by RK3
      DO 60 J = 1, NEQ
         Y3(J) = Y4(J) + H*(2.*K(J,1) + 3.*K(J,2) + 4.*K(J,3))/9.
60    CONTINUE

C     Calculate the function values at XOUT by RK4
      DO 70 J = 1, NEQ
         Y4(J) = Y4(J) + H*(K(J,1) + 2.*K(J,2) + 2.*K(J,4)+ K(J,5))/6.
70    CONTINUE

C     Calculate the MAX a posteriori error estimate for RK3
C     Since Y values for RK4 are output, this is the worst case error
      ERR = 0.
      DO 80 J = 1, NEQ
         TEMPX = ABS(Y4(J) - Y3(J))
         IF (TEMPX .GT. ERR) THEN
            ERR = TEMPX
         END IF
80    CONTINUE

      RETURN
      END
