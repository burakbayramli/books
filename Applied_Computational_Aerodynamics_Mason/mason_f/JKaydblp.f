      PROGRAM MASTER

c     file JKaydblp.f - double precision version of JKayVLM.f
c                       with variable number of sections, NSECT
c                       NSECT is currently set to 10

c     a program to estimate the subsonic aerodynamics and stability
c     and control derivatives for an assessment of the control power
c     required in conceptual aircraft designs

c     the basic method is the vortex ring variation of the vlm method

c     conceived and written by Jacob Kay
c     VPI Aerospace and Ocean Engineering Department, 1992

c     mods by W.H. Mason and Alex Benoliel, March 1993
c     last mod, May 15, 1993
c     last mod, May 15, 1994 by Valery Razgonyaev,
c                     e-mail: valery@apollo.aoe.vt.edu
c     mod to correct small bug, Nov. 16, 1996 by W.H. Mason

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!    In this code the max number of sections is 10.                !
c!    User can change this value by changing parameters             !
c!    NSECT in the main programm and NNSECT in the                  !
c!    subroutines CENTRAL and VLM on the number required.           !
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NSECT=10)
      REAL*8 
     + VCref(NSECT,10,6,3),  DCL(NSECT,2),    DELslat(NSECT),        
     + VC(NSECT,10,6,3),     DCM(NSECT,2),    DELflap(NSECT),
     + GAMMA(NSECT,8,10),    slatlon(NSECT),  slatlat(NSECT),
     + GAMMAref(NSECT,8,10), flaplon(NSECT),  flaplat(NSECT),
     + DCroll(NSECT,2),      DCy(NSECT,2),    DCn(NSECT,2),                    
     + bplon(NSECT,4,3),                       
     + bplat(NSECT,4,3)              
      REAL*8 
     + LIFT,      CmCL,    DRAG,   PITCH,   ROLL,   YAW,  
     + LIFT0,     Cm0,     Cdi,    PITCH0,  CROLLp, YAW0,
     + CL,        Cmalpha, CdiCL2, Cmq,     Clbeta, Cybeta,
     + CL0,                        CLq,     Clr,    Cnbeta,
     + CLalpha,   LCSDW,           QBAR,    p,      Cyr,                 
     + CLATAIL1,                            q,      Cnr,      
     + CLATAIL2,                                    CYAWp, 
     + LIFTTAIL,          
     + LIFTAIL0     
      REAL*8 
     + Xcg,Ycg,Zcg,Xcgref,CH,C,cref,S,ALPHA,BETA,MACH,V,                 
     + B,ASYM, ASYMDEFL,FSECT,PI            

      INTEGER I,J,K,LAT,IW1,IW2,IWING1,IWING2,ITAIL1,ITAIL2,
     +        ICASE,ISECT,JSECT
      character*1     input,lotus
      character*12    loninf, latinf,in

c     ===================================================================
c     This master program calls CENTRAL to determine the stability
c     and control derivatives of a design configuration.
c
c     Prepared by Jacob Kay,    Sept. 1992.
c     ===================================================================

      PI   = 4.d0*DATAN(1.d0)
      V    = 1.d0
      QBAR = 0.5d0*V*V
c
c     *** Get basic geometric information ***

      write(6,1300)
      write(*,*) 'Enter output file name (max. of 8 characters):'
      read (*,'(a8)') vlmout
      open(12,file=vlmout)
      rewind(12)
      write(*,*)  'Input from file (y/n)?'
      read(*,'(a1)') input
      if(input.eq.'y'.or.input.eq.'Y') then
	  write(*,*)'Enter file name:'
 	  read(*,1100)in
	  write(*,*)in
	  open(9,file=in)
        read(9,1005)mach,s,c,b,ch,icase,xcg,zcg
        read(9,1100) loninf
	  if(icase.eq.1.or.icase.eq.4)then
	    read(9,*)
        else
          read(9,1100) latinf
        endif
      else
	  write(*,*)'Enter Logitudinal Geometry File:'
        read(*,1100) loninf
        if(icase.ne.1.and.icase.ne.4)then
	    write(*,*)'Enter Lateral Geometry File:'
          read(*,1100) latinf
        endif
        write(6,*)  'ENTER MACH NUMBER: (0 - 0.8)'
        READ*, MACH

        write(6,*) 'ENTER REFERENCE AREA.'
        READ*,S

        write(6,*) 'ENTER REFERENCE CHORD.'
        READ*,C

        write(6,*) 'ENTER REFERENCE SPAN.'
        READ*,B

        write(6,*) 'ENTER HEIGHT ABOVE GROUND.'
        READ*,CH

c     *** Ask for longitudinal or lateral case ***

        write(6,*) 'ENTER: 1 - Longitudinal derivatives only'
        write(6,*) '       2 - Lateral/directional derivatives only'
        write(6,*) '       3 - Both'
        write(6,*) '       4 - Basic calculations only'
        READ*, ICASE


        write(6,*) 'ENTER X-cg.'
        READ*,Xcg 

        write(6,*)'ENTER Z-cg (+ UP)'
        READ*, Zcg
      endif
      write(*,*) 'Ouput LOTUS 1-2-3 file (y/n)?'
      read(*,'(a1)') lotus

c     *** Decide to turn on or off ground effect ***

      IF (CH.GT.5*B) CH = 99999.99999
      
 50   IF (ICASE.EQ.2) GOTO 60
      if(input.eq.'y'.or.input.eq.'Y') then
        read(9,1010)itail1,itail2,iw1,iw2
      else
        write(6,*)'ENTER 1st SECTION NO. DESIGNATED AS H. TAIL SURFACE.'
        READ*, ITAIL1
        write(6,*)'ENTER 2nd SECTION NO. DESIGNATED AS H. TAIL SURFACE.'
        READ*, ITAIL2
        write(6,*)'ENTER 1st SECTION NO. DESIGNATED AS WING SURFACE.'
        READ*, IW1
        write(6,*)'ENTER 2nd SECTION NO. DESIGNATED AS WING SURFACE.'
        READ*, IW2
      endif

c     *** Write conditions at top of output ***

 60   BETA = DSQRT(1.d0-MACH*MACH)
      Xcg = Xcg/BETA    
      C = C/BETA
      xcgref = xcg*beta
      cref   = c*beta
      write(6,1400)  icase, Mach, S, cref, b, ch, xcgref, zcg
      write(12,1400) icase, Mach, S, cref, b, ch, xcgref, zcg
 1300 format(//'VLM Based Stability & Control Evaluation Code'/
     1       '   Developed by Jacob Kay'/
     2       '   Modified by Alex Benoliel and W.H. Mason'/)
 1400 format(/3x,'VLM Based Stability & Control Evaluation Code'/
     1       /3x,'icase               = ', i12,
     2       /3x,'MACH NUMBER         = ', f12.5,
     3       /3x,'Sref                = ', f12.5, 
     4       /3x,'c ref               = ', f12.5, 
     5       /3x,'b ref               = ', f12.5,     
     6       /3x,'HEIGHT ABOVE GROUND = ', f12.5,
     7       /3x,'X-cg                = ', f12.5,
     8       /3x,'Z-cg                = ', f12.5)

c     *** Get geometry information ***

      OPEN(UNIT = 10, FILE = loninf, STATUS = 'OLD')
      REWIND(UNIT = 10)

* In reading lat/dir data, y coordinates are entered in as
*    z coordinates and z coordinates are entered in as y coordinates
*    to conform to the codes coordinate common coordinate system

      if(icase.ne.1.and.icase.ne.4) then
        OPEN(UNIT = 11, FILE = latinf, STATUS = 'OLD')
        REWIND(UNIT = 11)
        read(11,*)
        READ(11,1000) FSECT
        jsect = fsect
        write(6,1530) jsect
        DO 110, K = 1, jSECT
	     write(6,1505)k,k,k
           DO 100, I = 1, 4
              READ(11,1000) BPlat(k,I,1),BPlat(k,I,3),BPlat(k,I,2)
              write(6,1510) i,BPlat(k,I,1),BPlat(k,I,3),BPlat(k,I,2)
	        bplat(k,i,1) = bplat(k,i,1)/beta
 100       CONTINUE
           READ(11,1000) slatlat(K), flaplat(K)
           write(6,1520) k, slatlat(K),k, flaplat(K)
 110    continue
      endif

      read(10,*)
      READ(10,1000) FSECT
      ISECT     = FSECT
      write(6,1500) isect
      DO 130, I = 1, ISECT
         write(6,1505)i,i,i  
         DO 120, J = 1, 4
            READ(10,1000) BPlon(i,J,1),BPlon(i,J,2),BPlon(i,J,3)
            write(6,1510) j,BPlon(i,J,1),BPlon(i,J,2),BPlon(i,J,3) 
	      bplon(i,j,1) = bplon(i,j,1)/beta
 120     CONTINUE
         READ(10,1000) slatlon(I), flaplon(I)
         write(6,1520) i, slatlon(I),i, flaplon(I)
 130  CONTINUE


 1000 FORMAT(3F10.5)
 1005 format(/,f4.1,/,4(f10.2,/),i2,/,f10.3,/,f10.3)
 1010 format(3(i2,/),i2)
 1100 format(a12)
 1500 format(/3x,'reading longitudinal planform data, isect = ', i2)
 1505 format(/,3x,' Point No.     x(',i2,')',8x,'y(',i2,')',
     &        8x,'z(',i2,')')
 1510 format( 5x,i3,3x,3f12.5)
 1520 format(3x,2x,'slat(',i2,') = ',f9.5,2x'flap(',i2,') = ',f9.5)

 1530 format(/3x,'reading sideview/v tail data, isect = ',i2)


c     *** Longitudinal derivatives first ***

      Ycg      = 0.d0
     
c     *** Call 'CENTRAL' to get aerodynamic properties at AOA = 0 ***

      ALPHA    = 0.d0
      q        = 0.d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      =   0
      IWING1   =  99
      IWING2   = -99

c     *** Constant ASYM is a indicator to VLM that it's to calculate
c         for asymmetric span load of equal but opposite gamma 
c         distribution ***

c     *** Constant IWING1 & IWING2 are the range of section numbers
c         that corresponds to the wing's sections (IWING1<=IWING2)***

      write(6,*)'GETTING NOMINAL AERODYNAMICS.'

      CALL DELZERO(ISECT,DELslat,DELflap,NSECT)
      CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT0,DRAG,PITCH0,ROLL,YAW0,LIFTAIL0,VCref,GAMMAref,
     +             bplon,slatlon,flaplon,NSECT)

      Cdi            = 2.d0*DRAG/(QBAR*S)
      CL0            = 2.d0*LIFT0/(QBAR*S)
      CM0            = 2.d0*PITCH0/(QBAR*S*C)
      write(6,1405)  CL0,Cm0,Cdi
      write(12,1405) CL0,Cm0,Cdi
1405  format( 3x,'CL0                 = ',f12.5,
     1       /3x,'Cm0                 = ',f12.5,
     2       /3x,'Cdi(at alpha = 0)   = ',f12.5)

c     *** Skip if lateral derivatives only ***

      IF (ICASE .EQ. 2 ) GOTO 500

c     *** Get aerodynamic data at AOA = 5 ***

      ALPHA    = 5.d0*PI/180.d0
      q        = 0.d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      =   0

      write(6,*)'AT ALPHA = ', ALPHA,' rad.'

      CALL DELZERO(ISECT,DELslat,DELflap,NSECT)
c
c     *** CH changed to reflect that of CG's height above ground ***

      CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH+Xcg*DSIN(alpha),q,
     +             p,Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplon,slatlon,flaplon,NSECT)

c
      CLalpha        = 2.d0*(LIFT-LIFT0)/(QBAR*S)/ALPHA
      Cmalpha        = 2.d0*(PITCH-PITCH0)/(QBAR*S*c)/alpha
      CmCL           =    (PITCH-PITCH0)/C/(LIFT-LIFT0)
      CDiCL2         =    2*DRAG*(QBAR*S)/(2*LIFT)**2
      CL             = 2.d0*LIFT/(QBAR*S)
      Cdi            = 2.d0*DRAG/(QBAR*S)

      write(6,1410)  CLalpha,Cmalpha, cmcl,CdiCL2,CL,Cdi 
      write(12,1410) CLalpha,Cmalpha, cmcl,CdiCL2,CL,Cdi
 1410 format( 3x,'CL-alpha            = ', f12.5,
     1       /3x,'Cm-alpha            = ', f12.5,
     2       /3x,'Cm/CL               = ', f12.5,
     3       /3x,'Cdi/CL^2            = ', f12.5,
     4       /3x,'CL (at alpha = 5)   = ', f12.5,
     5       /3x,'Cdi(at alpha = 5)   = ', f12.5)

      if(icase .eq. 4) goto 70
      
      if(itail1.eq.NSECT+1.and.itail2.eq.NSECT+1) goto 70
c     
c     *** Calc. lift-curve slope of tail in downwash ***

      CLATAIL1 = 2.d0*(LIFTTAIL-LIFTAIL0)/(QBAR*S)/ALPHA

      write(6,*)'CLATAIL1 = ',CLATAIL1
c
c     *** Now run VLM without wings and determine the lift-curve
c         slope of the tail ***
c
      write(6,*) 'Performing wingless calculations:'

      IWING1   = IW1
      IWING2   = IW2
      ALPHA    = 0.d0
      q        = 0.d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      =   0

      CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTAIL0,VC,GAMMA,
     +             bplon,slatlon,flaplon,NSECT)

      ALPHA = 1.d0*PI/180.d0
      CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH+Xcg*DSIN(alpha),q,
     +             p,Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplon,slatlon,flaplon,NSECT)
      write(6,*)'LIFT OF TAIL',LIFTTAIL

c     *** Determine lift-curve slope of tail w/o downwash ***
      CLATAIL2 = 2.d0*(LIFTTAIL-LIFTAIL0)/(QBAR*S)/ALPHA
      write(6,*)'CLATAIL2 = ',CLATAIL2

c     *** Lift-curve slope of tial due to downwash ***
      LCSDW = CLATAIL2-CLATAIL1

      WRITE (6,1420) LCSDW
      WRITE(12,1420) LCSDW

 1420 format( 3x,'Lift-curve slope of',
     1       /3x,'tail due to downwash'
     2       /3x,'of wing             = ', f12.5)


 70   IWING1 = 99
      IWING2 =-99
c
c
c
c     *** Get pitch rate damping coefficients ***
c
      write(6,*)'PITCH RATE DAMPING COEFFICIENTS:'
      ALPHA    = 0.d0
      q        = 0.1d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      =   0
      CALL DELZERO(ISECT,DELslat,DELflap,NSECT)
      CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplon,slatlon,flaplon,NSECT)
c
c
      CLq = 2*(LIFT-LIFT0)/(QBAR*S)/(q*(C/2/V))
      CMq = 2*(PITCH-PITCH0)/(QBAR*S*C)/(q*(C/2/V))

      WRITE (6,1430) CLq,CMq
      WRITE(12,1430) CLq,CMq
      if(lotus.eq.'y'.or.lotus.eq.'Y') then
	   open (14,file='lotus.out')
	   write(14,1431)Mach,Cl0,0.0,CM0,S,CLalpha,cref,Cmalpha,b,
     &        CmCl,xcgref,CDiCL2,zcg,CLq,ch,CMq
      endif
 
 1430 format( 3x,'CL-q                = ', f12.5,
     &       /3x,'CM-q                = ', f12.5)
 1431 format(1x,8(f12.5,3x,f12.5,/,1x))

      if(icase .eq. 4) stop

c     *** Get longitudinal control effectiveness(symmetric deflection) ***

      if(icase.ne.4) then
      write(6,*)'PITCH CONTROL EFFECTIVENESS:'
      ALPHA    = 0.d0
      q        = 0.d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      =   0
      DO 200, I = 1, ISECT
         DO 210, J = 1, 2
c
c           *** Catch those cases w/o control surfaces ***
            IF ((J .EQ. 1) .AND. (slatlon(I) .EQ. 0.0)) GOTO 210
            IF ((J .EQ. 2) .AND. (flaplon(I) .EQ. 0.0)) GOTO 210
c           *** Skip TE deflection if all moving surface ***
            IF ((J .EQ. 2) .AND. (slatlon(I) .EQ. 1.0)) GOTO 210
c
            CALL DELZERO(ISECT,DELslat,DELflap,NSECT)
            IF (J .EQ. 1) DELslat(I) = 5.d0*PI/180.d0
            IF (J .EQ. 2) DELflap(I) = 5.d0*PI/180.d0
            CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +                   Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +                   ITAIL1,ITAIL2,IWING1,IWING2,
     +                   LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,      
     +                   bplon,slatlon,flaplon,NSECT)
            DCL(I,J) = 2*(LIFT-LIFT0)/(QBAR*S)/(5.d0*PI/180.d0)
            DCM(I,J) = 2*(PITCH-PITCH0)/(QBAR*S*C)/(5.0*PI/180.d0)   
            WRITE(12,2000) I,J, DCL(I,J)
            WRITE(12,2001) I,J, DCm(I,J)
            WRITE (6,2000) I,J, DCL(I,J)
            WRITE (6,2001) I,J, DCm(I,J)  
 210     CONTINUE
 200  CONTINUE
      endif

 2000 FORMAT (3x,'CL-delta [',I1,' ',I1,']      = ',f12.5)
 2001 FORMAT (3x,'Cm-delta [',I1,' ',I1,']      = ',f12.5)

c
c
c     *** Stop if Longitudinal derivatives only ***
      IF (ICASE .EQ. 1) STOP
c
c
c     *** LATERAL DERIVATIVES FROM HERE DOWN ***
c
c
c     *** Note that when non-dimensionalizinging yaw-rate & yaw moment
c         must account for compressibility distortion by dividing
c         the b (span) by BETA for each yaw-rate and yaw moment.
c         This does not apply to roll (moment and rate) nor
c         sideforce. ***

c     *** Get roll rate damping coefficient (w/o Vertical Tail) ***

 500  write(6,*)'ROLL RATE DAMPING COEFFICIENT:'
      ALPHA    =  0.d0
      q        =  0.d0
      p        =  0.1d0
      ASYM     = -1.d0
      ASYMDEFL =  1.d0
      LAT      =    0
      CALL DELZERO(ISECT,DELslat,DELflap,NSECT)
      CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplon,slatlon,flaplon,NSECT)

c     *** Multiplied by 2 for the other half of configuration ***
      CROLLp = 2.d0*ROLL/(p*(B/2/V))/(QBAR*S*B)
      CYAWp  = 2.d0*YAW/(p*(B/BETA/2/V))/(QBAR*S*B)


c     *** Hold off printing until counting the V. tail contribution ***

c     *** Determine roll control power (of each control surface
c         when deflected antisymmetrically about the x-axis ***

      if(icase.ne.4) then
      write(6,*)'Determining roll control EFFECTIVENESS:'
      ALPHA    =  0.d0
      p        =  0.d0
      q        =  0.d0
      ASYM     = -1.d0
      ASYMDEFL = -1.d0
      LAT      =    0
c
c
      DO 220, I = 1, ISECT
         DO 230, J = 1, 2
c
c           *** Catch those cases w/o control surfaces ***
            IF ((J .EQ. 1) .AND. (slatlon(I) .EQ. 0.0)) GOTO 230
            IF ((J .EQ. 2) .AND. (flaplon(I) .EQ. 0.0)) GOTO 230
c           *** Skip TE deflection if all moving surface ***
            IF ((J .EQ. 2) .AND. (slatlon(I) .EQ. 1.0)) GOTO 230
c
            CALL DELZERO(ISECT,DELslat,DELflap,NSECT)
            IF (J .EQ. 1) DELslat(I) = 5.d0*PI/180.d0
            IF (J .EQ. 2) DELflap(I) = 5.d0*PI/180.d0
C
            CALL CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplon,slatlon,flaplon,NSECT)
            write(6,*) 'yaw = ', yaw
            DCn(I,J) = 2.d0*YAW/(5.d0*PI/180.d0)/(QBAR*S*B/BETA)      
            DCroll(I,J) = -2.d0*ROLL/(5.d0*PI/180.d0)/(QBAR*S*B)
            WRITE(12,2002) I,J, DCroll(I,J)
            WRITE(12,2003) I,J, DCn(I,J)
            WRITE (6,2002) I,J, DCroll(I,J)
            WRITE (6,2003) I,J, DCn(I,J)
 230     CONTINUE
 220  CONTINUE
      endif

 2002 FORMAT (3x,'Cl-delta [',I1,' ',I1,']      = ',f12.5)
 2003 FORMAT (3x,'Cn-delta [',I1,' ',I1,']      = ',f12.5)


c     *** Estimating directional derivatives ***

      write(6,*)'DETERMINING C-y-beta & C-n-beta:'

c     *** Exchange y- & z-coordinates of CG for lat. stab. derivatives ***

      Ycg = Zcg
      Zcg = 0.d0

c
c     *** Estimating sideforce, yaw & roll moment due to side slip ***

      ALPHA = 5.d0*PI/180.d0
c
c     *** Get out of Ground effect for Lat/directional derivatives ***

      CH       = 9E9
      q        = 0.d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      = 1
      CALL DELZERO(JSECT,DELslat,DELflap,NSECT)
      CALL CENTRAL(JSECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplat,slatlat,flaplat,NSECT)

c
      Cybeta = -LIFT/(QBAR*S)/ALPHA
      Cnbeta = -PITCH/(QBAR*S*B/BETA)/ALPHA
      Clbeta = -ROLL/(QBAR*S*B)/ALPHA

      WRITE (6,1435) Cybeta,Cnbeta,Clbeta
      WRITE(12,1435) Cybeta,Cnbeta,Clbeta
 
 1435 format( 3x,'Cy-beta             = ', f12.5,
     1       /3x,'Cn-beta             = ', f12.5,
     2       /3x,'Cl-beta             = ', f12.5)
    

c     *** Estimating yaw rate coefficients ***

      write(6,*)'YAW RATE DAMPING COEFFICIENTS:'
      ALPHA    = 0.d0
      CH       = 9E9
      q        = 0.01d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      = 1
      CALL DELZERO(JSECT,DELslat,DELflap,NSECT)
      CALL CENTRAL(JSECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplat,slatlat,flaplat,NSECT)

      Cyr = LIFT/(QBAR*S)/(q*(B/BETA/2/V))
      Cnr = PITCH/(QBAR*S*B/BETA)/(q*(B/BETA/2/V))
      Clr = ROLL/(QBAR*S*B)/(q*(B/BETA/2/V))

      WRITE (6,1440) Cyr,Cnr,Clr
      WRITE(12,1440) Cyr,Cnr,Clr

 1440 format( 3x,'Cy-r                = ', f12.5,
     1       /3x,'Cn-r                = ', f12.5,
     2       /3x,'Cl-r                = ', f12.5)


c     *** Roll rate coefficient due to vertical tail ***

      write(6,*)'ROLL RATE DAMPING COEFFICIENT (due to V. tail):'
      ALPHA    = 0.d0
      CH       = 9E9
      q        = 0.d0
      p        = 1.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      = 1
      CALL DELZERO(JSECT,DELslat,DELflap,NSECT)
      CALL CENTRAL(JSECT,DELslat,DELflap,ALPHA,CH,q,p,
     +             Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +             ITAIL1,ITAIL2,IWING1,IWING2,
     +             LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +             bplat,slatlat,flaplat,NSECT)
c
c     *** Add to wing and H tail's contribution ***
      CROLLp = CROLLp+ROLL/(p*(B/2/V))/(QBAR*S*B)
      CYAWp  = CYAWp+PITCH/(p*(B/2/v))/(QBAR*S*B/BETA)

      WRITE (6,1450) CROLLp,CYAWp
      WRITE(12,1450) CROLLp,CYAWp

 1450 format( 3x,'Cl-p                = ', f12.5,
     1       /3x,'Cn-p                = ', f12.5)


c     *** Yaw effector coefficients ***

      if(icase.ne.4) then
      write(6,*)'RUDDER EFFECTIVENESS:'
      ALPHA    = 0.d0
      CH       = 9E9
      q        = 0.d0
      p        = 0.d0
      ASYM     = 1.d0
      ASYMDEFL = 1.d0
      LAT      = 1
      DO 260, I = 1, JSECT
         DO 270, J = 1, 2

c           *** Catch those cases w/o control surfaces ***
            IF ((J .EQ. 1) .AND. (slatlat(I) .EQ. 0.0)) GOTO 270
            IF ((J .EQ. 2) .AND. (flaplat(I) .EQ. 0.0)) GOTO 270
c
            CALL DELZERO(JSECT,DELslat,DELflap,NSECT)
            IF (J .EQ. 1) DELslat(I) = 5.d0*PI/180.d0
            IF (J .EQ. 2) DELflap(I) = 5.d0*PI/180.d0
            CALL CENTRAL(JSECT,DELslat,DELflap,ALPHA,CH,q,p,
     +                   Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +                   ITAIL1,ITAIL2,IWING1,IWING2,
     +                   LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,      
     +                   bplat,slatlat,flaplat,NSECT)
            DCy(I,J) = -LIFT/(QBAR*S)/(5.d0*PI/180.d0)
            DCn(I,J) = -PITCH/(QBAR*S*B/BETA)/(5.d0*PI/180.d0)
            DCl(I,J) = -ROLL/(QBAR*S*B)/(5.d0*PI/180.d0)   
            WRITE(12,2004) I,J, DCy(I,J)
            WRITE(12,2005) I,J, DCl(I,J)  
            WRITE(12,2006) I,J, DCN(I,J)
            WRITE (6,2004) I,J, DCy(I,J)
            WRITE (6,2005) I,J, DCl(I,J)  
            WRITE (6,2006) I,J, DCN(I,J)
 270     CONTINUE
 260  CONTINUE
      endif

 2004 FORMAT (3x,'Cy-delta [',I1,' ',I1,']      = ',f12.5)
 2005 FORMAT (3x,'Cl-delta [',I1,' ',I1,']      = ',f12.5)
 2006 FORMAT (3x,'Cn-delta [',I1,' ',I1,']      = ',f12.5)

      STOP
      END

c     *******************************************************************
      SUBROUTINE DELZERO(ISECT,DELslat,DELflap,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 DELslat(NSECT), DELflap(NSECT)
      INTEGER ISECT
c
c     ===================================================================
c     This subroutine sets all surface deflection to zero.
c     ===================================================================
c
      DO 100, I = 1, ISECT
         DELslat(I) = 0.d0
         DELflap(I) = 0.d0
 100  CONTINUE
      RETURN
      END

c     ******************************************************************* 

      SUBROUTINE CENTRAL(ISECT,DELslat,DELflap,ALPHA,CH,q,p,
     +                   Xcg,Ycg,Zcg,V,ASYM,ASYMDEFL,LAT,VCref,GAMMAref,
     +                   ITAIL1,ITAIL2,IWING1,IWING2,
     +                   LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,VC,GAMMA,
     +                   bpin,cslat,cflap,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NNSECT=10)
      REAL*8 VC(NSECT,10,6,3),BP(4,3),bpin(NSECT,4,3),OUT(10,6,3),
     +Cslat(NSECT),Cflap(NSECT)
      REAL*8 Hslat(NNSECT,2,3),Hflap(NNSECT,2,3),DHslat(2,3),
     +DHflap(2,3),DELslat(NSECT),DELflap(NSECT),CP(NNSECT,8,5,3),
     +VNOR(NNSECT,8,5,4)
      REAL*8 VCref(NSECT,10,6,3),GAMMA(NSECT,8,10),GAMMAref(NSECT,8,10)
      REAL*8 LIFT,PITCH,ROLL,LIFTTAIL
      INTEGER ISECT,IP(NNSECT),JP(NNSECT),IPslat(NNSECT),IPflap(NNSECT),
     +LAT

      OPEN (UNIT = 15, FILE = 'RESULTS')
      REWIND (UNIT = 15)

c     ===================================================================
c     This program controls the input of VLM and its grid geometry.
c     It repeatedly calls 'GEOMETRY' to determine the grid points of 
c     each section of the configuration.  Calling 'DEFLECTION' rotates
c     the points on the flap/slats about the hinge lines of the sections.
c     'NORMAL' determines the normal vectors at each control point.
c     After the geometry is set up, VLM is called.

c     Prepared by Jacob Kay,   Sept, 1992.
c     =====================================================================
c
      DO 90, I = 1, ISECT
         IP(I) = 8
         JP(I) = 5
 90   CONTINUE

c     *** For each section ***

      DO 100, K = 1, ISECT


 1000    FORMAT (3F10.5)
         do 30 i = 1,4
	      do 20 j = 1,3
               bp(i,j)=bpin(k,i,j)
   20       continue
   30    continue
c        *** Calculate vortex ring corner point coordinates ***
         CALL GEOMETRY(BP,OUT,ALPHA,IP(K),JP(K),Cslat(K),Cflap(K),
     +                 DHslat,DHflap,IPslat(K),IPflap(K))
c
c        *** Fill vortex ring corner pt coord. matrix ***
         DO 120, I = 1, IP(K)+2
            DO 130, J = 1, JP(K)+1
               VC(K,I,J,1) = OUT(I,J,1)
               VC(K,I,J,2) = OUT(I,J,2)
               VC(K,I,J,3) = OUT(I,J,3)
 130        CONTINUE
 120     CONTINUE
c
c        *** Fill hinge line end point coord. matrix ***
         DO 140, I = 1, 2
            DO 150, J = 1, 3
               Hslat(K,I,J) = DHslat(I,J)
               Hflap(K,I,J) = DHflap(I,J)
 150        CONTINUE
 140     CONTINUE
c
c
 100  CONTINUE
      write(6,*)'Finished defining vortex ring corners.'
c
c
c     *** Reconfigure geometry when deflecting ***
c
      write(15,1011)
c     *** For each section ***
      DO 200, K = 1, ISECT
c
c        *** Slat first ***
c
c        *** Skip if no deflection ***
         IF (DELslat(K) .EQ. 0.d0) GOTO 210
c        
c        *** For all-moving surface ***
         DO 230, J = 1, JP(K)+1
            DO 240, I = 1,IPslat(K)+1
               CALL DEFLECT(Hslat(K,1,1),Hslat(K,1,2),Hslat(K,1,3),
     +                      Hslat(K,2,1),Hslat(K,2,2),Hslat(K,2,3),
     +                      VC(K,I,J,1),VC(K,I,J,2),VC(K,I,J,3),
     +                      DELslat(K))
 240        CONTINUE
 230     CONTINUE
      write(6,*)'Slat deflected.'
c
c        *** Flap ***
 210     IF (DELflap(K) .EQ. 0.0) GOTO 250
         DO 260, J = 1, JP(K)+1
            DO 270, I = IP(K)-IPflap(K)+2,IP(K)+1
               CALL DEFLECT(Hflap(K,1,1),Hflap(K,1,2),Hflap(K,1,3),
     +                      Hflap(K,2,1),Hflap(K,2,2),Hflap(K,2,3),
     +                      VC(K,I,J,1),VC(K,I,J,2),VC(K,I,J,3),
     +                      DELflap(K))
 270        CONTINUE
 260     CONTINUE
      write(6,*)'Flap deflected.'
c
c
c        *** Print 'em out to check ***
 250     WRITE(15,1001)k
         DO 280, J = 1, JP(K)+1
            DO 290, I = 1, IP(K)+1
               WRITE(15,1000) VC(K,I,J,1),VC(K,I,J,2),VC(K,I,J,3)
 290        CONTINUE
 280     CONTINUE
c
c
 200  CONTINUE
 1001 FORMAT ('zone t="Section ',i2,'",i=9,j=6,f=point')
 1011 format ('Title="VLM Planform Output"',/,'variables=x,y,z')

c       
c
c     *** Determine control points and their normal vectors ***
      CALL CONPT(VC,CP,VNOR,IP,JP,ISECT,NSECT)
      write(6,*)'Defining control points and normal vectors.'
c
c     *** PRINT 'EM OUT TO CHECK ***
      WRITE(15,1002)
      DO 300, K = 1, ISECT
         DO 310, J = 1, JP(K)
            DO 320, I = 1, IP(K)
               WRITE(15,1000) CP(K,I,J,1), CP(K,I,J,2), CP(K,I,J,3)
 320        CONTINUE
 310     CONTINUE
 300  CONTINUE
 1002 FORMAT ('zone t="CONTROL POINTS"')
c
c     *** CHECK NORMAL VECTORS ***
      WRITE(15,1003)
      DO 400, K = 1, ISECT
         DO 410, J = 1, JP(K)
            DO 420, I = 1, IP(K)
               WRITE(15,1000) VNOR(K,I,J,1),VNOR(K,I,J,2),VNOR(K,I,J,3)
 420        CONTINUE
 410     CONTINUE
 400  CONTINUE
 1003 FORMAT('zone t="NORMAL VECTORS"')
 
c
c 
c     *** Perform VLM calculation ***
      write(6,*) 'Entering VLM.'
      CALL VLM(VC,CP,VNOR,IP,JP,ISECT,ALPHA,CH,V,Xcg,Ycg,Zcg,q,p,ASYM,
     +         VCref,GAMMAref,ASYMDEFL,DELslat,DELflap,Hslat,Hflap,
     +         IPslat,IPflap,LAT,ITAIL1,ITAIL2,IWING1,IWING2,
     +         LIFT,DRAG,PITCH,ROLL,YAW,LIFTTAIL,GAMMA,NSECT)
C
C
      END


c     *******************************************************************
      SUBROUTINE GEOMETRY(BP,VC,ALPHA,IP,JP,Cslat,Cflap,
     +                    Hslat,Hflap,IPslat,IPflap)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 BP(4,3), Cslat, Cflap, ALPHA, VC(10,6,3), LAMBDA
      REAL*8 Hslat(2,3), Hflap(2,3)

      INTEGER IP, JP, IPslat, IPflap, IPunflap

c     ===================================================================
c     This subroutine will use the corner points of sections of planforms
c     and determine the corner points of vortex rings.
c
c     Prepared by Jacob Kay, VPI&SU  Sept, 1992.
c     ===================================================================

c     *** Begin Loop ***


c     *** Some constants for the section ***

      B         = BP(2,2)-BP(1,2)
      DY        = B/JP
      Xfract    = 1.d0/IP
      IPslat    = NINT(Cslat/Xfract)
      IPflap    = NINT(Cflap/Xfract)
      Cr        = BP(4,1)-BP(1,1)
      Ct        = BP(3,1)-BP(2,1)
      LAMBDA    = Ct/Cr
      THETAr    = -DASIN((BP(4,3)-BP(1,3))/(BP(4,1)-BP(1,1)))
      THETAt    = -DASIN((BP(3,3)-BP(2,3))/(BP(3,1)-BP(2,1)))
c
c     *** At each spanwise station: ***
      DO 110, K = 1, JP+1
         Y      = DY*(K-1)
         CHORD  = Cr+(Ct-Cr)*Y/B
         THETA  = THETAr+DATAN((LAMBDA*DTAN(THETAt-THETAr)*Y/B)/
     +            (1.d0-(1.d0-LAMBDA)*Y/B))
         XLE    = BP(1,1)+(BP(2,1)-BP(1,1))*Y/B
         ZLE    = BP(1,3)+(BP(2,3)-BP(1,3))*Y/B
         XDIS   = 0.25d0*(CHORD/IP)
c
c        *** Define vortex rings' corners on slat ***
c    
c        *** Skip over if no slat ***
         IF (IPslat .EQ. 0) THEN
            DX = 0.d0
         else 
            DX           = Cslat*CHORD/IPslat
         endif
 50      DO 120, J    = 1, IPslat+1
            XINC      = DX*(J-1)
            X         = XLE+XINC+XDIS
            VC(J,K,1) = X*DCOS(ALPHA)-(ZLE-(X-XLE)*
     +                  DTAN(THETA))*DSIN(ALPHA)
            VC(J,K,2) = Y+BP(1,2)
            VC(J,K,3) = -X*DSIN(ALPHA)+(ZLE-(X-XLE)*
     +                   DTAN(THETA))*DCOS(ALPHA)
 120     CONTINUE
c        *** Skip to find hinge line end pts if all-moving surface ***
         IF (Cslat.EQ.1.d0) GO TO 110
c
c
c        *** Define vortex rings'corners on flap ***
         XHINGE = BP(4,1)+(BP(3,1)-BP(4,1))*Y/B-CfLAP*CHORD
c        *** If no flap ***
         IF (IPflap .EQ. 0) THEN
            DX = 0.d0
         else 
            DX = Cflap*CHORD/IPflap
         endif
 60      DO 130, J = IP+1-IPflap, IP+1
            XINC = DX*(J-IP+IPflap-1)
            X = XHINGE+XDIS+XINC
            VC(J,K,1) = X*DCOS(ALPHA)-(ZLE-(X-XLE)*
     +                  DTAN(THETA))*DSIN(ALPHA)
            VC(J,K,2) = Y+BP(1,2)
            VC(J,K,3) = -X*DSIN(ALPHA)+(ZLE-(X-
     +                  XLE)*DTAN(THETA))*DCOS(ALPHA)
 130     CONTINUE
c
c        *** Define vortex rings' corners on unflapped section ***
         IPunflap = IP-IPslat-IPflap
         DX = (VC(IP+1-IPflap,K,1)-VC(IPslat+1,K,1))/IPunflap
         DZ = (VC(IP+1-IPflap,K,3)-VC(IPslat+1,K,3))/IPunflap
         DO 140, J = IPslat+2, IP-IPflap
            X = VC(IPslat+1,K,1)+(J-IPslat-1)*DX
            Z = VC(IPslat+1,K,3)+(J-IPslat-1)*DZ
            VC(J,K,1) = X
            VC(J,K,2) = Y+BP(1,2)
            VC(J,K,3) = Z
 140     CONTINUE
c
c
c        *** Define downstream wake points ***
         VC(IP+2,K,1) = VC(IP+1,K,1)+100*B
         VC(IP+2,K,2) = VC(IP+1,K,2)
         VC(IP+2,K,3) = VC(IP+1,K,3)
c
 110  Continue
c
c     *** Hinge lines ***
      IF (Cslat .EQ. 1.d0) GOTO 300
c
c     *** End points of flap and slat hinge lines ***
      DO 200, I = 1, 3
         Hslat(1,I) = VC(IPslat+1,1,I)
         Hslat(2,I) = VC(IPslat+1,JP+1,I)
         Hflap(1,I) = VC(IP-IPflap+1,1,I)
         Hflap(2,I) = VC(IP-IPflap+1,JP+1,I)
 200  CONTINUE
c
c
c
      RETURN
c
c
c     *** Find hinge line end points if all-moving surface ***
c
c     *** One pt going thru c/4 of mean chord ***
 300  Ymac       = B*(1.d0+2.d0*LAMBDA)/(3.d0*(1.d0+LAMBDA))+BP(1,2)
      Cmac       = Cr+(Ct-Cr)*Ymac/B
      XLE        = BP(1,1)+(BP(2,1)-BP(1,1))*Ymac/B
      ZLE        = BP(1,3)+(BP(2,3)-BP(1,3))*Ymac/B
      X          = XLE+.25*Cmac
      THETA      = THETAr+DATAN((LAMBDA*DTAN(THETAt-THETAr)*Y/B)/
     +             (1.d0-(1.d0-LAMBDA)*Y/B))
      Hslat(2,1) = X*DCOS(ALPHA)-(ZLE-(X-XLE)*DTAN(THETA))*DSIN(ALPHA)
      Hslat(2,2) = Ymac
      Hslat(2,3) =-X*DSIN(ALPHA)+(ZLE-(X-XLE)*DTAN(THETA))*DCOS(ALPHA) 
c
c     *** The other pt forms a line perpendicular to A/C centerline ***
      XLE          = BP(1,1)
      ZLE          = BP(1,3)
      Hslat(1,1)   = Hslat(2,1)
      Hslat(1,2)   = BP(1,2)
      THETA        = THETAr
      X            = (ZLE*DSIN(ALPHA)+XLE*DTAN(THETA)*DSIN(ALPHA))/
     +               (DCOS(ALPHA)+DSIN(ALPHA)*DTAN(THETA))
      Hslat(1,3)   =-X*DSIN(ALPHA)+(ZLE-(X-XLE)*DTAN(THETA))
     +              *DCOS(ALPHA)

      RETURN                                             

      END



c     ******************************************************************
      SUBROUTINE DEFLECT(A1,A2,A3,B1,B2,B3,P1,P2,P3,DEL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 C(3), MAG, T(3,3), TINV(3,3)
c
c     ===================================================================
c     This subroutine calculates the new coordinates of a point
c     after been rotated certain degrees about an arbitrary axis of
c     rotation (described by two points).
c
c     Prepared by Jacob Kay, Sept, 1992.
c     ===================================================================
c
c
C     PI = 4.d0*DATAN(1.d0)
c
c     *** Find point C on a line-segment AB ***
      MAG = -((P1-A1)*(A1-B1)+(P2-A2)*(A2-B2)+
     +        (P3-A3)*(A3-B3))/((A1-B1)**2+(A2-B2)**2+
     +        (A3-B3)**2)
      C(1) = A1+MAG*(B1-A1)
      C(2) = A2+MAG*(B2-A2)
      C(3) = A3+MAG*(B3-A3)
c
c     *** Vector from A to B ***
      X0 = B1-A1
      Y0 = B2-A2
      Z0 = B3-A3
c     *** Vector from C to P ***
      X1 = P1-C(1)
      Y1 = P2-C(2)
      Z1 = P3-C(3)
c
c     *** Calculate transformation to rotate the inertial x-axis to 
c         be parallel to the vector from A to B ***
      PSI   =  DATAN2(Y0,X0)
      THETA = -DASIN(Z0/DSQRT(X0**2+Y0**2+Z0**2))
c
c     *** Get transformation matrix (including deflection) ***
      CALL TRANSFORM(PSI,THETA,-DEL,T)
c
c     *** Vector from C to P' (P after deflection) expressed in axis 
c         system where x-axis is parallel to the vector from A to B ***
      X2P = T(1,1)*X1+T(1,2)*Y1+T(1,3)*Z1
      Y2P = T(2,1)*X1+T(2,2)*Y1+T(2,3)*Z1
      Z2P = T(3,1)*X1+T(3,2)*Y1+T(3,3)*Z1
c
c     *** Getting back to the inertial axis system ***

      CALL TRANSFORM(PSI,THETA,0.0,T)
c
c     *** Transpose of T ***
      DO 100, I       = 1,3
         DO 110, J    = 1,3
            TINV(I,J) = T(J,I)
 110     CONTINUE
 100  CONTINUE
c
c     *** Vector from C to P' expressed in inertial axis system ***
      X2 = TINV(1,1)*X2P+TINV(1,2)*Y2P+TINV(1,3)*Z2P
      Y2 = TINV(2,1)*X2P+TINV(2,2)*Y2P+TINV(2,3)*Z2P
      Z2 = TINV(3,1)*X2P+TINV(3,2)*Y2P+TINV(3,3)*Z2P
c     *** Position of P' (P after deflection) in inertial axis system ***
      P1 = C(1)+X2
      P2 = C(2)+Y2
      P3 = C(3)+Z2
c
c
c
      RETURN
      END

c     ******************************************************************
      SUBROUTINE TRANSFORM(PSI, THETA, PHI, A)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(3,3), PSI, THETA, PHI
c
c     ==================================================================
c     This subroutine determines the transformation matrix for a axis
c     system rotation psi about the z-axis, then theta about the y'-axis
c     and finally phi about the x"-axis.
c     ==================================================================
c
c     *** CALC. MATRIX ELEMENT ***
      A(1,1) = DCOS(THETA)*DCOS(PSI)
      A(1,2) = DCOS(THETA)*DSIN(PSI)
      A(1,3) =-DSIN(THETA)
      A(2,1) = DSIN(PHI)*DSIN(THETA)*DCOS(PSI)-DCOS(PHI)*DSIN(PSI)
      A(2,2) = DSIN(PHI)*DSIN(THETA)*DSIN(PSI)+DCOS(PHI)*DCOS(PSI)
      A(2,3) = DSIN(PHI)*DCOS(THETA)
      A(3,1) = DCOS(PHI)*DSIN(THETA)*DCOS(PSI)+DSIN(PHI)*DSIN(PSI)
      A(3,2) = DCOS(PHI)*DSIN(THETA)*DSIN(PSI)-DSIN(PHI)*DCOS(PSI)
      A(3,3) = DCOS(PHI)*DCOS(THETA)
      RETURN	
      END


c     *******************************************************************
      SUBROUTINE CONPT(QF,QC,DS,IP,JP,ISECT,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 QF(NSECT,10,6,3),QC(NSECT,8,5,3),DS(NSECT,8,5,4)
      INTEGER IP(NSECT), JP(NSECT)
c
c
c     ==================================================================
c     This subroutine determines the locations and the normal vectors
c     of control points of each section.
c     ==================================================================
c     *** Control Points ***
      DO 100, K = 1, ISECT
         DO 120, J = 1, JP(K)
            DO 130, I = 1, IP(K)
               QC(K,I,J,1) = (QF(K,I,J,1)+QF(K,I,J+1,1)+
     +                        QF(K,I+1,J+1,1)+QF(K,I+1,J,1))/4.d0
               QC(K,I,J,2) = (QF(K,I,J,2)+QF(K,I,J+1,2)+
     +                        QF(K,I+1,J+1,2)+QF(K,I+1,J,2))/4.d0
               QC(K,I,J,3) = (QF(K,I,J,3)+QF(K,I,J+1,3)+
     +                        QF(K,I+1,J+1,3)+QF(K,I+1,J,3))/4.d0
c
c           *** Calulate panels' normal vectors ***
            CALL NORMAL (QF(K,I,J,1),QF(K,I,J,2),QF(K,I,J,3),
     +	                  QF(K,I+1,J,1),QF(K,I+1,J,2),QF(K,I+1,J,3),
     +                   QF(K,I,J+1,1),QF(K,I,J+1,2),QF(K,I,J+1,3),
     +                   QF(K,I+1,J+1,1),QF(K,I+1,J+1,2),
     +                   QF(K,I+1,J+1,3),DS(K,I,J,1),
     +                   DS(K,I,J,2),DS(K,I,J,3),DS(K,I,J,4))
 130        CONTINUE
 120     CONTINUE
 100  CONTINUE
c
c
      RETURN
      END


c     ********************************************************************
      SUBROUTINE NORMAL(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,c1,c2,c3,s)
      IMPLICIT REAL*8 (A-H,O-Z)
c
c     ====================================================================
c     This subroutine calculates the average normal vector of a 4-corner
c     panel and its area.
c     ====================================================================
c
      xa   = x2-x3
      ya   = y2-y3
      za   = z2-z3
      xb   = x4-x1
      yb   = y4-y1
      zb   = z4-z1
c     *** Normal vector components ***
      x    = ya*zb-za*yb
      y    = xb*za-xa*zb
      z    = xa*yb-ya*xb
      amag = DSQRT(x**2+y**2+z**2)
c     *** Normalizing the normal vector ***
      c1   = x/amag
      c2   = y/amag
      c3   = z/amag
c     *** Normal area ***
      e1   = x3-x1
      e2   = y3-y1
      e3   = z3-z1
      f1   = x2-x1
      f2   = y2-y1
      f3   = z2-z1
c   
      s11  = f2*zb-f3*yb
      s12  = xb*f3-f1*zb
      s13  = f1*yb-f2*xb
      s21  = yb*e3-zb*e2
      s22  = e1*zb-xb*e3
      s23  = xb*e2-yb*e1
      s    = 0.5d0*(DSQRT(s11**2+s12**2+s13**2)+
     +              DSQRT(s21**2+s22**2+s23**2))
c
c
      RETURN
      END


c     ******************************************************************
      SUBROUTINE VLM(QF,QC,DS,IP,JP,ISECT,ALPHA,CH,VF,Xcg,Ycg,Zcg,
     +               q,p,ASYM,
     +               QFref,GAMMAref,ASYMDEFL,DELslat,DELflap,Hslat,
     +               Hflap,IPslat,IPflap,LAT,ITAIL1,ITAIL2,
     +               IWING1,IWING2,
     +               LIFT,DRAG,PITCH,roll,YAW,LIFTTAIL,GAMMA,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NNSECT=10)
      REAL*8 LIFT,PITCH,ROLL, LIFTTAIL,xmact,zmact

      common /consts/ PI
      REAL*8 QF(NSECT,10,6,3), QC(NSECT,8,5,3), DS(NSECT,8,5,4)
      REAL*8 GAMMA(NSECT,8,10),GAMMAref(NSECT,8,10),QFref(NSECT,10,6,3)
      REAL*8 A(40*NNSECT,40*NNSECT),GAMMA1(40*NNSECT),DW(40*NNSECT),
     +        SYM(40*NNSECT)
      REAL*8 GAMMAnew(NNSECT,8,10),DELslat(NSECT),DELflap(NSECT)
      REAL*8 Hslat(NSECT,2,3),Hflap(NSECT,2,3),A1(NNSECT,9,5)
      INTEGER IPslat(NSECT),IPflap(NSECT),JP(NSECT),IP(NSECT)
c    
c     ==================================================================
c     This program uses a lattice of closed vortex rings to estimate
c     aerodynamic properties of a planform.  
c
c     Prepared by Jacob Kay		Sept. 1992.
c     ==================================================================
c
c
      RHO = 1.d0
c
      pi  = 4.d0*DATAN(1.d0)
c
c     *** Set gamma to 1 for influence coefficient matrix calulation. ***
      DO 102, K = 1, NSECT
         DO 100, I = 1, 8
            DO 101, J = 1, 10
               GAMMA(K,I,J) = 1.d0
 101        CONTINUE
 100     CONTINUE
 102  CONTINUE
      sn1 = DSIN(alpha)
      cs1 = DCOS(alpha)
c
c     *** For antisymmetric surface deflections, the reference gamma
c         distribution has to be reconfigured to account for the change
c         in geometry. ***
      IF (ASYM .EQ. -1.0) THEN
      DO 50, M = 1, ISECT
         DO 55, I = 1, 1
            DO 55, J = 1, JP(M)
               GAMMAnew(M,I,J) = GAMMAref(M,I,J)*(QFref(M,I,J+1,2)-
     +                           QFref(M,I,J,2))/
     +                           (QF(M,I,J+1,2)-QF(M,I,J,2))
 55      CONTINUE
         DO 50, I = 2, IP(M)
            DO 50, J = 1, JP(M)
               GAMMAnew(M,I,J) = (GAMMAref(M,I,J)-GAMMAref(M,I-1,J))*
     +                           (QFref(M,I,J+1,2)-QFref(M,I,J,2))/
     +                           (QF(M,I,J+1,2)-QF(M,I,J,2))+
     +                           GAMMAnew(M,I-1,J)
 50   CONTINUE
      ENDIF 
c
c
c     *** Influence Coefficients ***
c     *** Sum up the induced velocity due to all vortex rings at each
c         control point. ***
c
      K = 0
c     *** For each control pt. ***
      DO 105, M = 1, ISECT
      IF ((M.GE.IWING1).AND.(M.LE.IWING2)) GOTO 105
      write(6,*)'CALCULATING INFLUENCE COEFFICIENTS FOR SECTION',M
      DO 110, I = 1, IP(M)
         DO 120, J = 1, JP(M)
            K = K+1

 2001 format(2x,'m=',i2,2x,'i=',i2,2x,'ip(m)=',i2,2x,'j=',i2,2x,
     1          'jp(m)=',i2,2x,'k=',i3)


c           *** The original half of wing ***
            CASE = 1.d0
            CALL WING(QC(M,I,J,1),QC(M,I,J,2),QC(M,I,J,3),GAMMA,u,v,w,
     +                1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
c
            L = 0
            DO 125, M1 = 1, ISECT
               IF ((M1.GE.IWING1).AND.(M1.LE.IWING2)) GOTO 125
               DO 130, I1 = 1, IP(M1)
                  DO 140, J1 = 1, JP(M1)
                     L = L+1
                     A(K,L) = A1(M1,I1,J1)
 140              CONTINUE
 130           CONTINUE
 125        CONTINUE
c
c          *** Skip if lateral case ***
            IF (LAT .EQ. 1) GOTO 170
c          *** The other half of wing ***
            CASE = 2.d0

c           *** Reverse flap deflection for antisymmetric deflection ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT)
            CALL WING(QC(M,I,J,1),-QC(M,I,J,2),QC(M,I,J,3),GAMMA,u,v,w,
     +                1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
c
c           *** Return vortex corners back to its original deflected
c               position. ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT) 
            L = 0
            DO 145, M1 = 1, ISECT
               IF ((M1.GE.IWING1).AND.(M1.LE.IWING2)) GOTO 145
               DO 150, I1 = 1, IP(M1)
                  DO 160, J1 = 1, JP(M1)
                     L = L+1
                     A(K,L) = A(K,L)+ASYM*A1(M1,I1,J1)
 160              CONTINUE
 150           CONTINUE
 145        CONTINUE
            IF (CH.GT.9.0E4) GOTO 170
c           
c           *** Ground effect (mirror image) ***
            CASE = 3.d0
            CALL WING(QC(M,I,J,1),QC(M,I,J,2),-2*CH-QC(M,I,J,3),GAMMA,
     +                u,v,w,1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
            L = 0
            DO 175, M1 = 1, ISECT
               IF ((M1.GE.IWING1).AND.(M1.LE.IWING2)) GOTO 175
               DO 180, I1 = 1, IP(M1)
                  DO 190, J1 = 1, JP(M1)
                     L = L+1
                     A(K,L) = A(K,L)+A1(M1,I1,J1)
 190              CONTINUE
 180           CONTINUE
 175        CONTINUE
c
c           *** Ground effect due to other half of wing ***
            CASE = 4.d0
c           *** Reverse flap deflection for antisymmetric deflection ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT)
c
            CALL WING(QC(M,I,J,1),-QC(M,I,J,2),-2*CH-QC(M,I,J,3),GAMMA,
     +                u,v,w,1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
c
c           *** Return vortex corners back to its original deflected
c               position. ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT) 
            L = 0
            DO 195, M1 = 1, ISECT
               IF ((M1.GE.IWING1).AND.(M1.LE.IWING2)) GOTO 195
               DO 200, I1 = 1, IP(M1)
                  DO 210, J1 = 1, JP(M1)
                     L = L+1
                     A(K,L) = A(K,L)+ASYM*A1(M1,I1,J1)
 210              CONTINUE
 200           CONTINUE
 195        CONTINUE
c
c
c           *** Downwash due to free stream & roll/pitch rate induced
c               velocity components ***
c
c
c           *** If flow is antisymmetric as in the cases of roll rate
c               damping calculations and antisymmetric deflections,
c               the problem becomes solving the antisymmetric vortex
c               distribution.  The symmetric gamma distribution is
c               designated as GAMMAnew and is known.  It will be
c               accounted for on the RHS. ***
c           
  170       SYM(K) = 0.d0
            IF (ASYM.EQ.-1.d0) THEN

c 
c           *** The original half of wing ***
            CASE = 1.d0
            CALL WING(QC(M,I,J,1),QC(M,I,J,2),QC(M,I,J,3),GAMMAnew,
     +                u,v,w,
     +                1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
            SYM(K) = u*DS(M,I,J,1)+v*DS(M,I,J,2)+w*DS(M,I,J,3)
c
c
c           *** The other half of wing ***
            CASE = 2.d0
c           *** Reverse flap deflection for antisymmetric deflection ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT)
c 
           CALL WING(QC(M,I,J,1),-QC(M,I,J,2),QC(M,I,J,3),GAMMAnew,
     +                u,v,w,
     +                1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
c           *** Return vortex corners back to its original deflected
c               position. ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT) 
c
            SYM(K) = SYM(K)+u*DS(M,I,J,1)-v*DS(M,I,J,2)+w*DS(M,I,J,3)
            IF (CH.GT.100.d0) GOTO 215
c           
c           *** Ground effect (mirror image) ***
            CASE = 3.d0
            CALL WING(QC(M,I,J,1),QC(M,I,J,2),-2*CH-QC(M,I,J,3),
     +                GAMMAnew,
     +                u,v,w,1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
            SYM(K) = SYM(K)+u*DS(M,I,J,1)+v*DS(M,I,J,2)-w*DS(M,I,J,3)
c
c           *** Ground effect due to other half of wing ***
            CASE = 4.d0
c           *** Reverse flap deflection for antisymmetric deflection ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT)
c
            CALL WING(QC(M,I,J,1),-QC(M,I,J,2),-2*CH-QC(M,I,J,3),
     +                GAMMAnew,
     +                u,v,w,1.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
c
c           *** Return vortex corners back to its original deflected
c               position. ***
            IF (ASYMDEFL .EQ. -1.d0) CALL REVERSE(DELslat,DELflap,
     +                              QF,Hslat,Hflap,IPslat,IPflap,
     +                              ISECT,IP,JP,NSECT) 
c
            SYM(K) = SYM(K)+u*DS(M,I,J,1)-v*DS(M,I,J,2)-w*DS(M,I,J,3)
c
c           *** Done computing symmetrical gamma's contribution to be
c               added to the RHS ***
            ENDIF
c
c
 215        uinf = VF
            vinf = 0.d0
            winf = q*(QC(M,I,J,1)-Xcg)-p*(QC(M,I,J,2)-Ycg)
            DW(K) = -(uinf*DS(M,i,j,1)+vinf*DS(M,i,j,2)+winf*
     +              DS(M,i,j,3))-SYM(K)
c
c           *** Total number of panels
            ISUM = K         
 120     CONTINUE
 110  CONTINUE
 105  CONTINUE
C
C     *** check influence coefficient matrix ***
      DO 1001, I = 1,8
         WRITE(15,1002) A(I,1),A(I,2),A(I,3),A(I,4),A(I,5),A(I,6),
     +                  A(I,7),A(I,8),
     +                  A(I,9),DW(I)
 1001 CONTINUE
 1002 FORMAT(10(F8.5,2X))
C
c
c     *** Solve for vortex strengths ***
      write(6,*)'CALLING MATRIX'
      CALL MATRIX(ISUM,A,DW,GAMMA1,NSECT)
c
c
c     *** Reassign system solution to GAMMA array ***
      K = 0
      DO 225 M = 1, ISECT
         IF ((M.GE.IWING1).AND.(M.LE.IWING2)) GOTO 225
         DO 220, I = 1, IP(M)
            DO 230, J = 1, JP(M)
               K = K+1
               GAMMA(M,I,J) = GAMMA1(K)
 230        CONTINUE
 220     CONTINUE
 225  CONTINUE
c
c
c     *** Determine forces and moment ***
      FL = 0.d0
      FD = 0.d0
      FM = 0.d0
      RM = 0.d0
      YM = 0.d0

      DO 245 M=1,ISECT
         IF ((M.GE.IWING1).AND.(M.LE.IWING2)) GOTO 245
         DO 240, I = 1, IP(M)
            DO 250, J = 1, JP(M)
               IF (I.EQ.1) GAMMAIJ = GAMMA(M,I,J)
               IF (I.GT.1) GAMMAIJ = GAMMA(M,I,J)-GAMMA(M,I-1,J)
               SPANP = QF(M,I,J+1,2)-QF(M,I,J,2)
               DLIFT = RHO*VF*GAMMAIJ*SPANP
               DSIDEFORCE = -RHO*VF*GAMMAIJ*(QF(M,I,J+1,3)-
     +                      QF(M,I,J,3))
               FL = FL+DLIFT
c
c              *** Induced Drag ***
               CALL WING(QC(M,I,J,1),QC(M,I,J,2),QC(M,I,J,3),GAMMA,
     +                u1,v1,w1,
     +                0.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2)
               CALL WING(QC(M,I,J,1),-QC(M,I,J,2),QC(M,I,J,3),GAMMA,
     +                u2,v2,w2,
     +                0.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
               IF (CH .GT. 9.0E4) GOTO 260
               CALL WING(QC(M,I,J,1),QC(M,I,J,2),-2*CH-QC(M,I,J,3),
     +                GAMMA,u3,v3,w3,
     +                0.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
               CALL WING(QC(M,I,J,1),-QC(M,I,J,2),-2*CH-QC(M,I,J,3),
     +                GAMMA,u4,v4,w4,
     +                0.d0,M,I,J,DS,ip,jp,CASE,A1,QF,ISECT,
     +                IWING1,IWING2,NSECT)
               GOTO 265
 260           W3 = 0.d0
               W4 = 0.d0
 265           WIND = W1+W2-W3-W4
               ALPHAI = -WIND/VF
               DDRAG = RHO*SPANP*VF*GAMMAIJ*ALPHAI
               FD = FD+DDRAG
c
c 
c              *** Moments w/r C.G. ***
               xmact   =-((QF(M,I,J,1)+QF(M,I,J+1,1))/2-Xcg*DCOS(alpha))
               zmact   =-((QF(M,I,J,3)+QF(M,I,J+1,3))/2+Xcg*DSIN(alpha))
               DMOMENT =  xmact*DLIFT+zmact*ddrag
c              *** Moments for antisymmetric cases ***
c              *** To locate the aerodyanmic center of panel ***
               CLAMBDA = (QF(M,I+1,J+1,1)-QF(M,I,J+1,1))/
     +                   (QF(M,I+1,J,1)-QF(M,I,J,1))
               YARM    = QF(M,I,J,2)+(QF(M,I,J+1,2)-QF(M,I,J,2))/3.d0*
     +                   ((1.d0+2.d0*CLAMBDA)/(1.0+CLAMBDA))-Ycg
               ZARM    = (QF(M,I,J+1,3)+QF(M,I,J,3))/2.0-Zcg
               Rmoment = YARM*DLIFT-ZARM*DSIDEFORCE
               XARM    = (QF(M,I,J+1,1)+QF(M,I,J,1))/2.d0-Xcg
               Ymoment = XARM*DSIDEFORCE-YARM*DDRAG         
c
c
               FM = FM+DMOMENT
               RM = RM+RMOMENT
               YM = YM+YMOMENT
 250        CONTINUE
            WRITE(15,1002)GAMMA(M,I,1),GAMMA(M,I,2),GAMMA(M,I,3),
     +      GAMMA(M,I,4),GAMMA(M,I,5),GAMMA(M,I,6),GAMMA(M,I,7),
     +      GAMMA(M,I,8),GAMMA(M,I,9),GAMMA(M,I,10)
 240     CONTINUE
 245  CONTINUE

 2000 format(/4x,'m',4x,'i',4x,'j',3x,'dlift',5x,'xmact',5x,'dmoment',
     1         5x,'xcg',5x,'QF(M,I,J,1)',2x,'QF(M,I,J+1,1)')
 2005 format(3i5,6f12.5)
c
c
      LIFT = FL
      DRAG = FD
      PITCH = FM
c     *** 'roll' is the dimensional roll moment of half of plane (used
c         only for asymmetric loading only. ***
      roll = Rm
c
c     *** 'YAW' is the dimensional yaw moment of half of plane used 
c          only for asymmetric loading only. ***
      YAW = YM
c
c     *** This section is used only to calculate the lift due
c         tail section(s) ***
c
      LIFTTAIL = 0.d0
      DO 305, K = 1, ISECT
      IF ((K.GE.IWING1).AND.(K.LE.IWING2)) GOTO 305
c
      DO 300, I = 1, IP(K)
         IF ((QC(K,I,1,1).GE.QC(ITAIL1,1,1,1)).OR.
     +       (QC(K,I,1,1).GE.QC(ITAIL2,1,1,1))) THEN
         DO 310, J = 1, JP(K)
            IF(I.EQ.1) GAMMAIJ = GAMMA(K,I,J)
            IF(I.GT.1) GAMMAIJ = GAMMA(K,I,J)-GAMMA(K,I-1,J)
            SPANP = QF(K,I,J+1,2)-QF(K,I,J,2)
            LIFTTAIL = LIFTTAIL+RHO*VF*GAMMAIJ*SPANP
 310     CONTINUE
         ENDIF
 300  CONTINUE          
 305  CONTINUE     
c
c
c
      RETURN
      END

c     ********************************************************************
      SUBROUTINE WING(X,Y,Z,GAMMA,U,V,W,ONOFF,M1,I1,J1,
     +                DS,IP,JP,CASE,A1,QF,ISECT,IWING1,IWING2,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 GAMMA(NSECT,8,10),QF(NSECT,10,6,3),A1(NSECT,9,5),
     +DS(NSECT,8,5,4)
      INTEGER IP(NSECT),JP(NSECT)
c
c     ====================================================================
c     This subroutine determines the induced velocity at (x,y,z) due to 
c     the (semi-span) vortex lattice system.
c     ====================================================================
c
c     *** Initialize some variables ***
      u = 0.d0
      v = 0.d0
      w = 0.d0
c
      DO 50, M = 1, ISECT
         IF ((M.GE.IWING1).AND.(M.LE.IWING2)) GOTO 50      
         DO 100, I = 1, IP(M)+1
            DO 110, J = 1, JP(M)
c              *** For trailing vortices ***
               IF (I.EQ.IP(M)+1) THEN
                  VORTIC = GAMMA(M,IP(M),J)
                  GOTO 120
               ENDIF
c
c
               VORTIC = GAMMA(M,I,J)
               U0 = 0.d0
               V0 = 0.d0
               W0 = 0.d0
c
c              *** Skip for induced drag calc. ***
 120           continue
               U1 = 0.0D0
               V1 = 0.0D0
               W1 = 0.0D0
               U3 = 0.0D0
               V3 = 0.0D0
               W3 = 0.0D0

               IF (ONOFF.EQ.0.0) GOTO 130
c
c              *** Leading segment of vortex ring ***
               CALL VORTEX (X,Y,Z,QF(M,I,J,1),QF(M,I,J,2),QF(M,I,J,3),
     +                      QF(M,I,J+1,1),QF(M,I,J+1,2),QF(M,I,J+1,3),
     +                      VORTIC,U1,V1,W1)
c
c              *** Trailing segment of vortex ring ***  
               CALL VORTEX(X,Y,Z,QF(M,I+1,J+1,1),QF(M,I+1,J+1,2),
     +                     QF(M,I+1,J+1,3),QF(M,I+1,J,1),
     +                     QF(M,I+1,J,2),QF(M,I+1,J,3),
     +                     VORTIC,U3,V3,W3)
c
c              *** Two side segments ***
 130           CALL VORTEX (X,Y,Z,QF(M,I,J+1,1),QF(M,I,J+1,2),
     +                      QF(M,I,J+1,3),QF(M,I+1,J+1,1),
     +                      QF(M,I+1,J+1,2),QF(M,I+1,J+1,3),
     +                      VORTIC,U2,V2,W2)
               CALL VORTEX (X,Y,Z,QF(M,I+1,J,1),QF(M,I+1,J,2),
     +                      QF(M,I+1,J,3),QF(M,I,J,1),
     +                      QF(M,I,J,2),QF(M,I,J,3),
     +                      VORTIC,U4,V4,W4)
c
c           *** Sum vortex-induced velocity components from secgments ***
               U0 = U2+U4+(U1+U3)*ONOFF
               V0 = V2+V4+(V1+V3)*ONOFF
               W0 = W2+W4+(W1+W3)*ONOFF
c
c              *** Skip if Calculating Induced Drag ***
               IF (ONOFF .EQ. 0.d0) GOTO 140
c
c              *** For different semi-span vortice lattice systems ***
               IF (CASE .EQ. 1.d0)
     +            A1(M,I,J) = U0*DS(M1,I1,J1,1)+V0*DS(M1,I1,J1,2)+
     +                        W0*DS(M1,I1,J1,3)
               IF (CASE .EQ. 2.d0)
     +            A1(M,I,J) = U0*DS(M1,I1,J1,1)-V0*DS(M1,I1,J1,2)+
     +                        W0*DS(M1,I1,J1,3)
               IF (CASE .EQ. 3.d0)
     +            A1(M,I,J) = U0*DS(M1,I1,J1,1)+V0*DS(M1,I1,J1,2)-
     +                        W0*DS(M1,I1,J1,3)
               IF (CASE .EQ. 4.d0)
     +            A1(M,I,J) = U0*DS(M1,I1,J1,1)-V0*DS(M1,I1,J1,2)-
     +                        W0*DS(M1,I1,J1,3)

c              *** Adding trailing (free) vortex influence to T.E. vortex 
c                  ring since they have the same strength ***
 140           IF (I.EQ.IP(M)+1) A1(M,IP(M),J) = A1(M,IP(M),J)+
     +                                           A1(M,IP(M)+1,J)
               U = U+U0
               V = V+V0
               W = W+W0
c
c              
 110        CONTINUE
 100     CONTINUE
 50   CONTINUE
c
c
c
      RETURN
      END


c     ********************************************************************
      SUBROUTINE VORTEX(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,GAMMA,U,V,W)
      IMPLICIT REAL*8 (A-H,O-Z)
c
c     ====================================================================
c     This subroutine determines the induced velocity at (X,Y,Z) due to 
c     the vortex segment from (X1,Y1,Z1) to (X2,Y2,Z2).
c     ====================================================================
c
      common /consts/ PI

C     pi = 4.d0*DATAN(1.d0)
c     *** R1 is the vector from (X1,Y1,Z1) to (X,Y,Z) ***
c     *** R2 is the vector from (X2,Y2,Z2) to (X,Y,Z) ***
c     *** R1 cross R2 ***
      R1R2X  =   (Y-Y1)*(Z-Z2) - (Z-Z1)*(Y-Y2)
      R1R2Y  = -((X-X1)*(Z-Z2) - (X-X2)*(Z-Z1))
      R1R2Z  =   (X-X1)*(Y-Y2) - (X-X2)*(Y-Y1)
c
c     *** Magnitude (squared) of vectors ***
      SQUARE = (R1R2X**2) + (R1R2Y**2) + (R1R2Z**2)
      R1     = DSQRT((X-X1)**2 + (Y-Y1)**2 + (Z-Z1)**2)
      R2     = DSQRT((X-X2)**2 + (Y-Y2)**2 + (Z-Z2)**2)
c
c     *** Dot products ***
      R0R1   = (X2-X1)*(X-X1)+(Y2-Y1)*(Y-Y1)+(Z2-Z1)*(Z-Z1)
      R0R2   = (X2-X1)*(X-X2)+(Y2-Y1)*(Y-Y2)+(Z2-Z1)*(Z-Z2) 
c
      COEFF  = GAMMA/(4.d0*PI*SQUARE)*(R0R1/R1 - R0R2/R2)
      U      = R1R2X*COEFF
      V      = R1R2Y*COEFF
      W      = R1R2Z*COEFF    
C
      RETURN
      END

c     *******************************************************************
      SUBROUTINE MATRIX (N,A,D,C,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER N
      DIMENSION A(40*NSECT,40*NSECT),C(40*NSECT),D(40*NSECT)

C      NOTE:  N=SIZE OF MATRIX
C      USER MUST UPDATE THE VALUE OF N IN THE ABOVE PARAMETER STATEMENT
C      INPUT MATRIX [A] & [D]; OUTPUT MATRIX [C] SUCH THAT [A][C]=[D]

C      STARTING MATRIX INVERSION

      DO 10, I = 1,N
 10   C(I)     = 0.d0

      DO 1100 K= 1,N - 1
      M = K + 1
      L = K
 1130 Q = ABS(A(M,K)) - ABS(A(L,K))
      IF (Q.GT.0.d0) L = M
      IF (M.LT.N)   THEN
                    M=M+1
                    GO TO 1130
                    ENDIF
      IF (L.EQ.K) GO TO 1110

      DO 1200 J = K, N
      DU        = A(K,J)
      A(K,J)    = A(L,J)
      A(L,J)    = DU
 1200 CONTINUE

      DD        = D(K)
      D(K)      = D(L)
      D(L)      = DD

 1110 M         = K + 1
 1120 Q         = A(M,K)/A(K,K)

      A(M,K)    = 0.d0
      DO 1300 J = K+1, N
      A(M,J)    = A(M,J)-Q*A(K,J)
 1300 CONTINUE

      D(M)      = D(M) - Q*D(K)
      IF (M.LT.N) THEN
                  M = M+1
                  GO TO 1120
                  ENDIF
 1100 CONTINUE
      C(N)      = D(N)/A(N,N)
      DO 1500 M = N-1,1,-1
      Q         = 0.d0
      DO 1400 J = M+1,N
      Q         = Q + A(M,J)*C(J)
      C(M)      = (D(M)-Q)/A(M,M)
 1400 CONTINUE
 1500 CONTINUE

       END


c     *****************************************************************
      SUBROUTINE REVERSE(DELslat,DELflap,VC,Hslat,Hflap,IPslat,
     +                   IPflap,ISECT,IP,JP,NSECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 DELslat(NSECT),DELflap(NSECT),VC(NSECT,10,6,3),
     +Hslat(NSECT,2,3),Hflap(NSECT,2,3)
      INTEGER IPslat(NSECT),IPflap(NSECT),ISECT,IP(NSECT),JP(NSECT)
c
c     ==================================================================
c     This subroutine is to reverse the control surfaces' deflection
c     in the opposite direction for antisymmetric deflection cases.
c     Calling this subroutine the second time will deflect the affected
c     control surface(s) back to the original deflection angle.
c     ==================================================================
c
c     *** Find the affected vortex corner points ***
      DO 100, M = 1, ISECT
c
c        *** If the deflected is (are) slat(s) ***
         IF (DELslat(M) .NE. 0.d0) THEN
            DO 110, J = 1, JP(M)+1
               DO 120, I = 1, IPslat(M)
                  CALL DEFLECT(Hslat(M,1,1),Hslat(M,1,2),Hslat(M,1,3),
     +                         Hslat(M,2,1),Hslat(M,2,2),Hslat(M,2,3),
     +                         VC(M,I,J,1),VC(M,I,J,2),VC(M,I,J,3),
     +                         -2.d0*DELslat(M))
 120           CONTINUE
 110        CONTINUE
            DELslat(M) = -DELslat(M)
         ENDIF
c
c        *** For deflected flaps ***
         IF (DELflap(M) .NE. 0.d0) THEN
            DO 130, J = 1, JP(M)+1
               DO 140, I = IP(M)+2-IPFLAP(M), IP(M)+1
                  CALL DEFLECT(Hflap(M,1,1),Hflap(M,1,2),Hflap(M,1,3),
     +                         Hflap(M,2,1),Hflap(M,2,2),Hflap(M,2,3),
     +                         VC(M,I,J,1),VC(M,I,J,2),VC(M,I,J,3),
     +                         -2.d0*DELflap(M))
 140           CONTINUE
 130        CONTINUE
            DELflap(M) = -DELflap(M)
         ENDIF
 100  CONTINUE
      RETURN
      END
