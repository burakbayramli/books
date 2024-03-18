
      PROGRAM TRIM3S

      DIMENSION S(3),     B(3),     SIGMAOE(3,3), SCARET(3),
     +          E(3,3),   A(5,5),   D(5),         CL(5)
      REAL      L(3),     LCCG,     LCARET(3),    CBAR,
     +          WBAR,     CM0,      H(3),         MFS,
     +          LAMMT(3), LAMQC(3), FCOWC,        TRW,
     +          DELTAF,   AR(3),    BETA,         CLALPHA(3),
     +          DA0LW,    IWING,    AOA,          EFFAOA(3),
     +          TR(3),    CHORD(3), FCOCT,        TOCT,
     +          ITAIL,    DEFLCTN,  DELTAE,       FCOCC,
     +          TOCC,     ICANARD,  RESULT,       DELTACF,
     +          ANGLE(3)

      OPEN   (UNIT=15,FILE='RESULTS3S',STATUS='NEW')
      REWIND (UNIT=15)
      OPEN   (UNIT=10,FILE='3SURFACE.DAT',STATUS='OLD')
      REWIND (UNIT=10)

C*****************************************************************
C      THIS PROGRAM finds the LIFT FORCE DISTRIBUTION FOR AN 
C      AIRPLAN WITH 3 LONGITUDINAL SURFACES IN A STEADY-LEVEL FLIGHT
C      TO RESULT IN MINIMUM TRIM DRAG.  IN ADDITION, THE FULELAGE
C      INCIDENT ANGLE AND THE DEFLECTIONS OF the canard and 
C      elevator are determined.
C      PREPARED BY JACOB KAY, VIRGINIA TECH.  SEPT 1991
c      some i/o revision and evaluation: w.h. mason, Nov. 1992
C*****************************************************************

      write(6,10)

   10 format(/3x,'Trim Drag Code for Three Surface Configurations'//
     1       3x,' NASA TP 2907 by Goodrich, Sliwa and Lallman'/
     2       3x,' coded by Jacob Kay, Sept. 1991')

C****** DEFINITION OF VARIABLES ***
C      S(I)         = SURFACE AREA
C      B(I)         = SURFACE SPAN
C      SIGMAOE(I,J) = PRANDTL COEFFICIENT (OF I ON J)/EFFICIENCY FACTOR
C      SCARET(I)    = NORMALIZED SURFACE AREA W.R.T. WING AREA
C      E(I,J)       = UPPER LEFT 3X3 ELEMENTS OF INFLUENCE COEFFICIENT
C      A(I,J)       = 5X5 INFLUENCE COEFFICIENT
C      D(I)         = BOUNDARY CONDITION MATRIX
C      CL(I)        = LIFT DISTRIBUTION MATRIX
C      L(I)         = HOR. DISTANCE BETWEEN A.C. OF I-TH SURFACE AND WING A.C.
C      LCCG         = HOR. DISTANCE BETWEEN CG AND A.C. OF WING / WING CHORD

C    *** NOTE:  FOR ANY DISTANCE MEASURED W.R.T. WING'S A.C., POINTS
C               BEHIND OR BELOW WING'S A.C. are "+"; POINTS ABOVE OR 
C               IN FRONT OF WING'S A.C. are "-". 

C      LCARET(I)    = NOMALIZED HOR DISTANCE
C      CBAR         = MEAN AERODYNAMIC CHORD OF WING
C      WBAR         = CONFIGURATION (TOTAL) LIFT COEFFICIENT
C      CM0          = ZERO-LIFT MOMENT COEFFICIENT
C      ANGLE(I)     = DEFLECTION ANGLE OF SURFACE-I
C
C    ***** INPUT BASIC CONFIGURATION PARAMETERS FROM 'GEOMETRY.DAT'

      READ(10,100) WBAR,CM0,LCCG,S(1),S(2),S(3),B(1),B(2),B(3),
     +  L(2),L(3),SIGMAOE(1,1),SIGMAOE(2,2),SIGMAOE(3,3),SIGMAOE(1,2),
     +  SIGMAOE(1,3),SIGMAOE(2,3),CBAR

  100 FORMAT (T50,E10.3)

      write(6,20) WBAR,CM0,LCCG,S(1),S(2),S(3),B(1),B(2),B(3),
     +  L(2),L(3),SIGMAOE(1,1),SIGMAOE(2,2),SIGMAOE(3,3),SIGMAOE(1,2),
     +  SIGMAOE(1,3),SIGMAOE(2,3),CBAR

   20 format(/3x,'Input for this case:'/10x,'wbar         = ',f10.5,
     1 10x,'cmo          = ',f10.5/10x,'lccg         = ',f10.5,
     2 10x,'s(1)         = ',f10.5/10x,'s(2)         = ',f10.5,
     3 10x,'s(3)         = ',f10.5/10x,'b(1)         = ',f10.5,
     4 10x,'b(2)         = ',f10.5/10x,'b(3)         = ',f10.5,
     5 10x,'l(2)         = ',f10.5/10x,'l(3)         = ',f10.5,
     6 10x,'sigmaoe(1,1) = ',f10.5/10x,'sigmaoe(2,2) = ',f10.5,
     7 10x,'sigmaoe(3,3) = ',f10.5/10x,'sigmaoe(1,2) = ',f10.5,
     8 10x,'sigmaoe(1,3) = ',f10.5/10x,'sigmaoe(2,3) = ',f10.5,
     9 10x,'cbar         = ',f10.5)

C   *** READING ADDITIONAL CONFIGURATION PARAMETERS

      READ (10,100) MFS,LAMMT(1),LAMMT(2),LAMMT(3),LAMQC(1),
     +              LAMQC(2),LAMQC(3),FCOWC,TRW,DELTAF,IWING,TR(1),
     +              TR(2),TR(3),H(2),H(3)

      write(6,30) MFS,LAMMT(1),LAMMT(2),LAMMT(3),LAMQC(1),
     +            LAMQC(2),LAMQC(3),FCOWC,TRW,DELTAF,IWING,TR(1),
     +            TR(2),TR(3),H(2),H(3)

   30 format(/10x,'mfs          = ',f10.5,
     1 10x,'lammt(1)     = ',f10.5/10x,'lammt(2)     = ',f10.5,
     2 10x,'lammt(3)     = ',f10.5/10x,'lamqc(1)     = ',f10.5,
     3 10x,'lamqc(2)     = ',f10.5/10x,'lamqc(3)     = ',f10.5,
     4 10x,'fcowc        = ',f10.5/10x,'trw          = ',f10.5,
     5 10x,'deltaf       = ',f10.5/10x,'iwing        = ',f10.5,
     6 10x,'tr(1)        = ',f10.5/10x,'tr(2)        = ',f10.5,
     7 10x,'tr(3)        = ',f10.5/10x,'h(2)         = ',f10.5,
     8 10x,'h(3)         = ',f10.5)

      SIGMAOE(2,1) = SIGMAOE(1,2)
      SIGMAOE(3,1) = SIGMAOE(1,3)
      SIGMAOE(3,2) = SIGMAOE(2,3)
 
C   ***** SETTING UP MATRICES

      L(1)      = 1.0
      DO 200 I  = 1,3
      SCARET(I) = S(I) / S(1)
      LCARET(I) = L(I) / CBAR
  200 CONTINUE

      PI = 4.0*ATAN(1.0)

      DO 230 I = 1,3
      DO 220 J = 1,3
      E(I,J) = 2.0 * SIGMAOE(I,J) * S(I) * SCARET(J) / (PI * B(I) * 
     +           B(J))
      A(I,J) = E(I,J)
  220 CONTINUE
  230 CONTINUE

      A(1,4) =     1.0
      A(1,5) =     0.0
      A(2,4) =  SCARET(2)
      A(2,5) = -SCARET(2) * LCARET(2)
      A(3,4) =  SCARET(3)
      A(3,5) = -SCARET(3) * LCARET(3)
      A(4,1) =  A(1,4)
      A(4,2) =  A(2,4)
      A(4,3) =  A(3,4)
      A(4,4) =     0.0
      A(4,5) =     0.0
      A(5,1) =  A(1,5)
      A(5,2) =  A(2,5)
      A(5,3) =  A(3,5)
      A(5,4) =  A(4,5)
      A(5,5) =     0.0
 
C SET UP OF [D] MATRIX

      D(1)   =    0.0
      D(2)   =    0.0
      D(3)   =    0.0
      D(4)   =   WBAR
      D(5)   = -(CM0 + WBAR * LCCG)
 
C ***** CALLING MATRIX INVERSION SUBROUTINE 
C ***** TO CALCULATE LIFT DISTRIBUTION

      CALL MATRIX(A,D,CL)

      write(6,351)
      DO 300 I = 1,3
      write(6,301) I,CL(I)
  300 CONTINUE

  301 FORMAT (6X,'C-L-',I1,' = ',F7.4)

c ****** determination of drag

      sum = 0.0
      do 400 j = 1,3
      do 400 k = 1,3
      sum = sum + sigmaoe(j,k)*scaret(j)*scaret(k)/
     1            (b(j)*b(k))*cl(j)*cl(k)
  400 continue

      ars1   = b(1)**2/s(1)
      cdi    = s(1)/pi*sum
      eoswld = wbar**2/pi/ars1/cdi

      write(6,402) cdi,eoswld

  402 format(/4x,'CDi              = ',f10.5/4x,
     1           'Effective Span e = ',f10.5/)

C ********************************************************
C ***** THE REST OF THE PROGRAM IS DEVOTED TO THE 
C ***** DETERMINATION OF AEORDYNAMIC SURFACE DEFLECTIONS

C ***** DETERMINING FUSELAGE INCLINATION ANGLE (AOA)

      BETA = (ABS(1-MFS**2))**(.5)

C *** DEFINITION OF ADDITIONAL VARIABLES ***
C      MFS          = MACH NUMBER OF FREE STREAM
C      LAMMT(I)     = SWEEP ANGLE OF MAX THICKNESS LINE IN RAD
C      LAMQCW(I)    = SWEEP ANGLE OF QUARTER CHORD LINE IN RAD
C      FCOWC        = FLAP CHORD/WING CHORD
C      TRW          = THICKNESS RATIO OF WING
C      DELTAF       = OPTIMAL FLAP DEFLECTION IN RAD
C      DA0LW        = CHANGE IN INCIDENT ANGLE DUE TO DEFLECTION OF 
C                     SIMPLE HINGED FLAP
C      IWING        = INCIDENT ANGLE OF THE WING
C      AOA          = ANGLE OF ATTACK OF BODY REFERENCE LINE
C      TR(I)        = SPANWISE TAPER RATIO
C      CHORD(I)     = MEAN AERODYNAMIC CHORD
C      H(I)         = VERTICAL SEPERATION BETWEEN WING AC & AC OF SURFACE(I)
 
C ***** CALCULATION OF C-L-ALPHA & EFFECTIVE-AOA 
C       FOR THE THREE SURFACES *****

      write(6,353)
      DO 350 I     = 1,3
      CHORD(I)     = S(I) / B(I)
      AR(I)        = (B(I) ** 2.0) / S(I)
      CALL           CLA(AR(I),BETA,LAMMT(I),CLALPHA(I))
      write(6,352) i, CLALPHA(I)
      EFFAOA(I)    = CL(I) / CLALPHA(I)
  350 CONTINUE

  351 format(/4x,'Output results:'/)
  352 format(6x,'CLALPHA(',i2,') =',f7.4)
  353 format(1x)
 
C   ***** CALCULATION OF DA0L OF THE WING

      CALL DA0L(TRW,FCOWC,DELTAF,CLALPHA(1),DA0LW)
 
C   ***** CALCULATION OF UP/DOWNWASH ANGLE OF WING
C   *** UP/DOWNWASH ANGLE DUE TO H. TAIL

      CALL UPDOWN(AR(2),EFFAOA(2),LAMQC(2),TR(2),L(2),H(2),CL(2),
     +            CHORD(2),B(2),S(2),S(1),EP12)
 
C  *** UP/DOWNWASH ANGLE CONTRIBUTED BY CANARD

      CALL UPDOWN(AR(3),EFFAOA(3),LAMQC(3),TR(3),L(3),H(3),CL(3),
     +             CHORD(3),B(3),S(3),S(1),EP13)

      EPSILON   = EP12 + EP13

C   *** CALCULATION OF AOA OF BODY REFERENCE LINE

      AOA         = EFFAOA(1) - IWING + DA0LW - EPSILON
      ANGLE(1)    = AOA
      awrite      = AOA*180/PI
      write(6,354)  awrite

  354 format(/4x,'AOA OF BODY REFERENCE LINE (DEG) = ',f9.4)

C   ***** CALCULATION OF TAIL DEFLECTION OR INCIDENT ANGLE *****
C         NOTE: IF USING ALL-MOVING (VARIABLE-INCIDENT) TAIL, ENTER 0
C               FOR ELEVATOR-H TAIL CHORD RATIO, AND ENTER 0 FOR H TAIL
C               INCIDENT ANGLE IN THE INPUT (GEOMETRY.DAT) FILE.

C    ***** READING MORE TAIL GEOMETRY ******

      READ(10,100) FCOCT,TOCT,ITAIL

      write(6,40)  FCOCT,TOCT,ITAIL

   40 format(/10x,'more tail geometry'//
     1 10x,'fcoct  = ',f7.4/10x,'toct   = ',f7.4/
     2 10x,'itail  = ',f7.4/)

C   *** DEFINITION OF VARIABLES ***

C      FCOCT        = ELEVATOR-TO-H TAIL CHORD RATIO
C      TOCT         = THICKNESS RATIO OF H TAIL
C      DEFLCTN      = TEMPERARY DUMMY VARIABLE FOR CALCUALATED TAIL INCIDENT
C      ITAIL        = INCIDENT ANGLE OF TAIL
C      DELTAE       = ELEVATOR DEFLECTION

      IF (FCOCT.EQ.0.0) THEN
          CALL ALLMOVE(AR(1),EFFAOA(1),EFFAOA(2),LAMQC(1),TR(1),
     +                 L(2),H(2),CL(1),CHORD(1),B(1),S(1),AOA,DEFLCTN)
          ITAIL    = DEFLCTN
          ANGLE(2) = ITAIL
          awrite   = ITAIL*180/PI
          write(6,356) awrite
          ENDIF

  356 format(4x,'H TAIL INCIDENT ANGLE SHOULD BE:',f9.4)

      IF (FCOCT.NE.0.0) THEN
          CALL FLAP(AR(1),EFFAOA(1),EFFAOA(2),LAMQC(1),TR(1),
     +              L(2),H(2),CL(1),CHORD(1),B(1),S(1),
     +              AOA,FCOCT,TOCT,ITAIL,CLALPHA(2),DELTAE)
          ANGLE(2) = DELTAE
          awrite   = DELTAE*180/PI
          write(6,358) awrite 
          ENDIF

  358 format(4x,'ELEVATOR DEFLECTION ANGLE = ',f9.4)
 
C ****************************************************************
C ***** CALCULATION OF CANARD FLAP DEFLECTION OR INCIDENT ANGLE***
 
C          NOTE: IF USING ALL-MOVING (VARIABLE-INCIDENT) CANARD, ENTER 0
C                FOR FLAP-CANARD CHORD RATIO, AND  ENTER 0 FOR CANARD
C                INCIDENT ANGLE IN THE INPUT (GEOMETRY.DAT) FILE.
 
C **** READING MORE CANARD GEOMETRY ******

      READ(10,100) FCOCC,TOCC,ICANARD

      write(6,50)  FCOCC,TOCC,ICANARD

   50 format(/10x,'more canard geometry'//
     1 10x,'fcocc   = ',f7.4/10x,'tocc    = ',f7.4/
     2 10x,'icanard = ',f7.4/)

C*** DEFINITION OF GEOMETRY ***
C      FCOCC        = FLAP-CANARD CHORD RATIO
C      TOCC         = THICKNESS RATIO OF CANARD
C      ICANARD      = INCIDENT ANGLE OF CANARD
C      OUTPUT       = FLAP DEFLECTION ANGLE
C      RESULT       = TEMPERARY DUMMY VARIABLE FOR CANARD INCIDENT ANGLE
C      DELTACF      = CANARD FLAP DEFLECTION ANGLE

      IF (FCOCC.EQ.0.0) THEN
          CALL ALLMOVE(AR(1),EFFAOA(1),EFFAOA(3),LAMQC(1),TR(1),
     +                 L(3),H(3),CL(1),CHORD(1),B(1),S(1),AOA,RESULT)
          ICANARD  = RESULT
          ANGLE(3) = ICANARD
          awrite   = ICANARD*180/PI
          write(6,362) awrite
          ENDIF

  362 format(4x,'Canard INCIDENT ANGLE SHOULD BE:',f9.4)

      IF (FCOCC.NE.0.0) THEN
          CALL FLAP(AR(1),EFFAOA(1),EFFAOA(3),LAMQC(1),TR(1),
     +              L(3),H(3),CL(1),CHORD(1),B(1),S(1),
     +              AOA,FCOCC,TOCC,ICANARD,CLALPHA(3),DELTACF)
          ANGLE(3) = DELTACF
          awrite = DELTACF*180/PI
          write(6,364) awrite
          ENDIF

  364 format(4x,'CANARD FLAP DEFLECTION ANGLE = ', f9.4)
 
C   *****************************************************************
C   ***** WRITE NUMERICAL OUTPUTS TO FILE ****

      WRITE  (15,600)
  600 FORMAT (T3,'CL-WING',T13,'CL-H. TAIL',T25,'CL-CANARD',T39,'AOA',
     +         T48,'DELTA-TAIL',T60,'DELTA-CANARD')
      WRITE(15,500) CL(1),CL(2),CL(3),ANGLE(1)*180/PI,ANGLE(2)*180/PI,
     +              ANGLE(3)*180/PI
  500 FORMAT(6(3X,F8.4))

       END


C   ****************************************************************
C   ***** SUBROUTINE FLAP *****
C   ****************************************************************

      SUBROUTINE FLAP(AR,EFFAOAW,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,
     1                   AOA,FCOC,TOC,INCIDENT,CLALPHA,OUTPUT)
      REAL AR,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,EPSILON,FCOC,TOC,
     +       INCIDENT,C1,C2,C3,EFFAOAW,OUTPUT,CLDELTA

C ***** THIS SUBROUTINE FINDS THE FLAP DEFLECTION ANGLE OF H. TAIL
C       OR CANARD FOR A GIVEN CONFIGURATION LIFT DISTRIBUTION. *****

C ***** DEFINITION OF VARIABLES *****

C       AR              = ASPECT RATIO OF WING
C       EFFAOAW         = WING'S EFFECTIVE AOA
C       EFFAOA          = EFFECTIVE AOA OF H TAIL OR CANARD
C       LAMQC           = WING'S QUARTER-CHORD SWEEP ANGLE
C       TR              = TAPER RATIO OF WING
C       L               = DISTANCE FROM WING TO TAIL OR CANARD
C                         (+ FOR TAIL; - FOR CANARD)
C       H               = VERTICAL SEPERATION BETWEEN WING AND TAIL OR CANARD
C       CL              = LIFT COEFFIENT OF WING
C       CHORD           = MEAN AERODYNAMIC CHORD OF WING
C       B               = SPAN OF WING
C       S               = AREA OF WING
C       FCOC            = FLAP CHORD OVER CHORD OF TAIL OR CANARD
C       TOC             = THICKNESS RATIO OF H TAIL OR CANARD
C       INCIDENT        = INCIDENT ANGLE OF H TAIL OR CANARD
C       EPSILON         = UP/DOWNWASH ANGLE FOR CANARD OR H. TAIL DUE TO WING
C       C1,C2,C3,KPRIME = NUMERICAL VARIABLES FOR CALCUATION PURPOSES
C       CLALPHA         = C-L-ALPHA OF H TAIL OR CANARD
C       OUTPUT          = FLAP DEFLECTION ANGLE FOR H TAIL OR CANARD

C     ***  NOTE: THIS PROGRAM NEGLECTS THE UP/DOWNWASH EFFECTS
C                BETWEEN THE H TAIL AND THE CANARD.

C     FINDING UP/DOWNWASH ANGLE DUE TO WING:

      CALL UPDOWN(AR,EFFAOAW,LAMQC,TR,L,H,CL,CHORD,B,S,S,EPSILON)

      C1      =  1.242 - .5991 * TOC
      C2      =  12.98 + 12.44 * TOC
      C3      = -10.53 - .6497 * TOC
      CLDELTA =  C1 + C2 * FCOC + C3 * FCOC ** 2
      OUTPUT  = -CLALPHA / CLDELTA * 
     1                     (-EFFAOA + AOA + EPSILON + INCIDENT)

      RETURN
      END
 
 
C    ****************************************************************
C    ***** SUBROUTINE ALLMOVE ******
C    ****************************************************************

      SUBROUTINE ALLMOVE(AR,EFFAOAW,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,
     1                      AOA, OUTPUT)
      REAL AR,EFFAOAW,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,EPSILON,AOA

C    *** THIS SUBROUTINE FINDS THE INCIDENT ANGLE OF THE ALL-
C         MOVING H TAIL OR CANARD FOR A GIVEN CONFIGURATION OF LIFT 
C         DISTRIBUTION. ***

C    ***** DEFINITION OF VARIABLES *****

C        AR       = ASPECT RATIO OF WING
C        EFFAOAW  = WING'S EFFECTIVE AOA
C        EFFAOA   = EFFECTIVE AOA OF H TAIL OR CANARD
C        LAMQC    = WING'S QUARTER-CHORD SWEEP ANGLE
C        TR       = TAPER RATIO OF WING
C        L        = DISTANCE FROM WING TO TAIL OR CANARD
C                   (+ FOR TAIL; - FOR CANARD)
C        H        = VERTICAL SEPERATION BETWEEN WING AND TAIL OR CANARD
C        CL       = LIFT COEFFIENT OF WING
C        CHORD    = MEAN AERODYNAMIC CHORD OF WING
C        B        = SPAN OF WING
C        S        = AREA OF WING
C        EPSILON  = UP/DOWNWASH ANGLE FOR CANARD OR H. TAIL DUE TO WING
C        AOA      = AOA OF BODY REFERENCE LINE
C        OUTPUT   = INCIDENT ANGLE OF H TAIL OR CANARD

C    ***** FINDING UP/DOWNWASH ANGLE DUE TO WING ****

       CALL UPDOWN(AR,EFFAOAW,LAMQC,TR,L,H,CL,CHORD,B,S,S,EPSILON)

       OUTPUT = EFFAOA - AOA - EPSILON

       RETURN
       END

C
C
C    ******************************************************************
C    ***** SUBROUTINE UPDOWN *****
C    ******************************************************************
C

      SUBROUTINE UPDOWN (AR,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,
     +                   S,SWING,EPSILON)
      REAL AR,EFFAOA,LAMQC,TR,L,H,CL,CHORD,EPSILON

C     *** THIS SUBROUTINE CALCULATES THE UPWASH/DOWNWASH ANGLE OF 
C     SURFACE-B DUE TO THE LIFT GENERATED BY SURFACE-A. ***
C
C     ***** DEFINITION OF VARIABLES

C        AR       = ASPECT RATIO OF SURFACE-A
C        EFFAOA   = EFFECTIVE AOA OF SURFACE-A
C        LAMQC    = QUARTER CHORD LINE SWEEP ANGLE OF SURFACE-A
C        TR       = TAPER RATIO OF SURFACE-A
C        L        = DISTANCE BETWEEN SURFACES 
C                   ('-' IF SURFACE-B IS BEHIND SURFACE-A)
C        H        = VERTICAL DISTANCE BETWEEN THE TWO SURFACES
C        CL       = LIFT COEFFICIENT OF SURFACE-A
C        EPSILON  = DOWN/UPWASH ANGLE OF SURFACE-B DUE TO SURFACE-A 
C                   ('+'=UPWASH; '-'=DOWNWASH)
C        B        = SPAN OF SURFACE-A
C        S        = AREA OF SURFACE-A
C        SWIN     = AREA OF WING

      IF (CL*L.LT.0.0) CALL DOWNWASH(AR,EFFAOA,LAMQC,TR,
     +                               L,H,B,S,SWING,EPSILON)
      IF (CL*L.GT.0.0) CALL UPWASH(L,CHORD,EFFAOA,EPSILON)
      IF (CL.EQ.0.0)   EPSILON = 0.0

      RETURN
      END


C    ******************************************************************
C    ***** SUBROUTINE UPWASH *****
C    ******************************************************************

       SUBROUTINE UPWASH(L,C1,EFFAOA1,EPSILON)
       REAL L,C1,EFFAOA1,EPSILON,DEDA

C    *** THIS SUBROUTINE CALCULATES THE ABERAGE UPWASH ANGLE OF 
C        SURFACE-B DUE TO THE LIFT (+ OR -) GENERATED BY SURFACE-A BASED 
C        ON THE METHOD BY PERKINS & HAGE (1949).

C    *** DEFINITION OF VARIABLES ***

C        L        = HORIZONTAL SEPERATION BETWEEN THE TWO SURFACES
C        C1       = MEAN AERODYNAMIC CHORD OF SURFACE-A
C        EFFAOA1  = EFFECTIVE AOA OF SURFACE-A
C        EPSILON  = UPWASH ANGLE AT SURFACE-B DUE TO SURFACE-A
C
      DEDA    = .9 * (.722 ** (5.0 * ABS(L) / C1 - 2.0))
      EPSILON = DEDA * ABS(EFFAOA1)

      RETURN
      END


C     ******************************************************************
C     ***** SUBROUTINE DOWNWASH *****
C     ******************************************************************

      SUBROUTINE DOWNWASH(AR,EFFAOA,LAMQC,TR,L,H,B,S,SWING,EPSILON)
      REAL KA,KTR,KH,AR,EFFAOA,LAMQC,TR,L,H,S,SWING,EPSILON
C     *** THIS SUBROUTINE CALCULATES THE AVERAGE DOWNWASH ANGLE OF 
C         SURFACE-B INDUCED BY THE LIFT (+ OR -) GENERATED BY SURFACE-A 
C         BASED ON DATCOM(1978). ***

C     *** DEFINITION OF VARIABLES ***

C        AR        = ASPECT RATIO OF SURFACE-A
C        EFFAOA    = EFFECTIVE ANGLE OF ATTACK OF SURFACE-A
C        LAMQC     = SWP ANGLE OF QUARTER-CHORD LINE OF SURFACE-A
C        TR        = TAPER RATIO OF SURFACE-A
C        L         = HORIZONTAL DISTANCE BETWEEN SURFACE-A AND SURFACE-B
C        H         = VERTICAL DISTANCE BETWEEN SURFACE-A AND SURFACE-B
C        KA,KTR,KH = NUMERICAL VARIABLES FOR CALCULATION PURPOSED
C        EPSILON   = DOWNWASH ANGLE INDUCED BY SURFACE-A ON SURFACE-B
C        B         = SPAN OF SURFACE-A

      KA      =  1.0 / AR - (1.0 + AR ** 1.7) ** (-1.0)
      KTR     = (10.0 - 3.0 * TR) / 7.0
      KH      = (1.0 - ABS(H / B)) / (2.0 * ABS(L) / B) ** (1.0 / 3.0)
      DEDA    = -4.44 * S * (KA * KTR * KH * (COS(LAMQC)) ** (0.5)) ** 
     +           1.19 / SWING
      EPSILON = DEDA * ABS(EFFAOA)

      RETURN
      END


C    ******************************************************************
C    ***** SUBROUTINE C-L-ALPHA *****
C    ******************************************************************

      SUBROUTINE CLA(AR,BETA,LAMBDA,CLALPHA)
      REAL AR,BETA,PI,LAMBDA,CLALPHA

C     *** THIS SUBROUTINE CALCULATES THE LIFT CURVE SLOPE (C-L-ALPHA)
C         OF AN AERODYNAMIC SURFACE. ***
C
      PI      = 4.0 * ATAN(1.0)
      CLALPHA =  2 * PI * AR / (2. + (4. + (AR * BETA) ** 2 * ( 1. + 
     +          (TAN(LAMBDA) / BETA) ** 2)) ** .5)
      RETURN
      END


C     ******************************************************************
C     ***** SUBROUTINE DA0L *****
C     ******************************************************************

      SUBROUTINE DA0L(TR,FCOC,DELTAF,CLA,OUTPUT)
      REAL TR,FCOC,DELTAF,OUTPUT,CLDELTA,KPRIME,K1,K2,K3,K4,C1,C2,C3

C   *** THIS SUBROUTINE CALCULATES THE CHANGE IN INCIDENT ANGLE DUE TO 
C       THE DEFLECTION OF SIMPLE HINGED CONTROL FLAP FOR ANY AERODYNAMIC 
C       SURFACES. ***

C   ***** DEFINITION OF VARIABLES

C        TR                   = AERODYNAMIC SURFACE THICKNESS RATIO
C        FCOC                 = SIMPLE FLAP CHORD / AERODYNAMIC SURFACE CHORD
C        DELTAF               = FLAP DEFLECTION ANGLE
C        CLA                  = AERODYNAMIC SURFACE LIFT-CURVE SLOPE (C-L-ALPHA)
C        OUTPUT               = DELTA-ALPHA-0-L
C        CLDELTA              = LIFT EFFECTIVENESS OF PLAIN TRAILING-EDGE FLAP
C        C1,C2,C3,K1,K2,K3,K4 = EMPIRICAL PARAMETERS FROM DATCOM-1978
C        KPRIME               = FLAP-EFFECTIVENESS AT LARGE DEFLECTIONS

      C1      =  1.242 - .5991 * TR
      C2      =  12.98 + 12.44 * TR
      C3      = -10.53 - .6497 * TR
      K1      =  1.011 + .1740 * FCOC
      K2      = .002053 - .02069 * FCOC
      K3      = .0004845 + .00002479 * FCOC
      K4      = (5.688E-6) -( 1.217E-7) * FCOC
      CLDELTA = C1 + C2 * FCOC + C3 * FCOC ** 2
      KPRIME  = K1 + K2 * DELTAF + K3 * DELTAF ** 2.0 +
     +          K4 * DELTAF ** 3.0
      OUTPUT = -CLDELTA * DELTAF * KPRIME / CLA
      RETURN
      END


C******************************************************************
C***** SUBROUTINE MATRIX INVERSION *****
C******************************************************************

      SUBROUTINE MATRIX(A,D,C)
      INTEGER N
      PARAMETER (N=5)
      DIMENSION A(N,N), C(N),D(N)

C     NOTE:  N=SIZE OF MATRIX
C            USER MUST UPDATE THE VALUE OF N IN THE ABOVE PARAMETER STATEMENT
C            INPUT MATRIX [A] & [D]; OUTPUT MATRIX [C] SUCH THAT [A][C]=[D]
C
C     STARTING MATRIX INVERSION

      DO 1100 K = 1,N-1
      M         = K + 1
      L         = K
 1130 Q         = ABS(A(M,K)) - ABS(A(L,K))

      IF (Q .GT. 0.0) L = M
      IF (M .LT. N)   THEN
                      M = M + 1
                      GO TO 1130
                      ENDIF

      IF (L.EQ.K) GO TO 1110

      DO 1200 J = K,N
      DU        = A(K,J)
      A(K,J)    = A(L,J)
      A(L,J)    = DU
 1200 CONTINUE

      DD        = D(K)
      D(K)      = D(L)
      D(L)      = DD

 1110 M         = K + 1
 1120 Q         = A(M,K) / A(K,K)
      A(M,K)    = 0

      DO 1300 J = K + 1, N
      A(M,J)    = A(M,J) - Q * A(K,J)
 1300 CONTINUE

      D(M)      = D(M) - Q * D(K)

      IF (M .LT. N) THEN
                    M = M + 1
                    GO TO 1120
                    ENDIF
 1100 CONTINUE

      C(N)      = D(N) / A(N,N)
      DO 1500 M = N-1,1,-1
      Q         = 0
      DO 1400 J = M + 1, N
      Q         = Q + A(M,J) * C(J)
      C(M)      = (D(M) - Q) / A(M,M)
 1400 CONTINUE
 1500 CONTINUE

      END