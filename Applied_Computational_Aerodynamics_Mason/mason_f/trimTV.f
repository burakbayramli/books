      PROGRAM TRIMTV

      REAL WBAR,         CM0, LCCG, S(2), B(2), PI, L(3), 
     +     SIGMAOE(2,2), K1,  K2,   MUTL, CT,   CL(5),
     +     SCARET(2),    LCARET(3), CLALPHA(2), MFS,
     +     LAMMT(2),     A(5,5),    ZCARET(3),  CHORD(2),
     +     D(5),         AR(2),     Z(3),       EFFAOA(2),
     +     LAMQC(2),     FCOWC,     TRW,        DELTAF,
     +     DA0LW,        IWING,     AOA,        TR(2),
     +     H(2),         BETA,      EPSILON,    FCOCT,
     +     TOCT,         ITAIL,     DEFLCTN,    DELTAE,
     +     ANGLE(3),     kindx(2)

      OPEN   (UNIT=10,FILE='2SURFACE.DAT',STATUS='OLD')
      REWIND (UNIT=10)
      OPEN   (UNIT=15,FILE='RESULTSTV',STATUS='NEW')
      REWIND (UNIT=15)

C   *****************************************************************
C   THIS PROGRAM CALCULATES THE SURFACE LIFT COEFFICIENT AND THE
C   JET DEFLECTION ANGLE FOR A TWO-LIFTING SURFACE, THRUST-VECTORING
C   AIRPLANE WHOSE GEOMETRY IS LISTED IN 'DATA.DAT.'
C
C   PREPARED BY JACOB KAY, VIRGINIA TECH, SEPT 1991.
c   some revision, i/o mods and evaluation, w.h. mason, June 1993
C
C   BASED ON NASA TP 2907, MAY 1989
C   *****************************************************************

      write(6,10)

   10 format(/3x,'Trim drag code for two surface configurations'/
     1        3x,'with thrust vectoring for control'//
     2        3x,' NASA TP 2907 by Goodrich, Sliwa and Lallman'/
     3        3x,' coded by Jacob Kay, Sept. 1991')


C   ***** DEFINITIONS OF VARIABLES *****
C   WBAR     = TOTAL LIFT COEFFICIENT (WEIGHT/(REF AREA*DYNAMIC PRESS))
C   CM0      = ZERO-LIFT MOMENT COEFFICIENT (NOSE-DOWN NEGATIVE)
C   LCCG     = NORMALIZED HORIZ. DIST. BETWEEN C.G. AND WING'S A.C.
C   S(I)     = AREA OF LIFTING SURFACE-I
C   B(I)     = SPAN OF LIFTING SURFACE-I
C   L(2)     = HORIZONTAL DIST BETWEEN A.C. OF SURFACE 2 & WING A.C.
C   L(3)     = HORIZONTAL DIST BETWEEN JET NOZZLE & WING A.C.

C   *** NOTE:   FOR ALL VERT. & HORIZ. DISTANCE MEASURMENTS,
C               VALUES ARE  '+' IF BELOW OR BEHIND WING'S A.C.

C   CHORD(I)     = MEAN CHORD OF SURFACE-I
C   SIGMAOE(I,J) = PRANDTL COEFFICIENT (OF I BY J) / EFFICIENCY FACTOR
C   K1           = INDUCED LIFT PARAMETER OF WING BY THRUST
C   K2           = INDUCED LIFT PARAMETER OF TAIL/CANARD BY THRUST
C   MUTL         = FRACTION OF THRUST-LOSS (0<MUTL<.5)
C   CT           = THRUST COEFFICIENT (CLOSE TO DRAG COEFFICIENT)
C   CLALPHA(I)   = LIFT-CURVE SLOPE OF SURFACE-I
C   SCARET(I)    = NORMALIZED AREA OF SURFACE-I
C   LCARET(I)    = NORMALIZED LENGTH OF L(I)
C   AR(I)        = ASPECT RATIO OF SURFACE (I)
C   MFS          = FREE STREAM MACH NUMBER
C   LAMMT(I)     = SWEEP ANGLE OF MAX THICKNESS LINE OF SURFACE-I
C   IWING        = WING INCIDENT ANGLE
C   Z(I)         = VERTICAL DIST. BETWEEN WING AC & AC OF SURFACE-I
C   ZCARET(I)    = NORMALIZED Z(I)
C   CL(1)        = LIFT COEFFICIENT OF WING EXCLUDING INDUCED LIFT CAUSED
C                  BY SUPERCIRCULATION
C   CL(2)        = LIFT COEFFICIENT OF TAIL OR CANARD EXCLUDING INDUCED
C                  LIFT CAUSED BY SUPERCIRCULATION

C   ***** READING GEOMETRY *****
      READ(10,100) WBAR,CM0,LCCG,S(1),S(2),B(1),B(2),L(2),L(3),
     +     SIGMAOE(1,1),SIGMAOE(2,2),SIGMAOE(1,2),K1,K2,MUTL,
     +     CT,MFS,LAMMT(1),LAMMT(2),IWING,Z(3)
  100 FORMAT (T50,E10.3)

      write(6,20) WBAR,CM0,LCCG,S(1),S(2),B(1),B(2),
     +  L(2),L(3),SIGMAOE(1,1),SIGMAOE(2,2),SIGMAOE(1,2),
     +  K1,K2,MUTL,CT,MFS,LAMMT(1),LAMMT(2),IWING,Z(3)

   20 format(/3x,'Input for this case:'/10x,'wbar         = ',f10.5,
     1 10x,'cmo          = ',f10.5/10x,'lccg         = ',f10.5,
     2 10x,'s(1)         = ',f10.5/10x,'s(2)         = ',f10.5,
     3 10x,'b(1)         = ',f10.5/10x,'b(2)         = ',f10.5,
     4 10x,'l(2)         = ',f10.5/10x,'l(3)         = ',f10.5,
     5 10x,'sigmaoe(1,1) = ',f10.5/10x,'sigmaoe(2,2) = ',f10.5,
     6 10x,'sigmaoe(1,2) = ',f10.5/10x,'k1           = ',f10.5,
     7 10x,'k2           = ',f10.5/10x,'mutl         = ',f10.5,
     8 10x,'ct           = ',f10.5/10x,'mfs          = ',f10.5,
     9 10x,'lammt(1)     = ',f10.5/10x,'lammt(2)     = ',f10.5,
     A 10x,'iwing        = ',f10.5/10x,'z(3)         = ',f10.5)

C   *** DEFINITION OF ADDITIONAL VARIABLES ***
C   LAMQC(I)  = SWEEP ANGLE OF QUARTER CHORD LINE IN RAD
C   FCOWC     = FLAP CHORD/WING CHORD
C   TRW       = THICKNESS RATIO OF WING
C   DELTAF    = OPTIMAL FLAP DEFLECTION IN RAD
C   DA0LW     = CHANGE IN INCIDENT ANGLE DUE TO DEFLECTION OF 
C               SIMPLE HINGED FLAP
C   AOA       = ANGLE OF ATTACK OF BODY REFERENCE LINE
C   TR(I)     = SPANWISE TAPER RATIO
C   CHORD(I)  = MEAN AERODYNAMIC CHORD
C   H(I)      = VERTICAL SEPERATION BETWEEN WING AC & AC OF SURFACE(I)
C   EFFAOA(I) = EFFECTIVE ANGLE OF ATTACK OF SURFACE(I)

C   *** READING ADDITIONAL CONFIGURATION PARAMETERS

      READ (10,100) LAMQC(1),LAMQC(2),FCOWC,TRW,DELTAF,TR(1),TR(2),H(2)

      write(6,30) LAMQC(1),LAMQC(2),FCOWC,TRW,DELTAF,TR(1),TR(2),H(2)

   30 format(10x,'LAMQC(1)     = ',f10.5,
     1 10x,'LAMQC(2)     = ',f10.5/10x,'FCOWC        = ',f10.5,
     2 10x,'TRW          = ',f10.5/10x,'DELTAF       = ',f10.5,
     3 10x,'TR(1)        = ',f10.5/10x,'TR(2)        = ',f10.5,
     4 10x,'H(2)         = ',f10.5)

C   NOTE:  IF USING ALL-MOVING (VARIABLE-INCIDENT) TAIL, ENTER 0
C          FOR ELEVATOR-H TAIL CHORD RATIO, AND ENTER 0 FOR H TAIL
C          INCIDENT ANGLE IN THE INPUT (GEOMETRY.DAT) FILE.

C   *** DEFINITION OF VARIABLES ***
C   FCOCT   = ELEVATOR-TO-H TAIL CHORD RATIO
C   TOCT    = THICKNESS RATIO OF H TAIL
C   DEFLCTN = TEMPERARY DUMMY VARIABLE FOR CALCUALATED TAIL INCIDENT
C   ITAIL   = INCIDENT ANGLE OF TAIL
C   DELTAE  = ELEVATOR DEFLECTION

C   ***** READING MORE TAIL GEOMETRY ******

      READ(10,100) FCOCT,TOCT,ITAIL

      write(6,60) FCOCT,TOCT,ITAIL

   60 format(10x,'FCOCT        = ',f10.5,
     1 10x,'TOCT         = ',f10.5/10x,'ITAIL        = ',f10.5)

C   ***** SETING UP INFLUENCE MATRIX *****

      PI        = 4.0 * ATAN(1.0)
      L(1)      = 1.0
      BETA      = (ABS(1.0 - MFS ** 2.0)) ** (.5)
      DO 150 I  = 1, 2
      CHORD(I)  = S(I) / B(I)
      AR(I)     = B(I) ** 2.0 / S(I)
      SCARET(I) = S(I) / S(1)
      LCARET(i) = L(I) / CHORD(1)
      CALL CLA(AR(I),BETA,LAMMT(I),CLALPHA(I))
 150  CONTINUE

      LCARET(3)    = L(3) / CHORD(1)
      ZCARET(3)    = Z(3) / CHORD(1)
      sigmaoe(2,1) = sigmaoe(1,2)

C   ***** CALC. MATRIX ELEMENTS *****

      A(1,1) = 2 * S(1) * SIGMAOE(1,1) / (PI * B(1) ** 2.0)
      A(1,2) = 2 * S(2) * SIGMAOE(1,2) / (PI * B(1) * B(2))
      A(1,3) = 2 * (SIGMAOE(1,1) * S(1) * K1 / B(1) + SIGMAOE(1,2) *
     +              S(2) * K2 / B(2))/(PI*B(1))
      A(1,4) = 1 + CT / CLALPHA(1)
      A(1,5) = 0.0
      A(2,1) = A(1,2)
      A(2,2) = 2 * S(2) * SIGMAOE(2,2) * SCARET(2) / (PI * B(2) ** 2.0)
      A(2,3) = 2 * S(2) * (SIGMAOE(1,2) * K1 / B(1) + SIGMAOE(2,2) * 
     +                     SCARET(2) * K2 / B(2)) / (PI * B(2))
      A(2,4) =  SCARET(2)
      A(2,5) = -SCARET(2) * LCARET(2)
      A(3,1) = A(1,3)
      A(3,2) = A(2,3)
      A(3,3) = 2.0 / PI * (SIGMAOE(1,1) * S(1) * K1 ** 2.0 / B(1) **
     +           2.0 + 2.0 * SIGMAOE(1,2) * S(2) * K1 * K2 / (B(1) * 
     +           B(2)) + SIGMAOE(2,2) * S(2) * K2 ** 2 * SCARET(2) / 
     +           B(2)** 2) + MUTL / CT
      A(3,4) = K1 + K2 * SCARET(2) + 1
      A(3,5) = -(LCARET(3) + K2 * SCARET(2) * LCARET(2))
      A(4,1) = A(1,4)
      A(4,2) = A(2,4)
      A(4,3) = A(3,4)
      A(4,4) = 0
      A(4,5) = 0
      A(5,1) = A(1,5)
      A(5,2) = A(2,5)
      A(5,3) = A(3,5)
      A(5,4) = A(4,5)
      A(5,5) = 0.0
      D(1)   = 0.0
      D(2)   = 0.0
      D(3)   = 0.0
      D(4)   = WBAR +CT * IWING
      D(5)   = CT * ZCARET(3) - CM0 - WBAR * LCCG

      CALL MATRIX(A,D,CL)

      deltatv = cl(3)/CT
      deltatvd = deltatv*180./pi
      write(6,351)
      write(6,40) CL(1), CL(2), deltatvd

  351 format(/4x,'Output results:'/)
   40 format(/5x,'Wing CL                         = ', f10.5/
     1        5x,'H. Tail C-L                     = ', f10.5/
     2        5x,'Jet deflection angle (deg)      = ', f10.5)

c ****** determination of drag

      kindx(1) = k1
      kindx(2) = k2
      sum      = 0.0
      do 400 j = 1,2
      do 400 k = 1,2
      sum = sum + sigmaoe(j,k)*scaret(j)*scaret(k)/
     1            (b(j)*b(k))*(cl(j)-kindx(j)*deltatv*ct)*
     2                        (cl(k)-kindx(k)*deltatv*ct)
  400 continue

      ars1   = b(1)**2/s(1)
      cdi    = s(1)/pi*sum
      eoswld = (wbar-ct*sin(deltatv))**2/pi/ars1/cdi

      write(6,402) cdi,eoswld

  402 format(/5x,'CDi              = ',f10.5/
     1        5x,'Effective Span e = ',f10.5/)

C   ********************************************************
C   *** THE REST OF THE PROGRAM IS DEVOTED TO THE 
C   *** DETERMINATION OF AEORDYNAMIC SURFACE DEFLECTIONS
C   ********************************************************

C   ***** DETERMINING FUSELAGE INCLINATION ANGLE (AOA)

C   ***** CALCULATION OF EFFECTIVE-AOA FOR THE TWO SURFACES *****

      DO 350 I  = 1, 2
      EFFAOA(I) = CL(I) / CLALPHA(I)
 350  CONTINUE

C   ***** CALCULATION OF DA0L OF THE WING

      CALL DA0L(TRW,FCOWC,DELTAF,CLALPHA(1),DA0LW)

C   ***** CALCULATION OF UP/DOWNWASH ANGLE OF WING
C   *** UP/DOWNWASH ANGLE DUE TO H. TAIL OR CANARD

      CALL UPDOWN(AR(2),EFFAOA(2),LAMQC(2),TR(2),L(2),H(2),CL(2),
     +             CHORD(2),B(2),S(2),S(1),EPSILON)

C   *** CALCULATION OF AOA OF BODY REFERENCE LINE

      AOA      = EFFAOA(1) - IWING + DA0LW - EPSILON
      ANGLE(1) = AOA

      write(6,50) AOA*180/PI
   50 format(/5x,'AOA OF BODY REFERENCE LINE (DEG) =',f10.5)

C   ***** CALCULATION OF TAIL DEFLECTION OR INCIDENT ANGLE *****

      IF (FCOCT.EQ.0.0) THEN
              CALL ALLMOVE(AR(1),EFFAOA(1),EFFAOA(2),LAMQC(1),TR(1),
     +              L(2),H(2),CL(1),CHORD(1),B(1),S(1),AOA,DEFLCTN)
              ITAIL    = DEFLCTN
              ANGLE(2) = ITAIL
              write(6,70) ITAIL*180/PI
              ENDIF
      IF (FCOCT.NE.0.0) THEN
             CALL FLAP(AR(1),EFFAOA(1),EFFAOA(2),LAMQC(1),TR(1),
     +       L(2),H(2),CL(1),CHORD(1),B(1),S(1),
     +       AOA,FCOCT,TOCT,ITAIL,CLALPHA(2),DELTAE)
             ANGLE(2) = DELTAE
             write(6,80) DELTAE*180/PI 
             ENDIF

   70 format(/5x,'H TAIL INCIDENT ANGLE SHOULD BE  = ',f10.5)
   80 format(/5x,'ELEVATOR DEFLECTION ANGLE        = ',f10.5)

C *****************************************************************
C   ***** WRITE NUMERICAL OUTPUTS TO FILE ****
      WRITE (15,600) CL(1)
  600 FORMAT ('WING C-L = ',F7.4)
      WRITE (15,610) CL(2)
  610 FORMAT ('TAIL OR CANARD C-L = ',F7.4)
      WRITE (15,620) (CL(3)/CT)*180/PI
  620 FORMAT ('JET DEFLECTION ANGLE (DEG) = ',F7.3)
      WRITE (15,630) ANGLE(1)*180/PI
  630 FORMAT ('FUSELAGE ANGLE OF ATTACK (DEG) = ', F7.3)
      WRITE (15,640) ANGLE(2)*180/PI
  640 FORMAT ('CANARD/TAIL DEFLECTION ANGLE (DEG) =', F7.3) 
      END

C   ****************************************************************
C   ***** SUBROUTINE FLAP *****
C   ****************************************************************
      SUBROUTINE FLAP(AR,EFFAOAW,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,
     +                AOA,FCOC,TOC,INCIDENT,CLALPHA,OUTPUT)
      REAL AR,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,EPSILON,FCOC,TOC,
     +       INCIDENT,C1,C2,C3,EFFAOAW,OUTPUT,CLDELTA
C   ***** THIS SUBROUTINE FINDS THE FLAP DEFLECTION ANGLE OF H. TAIL
C         OR CANARD FOR A GIVEN CONFIGURATION LIFT DISTRIBUTION. *****

C   ***** DEFINITION OF VARIABLES *****
C   AR      = ASPECT RATIO OF WING
C   EFFAOAW = WING'S EFFECTIVE AOA
C   EFFAOA  = EFFECTIVE AOA OF H TAIL OR CANARD
C   LAMQC   = WING'S QUARTER-CHORD SWEEP ANGLE
C   TR      = TAPER RATIO OF WING
C   L       = DISTANCE FROM WING TO TAIL OR CANARD
C             (+ FOR TAIL; - FOR CANARD)
C   H       = VERTICAL SEPERATION BETWEEN WING AND TAIL OR CANARD
C   CL      = LIFT COEFFIENT OF WING
C   CHORD   = MEAN AERODYNAMIC CHORD OF WING
C   B       = SPAN OF WING
C   S       = AREA OF WING
C   FCOC    = FLAP CHORD OVER CHORD OF TAIL OR CANARD
C   TOC      = THICKNESS RATIO OF H TAIL OR CANARD
C   INCIDENT = INCIDENT ANGLE OF H TAIL OR CANARD
C   EPSILON  = UP/DOWNWASH ANGLE FOR CANARD OR H. TAIL DUE TO WING
C   C1,C2,C3,KPRIME = NUMERICAL VARIABLES FOR CALCUATION PURPOSES
C   CLALPHA  = C-L-ALPHA OF H TAIL OR CANARD
C   OUTPUT  = FLAP DEFLECTION ANGLE FOR H TAIL OR CANARD

C   ***  NOTE: THIS PROGRAM NEGLECTS THE UP/DOWNWASH EFFECTS
C               BETWEEN THE H TAIL AND THE CANARD.

C   FINDING UP/DOWNWASH ANGLE DUE TO WING:

      CALL UPDOWN(AR,EFFAOAW,LAMQC,TR,L,H,CL,CHORD,B,S,S,EPSILON)
      C1      =  1.242 - .5991 * TOC
      C2      =  12.98 + 12.44 * TOC
      C3      = -10.53 - .6497 * TOC
      CLDELTA =  C1 + C2 * FCOC + C3 * FCOC ** 2
      OUTPUT  = - CLALPHA / CLDELTA *
     1           (-EFFAOA + AOA + EPSILON + INCIDENT)
      RETURN
      END
C
C
C   ****************************************************************
C   ***** SUBROUTINE ALLMOVE ******
C   ****************************************************************
C
      SUBROUTINE ALLMOVE(AR,EFFAOAW,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,
     +                   AOA, OUTPUT)
      REAL AR,EFFAOAW,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,S,EPSILON,AOA

C   *** THIS SUBROUTINE FINDS THE INCIDENT ANGLE OF THE ALL-
C       MOVING H TAIL OR CANARD FOR A GIVEN CONFIGURATION OF LIFT 
C       DISTRIBUTION. ***
C
C   ***** DEFINITION OF VARIABLES *****
C   AR       = ASPECT RATIO OF WING
C   EFFAOAW  = WING'S EFFECTIVE AOA
C   EFFAOA   = EFFECTIVE AOA OF H TAIL OR CANARD
C   LAMQC    = WING'S QUARTER-CHORD SWEEP ANGLE
C   TR       = TAPER RATIO OF WING
C   L        = DISTANCE FROM WING TO TAIL OR CANARD
C              (+ FOR TAIL; - FOR CANARD)
C   H        = VERTICAL SEPERATION BETWEEN WING AND TAIL OR CANARD
C   CL       = LIFT COEFFIENT OF WING
C   CHORD    = MEAN AERODYNAMIC CHORD OF WING
C   B        = SPAN OF WING
C   S        = AREA OF WING
C   EPSILON  = UP/DOWNWASH ANGLE FOR CANARD OR H. TAIL DUE TO WING
C   AOA      = AOA OF BODY REFERENCE LINE
C   OUTPUT   = INCIDENT ANGLE OF H TAIL OR CANARD
C
C   ***** FINDING UP/DOWNWASH ANGLE DUE TO WING ****

      CALL UPDOWN(AR,EFFAOAW,LAMQC,TR,L,H,CL,CHORD,B,S,S,EPSILON)
      OUTPUT = EFFAOA-AOA-EPSILON
      RETURN
      END


C   ******************************************************************
C   ***** SUBROUTINE UPDOWN *****
C   ******************************************************************
C
      SUBROUTINE UPDOWN (AR,EFFAOA,LAMQC,TR,L,H,CL,CHORD,B,
     +                     S,SWING,EPSILON)
      REAL AR,EFFAOA,LAMQC,TR,L,H,CL,CHORD,EPSILON

C   *** THIS SUBROUTINE CALCULATES THE UPWASH/DOWNWASH ANGLE OF 
C   SURFACE-B DUE TO THE LIFT GENERATED BY SURFACE-A. ***
C
C   ***** DEFINITION OF VARIABLES
C    AR      = ASPECT RATIO OF SURFACE-A
C    EFFAOA  = EFFECTIVE AOA OF SURFACE-A
C    LAMQC   = QUARTER CHORD LINE SWEEP ANGLE OF SURFACE-A
C    TR      = TAPER RATIO OF SURFACE-A
C    L       = DISTANCE BETWEEN SURFACES 
C              ('-' IF SURFACE-B IS BEHIND SURFACE-A)
C    H       = VERTICAL DISTANCE BETWEEN THE TWO SURFACES
C    CL      = LIFT COEFFICIENT OF SURFACE-A
C    EPSILON = DOWN/UPWASH ANGLE OF SURFACE-B DUE TO SURFACE-A 
C              ('+'=UPWASH; '-'=DOWNWASH)
C    B       = SPAN OF SURFACE-A
C    S       = AREA OF SURFACE-A
C    SWING   = AREA OF WING

        IF (CL*L.LT.0.0) CALL DOWNWASH(AR,EFFAOA,LAMQC,TR,
     +                                    L,H,B,S,SWING,EPSILON)
      IF (CL*L.GT.0.0) CALL UPWASH(L,CHORD,EFFAOA,EPSILON)
      IF (CL.EQ.0.0) EPSILON = 0.0
      RETURN
      END


C   ******************************************************************
C   ***** SUBROUTINE UPWASH *****
C   ******************************************************************
      SUBROUTINE UPWASH(L,C1,EFFAOA1,EPSILON)
      REAL L,C1,EFFAOA1,EPSILON,DEDA

C   *** THIS SUBROUTINE CALCULATES THE ABERAGE UPWASH ANGLE OF 
C   SURFACE-B DUE TO THE LIFT (+ OR -) GENERATED BY SURFACE-A BASED 
C   ON THE METHOD BY PERKINS & HAGE (1949).

C    *** DEFINITION OF VARIABLES ***
C    L        = HORIZONTAL SEPERATION BETWEEN THE TWO SURFACES
C    C1       = MEAN AERODYNAMIC CHORD OF SURFACE-A
C    EFFAOA1  = EFFECTIVE AOA OF SURFACE-A
C    EPSILON  = UPWASH ANGLE AT SURFACE-B DUE TO SURFACE-A

      DEDA    = .9 * (.722 ** (5.0 * ABS(L) / C1 - 2.0))
      EPSILON = DEDA * ABS(EFFAOA1)

      RETURN
      END


C   ******************************************************************
C   ***** SUBROUTINE DOWNWASH *****
C   ******************************************************************
      SUBROUTINE DOWNWASH(AR,EFFAOA,LAMQC,TR,L,H,B,S,SWING,EPSILON)
      REAL KA,KTR,KH,AR,EFFAOA,LAMQC,TR,L,H,S,SWING,EPSILON
C   *** THIS SUBROUTINE CALCULATES THE AVERAGE DOWNWASH ANGLE OF 
C   SURFACE-B INDUCED BY THE LIFT (+ OR -) GENERATED BY SURFACE-A 
C   BASED ON DATCOM(1978). ***
C
C   *** DEFINITION OF VARIABLES ***
C   AR        = ASPECT RATIO OF SURFACE-A
C   EFFAOA    = EFFECTIVE ANGLE OF ATTACK OF SURFACE-A
C   LAMQC     = SWP ANGLE OF QUARTER-CHORD LINE OF SURFACE-A
C   TR        = TAPER RATIO OF SURFACE-A
C   L         = HORIZONTAL DISTANCE BETWEEN SURFACE-A AND SURFACE-B
C   H         = VERTICAL DISTANCE BETWEEN SURFACE-A AND SURFACE-B
C   KA,KTR,KH = NUMERICAL VARIABLES FOR CALCULATION PURPOSED
C   EPSILON   = DOWNWASH ANGLE INDUCED BY SURFACE-A ON SURFACE-B
C   B         = SPAN OF SURFACE-A
C
      KA      = 1.0 / AR - (1.0 + AR ** 1.7) ** (-1.0)
      KTR     = (10.0 - 3.0 * TR) / 7.0
      KH      = (1.0 - ABS(H / B)) / (2.0 * ABS(L) / B) ** (1.0 / 3.0)
      DEDA    = -4.44 * S * (KA * KTR * KH * (COS(LAMQC)) ** (0.5)) ** 
     +           1.19 / SWING
      EPSILON = DEDA * ABS(EFFAOA)
      RETURN
      END


C   ******************************************************************
C   ***** SUBROUTINE C-L-ALPHA *****
C   ******************************************************************
      SUBROUTINE CLA(AR,BETA,LAMBDA,CLALPHA)
      REAL AR,BETA,PI,LAMBDA,CLALPHA
C
C   *** THIS SUBROUTINE CALCULATES THE LIFT CURVE SLOPE (C-L-ALPHA)
C        OF AN AERODYNAMIC SURFACE. ***
C
      PI       = 4.0 * ATAN(1.0)
      CLALPHA = 2 * PI * AR / (2 + (4 + (AR * BETA) ** 2 * ( 1 + (TAN
     +            (LAMBDA) / BETA) ** 2)) ** .5)
      RETURN
      END


C   ******************************************************************
C   ***** SUBROUTINE DA0L *****
C   ******************************************************************
      SUBROUTINE DA0L(TR,FCOC,DELTAF,CLA,OUTPUT)
      REAL TR,FCOC,DELTAF,OUTPUT,CLDELTA,KPRIME,K1,K2,K3,K4,C1,C2,C3

C   *** THIS SUBROUTINE CALCULATES THE CHANGE IN INCIDENT ANGLE DUE 
C    TO THE DEFLECTION OF SIMPLE HINGED CONTROL FLAP FOR ANY 
C    AERODYNAMIC SURFACES. ***

C   ***** DEFINITION OF VARIABLES
C   TR      = AERODYNAMIC SURFACE THICKNESS RATIO
C   FCOC    = SIMPLE FLAP CHORD / AERODYNAMIC SURFACE CHORD
C   DELTAF  = FLAP DEFLECTION ANGLE
C   CLA     = AERODYNAMIC SURFACE LIFT-CURVE SLOPE (C-L-ALPHA)
C   OUTPUT  = DELTA-ALPHA-0-L
C   CLDELTA = LIFT EFFECTIVENESS OF PLAIN TRAILING-EDGE FLAP
C   C1,C2,C3,K1,K2,K3,K4 = EMPIRICAL PARAMETERS FROM DATCOM-1978
C   KPRIME  = FLAP-EFFECTIVENESS AT LARGE DEFLECTIONS
C
      C1      = 1.242 - .5991 * TR
      C2      = 12.98 + 12.44 * TR
      C3      = -10.53 - .6497 * TR
      K1      = 1.011 + .1740 * FCOC
      K2      = .002053 - .02069 * FCOC
      K3      = .0004845 + .00002479 * FCOC
      K4      = (5.688E-6) -( 1.217E-7) * FCOC
      CLDELTA =  C1 + C2 * FCOC + C3 * FCOC ** 2
      KPRIME  =  K1 + K2 * DELTAF + K3 * DELTAF ** 2.0  + 
     +           K4 * DELTAF ** 3.0
      OUTPUT  = -CLDELTA * DELTAF * KPRIME / CLA
      RETURN
      END


C   ******************************************************************
C   ***** SUBROUTINE MATRIX INVERSION *****
C   ******************************************************************
      SUBROUTINE MATRIX(A,D,C)
      INTEGER N
      PARAMETER (N=5)
      DIMENSION A(N,N), C(N),D(N)

C   NOTE:  N=SIZE OF MATRIX
C   USER MUST UPDATE THE VALUE OF N IN THE ABOVE PARAMETER STATEMENT
C   INPUT MATRIX [A] & [D]; OUTPUT MATRIX [C] SUCH THAT [A][C]=[D]

C     STARTING MATRIX INVERSION
      DO 1100 K = 1,N-1
      M = K + 1
      L = K
 1130 Q = ABS(A(M,K)) - ABS(A(L,K))
      IF (Q.GT.0.0) L = M
      IF (M.LT.N) THEN
      M = M + 1
      GO TO 1130
      ENDIF
      IF (L.EQ.K) GO TO 1110
      DO 1200 J = K,N
      DU = A(K,J)
      A(K,J) = A(L,J)
      A(L,J) = DU
 1200 CONTINUE
      DD = D(K)
      D(K) = D(L)
      D(L) = DD
 1110 M = K + 1
 1120 Q = A(M,K) / A(K,K)
      A(M,K) = 0
      DO 1300 J = K + 1, N
      A(M,J) = A(M,J) - Q * A(K,J)
 1300 CONTINUE
      D(M) = D(M) - Q * D(K)
      IF (M.LT.N) THEN
      M = M + 1
      GO TO 1120
      ENDIF
 1100 CONTINUE
      C(N) = D(N) / A(N,N)
      DO 1500 M = N-1,1,-1
      Q = 0
      DO 1400 J = M + 1, N
      Q = Q + A(M,J) * C(J)
      C(M) = (D(M) - Q) / A(M,M)
 1400 CONTINUE
 1500 CONTINUE

      END
