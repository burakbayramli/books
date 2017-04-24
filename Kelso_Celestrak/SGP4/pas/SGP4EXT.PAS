(*     ----------------------------------------------------------------      *)
(*                                                                           *)

                               UNIT SGP4EXT;

(*                                                                           *)
(*    this file contains extra routines needed for the main test program for *)
(*    sgp4. these routines are derived from the astro libraries.             *)
(*                                                                           *)
(*                           Companion code for                              *)
(*             Fundamentals of Astrodynamics and Applications                *)
(*                                  2007                                     *)
(*                            by David Vallado                               *)
(*                                                                           *)
(*     (W) 719-573-2600, email dvallado@agi.com                              *)
(*                                                                           *)
(*     *****************************************************************     *)
(*                                                                           *)
(*  Current :                                                                *)
(*             2 apr 07  david vallado                                       *)
(*                         misc fixes for new versions                       *)
(*  Changes :                                                                *)
(*            14 Aug 06  david vallado                                       *)
(*                         original baseline                                 *)
(*                                                                           *)
(*     ----------------------------------------------------------------      *)

                                  INTERFACE

(*     ----------------------------------------------------------------      *)

  TYPE
     Str3   = STRING[3];
     Vector = ARRAY[1..4] of EXTENDED;

  VAR
     MonthTitle  : ARRAY[1..12] of Str3;


FUNCTION  SGN                ( XVal                                  : EXTENDED ) : EXTENDED;

PROCEDURE MAG                ( VAR Vec                               : Vector   );

FUNCTION  DOT                ( Vec1,Vec2                             : Vector   ) : EXTENDED;

PROCEDURE CROSS              ( Vec1,Vec2                             : Vector;
                               VAR OutVec                            : Vector   );

FUNCTION  POWER              ( Base, Exponent                        : EXTENDED ) : EXTENDED;

FUNCTION  MODFUNC            ( XVal, Modby                           : EXTENDED ) : EXTENDED;

FUNCTION  TAN                ( XVal                                  : EXTENDED ) : EXTENDED;

FUNCTION  ATAN2              ( SinValue, CosValue                    : EXTENDED ) : EXTENDED;

FUNCTION  ASIN               ( XVal                                  : EXTENDED ) : EXTENDED;

FUNCTION  ACOS               ( XVal                                  : EXTENDED ) : EXTENDED;

FUNCTION  COSH               ( XVal                                  : EXTENDED ) : EXTENDED;

FUNCTION  SINH               ( XVal                                  : EXTENDED ) : EXTENDED;

FUNCTION  ASINH              ( XVal                                  : EXTENDED ) : EXTENDED;

PROCEDURE CUBIC              ( a,b,c,d                               : EXTENDED;
                               VAR R1r,R1i,R2r,R2i,R3r,R3i           : EXTENDED );

PROCEDURE ANGLE              ( Vec1,Vec2                             : Vector;
                               VAR Theta                             : EXTENDED );

PROCEDURE NEWTONNU           ( Ecc,Nu                                : EXTENDED;
                               VAR E0,M                              : EXTENDED );

PROCEDURE rv2coe             ( R,V                                   : Vector;
                               mu                                    : Extended;
                               VAR P,A,Ecc,Incl,Omega,Argp,
                               Nu,M,ArgLat,TrueLon,LonPer            : EXTENDED );

PROCEDURE JDAY               ( Year,Mon,Day,Hr,MIN                   : INTEGER;
                               SEC                                   : EXTENDED;
                               VAR JD                                : EXTENDED );

PROCEDURE DAYS2MDHMS         ( Year                                  : INTEGER;
                               Days                                  : EXTENDED;
                               VAR Mon,Day,Hr,MIN                    : INTEGER;
                               VAR SEC                               : EXTENDED );

PROCEDURE INVJDAY            ( JD                                    : EXTENDED;
                               VAR Year,Mon,Day,Hr,MIN               : INTEGER;
                               VAR SEC                               : EXTENDED );


(*     ----------------------------------------------------------------      *)

                                IMPLEMENTATION

(*     ----------------------------------------------------------------      *)

{ ------------------------------------------------------------------------------
|
|                           FUNCTION SGN
|
|  This function determines the sign of a number.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - Value to determine sign of
|
|  OutPuts       :
|    SGN         - Result                         +1 or -1
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION SGN                   ( XVal                      : EXTENDED ) : EXTENDED;
   VAR
     Temp : EXTENDED;
   BEGIN
     IF XVal > 0.0 THEN
         Temp:= 1.0
       ELSE
         Temp:= -1.0;
     SGN:= Temp;
   END;  { FUNCTION SGN }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE MAG
|
|  This procedure finds the magnitude of a vector.  The tolerance is set to
|    0.000001, thus the 1.0E-12 for the squared test of underflows.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Vec         - Vector
|
|  OutPuts       :
|    Vec         - Answer stored in fourth component
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

PROCEDURE MAG                ( VAR Vec                               : Vector );
   VAR Temp: EXTENDED;
   BEGIN
     Temp:= Vec[1]*Vec[1] + Vec[2]*Vec[2] + Vec[3]*Vec[3];

     IF ABS( Temp ) >= 1.0E-12 THEN
         Vec[4]:= SQRT( Temp )
       ELSE
         Vec[4]:= 0.0;
   END;  { PROCEDURE MAG }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE CROSS
|
|  This procedure crosses two vectors.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Vec1        - Vector number 1
|    Vec2        - Vector number 2
|
|  OutPuts       :
|    OutVec      - Vector result of A x B
|
|  Locals        :
|    None.
|
|  Coupling      :
|    MAG           Magnitude of a vector
|
 ----------------------------------------------------------------------------- }

PROCEDURE CROSS              ( Vec1,Vec2                             : Vector;
                               VAR OutVec                            : Vector );
   BEGIN
     OutVec[1]:= Vec1[2]*Vec2[3]-Vec1[3]*Vec2[2];
     OutVec[2]:= Vec1[3]*Vec2[1]-Vec1[1]*Vec2[3];
     OutVec[3]:= Vec1[1]*Vec2[2]-Vec1[2]*Vec2[1];

     MAG( OutVec );
   END;   { PROCEDURE CROSS }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION DOT
|
|  This function finds the DOT product of two vectors.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Vec1        - Vector number 1
|    Vec2        - Vector number 2
|
|  OutPuts       :
|    DOT         - Result
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION DOT                 ( Vec1,Vec2                             : Vector ) : EXTENDED;
   BEGIN
     DOT:= Vec1[1]*Vec2[1] + Vec1[2]*Vec2[2] + Vec1[3]*Vec2[3];
   END;  { FUNCTION DOT }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION POWER
|
|  This FUNCTION performs the raising of a base to a POWER.  Notice the many
|    statements to allow processing of negative INTEGER values.
|
|  Algorithm     : IF the base and exponent are positive, calculate the answer
|                  Otherwise, check for 0 base 1st, THEN 0 exponent
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Base        - Base value
|    Pwr         - POWER to raise base to
|
|  OutPuts       :
|    POWER       - Result
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
|  References    : PC Magazine, 10 Sep 91, pg. 465-466.
|
 ----------------------------------------------------------------------------- }

FUNCTION POWER               ( Base, Exponent                        : EXTENDED ) : EXTENDED;
   CONST
     Small     : EXTENDED = 0.00000000001;
   VAR Recip   : EXTENDED;

   FUNCTION IsInt( Num : EXTENDED ): Boolean;
     BEGIN
       IsInt:= ((ABS(FRAC(Num)) < Small) or
               (1.0-ABS(FRAC(Num)) < Small)) and (ABS(Num) < MAXLongInt );
     END;  { Internal FUNCTION IsInt }

   FUNCTION SignFix( Num : LongInt ): ShortInt;
     BEGIN
       IF ODD(Num) THEN
           SignFix:= -1
         ELSE
           SignFix:= 1;
     END;  { Internal FUNCTION SignFix }

   BEGIN
     IF ABS(Base) > Small THEN
       BEGIN
         IF ABS(Exponent) > Small THEN
           BEGIN
             { --------------- Base is (+) and Exponent <> 0 ---------------- }
             IF Base > 0.0 THEN
                 POWER := Exp( Exponent * LN( Base ) )
               ELSE
               BEGIN
                 { --------- Base is (-) and Exponent is an INTEGER --------- }
                 IF IsInt( Exponent ) THEN
                     POWER := Exp( Exponent * LN(-Base) )*SignFix( ROUND(Exponent) )
                   ELSE
                   { ----------- Base is (-) and Exponent is real ----------- }
                   BEGIN
                     Recip:= 1.0/Exponent;
                     { ---- Base is (-) and Exponent is recip of odd int ---- }
                     IF ( ABS(ROUND(Recip) - Recip) < 0.000001 ) and
                        ( ODD(ROUND(Recip)) ) THEN
                         POWER := -Exp( Exponent * LN(-Base) )
                       ELSE
                       BEGIN
                         POWER:= 0.0;
                         Writeln( 'Error in POWER with base = ',base,' and Pwr = ',Exponent );
                       END;
                   END;
               END;
           END
           ELSE
             { ---- IF Exponent = 0 ---- }
             POWER:= 1.0;
       END
       ELSE
         { -------- IF Base = 0 -------- }
         POWER:= 0.0;
   END;  { FUNCTION POWER }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION MODFUNC
|
|  This function performs the MOD operation for REALs.
|
|  Algorithm     : Assign a temporary variable
|                  Subtract off an INTEGER number of values while the xval is
|                     too large
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - Value to MOD
|    ModBy       - Value to MOD with
|
|  OutPuts       :
|    MODFUNC     - Result                         -ModBy <=  Answer  <= +ModBy
|
|  Locals        :
|    TempValue   - Temporary EXTENDED value
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION MODFUNC             ( XVal,Modby                            : EXTENDED ) : EXTENDED;
   VAR
       TempValue: EXTENDED;
   BEGIN
     TempValue := XVal;
     WHILE ABS(TempValue) > ModBy DO
         TempValue:= TempValue - INT(XVal/ModBy)*ModBy;
     MODFUNC:= TempValue;
   END;  { FUNCTION MODFUNC }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION TAN
|
|  This function finds the tangent of an ANGLE in radians.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE to take Tangent of                     rad
|
|  OutPuts       :
|    TAN         - Result
|
|  Locals        :
|    Temp        - Temporary Real variable
|
 ----------------------------------------------------------------------------- }

FUNCTION TAN                 ( XVal                                  : EXTENDED ) : EXTENDED;
   CONST
     Infinity   : EXTENDED = 999999.9;
     Small      : EXTENDED =     0.000001;
   VAR
     Temp : EXTENDED;
   BEGIN
     Temp := COS( XVal );

     IF ABS( Temp ) < Small THEN
        TAN := Infinity
      ELSE
        TAN := SIN( XVal ) / Temp;
   END;  { FUNCTION TAN }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION ATAN2
|
|  This function performs the arc tangent 2 FUNCTION which resolves
|    quadrants.  The arguments passed are the sine and cosine values.
|
|  Algorithm     : Determine the quadrant using IF statments
|                  IF the answer is not a sepcial case, 0, 180, etc
|                     find the arctangent
|                  otherwise, find the special case values
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    SinValue    - Sine of desired ANGLE                        rad
|    CosValue    - Cosine of desired value                      rad
|
|  OutPuts       :
|    ATAN2       - Arctangent with resolved quadrants           0.0 to 2Pi rad
|
|  Locals        :
|    TanArg      - Temporary EXTENDED Value
|    Quadrant    - Quadrant of the answer                       1 2 3 4
|    SinINTEGER  - Sign of the value                            +1 or -1
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION ATAN2               ( SinValue,CosValue                     : EXTENDED ) : EXTENDED;
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
   VAR
     TanArg               : EXTENDED;
     Quadrant, SinINTEGER : INTEGER;
   BEGIN
     Quadrant:= 5;
     IF (SinValue > 0.0 ) and (SinValue < 1.0 ) and
        (CosValue > 0.0 ) and (CosValue < 1.0 ) THEN
          quadrant:= 1;
     IF (SinValue > 0.0 ) and (SinValue < 1.0 ) and
        (CosValue < 0.0 ) and (CosValue >-1.0 ) THEN
          quadrant:= 2;
     IF (SinValue > -1.0 ) and (SinValue < 0.0 ) and
        (CosValue <  0.0 ) and (CosValue >-1.0 ) THEN
          quadrant:= 3;
     IF (SinValue > -1.0 ) and (SinValue < 0.0 ) and
        (CosValue >  0.0 ) and (CosValue < 1.0 ) THEN
          quadrant:= 4;
     IF Quadrant <> 5 THEN
       BEGIN
         tanarg:= ARCTAN(SinValue/CosValue);
         IF (Quadrant <  4) and (Quadrant <> 1) THEN
             tanarg:= tanarg + Pi
           ELSE
             IF Quadrant =  4 THEN
                 tanarg:= tanarg + TwoPi;
       END
       ELSE
         BEGIN                                  { Special Cases }
           SinINTEGER:= Round(SinValue);
           CASE SinINTEGER OF
             -1 :  TanArg:= 3.0*Pi/2.0;
              0 :  IF ROUND(CosValue) > 0.0 THEN
                       TanArg:= 0.0
                     ELSE
                       TanArg:= Pi;
              1 :  TanArg:=  Pi/2.0;
           END; { Case }
         END;
     ATAN2:= tanarg;
   END; { FUNCTION ATAN2 }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION ASIN
|
|  This function evaluates Arc Sine using the standard Arc Tangent FUNCTION.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE value                                  -1.0 to 1.0 rad
|
|  OutPuts       :
|    ASIN      - Result                                       -Pi/2 to Pi/2 rad
|
|  Locals        :
|    Temp        - Temporary EXTENDED Value
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION ASIN              ( XVal                                  : EXTENDED ) : EXTENDED;
   CONST
     Small      : EXTENDED =     0.000001;
   VAR
     Temp : EXTENDED;
   BEGIN
     IF ABS( XVal ) < 1.0 THEN
         Temp:= ARCTAN( XVal / SQRT(1.0 - XVal*XVal) )
       ELSE
         IF ABS(XVal)-1.0 < Small THEN
             IF XVal > 0.0  THEN
                 Temp:=   Pi / 2.0  { XVal =  1.0 }
               ELSE
                 Temp:=  -Pi / 2.0  { XVal = -1.0 }
           ELSE
             Writeln( 'Error in ASIN argument = ',XVal );

     ASIN:= Temp;
   END;  { FUNCTION ASIN }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION ACOS
|
|  This function evaluates Arc Cosine using the ASIN FUNCTION.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE Value                                  -1.0 to 1.0 rad
|
|  OutPuts       :
|    ACOS      - Result                                       0.0 to Pi rad
|
|  Locals        :
|    Temp        - Temporary EXTENDED Value
|
|  Coupling      :
|    ASIN        Sine of an ANGLE in radians
|
 ----------------------------------------------------------------------------- }

FUNCTION ACOS              ( XVal                                  : EXTENDED ) : EXTENDED;
   CONST
     Small      : EXTENDED =     0.000001;
   VAR
     Temp : EXTENDED;
   BEGIN
     IF ABS(XVal) < 1.0 THEN
       BEGIN
         Temp:= ASIN( SQRT(1.0 - XVal*XVal) );
         IF XVal < 0.0 THEN
             Temp:= Pi - Temp;
       END
       ELSE
         IF ABS(XVal)-1.0 < Small THEN
             IF XVal > 0.0  THEN
                 Temp:=  0.0    { XVal =  1.0 }
               ELSE
                 Temp:=  Pi     { XVal = -1.0 }
           ELSE
             Writeln( 'Error in ACOS argument = ',XVal );

     ACOS:= Temp;
   END;  { FUNCTION ACOS }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION COSH
|
|  This function evaluates the hyperbolic cosine FUNCTION.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE value                                  any real
|
|  OutPuts       :
|    COSH        - Result                                       1.0 to Infinity
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION COSH                ( XVal                                  : EXTENDED ) : EXTENDED;
   BEGIN
     COSH:= 0.5*( EXP(XVal) + EXP(-XVal) );
   END;  { FUNCTION COSH }



{ ------------------------------------------------------------------------------
|
|                           FUNCTION ACOSH
|
|  This function evaluates the inverse hyperbolic cosine FUNCTION.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE Value                                  1.0 to Infinity
|
|  OutPuts       :
|    ACOSH     - Result                                       any real
|
|  Locals        :
|    Temp        - Temporary EXTENDED Value
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION ACOSH             ( XVal                                  : EXTENDED ) : EXTENDED;
   CONST
     Undefined  : EXTENDED = 999999.1;
   VAR
     Temp : EXTENDED;
   BEGIN
     IF XVal*XVal - 1.0 < 0.0 THEN
       BEGIN
         Temp:= Undefined;
         WriteLn( 'Error in ACOSH FUNCTION ' );
       END
       ELSE
         Temp:= LN( XVal + SQRT( XVal*XVal - 1.0 ) );

     ACOSH:= Temp;
   END;  { FUNCTION ACOSH }


{ ------------------------------------------------------------------------------
|
|                           FUNCTION SINH
|
|  This function evaluates the hyperbolic sine FUNCTION.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE Value                                  any real
|
|  OutPuts       :
|    SINH        - Result                                       any real
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION SINH                ( XVal                                  : EXTENDED ) : EXTENDED;
   BEGIN
     SINH:= 0.5*( EXP(XVal) - EXP(-XVal) );
   END;  { FUNCTION SINH }




{ ------------------------------------------------------------------------------
|
|                           FUNCTION ASINH
|
|  This function evaluates the inverse hyperbolic sine FUNCTION.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    XVal        - ANGLE Value                                  any real
|
|  OutPuts       :
|    ASINH     - Result                                       any real
|
|  Locals        :
|    None.
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

FUNCTION ASINH             ( XVal                                  : EXTENDED ) : EXTENDED;
   BEGIN

     ASINH:= LN( XVal + SQRT( XVal*XVal + 1.0 ) );

   END;  { FUNCTION ASINH }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE ANGLE
|
|  This procedure calculates the ANGLE between two vectors.  The output is
|    set to 999999.1 to indicate an undefined value.  Be SURE to check for
|    this at the output phase.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Vec1        - Vector number 1
|    Vec2        - Vector number 2
|
|  OutPuts       :
|    Theta       - ANGLE between the two vectors  -Pi to Pi
|
|  Locals        :
|    Temp        - Temporary REAL variable
|
|  Coupling      :
|    DOT           DOT Product of two vectors
|    ACOS        Arc Cosine FUNCTION
|
 ----------------------------------------------------------------------------- }

PROCEDURE ANGLE              ( Vec1,Vec2                             : Vector;
                               VAR Theta                             : EXTENDED );
   CONST
     Small    : EXTENDED = 0.000001;
     Undefined: EXTENDED = 999999.1;
   VAR
     Temp : EXTENDED;
   BEGIN
     IF Vec1[4]*Vec2[4] > SQR(Small) THEN
       BEGIN
         Temp:= DOT(Vec1,Vec2) / (Vec1[4]*Vec2[4]);
         IF ABS( Temp ) > 1.0 THEN
             Temp:= SGN(Temp) * 1.0;
         Theta:= ACOS( Temp );
       END
       ELSE
         Theta:= Undefined;
   END;  { PROCEDURE ANGLE   }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE CUBIC
|
|  This procedure solves for the three Roots of a CUBIC equation.  There are
|    no restrictions on the coefficients, and imaginary results are passed
|    out as separate values.  The general form is y = ax3 + bx2 + cx + d.  Note
|    that R1i will ALWAYS be ZERO since there is ALWAYS at least one REAL Root.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    a           - Coefficient of x cubed term
|    b           - Coefficient of x squared term
|    c           - Coefficient of x term
|    d           - Constant
|
|  OutPuts       :
|    R1r         - Real portion of Root 1
|    R1i         - Imaginary portion of Root 1
|    R2r         - Real portion of Root 2
|    R2i         - Imaginary portion of Root 2
|    R3r         - Real portion of Root 3
|    R3i         - Imaginary portion of Root 3
|
|  Locals        :
|    Temp1       - Temporary value
|    Temp2       - Temporary value
|    Root1       - Temporary value of the Root
|    Root2       - Temporary value of the Root
|    Root3       - Temporary value of the Root
|    P           - Coefficient of x squared term where x cubed term is 1.0
|    Q           - Coefficient of x term where x cubed term is 1.0
|    R           - Coefficient of constant term where x cubed term is 1.0
|    Delta       - Discriminator for use with Cardans formula
|    E0          - ANGLE holder for trigonometric solution
|    Phi         - ANGLE used in trigonometric solution
|    CosPhi      - Cosine of Phi
|    SinPhi      - Sine of Phi
|
|  Coupling      :
|    ATAN2         Arctangent including check for 180-360 deg
|    POWER         Raise a base to an exponent
|
|  References    :
|    Vallado       2007, 975
|
 ----------------------------------------------------------------------------- }

PROCEDURE CUBIC              ( a,b,c,d                                : EXTENDED;
                               VAR R1r,R1i,R2r,R2i,R3r,R3i            : EXTENDED  );
   CONST
     rad2deg  : EXTENDED =   180.0 / pi;    { 57.29577951308230; }
     OneThird : EXTENDED = 1.0/3.0;
     Small    : EXTENDED = 0.000001;
   VAR
     Temp1, Temp2, Root1, Root2, Root3, P, Q, R, Delta,
     E0, CosPhi, SinPhi, Phi : EXTENDED;
   BEGIN
     { ----------------------   Initialize   ----------------------- }
     R1r  := 0.0;
     R1i  := 0.0;
     R2r  := 0.0;
     R2i  := 0.0;
     R3r  := 0.0;
     R3i  := 0.0;
     Root1:= 0.0;
     Root2:= 0.0;
     Root3:= 0.0;

     { ------------ Force coefficients into std form --------------- }
     P:= B/A;
     Q:= C/A;
     R:= D/A;

     a:= OneThird*( 3.0*Q - P*P );
     b:= (1.0/27.0)*( 2.0*P*P*P - 9.0*P*Q + 27.0*R );

     Delta:= (a*a*a/27.0) + (b*b*0.25);

     { ------------------- Use Cardans formula --------------------- }
     IF Delta > Small THEN
       BEGIN
         Temp1:= (-b*0.5)+SQRT(Delta);
         Temp2:= (-b*0.5)-SQRT(Delta);
         IF ABS(Temp1) > Small THEN
             Temp1:= POWER( Temp1,OneThird );
         IF ABS(Temp2) > Small THEN
             Temp2:= POWER( Temp2,OneThird );
         Root1:= Temp1 + Temp2;
         Root2:= -0.5*(Temp1 + Temp2);
         Root3:= -0.5*(Temp1 + Temp2);
         R2i:= -0.5*SQRT( 3.0 )*(Temp1 - Temp2);
         R3i:= -R2i;
       END
       ELSE
     { ---------------- Evaluate zero point ------------------------ }
       BEGIN
         IF ABS( Delta ) < Small  THEN
           BEGIN
             IF ABS(b) > Small THEN
               BEGIN
                 Root1:= -2.0*POWER( b*0.5,OneThird );
                 Root2:=  POWER( b*0.5,OneThird );
                 Root3:= Root2;
               END;
           END
           ELSE
     { ---------------- Use trigonometric identities --------------- }
           BEGIN
             E0     := 2.0*SQRT(-a*OneThird);
             CosPhi := (-b/(2.0*SQRT(-a*a*a/27.0)) );
             SinPhi := SQRT( 1.0-CosPhi*CosPhi );
             Phi    := ATAN2( SinPhi,CosPhi );
             Root1:= E0*COS( Phi*OneThird );
             Root2:= E0*COS( Phi*OneThird + 120.0/rad2deg );
             Root3:= E0*COS( Phi*OneThird + 240.0/rad2deg );
           END;
       END;

     R1r:= Root1 - P*OneThird;
     R2r:= Root2 - P*OneThird;
     R3r:= Root3 - P*OneThird;
   END;  { PROCEDURE CUBIC }

{ ------------------------------------------------------------------------------
|
|                           PROCEDURE NEWTONNU
|
|  This procedure solves Keplers equation when the true anomaly is known.
|    The Mean and Eccentric, parabolic, or hyperbolic anomalies are also found.
|    The parabolic limit at 168ø is arbitrary.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Ecc         - Eccentricity                   0.0 to
|    Nu          - True Anomaly                   -2Pi to 2Pi rad
|
|  Outputs       :
|    E0          - Eccentric Anomaly              0.0 to 2Pi rad
|    M           - Mean Anomaly                   0.0 to 2Pi rad
|
|  Locals        :
|    SinE        - Sine of E
|    CosE        - Cosine of E
|    Ktr         - Index
|
|  Coupling      :
|    ATAN2       - Arc tangent FUNCTION which also resloves quadrants
|    ASINH     - Arc hyperbolic sine
|    SINH        - Hyperbolic Sine
|
|  References    :
|    Vallado       2007, 85, Alg 5
|
 ----------------------------------------------------------------------------- }

PROCEDURE NEWTONNU           ( Ecc,Nu                                : EXTENDED;
                               VAR E0,M                              : EXTENDED );
   VAR
     SinE, CosE : EXTENDED;
   BEGIN
     E0:= 999999.9;     M := 999999.9;
     { ---------------------------- Circular ------------------------ }
     IF ABS( Ecc ) < 0.000001 THEN
       BEGIN
         M := Nu;
         E0:= Nu;
       END
       ELSE
         { ----------------------- Elliptical ----------------------- }
         IF Ecc < 0.999 THEN
           BEGIN
             SinE:= ( SQRT( 1.0-Ecc*Ecc ) * SIN(Nu) ) / ( 1.0+Ecc*COS(Nu) );
             CosE:= ( Ecc + COS(Nu) ) / ( 1.0 + Ecc*COS(Nu) );
             E0  := ATAN2( SinE,CosE );
             M   := E0 - Ecc*SIN(E0);
           END
           ELSE
             { --------------------- Hyperbolic  -------------------- }
             IF Ecc > 1.0001 THEN
               BEGIN
                 IF ((Ecc > 1.0) and (ABS(Nu)+0.00001 < Pi-ACOS(1.0/Ecc)) ) THEN
                   BEGIN
                     SinE:= ( SQRT( Ecc*Ecc-1.0 ) * SIN(Nu) ) /
                            ( 1.0 + Ecc*COS(Nu) );
                     E0  := ASINH( SinE );
                     M   := Ecc*SINH(E0) - E0;
                   END;
               END
               ELSE
                 { ------------------ Parabolic --------------------- }
                 IF ABS(Nu) < 168.0/57.29578 THEN { TAN undef at 180 }
                   BEGIN
                     E0:= TAN( Nu*0.5 );
                     M := E0 + (E0*E0*E0)/3.0;
                   END;

     IF Ecc < 1.0 THEN  { Since B and H are areas }
       BEGIN
         MODFUNC( M,2.0*Pi );
         IF M < 0.0 THEN
             M:= M + 2.0*Pi;
         MODFUNC( E0,2.0*Pi );
       END;

   END;  { PROCEDURE NEWTONNU }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE rv2coe
|
|  This procedure finds the classical orbital elements given the Geocentric
|    Equatorial Position and Velocity vectors.  Special cases for equatorial
|    and circular orbits are also handled.  IF the elements are Infinite, they
|    are set to 999999.9. IF elements are Undefined, they are set to 999999.1.
|    Be sure to check for these during output or subsequent use!!
|
|  Algorithm     : Initialze variables
|                  IF the HBar magnitude exists, contiNue, otherwise exit and
|                       assign undefined values
|                    Find vectors and values
|                    Determine the type of orbit with IF statements
|                    Find angles depending on the orbit type
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    R           - IJK Position vector            km
|    V           - IJK Velocity vector            km / s
|
|  Outputs       :
|    P           - SemiLatus rectum               km
|    A           - semimajor axis                 km
|    Ecc         - Eccentricity
|    Incl        - inclination                    0.0 to Pi rad
|    Omega       - Longitude of Ascending Node    0.0 to 2Pi rad
|    Argp        - Argument of Perigee            0.0 to 2Pi rad
|    Nu          - True anomaly                   0.0 to 2Pi rad
|    M           - Mean anomaly                   0.0 to 2Pi rad
|    ArgLat      - Argument of Latitude      (CI) 0.0 to 2Pi rad
|    LamTrue     - True Longitude            (CE) 0.0 to 2Pi rad
|    LonPer      - Longitude of Periapsis    (EE) 0.0 to 2Pi rad
|
|  Locals        :
|    HBar        - Angular Momentum H Vector      km2 / s
|    EBar        - Eccentricity     E Vector
|    NBar        - Line of Nodes    N Vector
|    c1          - V**2 - u/R
|    RDotV       - R DOT V
|    Hk          - Hk unit vector
|    SME         - Specfic Mechanical Energy      km2 / s2
|    i           - index
|    E           - Eccentric, Parabolic,
|                  Hyperbolic Anomaly             rad
|    Temp        - Temporary variable
|    TypeOrbit   - Type of orbit                  EE, EI, CE, CI
|
|  Coupling      :
|    MAG         - Magnitude of a vector
|    CROSS       - CROSS product of two vectors
|    DOT         - DOT product of two vectors
|    ACOS        - Arc Cosine FUNCTION
|    ANGLE       - Find the ANGLE between two vectors
|    NEWTONNU    - Find the mean anomaly
|    MODFUNC     - MOD FUNCTION for REAL variables
|
|  References    :
|    Vallado       2007, 121, Alg 9, Ex 2-5
 ----------------------------------------------------------------------------- }

PROCEDURE rv2coe             ( R,V                                   : Vector;
                               Mu                                    : Extended; 
                               VAR P,A,Ecc,Incl,Omega,Argp,
                               Nu,M,ArgLat,TrueLon,LonPer            : EXTENDED );
   CONST
     TwoPi      : EXTENDED =     2.0 * pi;    { 6.28318530717959; }
     HalfPi     : EXTENDED =     0.5 * pi;    { 1.57079632679490; }
     Small      : EXTENDED =     0.00000001;       { Small value for tolerances }
     Infinite   : EXTENDED = 999999.9;             { Infinite value  }
     Undefined  : EXTENDED = 999999.1;             { Undefined value }
   VAR
     i                  : INTEGER;
     TypeOrbit          : String[2];
     HBar, EBar, NBar   : Vector;
     E, c1, RDotV, Hk, SME, Temp : EXTENDED;
   BEGIN
     { --------------------  Initialize values   -------------------- }
     MAG( R );
     MAG( V );
     { -------------------  Find H N and E vectors   ---------------- }
     CROSS( R,V,HBar );
     IF HBar[4] > Small THEN
       BEGIN
         NBar[1]:= -HBar[2];
         NBar[2]:=  HBar[1];
         NBar[3]:=   0.0;
         MAG( NBar );
         c1 := V[4]*V[4] - mu/R[4];
         RDotV:= DOT( R,V );
         FOR i:= 1 to 3 DO
             EBar[i]:= (c1*R[i] - RDotV*V[i])/mu;
         MAG( EBar );

         { -------------  Find a e and semi-Latus rectum   ---------- }
         SME:= ( V[4]*V[4]*0.5 ) - ( mu/R[4] );
         IF ABS( SME ) > Small THEN
             A:= -mu / (2.0*SME)
           ELSE
             A:= Infinite;       { Parabola }
         Ecc:= EBar[4];
         P := HBar[4]*HBar[4]/mu;

         { ------------------  Find inclination   ------------------- }
         Hk:= HBar[3]/HBar[4];
         IF ABS( ABS(Hk) - 1.0 ) < Small THEN
             { -------  Equatorial Orbits   ---------- }
             IF ABS(HBar[3]) > 0.0 THEN
                 Hk:= SGN(HBar[3]) * 1.0;
         Incl:= ACOS( Hk );

         { ---------  Determine type of orbit for Later use  -------- }
         { ------- Elliptical, Parabolic, Hyperbolic Inclined ------- }
         TypeOrbit:= 'EI';
         IF Ecc < Small THEN
             { --------------  Circular Equatorial -------------- }
             IF ( Incl < Small ) or ( ABS(Incl-Pi) < Small ) THEN
                 TypeOrbit:= 'CE'
               ELSE
                 { ------------  Circular Inclined -------------- }
                 TypeOrbit:= 'CI'
           ELSE
             { -- Elliptical, Parabolic, Hyperbolic Equatorial -- }
             IF ( Incl < Small ) or ( ABS(Incl-Pi) < Small ) THEN
               TypeOrbit:= 'EE';

         { -----------  Find Longitude of Ascending Node ------------ }
         IF NBar[4] > Small THEN
           BEGIN
             Temp:= NBar[1] / NBar[4];
             IF ABS(Temp) > 1.0 THEN
                 Temp:= SGN(Temp) * 1.0;
             Omega:= ACOS( Temp );
             IF NBar[2] < 0.0 THEN
                 Omega:= TwoPi - Omega;
           END
           ELSE
             Omega:= Undefined;

         { ----------------- Find Argument of perigee --------------- }
         IF TypeOrbit = 'EI' THEN
           BEGIN
             ANGLE( NBar,EBar,  Argp );
             IF EBar[3] < 0.0 THEN
                 Argp:= TwoPi - Argp;
           END
           ELSE
             Argp:= Undefined;

         { -------------  Find True Anomaly at Epoch    ------------- }
         IF TypeOrbit[1] = 'E' THEN
           BEGIN
             ANGLE( EBar,r,  Nu );
             IF RDotV < 0.0 THEN
                 Nu:= TwoPi - Nu;
           END
           ELSE
             Nu:= Undefined;

         { -----  Find Argument of Latitude - Circular Inclined ----- }
         IF TypeOrbit = 'CI' THEN
           BEGIN
             ANGLE( NBar,R,  ArgLat );
             IF R[3] < 0.0 THEN
                 ArgLat:= TwoPi - ArgLat;
           END
           ELSE
             ArgLat:= Undefined;

         { --- Find Longitude of Perigee - Elliptical Equatorial ---- }
         IF ( EBar[4] > Small ) and ( TypeOrbit = 'EE' ) THEN
           BEGIN
             Temp:= EBar[1]/EBar[4];
             IF ABS(Temp) > 1.0 THEN
                 Temp:= SGN(Temp) * 1.0;
             LonPer:= ACOS( Temp );
             IF EBar[2] < 0.0 THEN
                 LonPer:= TwoPi - LonPer;
             IF Incl > HalfPi THEN
                 LonPer:= TwoPi - LonPer;
           END
           ELSE
             LonPer:= Undefined;

         { ------- Find True Longitude - Circular Equatorial -------- }
         IF ( R[4] > Small ) and ( TypeOrbit = 'CE' ) THEN
           BEGIN
             Temp:= R[1]/R[4];
             IF ABS(Temp) > 1.0 THEN
                 Temp:= SGN(Temp) * 1.0;
             TrueLon:= ACOS( Temp );
             IF R[2] < 0.0 THEN
                 TrueLon:= TwoPi - TrueLon;
             IF Incl > HalfPi THEN
                 TrueLon:= TwoPi - TrueLon;
           END
           ELSE
             TrueLon:= Undefined;

         { ------------- Find Mean Anomaly for all orbits ----------- }
         NEWTONNU(Ecc,Nu, E,M );

(*       IF Show IN ['Y','S'] THEN
         BEGIN
           WriteLn( FileOut,'H = ':6,HBar[1]:13:7,HBar[2]:14:7,HBar[3]:14:7,HBar[4]:14:7 );
           WriteLn( FileOut,'N = ':6,NBar[1]:13:7,NBar[2]:14:7,NBar[3]:14:7,NBar[4]:14:7 );
           WriteLn( FileOut,'E = ':6,EBar[1]:13:7,EBar[2]:14:7,EBar[3]:14:7,EBar[4]:14:7 );
           WriteLn( FileOut,'SME=':6,SME:13:7,' ER2/TU2' );
           WriteLn( FileOut,'Anomaly ',E*57.29578:14:7,' TypeOrbit = ',TypeOrbit:3 );
         END;
*)

      END  { IF HBar[4] > 0 the orbit is possible }
      ELSE
      BEGIN
        P    := Undefined;
        A    := Undefined;
        Ecc  := Undefined;
        Incl := Undefined;
        Omega:= Undefined;
        Argp := Undefined;
        Nu   := Undefined;
        M    := Undefined;
        ArgLat  := Undefined;
        TrueLon:= Undefined;
        LonPer := Undefined;
      END;

   END; { PROCEDURE rv2coe }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE DAYS2MDHMS
|
|  This procedure converts the day of the year, days, to the equivalent month
|    day, hour, Minute and second.
|
|  Algorithm     : Set up array for the Number of days per month
|                  Find Leap Year - Use 1900 because 2000 is a leap year
|                  Loop through a Temp value while the value is < the days
|                  Perform INTEGER conversions to the correct day and month
|                  Convert remainder into H M S using type conversions
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Year        - Year                           1900 .. 2100
|    Days        - Julian Day of the year         0.0  .. 366.0
|
|  OutPuts       :
|    Mon         - Month                          1 .. 12
|    Day         - Day                            1 .. 28,29,30,31
|    Hr          - Hour                           0 .. 23
|    MIN         - Minute                         0 .. 59
|    SEC         - Second                         0.0 .. 59.999
|
|  Locals        :
|    DayofYr     - Day of year
|    Temp        - Temporary EXTENDED values
|    IntTemp     - Temporary INTEGER value
|    i           - Index
|    LMonth[12]  - INTEGER Array containing the Number of days per month
|
|  Coupling      :
|    None.
|
 ----------------------------------------------------------------------------- }

PROCEDURE DAYS2MDHMS         ( Year                                  : INTEGER;
                               Days                                  : EXTENDED;
                               VAR Mon,Day,Hr,MIN                    : INTEGER;
                               VAR SEC                               : EXTENDED );
   VAR
     IntTemp, i, DayofYr : INTEGER;
     Temp                : EXTENDED;
     LMonth              : Array[1..12] of INTEGER;
   BEGIN
     { --------------- Set up array of days in month  --------------- }
     FOR i:= 1 to 12 DO
       BEGIN
         CASE i OF
           1,3,5,7,8,10,12 : LMonth[i]:= 31;
           4,6,9,11        : LMonth[i]:= 30;
           2               : LMonth[i]:= 28;
         END;  { Case }
       END;

     DayofYr:= TRUNC(Days );

     { ----------------- Find month and Day of month ---------------- }
     IF ( (Year-1900) MOD 4 ) = 0 THEN
         LMonth[2]:= 29;
     i:= 1;
     IntTemp:= 0;
     WHILE ( DayofYr > IntTemp + LMonth[i] ) and ( i < 12 ) DO
       BEGIN
         IntTemp:= IntTemp + LMonth[i];
         i:= i+1;
       END;
     Mon:= i;
     Day:= DayofYr - IntTemp;

     { ----------------- Find hours minutes and seconds ------------- }
     Temp:= (Days - DayofYr )*24.0;
     Hr  := TRUNC( Temp );
     Temp:= (Temp-Hr) * 60.0;
     MIN := TRUNC( Temp );
     SEC := (Temp-MIN) * 60.0;

        { ---- Check for roundoff errors }
        IF (Sec >= 59.9999) THEN
          begin
            Sec := 0.0;
            Min := Min + 1;
            IF (Min > 59) THEN
              begin
                Min := 0;
                Hr := Hr + 1;
                IF (Hr > 23) THEN
                  begin
                    Hr := 0;
                    Day := Day + 1;
                  END;
              END;
          END;
   END;  { PROCEDURE DAYS2MDHMS }

{ ------------------------------------------------------------------------------
|
|                           PROCEDURE JDAY
|
|  This procedure finds the Julian date given the Year, Month, Day, and Time.
|    The Julian date is defined by each elapsed day since noon, Jan 1, 4713 BC.
|
|  Algorithm     : Calculate the answer in one step for efficiency
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Year        - Year                           1900 .. 2100
|    Mon         - Month                          1 .. 12
|    Day         - Day                            1 .. 28,29,30,31
|    Hr          - Universal Time Hour            0 .. 23
|    MIN         - Universal Time MIN             0 .. 59
|    SEC         - Universal Time SEC             0.0 .. 59.999
|    WhichType   - Julian or Gregorian calender   'J' or 'G'
|
|  Outputs       :
|    JD          - Julian Date                    days from 4713 BC
|
|  Locals        :
|    B           - Var to aid Gregorian dates
|
|  Coupling      :
|    None.
|
|  References    :
|    Vallado       2007, 189, Alg 14, Ex 3-14
|
 ----------------------------------------------------------------------------- }

PROCEDURE JDAY          ( Year,Mon,Day,Hr,MIN                   : INTEGER;
                               SEC                                   : EXTENDED;
                               VAR JD                                : EXTENDED );
   BEGIN
     JD:= 367.0 * Year
          - TRUNC( (7* (Year+TRUNC ( (Mon+9)/12.0) ) ) * 0.25 )
          + TRUNC( 275*Mon / 9.0 )
          + Day + 1721013.5
          + ( (SEC/60.0 + MIN ) / 60.0 + Hr ) / 24.0;  { UT in days }
          { - 0.5*SGN(100.0*Year + Mon - 190002.5) + 0.5; }
   END;  { PROCEDURE JDAY }

{ ------------------------------------------------------------------------------
|
|                           PROCEDURE INVJDAY
|
|  This procedure finds the Year, month, day, hour, Minute and second
|  given the Julian date. TU can be UT1, TDT, TDB, etc.
|
|  Algorithm     : Set up starting values
|                  Find Leap Year - Use 1900 because 2000 is a leap year
|                  Find the elapsed days through the year in a loop
|                  Call routine to find each individual value
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    JD          - Julian Date                    days from 4713 BC
|
|  OutPuts       :
|    Year        - Year                           1900 .. 2100
|    Mon         - Month                          1 .. 12
|    Day         - Day                            1 .. 28,29,30,31
|    Hr          - Hour                           0 .. 23
|    MIN         - Minute                         0 .. 59
|    SEC         - Second                         0.0 .. 59.999
|
|  Locals        :
|    Days        - Day of year plus fractional
|                  portion of a day               days
|    Tu          - Julian Centuries from 0 h
|                  Jan 0, 1900
|    Temp        - Temporary real values
|    LeapYrs     - Number of Leap years from 1900
|
|  Coupling      :
|    DAYS2MDHMS  - Finds Month, day, hour, Minute and second given Days and Year
|
|  References    :
|    Vallado       2007, 208, Alg 22, Ex 3-13
|
 ----------------------------------------------------------------------------- }

PROCEDURE INVJDAY       ( JD                                    : EXTENDED;
                               VAR Year,Mon,Day,Hr,MIN               : INTEGER;
                               VAR SEC                               : EXTENDED );
   VAR
     LeapYrs         : INTEGER;
     Days, Tu, Temp  : EXTENDED;
   BEGIN
     { --------------- Find Year and Days of the year --------------- }
     Temp   := JD-2415019.5;
     Tu     := Temp / 365.25;
     Year   := 1900 + TRUNC( Tu );
     LeapYrs:= TRUNC( ( Year-1901 )*0.25 );
     Days   := Temp - ((Year-1900)*365.0 + LeapYrs ){ + 0.00000000001}; { Nudge by 8.64x10-7 Sec to get even outputs }

     { ------------ Check for case of beginning of a year ----------- }
     IF Days < 1.0 THEN
       BEGIN
         Year   := Year - 1;
         LeapYrs:= TRUNC( ( Year-1901 )*0.25 );
         Days   := Temp - ((Year-1900)*365.0 + LeapYrs );
       END;

     { ----------------- Find remaing data  ------------------------- }
     DAYS2MDHMS( Year,Days, Mon,Day,Hr,MIN,SEC );
     Sec:= Sec{ - 0.00000086400};
   END;  { PROCEDURE InverseJDAY }
{}
{ ------------------------------------------------------------------------------
|
|                           PROCEDURE FINDDAYS
|
|  This procedure finds the fractional days through a year given the year,
|    month, day, hour, Minute and second.
|
|  Algorithm     : Set up array for the Number of days per month
|                  Find Leap Year - Use 1900 because 2000 is a leap year
|                  Check for a leap year
|                  Loop to find the elapsed days in the year
|
|  Author        : David Vallado                  719-573-2600
|
|  Inputs          Description                    Range / Units
|    Year        - Year                           1900 .. 2100
|    Mon         - Month                          1 .. 12
|    Day         - Day                            1 .. 28,29,30,31
|    Hr          - Hour                           0 .. 23
|    MIN         - Minute                         0 .. 59
|    SEC         - Second                         0.0 .. 59.999
|
|  OutPuts       :
|    Days        - Day of year plus fraction of a
|                    day                          days
|
|  Locals        :
|    LMonth      - Length of months of year
|    i           - Index
|
|  Coupling      :
|    None.
|
|  References    :
|    Vallado       2007, 208, Ex 3-12
|
 ----------------------------------------------------------------------------- }

PROCEDURE FINDDAYS           ( Year,Month,Day,Hr,MIN                 : INTEGER;
                               SEC                                   : EXTENDED;
                               VAR Days                              : EXTENDED );
   VAR
     i      : BYTE;
     LMonth : ARRAY[1..12] of INTEGER;
   BEGIN
     FOR i:= 1 to 12 DO
       BEGIN
         CASE i OF
           1,3,5,7,8,10,12 : LMonth[i]:= 31;
           4,6,9,11 : LMonth[i]:= 30;
           2 : LMonth[i]:= 28;
         END;  { Case }
       END;
     IF (Year-1900) MOD 4 = 0 THEN
         LMonth[2]:= 29;

     i   := 1;
     Days:= 0.0;
     WHILE (i < Month) and ( i < 12 ) DO
       BEGIN
         Days:= Days + LMonth[i];
         i:= i + 1;
       END;

     Days:= Days + Day + Hr/24.0 + MIN/1440.0 + SEC/86400.0;
   END;  { PROCEDURE FINDDAYS }


{ ------------------------------------------------------------------------------
|
|                           PROCEDURE NEWTONM
|
|  This procedure performs the Newton Rhapson iteration to find the
|    Eccentric Anomaly given the Mean anomaly.  The True Anomaly is also
|    calculated.
|
|  Author        : David Vallado                  719-573-2600    1 Mar 2001
|
|  Inputs          Description                    Range / Units
|    Ecc         - Eccentricity                   0.0 to
|    M           - Mean Anomaly                   -2Pi to 2Pi rad
|
|  Outputs       :
|    E0          - Eccentric Anomaly              0.0 to 2Pi rad
|    Nu          - True Anomaly                   0.0 to 2Pi rad
|
|  Locals        :
|    E1          - Eccentric Anomaly, next value  rad
|    Sinv        - Sine of Nu
|    Cosv        - Cosine of Nu
|    Ktr         - Index
|    R1r         - CUBIC roots - 1 to 3
|    R1i         - imaginary component
|    R2r         -
|    R2i         -
|    R3r         -
|    R3i         -
|    S           - Variables for parabolic solution
|    W           - Variables for parabolic solution
|
|  Coupling      :
|    ATAN2       - Arc tangent FUNCTION which also resloves quadrants
|    CUBIC       - Solves a CUBIC polynomial
|    POWER       - Raises a base Number to an arbitrary POWER
|    SINH        - Hyperbolic Sine
|    COSH        - Hyperbolic Cosine
|    SGN         - Returns the sign of an argument
|
|  References    :
|    Vallado       2007, 73, Alg 2, Ex 2-1
|
 ----------------------------------------------------------------------------- }

PROCEDURE NEWTONM            ( Ecc,M                                 : EXTENDED;
                               VAR E0,Nu                             : EXTENDED );
   CONST
     NumIter    : INTEGER  =    50;
     Small      : EXTENDED =     0.00000001;       { Small value for tolerances }
   VAR
     Ktr : INTEGER;
     s, w, E1, Sinv, Cosv, R1r, R1i, R2r, R2i, R3r, R3i : EXTENDED;
   BEGIN
     { -------------------------- Hyperbolic  ----------------------- }
     IF (Ecc-1.0) > Small THEN
       BEGIN
         { ------------  Initial Guess ------------- }
         IF Ecc < 1.6 THEN
             IF ((M < 0.0) and (M > -Pi)) or (M > Pi) THEN
                 E0:= M - Ecc
               ELSE
                 E0:= M + Ecc
           ELSE
             IF (Ecc < 3.6) and (ABS(M) > Pi) THEN { just edges }
                 E0:= M - SGN(M)*Ecc
               ELSE
                 E0:= M/(Ecc-1.0); { best over 1.8 in middle }
         Ktr:= 1;
         E1 := E0 + ( ( M - Ecc*SINH(E0) + E0 ) / ( Ecc*COSH(E0) - 1.0 ) );
         WHILE ( ABS(E1-E0) > Small ) and ( Ktr <= NumIter ) DO
           BEGIN
             E0:= E1;
             E1:= E0 + ( ( M - Ecc*SINH(E0) + E0 ) / ( Ecc*COSH(E0) - 1.0 ) );
             INC( Ktr );
           END;
         { ---------  Find True Anomaly  ----------- }
         Sinv:= -( SQRT( Ecc*Ecc-1.0 ) * SINH(E1) ) / ( 1.0 - Ecc*COSH(E1) );
         Cosv:= ( COSH(E1) - Ecc ) / ( 1.0 - Ecc*COSH(E1) );
         Nu  := ATAN2( Sinv,Cosv );
       END
       ELSE
         { ---------------------- Parabolic ------------------------- }
         IF ABS( Ecc-1.0 ) < Small THEN
           BEGIN
             CUBIC( 1.0/3.0,0.0,1.0,-M, R1r,R1i,R2r,R2i,R3r,R3i );
             E0:= R1r;
{             WriteLn( outfile,'roots',r1r:11:7,r1i:11:7,r2r:11:7,r2i:11:7,r3r:11:7,r3i:11:7 );}
{                 S := 0.5 * (HalfPi - ARCTAN( 1.5*M ) );
                 W := ARCTAN( POWER( TAN( S ) ,1.0/3.0 ) );
                 E0:= 2.0*COT(2.0*W);}
             Ktr:= 1;
             Nu := 2.0 * ARCTAN(E0);
           END
           ELSE
             { --------------------- Elliptical --------------------- }
             IF Ecc > Small THEN
               BEGIN
                 { ------------  Initial Guess ------------- }
                 IF ((M < 0.0) and (M > -Pi)) or (M > Pi) THEN
                     E0:= M - Ecc
                   ELSE
                     E0:= M + Ecc;
                 Ktr:= 1;
                 E1 := E0 + ( M - E0 + Ecc*SIN(E0) ) / ( 1.0 - Ecc*COS(E0) );
                 WHILE ( ABS(E1-E0) > Small ) and ( Ktr <= NumIter ) DO
                   BEGIN
                     INC( Ktr );
                     E0:= E1;
                     E1:= E0 + ( M - E0 + Ecc*SIN(E0) ) /
                               ( 1.0 - Ecc*COS(E0) );
                   END;  { While }
                 { ---------  Find True Anomaly  ----------- }
                 Sinv:= ( SQRT( 1.0-Ecc*Ecc ) * SIN(E1) ) /
                        ( 1.0-Ecc*COS(E1) );
                 Cosv:= ( COS(E1)-Ecc ) / ( 1.0 - Ecc*COS(E1) );
                 Nu  := ATAN2( Sinv,Cosv );
               END
               ELSE
               { --------------------- Circular --------------------- }
               BEGIN
                 Ktr:= 0;   Nu:= M;    E0:= M;
               END;
     IF Ktr > NumIter THEN
         WriteLn( 'NewtonRhapson not converged in ',NumIter:3,' Iterations' );
   END;  { PROCEDURE NEWTONM }

BEGIN

END.
