Unit SGP_Math;
{           Author:  Dr TS Kelso }
{ Original Version:  1991 Oct 30 }
{ Current Revision:  1998 Mar 17 }
{          Version:  3.00 }
{        Copyright:  1991-1998, All Rights Reserved }
{$N+}

interface

type
  vector = array [1..4] of double;

const
  twopi = 2 * pi;
  zero : vector = (0,0,0,0);

Function Sign(arg : double) : shortint;
Function Cube(arg : double) : double;
Function Power(arg,pwr : double) : double;
Function Radians(arg : double) : double;
Function Degrees(arg : double) : double;
Function Tan(arg : double) : double;
Function ArcSin(arg : double) : double;
Function ArcCos(arg : double) : double;
Function Modulus(arg1,arg2 : double) : double;
Function Fmod2p(arg : double) : double;
Function AcTan(sinx,cosx : double) : double;
Procedure Magnitude(var v : vector);
Procedure Vec_Add(v1,v2 : vector; var v3 : vector);
Procedure Vec_Sub(v1,v2 : vector; var v3 : vector);
Procedure Scalar_Multiply(k : double; v1 : vector; var v2 : vector);
Function Dot(v1,v2 : vector) : double;
Function Angle(var v1,v2 : vector) : double;
Procedure Cross(v1,v2 : vector; var v3 : vector);
Procedure Normalize(var v : vector);

implementation

uses
  Dialogs,
  Support;

type
  matrix = array [1..3,1..3] of double;

Function Sign(arg : double) : shortint;
  begin
  if arg > 0 then
    Sign := 1
  else if arg < 0 then
    Sign := -1
  else
    Sign := 0;
  end; {Function Sign}

Function Cube(arg : double) : double;
  begin
  Cube := arg*Sqr(arg);
  end; {Function Cube}

Function Power(arg,pwr : double) : double;
  begin
  if arg > 0 then
    Power := Exp(pwr*Ln(arg))
  else
    Writeln(output,'Invalid argument in Function Power!');
  end; {Function Power}

Function Radians(arg : double) : double;
  begin
  Radians := arg*pi/180;
  end; {Function Radians}

Function Degrees(arg : double) : double;
  begin
  Degrees := arg*180/pi;
  end; {Function Degrees}

Function Tan(arg : double) : double;
  begin
  Tan := Sin(arg)/Cos(arg);
  end; {Function Tan}

Function ArcSin(arg : double) : double;
  begin
  if Abs(arg) >= 1 then
    ArcSin := Sign(arg)*pi/2
  else
    ArcSin := ArcTan(arg/Sqrt(1-Sqr(arg)));
  end; {Function ArcSin}

Function ArcCos(arg : double) : double;
  begin
  ArcCos := pi/2 - ArcSin(arg);
  end; {Function ArcCos}

Function Modulus(arg1,arg2 : double) : double;
  var
    modu : double;
  begin
  modu := arg1 - Trunc(arg1/arg2) * arg2;
  if modu >= 0 then
    Modulus := modu
  else
    Modulus := modu + arg2;
  end; {Function Modulus}

Function Fmod2p(arg : double) : double;
  begin
  Fmod2p := Modulus(arg,twopi);
  end; {Function Fmod2p}

Function AcTan(sinx,cosx : double) : double;
  begin
  if cosx = 0 then
    if sinx > 0 then
      Actan := pi/2
    else
      Actan := 3*pi/2
  else if cosx > 0 then
    Actan := ArcTan(sinx/cosx)
  else
    Actan := pi + ArcTan(sinx/cosx);
  end; {Function Actan}

Procedure Magnitude(var v : vector);
  begin
  v[4] := Sqrt(Sqr(v[1]) + Sqr(v[2]) + Sqr(v[3]));
  end; {Procedure Magnitude}

Procedure Vec_Add(v1,v2 : vector; var v3 : vector);
{ Adds vectors v1 and v2 together to produce v3 }
  var
    i : byte;
  begin
  for i := 1 to 3 do
    v3[i] := v1[i] + v2[i];
  Magnitude(v3);
  end; {Procedure Vec_Add}

Procedure Vec_Sub(v1,v2 : vector; var v3 : vector);
{ Subtracts vector v2 from v1 to produce v3 }
  var
    i : byte;
  begin
  for i := 1 to 3 do
    v3[i] := v1[i] - v2[i];
  Magnitude(v3);
  end; {Procedure Vec_Sub}

Procedure Scalar_Multiply(k : double;
                         v1 : vector;
                     var v2 : vector);
{ Multiplies the vector v1 by the scalar k to produce the vector v2 }
  var
    i : byte;
  begin
  for i := 1 to 3 do
    v2[i] := k*v1[i];
  v2[4] := Abs(k)*v1[4];
  end; {Procedure Scalar_Multiply}

Function Dot(v1,v2 : vector) : double;
  begin
  Dot := v1[1]*v2[1] + v1[2]*v2[2] + v1[3]*v2[3];
  end;  {Function Dot}

Function Angle(var v1,v2 : vector) : double;
{ Calculates the angle between vectors v1 and v2 }
  begin
  Magnitude(v1);
  Magnitude(v2);
  Angle := ArcCos(Dot(v1,v2)/(v1[4]*v2[4]));
  end; {Function Angle}

Procedure Cross(v1,v2 : vector; var v3 : vector);
  begin
  v3[1] := v1[2]*v2[3] - v1[3]*v2[2];
  v3[2] := v1[3]*v2[1] - v1[1]*v2[3];
  v3[3] := v1[1]*v2[2] - v1[2]*v2[1];
  Magnitude(v3);
  end; {Procedure Cross}

Procedure Normalize(var v : vector);
  var
    i : byte;
  begin
  for i := 1 to 3 do
    v[i] := v[i]/v[4];
  end; {Procedure Normalize}

end.
