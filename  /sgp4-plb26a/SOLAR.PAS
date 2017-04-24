Unit Solar;
{           Author:  Dr TS Kelso }
{ Original Version:  1990 Jul 29 }
{ Current Revision:  1999 Nov 27 }
{          Version:  1.30 }
{        Copyright:  1990-1999, All Rights Reserved }
{$N+}

INTERFACE
  Uses SGP_Math;

const
  eclipsed : boolean = false;
  show_vis : boolean = false;

var
  civil,
  nautical,
  astronomical : double;  {Twilight elevations}

Procedure Calculate_Solar_Position(time : double;
                       var solar_vector : vector);
Function Sat_Eclipsed(sat,sol : vector;
                    var depth : double) : boolean;

IMPLEMENTATION
  Uses SGP_Intf,SGP_Time;

const
  sr = 696000.0;       {Solar radius - kilometers (IAU 76)}
  AU = 1.49597870E8;   {Astronomical unit - kilometers (IAU 76)}

Procedure Calculate_Solar_Position(time : double;
                       var solar_vector : vector);
  var
    mjd,year,T,M,L,e,C,O,Lsa,nu,R,eps : double;
    ob                                : vector;
  begin
  mjd := time - 2415020.0;
  year := 1900 + mjd/365.25;
  T := (mjd + Delta_ET(year)/secday)/36525.0;
  M := Radians(Modulus(358.47583 + Modulus(35999.04975*T,360.0)
     - (0.000150 + 0.0000033*T)*Sqr(T),360.0));
  L := Radians(Modulus(279.69668 + Modulus(36000.76892*T,360.0)
     + 0.0003025*Sqr(T),360.0));
  e := 0.01675104 - (0.0000418 + 0.000000126*T)*T;
  C := Radians((1.919460 - (0.004789 + 0.000014*T)*T)*Sin(M)
     + (0.020094 - 0.000100*T)*Sin(2*M) + 0.000293*Sin(3*M));
  O := Radians(Modulus(259.18 - 1934.142*T,360.0));
  Lsa := Modulus(L + C - Radians(0.00569 - 0.00479*Sin(O)),twopi);
  nu := Modulus(M + C,twopi);
  R := 1.0000002*(1 - Sqr(e))/(1 + e*Cos(nu));
  eps := Radians(23.452294 - (0.0130125 + (0.00000164 - 0.000000503*T)*T)*T
       + 0.00256*Cos(O));
  R := AU*R;
  solar_vector[1] := R*Cos(Lsa);
  solar_vector[2] := R*Sin(Lsa)*Cos(eps);
  solar_vector[3] := R*Sin(Lsa)*Sin(eps);
  solar_vector[4] := R;
  end; {Procedure Calculate_Solar_Position}

Function Sat_Eclipsed(sat,sol : vector;
                    var depth : double) : boolean;
  var
    sd_sun,sd_earth : double;
    delta           : double;
    rho,earth       : vector;
  begin
{ Determine partial eclipse }
  sd_earth := ArcSin(xkmper/sat[4]);
  Vec_Sub(sol,sat,rho);
  sd_sun := ArcSin(sr/rho[4]);
  Scalar_Multiply(-1,sat,earth);
  delta := Angle(sol,earth);
  depth := sd_earth - sd_sun - delta;
  if sd_earth < sd_sun then
    Sat_Eclipsed := false
  else
    if depth >= 0 then
      Sat_Eclipsed := true
    else
      Sat_Eclipsed := false;
  end; {Function Sat_Eclipsed}

  begin
  civil        := Radians(-6);
  nautical     := Radians(-12);
  astronomical := Radians(-18);

end.
