Unit SGP_Intf;
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Sep 03 }
{ Current Revision:  1992 Sep 13 }
{          Version:  1.02 }
{        Copyright:  1992, All Rights Reserved }
{$N+}

INTERFACE

const
  ae       = 1;
  tothrd   = 2/3;
  xkmper   = 6378.135;        {Earth equatorial radius - kilometers (WGS '72)}
  f        = 1/298.26;        {Earth flattening (WGS '72)}
  ge       = 398600.8;        {Earth gravitational constant (WGS '72)}
  J2       = 1.0826158E-3;    {J2 harmonic (WGS '72)}
  J3       = -2.53881E-6;     {J3 harmonic (WGS '72)}
  J4       = -1.65597E-6;     {J4 harmonic (WGS '72)}
  ck2      = J2/2;
  ck4      = -3*J4/8;
  xj3      = J3;
  qo       = ae + 120/xkmper;
  s        = ae + 78/xkmper;
  e6a      = 1E-6;
  dpinit   = 1;               {Deep-space initialization code}
  dpsec    = 2;               {Deep-space secular code}
  dpper    = 3;               {Deep-space periodic code}

var
  iflag,ideep                : integer;
  xmo,xnodeo,omegao,eo,xincl,
  xno,xndt2o,xndd6o,bstar,
  julian_epoch,xke           : double;

IMPLEMENTATION

end.
