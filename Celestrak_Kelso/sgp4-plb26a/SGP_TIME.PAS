Unit SGP_Time;
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 02 }
{ Current Revision:  2000 Jan 22 }
{ Modified for Y2K:  1999 Mar 07 }
{          Version:  2.05 }
{        Copyright:  1992-1999, All Rights Reserved }
{ Version 1.50 added Y2K support. Due to limitations in the current
  format of the NORAD two-line element sets, however, only dates
  through 2056 December 31/2359 UTC are valid. }
{ Version 1.60 modifies Calendar_Date to ensure date matches time
  resolution and modifies Time_of_Day to make it more robust. }
{ Version 2.00 adds Julian_Date, Date_Time, and Check_Date to support
  checking for valid date/times, permitting the use of Time_to_UTC and
  Time_from_UTC for UTC/local time conversions. }
{ Version 2.05 modifies UTC_offset to allow non-integer offsets. }
{$N+}

INTERFACE
  Uses SGP_Math,Support;

type
  clock_time = string[12];
  date       = string[11];

const
  xmnpda   =  1440.0;        {Minutes per day}
  secday   = 86400.0;        {Seconds per day}
  omega_E  = 1.00273790934;  {Earth rotations per sidereal day (non-constant)}
  omega_ER = omega_E*twopi;  {Earth rotation, radians per sidereal day}

var
  ds50 : double;

Function Julian_Date_of_Year(year : double) : double;
Function Julian_Date_of_Epoch(epoch : double) : double; {Modified for Y2K}
Function Epoch_Time(jd : double) : double;
Function DOY(yr,mo,dy : word) : word;
Function Fraction_of_Day(hr,mi,se,hu : word) : double;
Function Calendar_Date(jd : double;
                      res : byte) : date;
Function Time_of_Day(jd : double;
                   full : boolean;
                    res : byte) : clock_time;
Function Julian_Date(ts : time_set) : double;
Procedure Date_Time(jd : double;
                var ts : time_set);
Procedure Check_Date(var ts : time_set);
Procedure Time_to_UTC(var ts : time_set);
Procedure JD_to_UTC(var jt : double);
Procedure Time_from_UTC(var ts : time_set);
Procedure JD_from_UTC(var jt : double);
Function ThetaG(epoch : double) : double;               {Modified for Y2K}
Function ThetaG_JD(jd : double) : double;
Function Delta_ET(year : double) : double;

IMPLEMENTATION
  Uses MinMax,SGP_Init;

Function Julian_Date_of_Year(year : double) : double;
  { Astronomical Formulae for Calculators, Jean Meeus, pages 23-25 }
  { Calculate Julian Date of 0.0 Jan year }
  var
    A,B   : longint;
  begin
  year := year - 1;
  A := Trunc(year/100);
  B := 2 - A + Trunc(A/4);
  Julian_Date_of_Year := Trunc(365.25 * year)
                       + Trunc(30.6001 * 14)
                       + 1720994.5 + B;
  end; {Function Julian_Date_of_Year}

Function Julian_Date_of_Epoch(epoch : double) : double;
  var
    year,day : double;
  begin
{ Modification to support Y2K }
{ Valid 1957 through 2056 }
  year := Int(epoch*1E-3);
  if year < 57 then
    year := year + 2000
  else
    year := year + 1900;
{ End modification }
  day  := Frac(epoch*1E-3)*1E3;
  Julian_Date_of_Epoch := Julian_Date_of_Year(year) + day;
  end; {Function Julian_Date_of_Epoch}

Function Epoch_Time(jd : double) : double;
  var
    year,mo,dy    : word;
    yr,time,epoch : double;
    edate         : date;
  begin
  edate := Calendar_Date(jd,3);
  year := Integer_Value(edate,1,4);
  yr := Integer_Value(edate,3,2);
  mo := Pos(Copy(edate,6,3),'  JanFebMarAprMayJunJulAugSepOctNovDec') div 3;
  dy := Integer_Value(edate,10,2);
  time := Frac(jd + 0.5);
  Epoch_Time := yr*1000 + DOY(year,mo,dy) + time;
  end; {Function Epoch_Time}

Function DOY(yr,mo,dy : word) : word;
  const
    days : array [1..12] of word = (31,28,31,30,31,30,31,31,30,31,30,31);
  var
    i,day : word;
  begin
  day := 0;
  for i := 1 to mo-1 do
    day := day + days[i];
  day := day + dy;
  if ((yr mod 4) = 0) and
     (((yr mod 100) <> 0) or ((yr mod 400) = 0)) and
     (mo > 2) then
    day := day + 1;
  DOY := day;
  end; {Function DOY}

Function Fraction_of_Day(hr,mi,se,hu : word) : double;
  begin
  Fraction_of_Day := (hr + (mi + (se + hu/100)/60)/60)/24;
  end; {Function Fraction_of_Day}

Function Calendar_Date(jd : double;
                      res : byte) : date;
  { Astronomical Formulae for Calculators, Jean Meeus, pages 26-27 }
  var
    Z,month           : longint;
    A,B,C,D,E,F,alpha : double;
    day,year          : double;
    factor            : double;
    syear             : string[4];
    cdate             : date;
  begin
  factor := 0.5/secday/Power(10,res);
  F := Frac(jd + 0.5);
  if (F + factor >= 1.0) then
    begin
    jd := jd + factor;
    F  := 0.0;
    end; {if}
  Z := Trunc(jd + 0.5);
  if Z < 2299161 then
    A := Z
  else
    begin
    alpha := Int((Z - 1867216.25)/36524.25);
    A := Z + 1 + alpha - Int(alpha/4);
    end; {else}
  B := A + 1524;
  C := Int((B - 122.1)/365.25);
  D := Int(365.25 * C);
  E := Int((B - D)/30.6001);
  day := B - D - Int(30.6001 * E) + F;
  if E < 13.5 then
    month := Round(E - 1)
  else
    month := Round(E - 13);
  if month > 2.5 then
    year := C - 4716
  else
    year := C - 4715;
  Str(year:4:0,syear);
  cdate := syear + ' '
         + Copy('  JanFebMarAprMayJunJulAugSepOctNovDec',3*month,3) + ' '
         + TwoDigit(Trunc(day));
  Calendar_Date := cdate;
  end; {Function Calendar_Date}

Function Time_of_Day(jd : double;
                   full : boolean;
                    res : byte) : clock_time;
  var
    hr,mn   : longint;
    factor,
    time,sc : double;
    ctime   : string;
  begin
  res := IMin(IMax(0,res),3);
  factor := Power(10,res);
  time := Frac(jd - 0.5)*secday;
  time := Round(time*factor)/factor;
  hr := Trunc(time/3600.0);
  time := time - 3600.0*hr;
  if hr = 24 then hr := 0;
  mn := Trunc(time/60.0);
  sc := time - 60.0*mn;
  time := 1000000 + 10000 * hr + 100 * mn + sc;
  Str(time:(7+res):res,ctime);
  Delete(ctime,1,1);
  if full then
    begin
    Insert(':',ctime,5);
    Insert(':',ctime,3);
    end; {if}
  Time_of_Day := ctime;
  end; {Function Time_of_Day}

Function Julian_Date(ts : time_set) : double;
  begin
  with ts do
    Julian_Date := Julian_Date_of_Year(yr) + DOY(yr,mo,dy)
                 + Fraction_of_Day(hr,mi,se,hu);
  end; {Function Julian_Date}

Procedure Date_Time(jd : double;
                var ts : time_set);
  var
    da : date;
    tm : clock_time;
  begin
  da := Calendar_Date(jd,3);
  tm := Time_of_Day(jd,false,3);
  with ts do
    begin
    yr := Integer_Value(da,1,4);
    mo := Pos(Copy(da,6,3),'  JanFebMarAprMayJunJulAugSepOctNovDec') div 3;
    dy := Integer_Value(da,10,2);
    hr := Integer_Value(tm,1,2);
    mi := Integer_Value(tm,3,2);
    se := Integer_Value(tm,5,2);
    hu := Integer_Value(tm,8,2);
    end; {with ts}
  end; {Procedure Date_Time}

Procedure Check_Date(var ts : time_set);
  var
    jt : double;
  begin
  jt := Julian_Date(ts);
  Date_Time(jt,ts);
  end; {Procedure Check_Date}

Procedure Time_to_UTC(var ts : time_set);
  var
    jt : double;
  begin
  jt := Julian_Date(ts);
  jt := jt - UTC_offset/24.0;
  if DST then
    jt:= jt - 1.0/24.0;
  Date_Time(jt,ts);
  end; {Procedure Time_to_UTC}

Procedure JD_to_UTC(var jt : double);
  begin
  jt := jt - UTC_offset/24.0;
  if DST then
    jt:= jt - 1.0/24.0;
  end; {Procedure JD_to_UTC}

Procedure Time_from_UTC(var ts : time_set);
  var
    jt : double;
  begin
  jt := Julian_Date(ts);
  jt := jt + UTC_offset/24.0;
  if DST then
    jt:= jt + 1.0/24.0;
  Date_Time(jt,ts);
  end; {Procedure Time_from_UTC}

Procedure JD_from_UTC(var jt : double);
  begin
  jt := jt + UTC_offset/24.0;
  if DST then
    jt:= jt + 1.0/24.0;
  end; {Procedure JD_from_UTC}

Function ThetaG(epoch : double) : double;
{ Reference:  The 1992 Astronomical Almanac, page B6. }
  var
    year,day,UT,jd,TU,GMST : double;
  begin
{ Modification to support Y2K }
{ Valid 1957 through 2056 }
  year := Int(epoch*1E-3);
  if year < 57 then
    year := year + 2000
  else
    year := year + 1900;
{ End modification }
  day  := Frac(epoch*1E-3)*1E3;
  UT   := Frac(day);
  day  := Int(day);
  jd   := Julian_Date_of_Year(year) + day;
  TU   := (jd - 2451545.0)/36525;
  GMST := 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2E-6));
  GMST := Modulus(GMST + secday*omega_E*UT,secday);
  ThetaG := twopi * GMST/secday;
  ds50 := jd - 2433281.5 + UT;
{ ThetaG := Modulus(6.3003880987*ds50 + 1.72944494,twopi); }
  end; {Function ThetaG}

Function ThetaG_JD(jd : double) : double;
{ Reference:  The 1992 Astronomical Almanac, page B6. }
  var
    UT,TU,GMST : double;
  begin
  UT   := Frac(jd + 0.5);
  jd   := jd - UT;
  TU   := (jd - 2451545.0)/36525;
  GMST := 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2E-6));
  GMST := Modulus(GMST + secday*omega_E*UT,secday);
  ThetaG_JD := twopi * GMST/secday;
  end; {Function ThetaG_JD}

Function Delta_ET(year : double) : double;
  { Values determined using data from 1950-1991 in the 1990 Astronomical
    Almanac.  See DELTA_ET.WQ1 for details. }
  begin
  Delta_ET := 26.465 + 0.747622*(year - 1950)
             + 1.886913*Sin(twopi*(year - 1975)/33);
  end; {Function Delta_ET}

end.
