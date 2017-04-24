Unit SGP_Conv;
{           Author:  Dr TS Kelso }
{ Original Version:  1991 Oct 30}
{ Current Revision:  1992 Sep 03}
{          Version:  1.00 }
{        Copyright:  1992, All Rights Reserved }
{$N+}

INTERFACE
  Uses SGP_Math;

Procedure Convert_Satellite_Data(arg : integer);
Procedure Convert_Sat_State(var pos,vel : vector);

IMPLEMENTATION
  Uses Support,
       SGP_Intf,
       SGP_Init,SGP_Time;

Procedure Convert_Satellite_Data(arg : integer);
  var
    iexp,ibexp                 : integer;
    a1,ao,del1,delo,xnodp,temp : double;
    abuf                       : two_line;
  begin
  abuf := sat_data[arg];
{* Decode Card 1 *}
  catnr    := Copy(abuf[1],3,5);
  epoch    := Real_Value(abuf[1],19,14);
  julian_epoch := Julian_Date_of_Epoch(epoch);
  xndt2o   := Real_Value(abuf[1],34,10);
  xndd6o   := Real_Value(abuf[1],45,6)*1E-5;
  iexp     := Integer_Value(abuf[1],51,2);
  bstar    := Real_Value(abuf[1],54,6)*1E-5;
  ibexp    := Integer_Value(abuf[1],60,2);
  elset    := ThreeDigit(Integer_Value(abuf[1],66,3));
{* Decode Card 2 *}
  xincl    := Real_Value(abuf[2],9,8);
  xnodeo   := Real_Value(abuf[2],18,8);
  eo       := Real_Value(abuf[2],27,7)*1E-7;
  omegao   := Real_Value(abuf[2],35,8);
  xmo      := Real_Value(abuf[2],44,8);
  xno      := Real_Value(abuf[2],53,11);
{ period   := 1/xno; }
{* Convert to proper units *}
  xndd6o   := xndd6o*Power(10.0,iexp);
  bstar    := bstar*Power(10.0,ibexp)/ae;
  xnodeo   := Radians(xnodeo);
  omegao   := Radians(omegao);
  xmo      := Radians(xmo);
  xincl    := Radians(xincl);
  xno      := xno*twopi/xmnpda;
  xndt2o   := xndt2o*twopi/Sqr(xmnpda);
  xndd6o   := xndd6o*twopi/Cube(xmnpda);
{* Determine whether Deep-Space Model is needed *}
  a1 := Power(xke/xno,tothrd);
  temp := 1.5*ck2*(3*Sqr(Cos(xincl))-1)/Power(1 - eo*eo,1.5);
  del1 := temp/(a1*a1);
  ao := a1*(1 - del1*(0.5*tothrd + del1*(1 + 134/81*del1)));
  delo := temp/(ao*ao);
  xnodp := xno/(1 + delo);
  if (twopi/xnodp >= 225) then
    ideep := 1
  else
    ideep := 0;
  iflag := 1;
  end; {Procedure Convert_Satellite_Data}

Procedure Convert_Sat_State(var pos,vel : vector);
  var
    i : byte;
  begin
  for i := 1 to 3 do
    begin
    pos[i] := pos[i]*xkmper;    {kilometers}
    vel[i] := vel[i]*xkmper/60; {kilometers/second}
    end; {for i}
  Magnitude(pos);
  Magnitude(vel);
  end; {Procedure Convert_Sat_State}

end.
