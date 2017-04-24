Unit SGP_Out;
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Aug 24 }
{ Current Revision:  1999 Nov 27 }
{          Version:  1.60 }
{        Copyright:  1992-1999, All Rights Reserved }
{$N+}

INTERFACE
  Uses SGP_Math;

const
  day_date  : boolean = true;
  full_time : boolean = true;
  N_E_W_S   : boolean = false;
  D_M_S     : boolean = false;
  time_res  : byte = 2;
  angle_res : byte = 4;
  dist_res  : byte = 3;

var
  fout : text;

Procedure Output_Time(time : double);
Procedure Output_ECI(time : double;
                  pos,vel : vector);
Procedure Output_Angle(angle : double;
                   width,dec : byte;
                     degrees : boolean);
Procedure Output_LatLonAlt(time : double;
                       geodetic : vector);
Procedure Output_Obs(time : double;
                      obs : vector);
Procedure Output_RADec(time : double;
                        obs : vector);

IMPLEMENTATION
  Uses Support,SGP_Init,SGP_Time,Solar;

Procedure Output_Time(time : double);
  begin
  if day_date then
    begin
  { Convert to local time }
    JD_from_UTC(time);
    Write(fout,Calendar_Date(time,time_res),'  ');
    Write(fout,Time_of_Day(time,full_time,time_res));
    end {if}
  else
    Write(fout,time:16:8);
  end; {Procedure Output_Time}

Procedure Output_ECI(time : double;
                  pos,vel : vector);
  var
    i : byte;
  begin
  Output_Time(time);
  for i := 1 to 3 do
    Write(fout,pos[i]:11:3);
  for i := 1 to 3 do
    Write(fout,vel[i]:11:6);
  if show_vis then
    if eclipsed then
      Writeln(fout,' ECL')
    else
      Writeln(fout,'    ')
  else
    Writeln(fout);
  end; {Procedure Output_ECI}

Procedure Output_Angle(angle : double;
                   width,dec : byte;
                     degrees : boolean);
  var
    asign       : shortint;
    deg,min,sec : longint;
    tmp         : double;
  begin
  if dec > 4 then
    dec := 4;
  if D_M_S then
    begin
    asign := Sign(angle);
    angle := Abs(angle);
    case dec of
      0 : begin
          angle := Modulus(angle + 0.5,360);
          deg := asign*Trunc(angle);
          end; {0}
      1 : begin
          angle := Modulus(angle + 1/12,360);
          deg := Trunc(angle);
          min := Trunc((angle - deg) * 6)*10;
          deg := asign*deg;
          end; {1}
      2 : begin
          angle := Modulus(angle + 1/120,360);
          deg := Trunc(angle);
          min := Trunc((angle - deg) * 60);
          deg := asign*deg;
          end; {2}
      3 : begin
          angle := Modulus(angle + 1/720,360);
          deg := Trunc(angle);
          tmp := (angle - deg) * 60;
          min := Trunc(tmp);
          sec := Trunc(Frac(tmp)*6)*10;
          deg := asign*deg;
          end; {3}
      4 : begin
          angle := Modulus(angle + 1/7200,360);
          deg := Trunc(angle);
          tmp := (angle - deg) * 60;
          min := Trunc(tmp);
          sec := Trunc(Frac(tmp)*60);
          deg := asign*deg;
          end; {4}
      end; {case}
    if degrees then
      case dec of
          0 : Write(fout,deg:width,'º');
        1,2 : Write(fout,deg:width,'º',TwoDigit(min),'''');
        3,4 : Write(fout,deg:width,'º',TwoDigit(min),'''',TwoDigit(sec),'"');
        end {case}
    else
      case dec of
          0 : Write(fout,deg:width,'h');
        1,2 : Write(fout,deg:width,'h',TwoDigit(min),'m');
        3,4 : Write(fout,deg:width,'h',TwoDigit(min),'m',TwoDigit(sec),'s');
        end {case}
    end {if}
  else
    if dec = 0 then
      Write(fout,angle:width:0)
    else
      Write(fout,angle:width+dec+1:dec);
  end; {Procedure Output_Angle}

Procedure Output_LatLonAlt(time : double;
                       geodetic : vector);
  begin
  Output_Time(time);
  if N_E_W_S then
    begin
    Output_Angle(Abs(Degrees(geodetic[1])),5,angle_res,true);
    if geodetic[1] >= 0 then
      Write(fout,' N')
    else
      Write(fout,' S');
    if geodetic[2] > pi then
      geodetic[2] := geodetic[2] - twopi;
    Output_Angle(Abs(Degrees(geodetic[2])),6,angle_res,true);
    if geodetic[2] >= 0 then
      Write(fout,' E')
    else
      Write(fout,' W');
    end {if}
  else
    begin
    Output_Angle(Degrees(geodetic[1]),5,angle_res,true);
    Output_Angle(Degrees(geodetic[2]),6,angle_res,true);
    end; {if}
  Write(fout,geodetic[3]:11:3);
  if show_vis then
    if eclipsed then
      Writeln(fout,' ECL')
    else
      Writeln(fout,'    ')
  else
    Writeln(fout);
  end; {Procedure Output_LatLonAlt}

Procedure Output_Obs(time : double;
                      obs : vector);
  const
    first : boolean = false;
  begin
  if not visible then
    begin
    if first then Writeln(fout);
    first := false;
    Exit;
    end; {if}
  first := true;
  Output_Time(time);
  Write(fout,Copy(obs_name,1,3):5);
  Writeln(fout,Degrees(obs[1]):angle_res+6:angle_res,
               Degrees(obs[2]):angle_res+5:angle_res,
               obs[3]:dist_res+8:dist_res,
               obs[4]:dist_res+8:dist_res+3);
  end; {Procedure Output_Obs}

Procedure Output_RADec(time : double;
                        obs : vector);
  const
    first : boolean = false;
  begin
  if not visible then
    begin
    if first then Writeln(fout);
    first := false;
    Exit;
    end; {if}
  first := true;
  Output_Time(time);
  Write(fout,Copy(obs_name,1,3):5);
  Output_Angle(Degrees(obs[1])/15,4,angle_res,false);
  Output_Angle(Degrees(obs[2]),5,angle_res,true);
  Writeln(fout);
  end; {Procedure Output_RADec}

end.
