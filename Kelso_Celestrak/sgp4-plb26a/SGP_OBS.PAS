Unit SGP_Obs;
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 02 }
{ Current Revision:  1992 Sep 28 }
{          Version:  1.40 }
{        Copyright:  1992, All Rights Reserved }
{$N+}

INTERFACE
  Uses SGP_Math;

Procedure Calculate_User_PosVel(var geodetic : vector;
                                        time : double;
                         var obs_pos,obs_vel : vector);
Procedure Calculate_LatLonAlt(pos : vector;
                             time : double;
                     var geodetic : vector);
Procedure Calculate_Obs(pos,vel,geodetic : vector;
                                    time : double;
                             var obs_set : vector);
Procedure Calculate_RADec(pos,vel,geodetic : vector;
                                      time : double;
                               var obs_set : vector);

IMPLEMENTATION
  Uses SGP_Intf,SGP_Init,SGP_Time;

Procedure Calculate_User_PosVel(var geodetic : vector;
                                        time : double;
                         var obs_pos,obs_vel : vector);
{ Reference:  The 1992 Astronomical Almanac, page K11. }
  const
    mfactor = twopi*omega_E/secday;
  var
    lat,lon,alt,
    theta,c,s,achcp : double;
  begin
  lat := geodetic[1];
  lon := geodetic[2];
  alt := geodetic[3];
  theta := Modulus(ThetaG_JD(time) + lon,twopi);
  geodetic[4] := theta; {LMST}
  c := 1/Sqrt(1 + f*(f - 2)*Sqr(Sin(lat)));
  s := Sqr(1 - f)*c;
  achcp := (xkmper*c + alt)*Cos(lat);
  obs_pos[1] := achcp*Cos(theta);          {kilometers}
  obs_pos[2] := achcp*Sin(theta);
  obs_pos[3] := (xkmper*s + alt)*Sin(lat);
  obs_vel[1] := -mfactor*obs_pos[2];       {kilometers/second}
  obs_vel[2] :=  mfactor*obs_pos[1];
  obs_vel[3] :=  0;
  Magnitude(obs_pos);
  Magnitude(obs_vel);
  end; {Procedure Calculate_User_PosVel}

Procedure Calculate_LatLonAlt(pos : vector;
                             time : double;
                     var geodetic : vector);
{ Reference:  The 1992 Astronomical Almanac, page K12. }
  var
    lat,lon,alt,
    theta,r,e2,phi,c : double;
  begin
  theta := AcTan(pos[2],pos[1]);
  lon := Modulus(theta - ThetaG_JD(time),twopi);
  r := Sqrt(Sqr(pos[1]) + Sqr(pos[2]));
  e2 := f*(2 - f);
  lat := AcTan(pos[3],r);
  repeat
    phi := lat;
    c := 1/Sqrt(1 - e2*Sqr(Sin(phi)));
    lat := AcTan(pos[3] + xkmper*c*e2*Sin(phi),r);
  until Abs(lat - phi) < 1E-10;
  alt := r/Cos(lat) - xkmper*c;
  geodetic[1] := lat;   {radians}
  geodetic[2] := lon;   {radians}
  geodetic[3] := alt;   {kilometers}
  geodetic[4] := theta; {radians}
  end; {Procedure Calculate_LatLonAlt}

Procedure Calculate_Obs(pos,vel,geodetic : vector;
                                    time : double;
                             var obs_set : vector);
  var
    i                   : integer;
    lat,lon,alt,theta,
    sin_lat,cos_lat,
    sin_theta,cos_theta : double;
    el,azim             : double;
    top_s,top_e,top_z   : double;
    obs_pos,obs_vel,
    range,rgvel         : vector;
  begin
  Calculate_User_PosVel(geodetic,time,obs_pos,obs_vel);
  for i := 1 to 3 do
    begin
    range[i] := pos[i] - obs_pos[i];
    rgvel[i] := vel[i] - obs_vel[i];
    end; {for i}
  Magnitude(range);
  lat := geodetic[1];
  lon := geodetic[2];
  alt := geodetic[3];
  theta := geodetic[4];
  sin_lat := Sin(lat);
  cos_lat := Cos(lat);
  sin_theta := Sin(theta);
  cos_theta := Cos(theta);
  top_s := sin_lat*cos_theta*range[1]
         + sin_lat*sin_theta*range[2]
         - cos_lat*range[3];
  top_e := -sin_theta*range[1]
         + cos_theta*range[2];
  top_z := cos_lat*cos_theta*range[1]
         + cos_lat*sin_theta*range[2]
         + sin_lat*range[3];
  azim := ArcTan(-top_e/top_s); {Azimuth}
  if top_s > 0 then
    azim := azim + pi;
  if azim < 0 then
    azim := azim + twopi;
  el := ArcSin(top_z/range[4]);
  obs_set[1] := azim;                      {Azimuth (radians)}
  obs_set[2] := el;                        {Elevation (radians)}
  obs_set[3] := range[4];                  {Range (kilometers)}
  obs_set[4] := Dot(range,rgvel)/range[4]; {Range Rate (kilometers/second)}
{ Corrections for atmospheric refraction }
{ Reference:  Astronomical Algorithms by Jean Meeus, pp. 101-104 }
{ Note:  Correction is meaningless when apparent elevation is below horizon }
  obs_set[2] := obs_set[2] + Radians((1.02/Tan(Radians(Degrees(el)+10.3/(Degrees(el)+5.11))))/60);
  if obs_set[2] >= 0 then
    visible := true
  else
    begin
    obs_set[2] := el;  {Reset to true elevation}
    visible := false;
    end; {else}
  end; {Procedure Calculate_Obs}

Procedure Calculate_RADec(pos,vel,geodetic : vector;
                                      time : double;
                               var obs_set : vector);
{ Reference:  Methods of Orbit Determination by Pedro Ramon Escobal, pp. 401-402}
  var
    phi,theta,
    sin_theta,cos_theta,
    sin_phi,cos_phi,
    az,el,
    Lxh,Lyh,Lzh,
    Sx,Ex,Zx,
    Sy,Ey,Zy,
    Sz,Ez,ZZ,
    Lx,Ly,Lz,
    cos_delta,
    sin_alpha,cos_alpha : double;
  begin
  Calculate_Obs(pos,vel,geodetic,time,obs_set);
  if visible then
    begin
    az := obs_set[1];
    el := obs_set[2];
    phi   := geodetic[1];
    theta := Modulus(ThetaG_JD(time) + geodetic[2],twopi);
    sin_theta := Sin(theta);
    cos_theta := Cos(theta);
    sin_phi := Sin(phi);
    cos_phi := Cos(phi);
    Lxh := -Cos(az)*Cos(el);
    Lyh :=  Sin(az)*Cos(el);
    Lzh :=  Sin(el);
    Sx := sin_phi*cos_theta;
    Ex := -sin_theta;
    Zx := cos_theta*cos_phi;
    Sy := sin_phi*sin_theta;
    Ey := cos_theta;
    Zy := sin_theta*cos_phi;
    Sz := -cos_phi;
    Ez := 0;
    Zz := sin_phi;
    Lx := Sx*Lxh + Ex*Lyh + Zx*Lzh;
    Ly := Sy*Lxh + Ey*Lyh + Zy*Lzh;
    Lz := Sz*Lxh + Ez*Lyh + Zz*Lzh;
    obs_set[2] := ArcSin(Lz);                 {Declination (radians)}
    cos_delta := Sqrt(1 - Sqr(Lz));
    sin_alpha := Ly/cos_delta;
    cos_alpha := Lx/cos_delta;
    obs_set[1] := AcTan(sin_alpha,cos_alpha); {Right Ascension (radians)}
    obs_set[1] := Modulus(obs_set[1],twopi);
    end; {if}
  end; {Procedure Calculate_RADec}

end.
