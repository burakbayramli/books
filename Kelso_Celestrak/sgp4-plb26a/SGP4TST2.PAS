Program SGP4_Test;
{$N+}
  Uses CRT,
       SGP_Init,SGP_Conv,
       SGP_Math,SGP_Time,
       SGP4SDP4;

var
  satnumber,interval,i    : integer;
  delta,time,tsince,k1,k2 : double;
  pos,vel                 : vector;

BEGIN

  sat_data[1,1] := '1 88888U          80275.98708465  .00073094  13844-3  66816-4 0    8 ';
  sat_data[1,2] := '2 88888  72.8435 115.9689 0086731  52.6988 110.5714 16.05824518  105 ';
  sat_data[2,1] := '1 11801U          80230.29629788  .01431103  00000-0  14311-1        ';
  sat_data[2,2] := '2 11801  46.7916 230.4354 7318036  47.4722  10.4117  2.28537848      ';

  delta := 360;
  for satnumber := 1 to 2 do
    begin
    ClrScr;
    Writeln(sat_data[satnumber,1]);
    Writeln(sat_data[satnumber,2]);
    Writeln;
    Writeln('     TSINCE              X                Y                Z');
    GotoXY(1,12);
    Writeln('                      XDOT             YDOT             ZDOT');
    Convert_Satellite_Data(satnumber);
    time := Julian_Date_of_Epoch(epoch);
    tsince := 0;
    for interval := 0 to 4 do
      begin
      SGP(time,pos,vel);
      Convert_Sat_State(pos,vel);
      GotoXY(1,6+interval);
      Writeln(tsince:16:8,pos[1]:17:8,pos[2]:17:8,pos[3]:17:8);
      GotoXY(1,14+interval);
      Writeln('                ',vel[1]:17:8,vel[2]:17:8,vel[3]:17:8);
      tsince := tsince + delta;
      time := time + delta/xmnpda;
      end; {for int}
    repeat until keypressed;
    end; {for satnumber}

END.
