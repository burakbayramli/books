Unit SGP_In;  (** This unit contains machine-specific code **)
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 25 }
{ Current Revision:  1999 Nov 27 }
{          Version:  2.10 }
{        Copyright:  1992-1999, All Rights Reserved }
{$N+}

INTERFACE
  Uses SGP_Math,SGP_Init,Support;

const
  data_type : byte = 3;

var
  fsat,fobs : text;

Procedure Select_Time(message : string;
                    xpos,ypos : byte;
                  var default : time_set;
                    precision : byte);
Procedure Select_Time_Interval(message : string;
                             xpos,ypos : byte;
                           var default : time_set;
                             precision : byte);
Function Checksum_Good(line : line_data) : boolean;
Function Good_Elements(line : two_line) : boolean;
Procedure Input_Satellite(index : word);
Function Input_Satellite_Data(fn : string) : word;
Procedure Select_Satellites(title : string;
                          x,y,w,h : byte;
                           number : word);
Procedure Input_Observer(var geodetic : vector);

IMPLEMENTATION
  Uses CRT,DOS,MinMax;

var
  i : byte;

Procedure Echo_Time(time : time_set;
          item,precision : byte);
  begin
  GotoXY(2,1);
  ClrEOL;
  if item = 1 then ReverseVideo;
  Write(time.yr);  NormalVideo;
  Write(' ');
  if item = 2 then ReverseVideo;
  Write(Copy('  JanFebMarAprMayJunJulAugSepOctNovDec',3*time.mo,3));
  NormalVideo;
  Write(' ');
  if item = 3 then ReverseVideo;
  Write(TwoDigit(time.dy));
  NormalVideo;
  if precision > 3 then
    begin
    Write('  ');
    if item = 4 then ReverseVideo;
    Write(TwoDigit(time.hr));
    NormalVideo;
    end; {if}
  if precision > 4 then
    begin
    Write(':');
    if item = 5 then ReverseVideo;
    Write(TwoDigit(time.mi));
    NormalVideo;
    end; {if}
  if precision > 5 then
    begin
    Write(':');
    if item = 6 then ReverseVideo;
    Write(TwoDigit(time.se));
    NormalVideo;
    end; {if}
  if precision > 6 then
    begin
    Write('.');
    if item = 7 then ReverseVideo;
    Write(TwoDigit(time.hu));
    NormalVideo;
    end; {if}
  end; {Procedure Echo_Time}

Procedure Update_Field(choice : char;
                    var value : word;
                    llim,ulim : word;
                      var pos : byte;
                    lpos,upos : byte);
  begin
  case choice of
      Up : value := IMin(value+1,ulim);
      Dn : value := IMax(value-1,llim);
    Home : value := llim;
    Endd : value := ulim;
      Lt : pos := IMax(lpos,pos-1);
      Rt : pos := IMin(pos+1,upos);
    end; {case}
  end; {Procedure Update_Field}

Procedure Select_Time(message : string;
                    xpos,ypos : byte;
                  var default : time_set;
                    precision : byte);
  const
    days : array [0..1,1..12] of byte
         = ((31,28,31,30,31,30,31,31,30,31,30,31),
            (31,29,31,30,31,30,31,31,30,31,30,31));
  var
    pos,w     : byte;
    choice    : char;
    llim,ulim : array [1..7] of word;
  Function LY(year : word) : byte;
    begin
    if (year mod 4 = 0) and
       ((year mod 100 <> 0) or (year mod 400 = 0)) then
      LY := 1
    else
      LY := 0;
    end; {Function LY}
  begin
  Cursor_Off;
  precision := IMin(IMax(3,precision),7);
  case precision of
    3 : w := 13;
    4 : w := 17;
    5 : w := 20;
    6 : w := 23;
    7 : w := 26;
    end; {case}
  if precision < 7 then
    default.hu := 0;
  if precision < 6 then
    default.se := 0;
  if precision < 5 then
    default.mi := 0;
  if precision < 4 then
    default.hr := 0;
  MakeWindow(xpos,ypos,w,1,white,message);
  llim[1] := 1957;  ulim[1] := 2056;
  llim[2] :=    1;  ulim[2] :=   12;
  llim[3] :=    1;  ulim[3] := days[LY(default.yr),default.mo];
  llim[4] :=    0;  ulim[4] :=   23;
  llim[5] :=    0;  ulim[5] :=   59;
  llim[6] :=    0;  ulim[6] :=   59;
  llim[7] :=    0;  ulim[7] :=   99;
  pos := 1;
  Echo_Time(default,pos,precision);
  repeat
    choice := ReadKey;
    if choice = #00 then
      begin
      choice := ReadKey;
      if choice in [Up,Dn,Home,Endd,Lt,Rt] then
        begin
        case pos of
          1 : Update_Field(choice,default.yr,llim[pos],ulim[pos],pos,1,precision);
          2 : Update_Field(choice,default.mo,llim[pos],ulim[pos],pos,1,precision);
          3 : Update_Field(choice,default.dy,llim[pos],ulim[pos],pos,1,precision);
          4 : Update_Field(choice,default.hr,llim[pos],ulim[pos],pos,1,precision);
          5 : Update_Field(choice,default.mi,llim[pos],ulim[pos],pos,1,precision);
          6 : Update_Field(choice,default.se,llim[pos],ulim[pos],pos,1,precision);
          7 : Update_Field(choice,default.hu,llim[pos],ulim[pos],pos,1,precision);
          end; {case}
        ulim[3] := days[LY(default.yr),default.mo];
        if default.dy > ulim[3] then
          default.dy := ulim[3];
        Echo_Time(default,pos,precision);
        end; {if}
      end; {if}
  until choice = ^M;
  MakeWindow(xpos,ypos,w,1,lightgray,message);
  Echo_Time(default,0,precision);
  Window(1,1,80,25);
  end; {Procedure Input_Time}

Procedure Echo_Time_Interval(time : time_set;
                   item,precision : byte);
  begin
  GotoXY(5,2);
  ClrEOL;
  if item = 3 then ReverseVideo;
  Write(time.dy);
  NormalVideo;
  if precision > 3 then
    begin
    Write('  ');
    if item = 4 then ReverseVideo;
    Write(TwoDigit(time.hr));
    NormalVideo;
    end; {if}
  if precision > 4 then
    begin
    Write(':');
    if item = 5 then ReverseVideo;
    Write(TwoDigit(time.mi));
    NormalVideo;
    end; {if}
  if precision > 5 then
    begin
    Write(':');
    if item = 6 then ReverseVideo;
    Write(TwoDigit(time.se));
    NormalVideo;
    end; {if}
  if precision > 6 then
    begin
    Write('.');
    if item = 7 then ReverseVideo;
    Write(TwoDigit(time.hu));
    NormalVideo;
    end; {if}
  end; {Procedure Echo_Time_Interval}

Procedure Select_Time_Interval(message : string;
                             xpos,ypos : byte;
                           var default : time_set;
                             precision : byte);
  var
    pos,w     : byte;
    choice    : char;
    llim,ulim : array [1..7] of word;
  begin
  Cursor_Off;
  precision := IMin(IMax(3,precision),7);
  case precision of
    3 : w :=  5;
    4 : w :=  9;
    5 : w := 12;
    6 : w := 15;
    7 : w := 18;
    end; {case}
  if precision < 7 then
    default.hu := 0;
  if precision < 6 then
    default.se := 0;
  if precision < 5 then
    default.mi := 0;
  if precision < 4 then
    default.hr := 0;
  MakeWindow(xpos,ypos,IMax(w+1,13),2,white,message);
  Write(Copy(' days  hr:mn:sc.hu',1,w));
  llim[3] := 0;  ulim[3] :=  9;
  llim[4] := 0;  ulim[4] := 23;
  llim[5] := 0;  ulim[5] := 59;
  llim[6] := 0;  ulim[6] := 59;
  llim[7] := 0;  ulim[7] := 99;
  pos := 3;
  Echo_Time_Interval(default,pos,precision);
  repeat
    choice := ReadKey;
    if choice = #00 then
      begin
      choice := ReadKey;
      if choice in [Up,Dn,Home,Endd,Lt,Rt] then
        begin
        case pos of
          3 : Update_Field(choice,default.dy,llim[pos],ulim[pos],pos,3,precision);
          4 : Update_Field(choice,default.hr,llim[pos],ulim[pos],pos,3,precision);
          5 : Update_Field(choice,default.mi,llim[pos],ulim[pos],pos,3,precision);
          6 : Update_Field(choice,default.se,llim[pos],ulim[pos],pos,3,precision);
          7 : Update_Field(choice,default.hu,llim[pos],ulim[pos],pos,3,precision);
          end; {case}
        Echo_Time_Interval(default,pos,precision);
        end; {if}
      end; {if}
  until choice = ^M;
  MakeWindow(xpos,ypos,IMax(w+1,13),2,lightgray,message);
  Echo_Time_Interval(default,0,precision);
  Window(1,1,80,25);
  end; {Procedure Input_Time_Interval}

Function Checksum_Good(line : line_data) : boolean;
  var
    i,checksum,check_digit : integer;
  begin
  checksum := 0;
  for i := 1 to 68 do
    case line[i] of
      '0'..'9' : checksum := checksum + Ord(line[i]) - Ord('0');
           '-' : checksum := checksum + 1;
      end; {case}
  checksum := checksum mod 10;
  check_digit := Ord(line[69]) - Ord('0');
  Checksum_Good := (checksum = check_digit);
  end; {Function Checksums_Good}

Function Good_Elements(line : two_line) : boolean;
  var
    result : boolean;
  begin
  result := Checksum_Good(line[1]) and Checksum_Good(line[2]);
  if (line[1,1] <> '1') or
     (line[2,1] <> '2') or
     (Copy(line[1],3,5) <> Copy(line[2],3,5)) then
    result := false;
  if (line[1,24] <> '.') or
     (line[1,35] <> '.') or
     (Copy(line[1],62,3) <> ' 0 ') or
     (line[2,12] <> '.') or
     (line[2,21] <> '.') or
     (line[2,38] <> '.') or
     (line[2,47] <> '.') or
     (line[2,55] <> '.') then
    result := false;
  Good_Elements := result;
  end; {Function Good_Elements}

Procedure Input_Satellite(index : word);
  begin
  if not EOF(fsat) then
    begin
    if data_type = 3 then
      Readln(fsat,sat_name[index]);
    Readln(fsat,sat_data[index,1]);
    Readln(fsat,sat_data[index,2]);
    end; {if}
  end; {Procedure Input_Satellite}

Function Input_Satellite_Data(fn : string) : word;
  var
    count : word;
  begin
  if data_type in [2,3] then
    begin
    count := 0;
    Assign(fsat,data_drive + data_dir + fn);
    Reset(fsat);
    repeat
      count := count + 1;
      Input_Satellite(count);
    until EOF(fsat) or (count = max_sats);
    Close(fsat);
    Input_Satellite_Data := count;
    end {if}
  else
    begin
    GotoXY(1,24);
    Writeln('Invalid data type!');
    Halt;
    end; {else}
  end; {Procedure Input_Satellite_Data}

Procedure Select_Satellites(title : string;
                          x,y,w,h : byte;
                           number : word);
  var
    choice              : char;
    nr_selected,
    start,stop,select,i : word;
  begin
  Cursor_Off;
  nr_selected := 0;
  h := IMin(h,number);
  select := 1;
  w := IMax(w,12);
  MakeWindow(x,y,w,h,white,title);
  start := IMin(number - h + 1,select);
  stop  := start + h - 1;
  repeat
    ClrScr;
    for i := start to stop do
      begin
      GotoXY(1,i-start+1);
      if i = select then TextBackground(blue);
      if selected[i] then Write('[*] ') else Write('[ ] ');
      Write(Copy(sat_data[i,1],3,5),'  ',Copy(sat_name[i],1,w-12));
      ClrEOL;
      if i = select then TextBackground(black);
      end; {for i}
    choice := ReadKey;
    if choice = #0 then
      begin
      choice := ReadKey;
      case choice of
          Up : begin
               select := IMax(1,select-1);
               if select < start then
                 begin
                 start := select;
                 stop  := start + h - 1;
                 end; {if}
               end; {Up}
        PgUp : begin
               select := IMax(1,select-h);
               if select < start then
                 begin
                 start := select;
                 stop  := start + h - 1;
                 end; {if}
               end; {PgUp}
          Dn : begin
               select := IMin(number,select+1);
               if select > stop then
                 begin
                 stop  := select;
                 start := stop - h + 1;
                 end; {if}
               end; {Dn}
        PgDn : begin
               select := IMin(number,select+h);
               if select > stop then
                 begin
                 stop  := select;
                 start := stop - h + 1;
                 end; {if}
               end; {PgDn}
        end; {case}
      end {if}
    else
      case UpCase(choice) of
        ' ' : begin
              selected[select] := Not(selected[select]);
              if selected[select] then
                nr_selected := nr_selected + 1
              else
                nr_selected := nr_selected - 1;
              select := IMin(number,select+1);
              if select > stop then
                begin
                stop  := select;
                start := stop - h + 1;
                end; {if}
              end; {Toggle}
        'A' : begin
              for i := 1 to number do
                begin
                selected[i] := Not(selected[i]);
                if selected[i] then
                  nr_selected := nr_selected + 1
                else
                  nr_selected := nr_selected - 1;
                end; {for i}
              end; {Toggle All}
        end; {case}
  until (choice = CR) and (nr_selected > 0);
  Delay(500);
  MakeWindow(x,y,w,h,lightgray,title);
  Window(1,1,80,25);
  end; {Procedure Select_Satellites}

Procedure Input_Observer(var geodetic : vector);
  begin
  if not EOF(fobs) then
    begin
    Readln(fobs,obs_name,geodetic[1],geodetic[2],geodetic[3]);
    geodetic[1] := Radians(geodetic[1]);
    geodetic[2] := Radians(Modulus(geodetic[2],360));
    geodetic[3] := geodetic[3]*0.001;
    end; {if}
  end; {Procedure Input_Observer}

  begin
  for i := 1 to max_sats do
    selected[i] := false;

end.
