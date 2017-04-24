Unit Support;  (** This unit contains machine-specific code **)
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 25 }
{ Current Revision:  1992 Sep 14 }
{          Version:  1.81 }
{        Copyright:  1992, All Rights Reserved }
{$N+}

INTERFACE

const  {IBM PC screen codes}
  BS       =  ^H;                      {Backspace}
  CR       =  ^M;                      {Carriage Return}
  CRLF     =  ^M^J;                    {Carriage Return/Line Feed}
  BELL     =  ^G;                      {Terminal Bell}
  ESC      =  ^[;                      {Escape}
  DEL      =  #$7F;                    {Delete}
  Up       =  #72;                     {Up Cursor}
  Dn       =  #80;                     {Down Cursor}
  Rt       =  #77;                     {Right Cursor}
  Lt       =  #75;                     {Left Cursor}
  Home     =  #71;                     {Home Key}
  Endd     =  #79;                     {End Key}
  PgUp     =  #73;                     {Page Up}
  PgDn     =  #81;                     {Page Down}
  C_Lt     =  #115;                    {Control-Left Cursor}
  C_Rt     =  #116;                    {Control-Right Cursor}
  C_PgUp   =  #132;                    {Control-Page Up}
  C_PgDn   =  #118;                    {Control-Page Down}
  UpDown   =  #24#25;                  {Up/Down Arrows}
  Cursors  =  #24#25#26#27;            {Up/Down/Left/Right Arrows}
  SFrame : string = '⁄ƒø≥¿Ÿ¡¬√≈¥';     {Single-Line Frame Characters}
  DFrame : string = '…Õª∫»º ÀÃŒπ';     {Double-Line Frame Characters}
  MFrame : string = 'µ∆∏æ‘’';          {Mixed-Line Frame Characters}

type
  options  = array [0..10] of string;
  time_set = record
    yr,mo,dy,hr,mi,se,hu : word;
    end; {record}

Procedure Cursor_On;
Procedure Cursor_Off;
Procedure Save_Cursor;
Procedure Restore_Cursor;
Procedure ReverseVideo;
Procedure NormalVideo;
Procedure BoldVideo;
Procedure FrameWindow(x,y,w,h,color : byte; title : string);
Procedure MakeWindow(x,y,w,h,color : byte; title : string);
Procedure ClearWindow(x,y,w,h : byte);
Procedure Show_Status_Line(title : string);
Procedure Show_Instructions(title : string);
Procedure Clear_Status_Line;
Procedure Report_Error(x,y : byte; title : string);
Procedure Beep;
Procedure Buzz;
Procedure Mark_Time;
Procedure Zero_Time(var time : time_set);
Procedure Get_Current_Time(var time : time_set);
Function Yes : boolean;
Function TwoDigit(arg : integer) : string;
Function ThreeDigit(arg : integer) : string;
Procedure Convert_Blanks(var field : string);
Function Integer_Value(buffer : string;
                 start,length : integer) : integer;
Function Real_Value(buffer : string;
              start,length : integer) : double;
Function File_Exists(filename : string) : boolean;
Function Select_File(title,pattern,default : string; x,y,w,h : byte) : string;
Function Select_Option(menu : options; number,x,y,w,h : byte) : byte;

IMPLEMENTATION
  Uses CRT,DOS,MinMax;

var
  Last_X,Last_Y : byte;

Procedure Cursor_On;
  var
    regs : registers;
  begin
  with regs do
    begin
    ah := $01;
    ch := 0;
    cl := 7;
    end; {with}
  Intr($10,regs);
  end; {Procedure Cursor_On}

Procedure Cursor_Off;
  var
    regs : registers;
  begin
  with regs do
    begin
    ah := $01;
    ch := $20;
    cl := $00;
    end; {with}
  Intr($10,regs);
  end; {Procedure Cursor_Off}

Procedure Save_Cursor;
  begin
  Last_X := WhereX;
  Last_Y := WhereY;
  end; {Procedure Save_Cursor}

Procedure Restore_Cursor;
  begin
  GotoXY(Last_X,Last_Y);
  end; {Procedure Restore_Cursor}

Procedure ReverseVideo;
  begin
  TextColor(black);
  TextBackground(lightgray);
  end; {Procedure ReverseVideo}

Procedure NormalVideo;
  begin
  TextColor(lightgray);
  TextBackground(black);
  end; {Procedure NormalVideo}

Procedure BoldVideo;
  begin
  TextColor(yellow);
  TextBackground(black);
  end; {Procedure BoldVideo}

Procedure FrameWindow(x,y,w,h,color : byte;
                              title : string);
  var
    i : byte;
  begin
{  Window(x,y,x+w+3,y+h+1);}
{  ClrScr; }
  Window(x,y,x+w+3,y+h+2);
  TextColor(color);
  Write(DFrame[1]);
  for i := 1 to w+2 do
    Write(DFrame[2]);
  Write(DFrame[3]);
  for i := 1 to h do
    begin
    GotoXY(1,i+1);
    Write(DFrame[4]);
    GotoXY(w+4,i+1);
    Write(DFrame[4]);
    end; {for i}
  GotoXY(1,h+2);
  Write(DFrame[5]);
  for i := 1 to w+2 do
    Write(DFrame[2]);
  Write(DFrame[6]);
  GotoXY(2,1);
  Write(MFrame[4],Copy(title,1,w),MFrame[6]);
  NormalVideo;
  end; {Procedure FrameWindow}

Procedure MakeWindow(x,y,w,h,color : byte;
                             title : string);
  begin
  FrameWindow(x,y,w,h,color,title);
  Window(x+2,y+1,x+w+1,y+h);
  end; {Procedure MakeWindow}

Procedure ClearWindow(x,y,w,h : byte);
  begin
  Window(x,y,x+w+3,y+h+1);
  ClrScr;
  Window(1,1,80,25);
  end; {Procedure ClearWindow}

Procedure Show_Status_Line(title : string);
  begin
  GotoXY(1,25);
  Write(Copy(title,1,79));
  ClrEOL;
  end; {Procedure Show_Status_Line}

Procedure Show_Instructions(title : string);
  begin
  GotoXY(80-Length(title),25);
  Write(Copy(title,1,79));
  ClrEOL;
  end; {Procedure Show_Instructions}

Procedure Clear_Status_Line;
  begin
  Show_Status_Line('');
  end; {Procedure Clear_Status_Line}

Procedure Report_Error(x,y : byte; title : string);
  begin
  GotoXY(x,y);
  BoldVideo;
  Write(title);
  NormalVideo;
  GotoXY(1,24);
  Cursor_On;
  Halt;
  end; {Procedure Report_Error}

Procedure Beep;
  var
    i : integer;
  begin
  for i := 1 to 3 do
    begin
    Sound(1500);
    Delay(100);
    NoSound;
    Delay(10);
    end; {for}
  end; {Procedure Beep}

Procedure Buzz;
  var
    i : integer;
  begin
  for i := 1 to 3 do
    begin
    Sound(500);
    Delay(100);
    NoSound;
    Delay(10);
    end; {for}
  end; {Procedure Buzz}

Procedure Mark_Time;
  const
    time_count : byte = 0;
  begin
  case time_count of
    0 : Write('-');
    1 : Write('\');
    2 : Write('|');
    3 : Write('/');
    end; {case}
  time_count := (time_count + 1) mod 4;
  Write(^H);
  end; {Procedure Mark_Time}

Procedure Zero_Time(var time : time_set);
  begin
  with time do
    begin
    yr := 0;
    mo := 0;
    dy := 0;
    hr := 0;
    mi := 0;
    se := 0;
    hu := 0;
    end; {with}
  end; {Procedure Zero_Time}

Procedure Get_Current_Time(var time : time_set);
  var
    dw : word;
  begin
  with time do
    begin
    GetDate(yr,mo,dy,dw);
    GetTime(hr,mi,se,hu);
    end; {with time}
  end; {Procedure Get_Current_Time}

Function Yes : boolean;
  var
    ch    : char;
    valid : boolean;
  begin
  Cursor_On;
  repeat
    ch := Upcase(ReadKey);
    valid := true;
    case ch of
      'Y' : begin
            writeln('Yes');
            Yes := true;
            end; {Yes}
      'N' : begin
            writeln('No');
            Yes := false;
            end; {No}
    else
      valid := false;
    end; {case}
  until valid;
  Cursor_Off;
  end; {Function Yes}

Function TwoDigit(arg : integer) : string;
  begin
  TwoDigit := Chr((arg div 10) + Ord('0'))
            + Chr((arg mod 10) + Ord('0'));
  end; {Function TwoDigit}

Function ThreeDigit(arg : integer) : string;
  var
    hundreds,barg : integer;
  begin
  hundreds := arg div 100;
  barg := arg - 100*hundreds;
  ThreeDigit := Chr(hundreds + Ord('0')) + TwoDigit(barg);
  end; {Function ThreeDigit}

Procedure Convert_Blanks(var field : string);
  var
    i : integer;
  begin
  for i := length(field) downto 1 do
    if field[i] = ' ' then
      field[i] := '0';
  end; {Procedure Convert_Blanks}

Function Integer_Value(buffer : string;
                 start,length : integer) : integer;
  var
    answer,result : integer;
  begin
  buffer := Copy(buffer,start,length);
  Convert_Blanks(buffer);
  Val(buffer,answer,result);
  if result = 0 then
    Integer_Value := answer
  else
    Integer_Value := 0;
  end; {Function Integer_Value}

Function Real_Value(buffer : string;
              start,length : integer) : double;
  var
    result : integer;
    answer : double;
  begin
  buffer := Copy(buffer,start,length);
  Convert_Blanks(buffer);
  if buffer = '' then
    buffer := '0';
  Val(buffer,answer,result);
  if result = 0 then
    Real_Value := answer
  else
    Real_Value := 0.0;
  end; {Function Real_Value}

Function File_Exists(filename : string) : boolean;
  var
    filehandle : text;
  begin
  Assign(filehandle,filename);
  {$i-} Reset(filehandle); {$i+}
  if IOResult = 0 then
    begin
    File_Exists := true;
    Close(filehandle);
    end {if}
  else
    File_Exists := false;
  end; {Function File_Exists}

Function Select_File(title,pattern,default : string;
                                   x,y,w,h : byte) : string;
  var
    choice         : char;
    start,stop,
    count,select,i : word;
    dirinfo        : SearchRec;
    filedata       : array [1..50] of string;
  begin
  Cursor_Off;
  FindFirst(pattern,AnyFile,dirinfo);
  count := 0;
  select := 1;
  while DosError = 0 do
    begin
    count := count + 1;
    filedata[count] := dirinfo.name;
    if filedata[count] = default then
      select := count;
    FindNext(dirinfo);
    end; {while}
  w := IMax(12,w);
  h := IMin(h,IMax(1,count));
  MakeWindow(x,y,w,h,white,title);
  if count = 0 then
    begin
    BoldVideo;
    Write('No files!');
    Delay(1000);
    NormalVideo;
    Window(1,1,80,25);
    GotoXY(1,25);
    ClrEOL;
    GotoXY(1,24);
    Cursor_On;
    Halt;
    end {if}
  else
    begin
    start := IMin(count - h + 1,select);
    stop  := start + h - 1;
    repeat
      ClrScr;
      for i := start to stop do
        begin
        GotoXY(1,i-start+1);
        if i = select then BoldVideo;
        Write(Copy(filedata[i],1,w));
        if i = select then NormalVideo;
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
          Dn : begin
               select := IMin(count,select+1);
               if select > stop then
                 begin
                 stop  := select;
                 start := stop - h + 1;
                 end; {if}
               end; {Dn}
          end; {case}
        end; {if}
    until choice = CR;
    Select_File := filedata[select];
    Delay(500);
    end; {else}
  MakeWindow(x,y,w,h,lightgray,title);
  Window(1,1,80,25);
  end; {Function Select_File}

Function Select_Option(menu : options;
             number,x,y,w,h : byte) : byte;
  var
    choice              : char;
    start,stop,select,i : word;
  begin
  Cursor_Off;
  h := IMin(h,number);
  select := 1;
  MakeWindow(x,y,w,h,white,menu[0]);
  start := IMin(number - h + 1,select);
  stop  := start + h - 1;
  repeat
    ClrScr;
    for i := start to stop do
      begin
      GotoXY(1,i-start+1);
      if i = select then BoldVideo;
      Write(menu[i]);
      if i = select then NormalVideo;
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
        Dn : begin
             select := IMin(number,select+1);
             if select > stop then
               begin
               stop  := select;
               start := stop - h + 1;
               end; {if}
             end; {Dn}
        end; {case}
      end; {if}
  until choice = CR;
  Select_Option := select;
  Delay(500);
  MakeWindow(x,y,w,h,lightgray,menu[0]);
  Window(1,1,80,25);
  end; {Function Select_Option}

end.
