Unit SGP_Init;
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Sep 01 }
{ Current Revision:  2000 Jan 22 }
{          Version:  1.25 }
{        Copyright:  1992-2000, All Rights Reserved }
{$N+}

INTERFACE

const
  max_sats = 250;

type
  line_data = string[69];
  two_line  = array [1..2] of line_data;

var
  visible              : boolean;
  epoch                : double;
  catnr,elset          : string;
  obs_name             : string[25];
  selected             : array [1..max_sats] of boolean;
  sat_name             : array [1..max_sats] of string[24];
  sat_data             : array [1..max_sats] of two_line;
  data_drive,data_dir,
  work_drive,work_dir  : string;
  UTC_offset           : double;
  DST                  : boolean;

Procedure Program_Initialize(program_name : string);
Procedure Program_End;

IMPLEMENTATION
  Uses CRT,Support;

Procedure Program_Initialize(program_name : string);
  var
    space,lines : byte;
    key         : char;
    line,fn     : string;
    fi          : text;
  begin
{ Input header file describing program, 22 lines by 79 columns maximum }
  ClrScr;
  fn := program_name + '.HDR';
  if File_Exists(fn) then
    begin
    Assign(fi,fn);
    Reset(fi);
    lines := 0;
    repeat
      lines := lines + 1;
      Readln(fi,line);
      Writeln(line);
    until EOF(fi) or (lines = 22);
    Close(fi);
    end; {if}
{ Input directory configuration file }
  fn := program_name + '.CFG';
  if File_Exists(fn) then
    begin
    Assign(fi,fn);
    Reset(fi);
    Readln(fi,key);
    key := Upcase(key);
    if key in ['A'..'Z'] then
      data_drive := Upcase(key) + ':'
    else
      data_drive := '';
    Readln(fi,data_dir);
    if Pos(' ',data_dir) <> 0 then
      data_dir := Copy(data_dir,1,Pos(' ',data_dir)-1);
    if (data_dir <> '') and (data_dir[Length(data_dir)] <> '\') then
        data_dir := data_dir + '\';
    Readln(fi,key);
    key := Upcase(key);
    if key in ['A'..'Z'] then
      work_drive := Upcase(key) + ':'
    else
      work_drive := '';
    Readln(fi,work_dir);
    if Pos(' ',work_dir) <> 0 then
      work_dir := Copy(work_dir,1,Pos(' ',work_dir)-1);
    if (work_dir <> '') and (work_dir[Length(work_dir)] <> '\') then
        work_dir := work_dir + '\';
    Readln(fi,UTC_offset);
    Readln(fi,key);
    if Upcase(key) = 'Y' then
      DST := true
    else
      DST := false;
    Close(fi);
    end {if}
  else
    begin
    data_drive := '';
    data_dir   := '';
    work_drive := '';
    work_dir   := '';
    UTC_offset := 0.0;
    DST        := false;
    end; {else}
  end; {Procedure Initialize}

Procedure Program_End;
  begin
  GotoXY(1,24);
  Cursor_On;
  end; {Procedure Program_End}

end.
