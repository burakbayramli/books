unit CmnProcD;    {main}

{EUGene  Copyright 1997-2003+  D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

     {This is the commands for the unit cmnprocs.}
      { This file has common procedures used by my programs.  }
      {These are mathematical and generic procedures, not data / data reading oriented procs.}
      {These are converted for Delphi in this version.}
      {In the conversion, some functions were dropped b/c Delphi has built
      in procedures.}

interface

uses dialogs, sysutils;

{Only very basic types need to be defined in this ...}

const path_length = 255;
      missing_value = -9;

{No new vars need to be defined in this...}

{Delphi 2 ? has a power function and log functions, but delphi 1 doesnt.
 Don't need these for D2 version - I think.}
   function intpower (num: longint; power: longint): longint;   {returns num ** power where power is integer}
   function realpower (num: real; power: real): real;    {returns num ** power where power is a real}
   function log10 (x : single) : single;                       {returns log base 10}

{Delphi has integer/string type conversion., and fileexists func}
{   function inttostr (anint : longint) : string;
{   function strtoint (astring : string) : longint;}
{   function file_exists (afile_name : TFileName) : boolean;
    Now FileExists}

  {Deleted this, not used anywhere.}
   {function getyn : char;}
{   function maxreal (num1, num2: real): real;           {returns the larger of two entered reals}
{   function maxint (num1, num2: integer): integer;      {returns the larger of two entered integers}
{   function maxlongint (num1, num2: longint): longint;  {returns the larger of two entered ints}
{   function minint (num1, num2: integer): integer;      {returns the smaller of two entered integers}
{   function minlongint (num1, num2: longint): longint;      {returns the smaller of two entered integers}
{   function minreal (num1, num2: real): real;        {returns the smaller of two entered reals}
   procedure wait_seconds (time: integer);
   function read_csv_string (var afile : text) : string;
   function read_csv_int (var afile : text) : integer;
   function read_csv_boolean (var afile : text) : boolean;
   function read_csv_longint (var afile : text) : longint;
   function read_csv_real (var afile : text) : real;
   procedure switchint (var num1, num2 : integer);
   function RadToDegr(radian_value : single) : single;
   function DegrToRad(degree_value : single) : single;
   function arccos (x : single): single;        {x in radians}
   function realtostring (x : real; num_digits, num_decimals : integer): string;
   function booltostring (x : boolean): string;

implementation

 { ---------------------------------------------------------------  }

   function intpower (num: longint; power: longint): longint;
   begin
      if num = 0 then
         intpower := 0
      else if power = 0 then
         intpower := 1
      else if num < 0 then
         begin
            ShowMessage ('Cannot take log of a negative number - error in power function.  Setting to 0');
            intpower := 0;
         end
      else
         intpower := round(exp(power * (ln(num))));
   end;       {function intpower}

   { ---------------------------------------------------------------  }

   function realpower (num: real; power: real): real;
   begin
      if num = 0 then
         realpower := 0
      else if power = 0 then
         realpower := 1
      else if num < 0 then
         begin
            ShowMessage ('Cannot take log of a negative number - error in power function.  Setting to 0');
            realpower := 0;
         end
      else
         realpower := exp(power * (ln(num)));
   end;       {function realpower}

 { ---------------------------------------------------------------  }

   function log10 (x : single) : single;    {returns base 10 log}
     begin
        log10 := ln(x) / ln(10);
     end;

   { ---------------------------------------------------------------  }

   function maxreal (num1, num2: real): real;                             {returns the larger of two entered reals}
                                                                               {or the first if they are equal}
   begin
      if num1 >= num2 then
         maxreal := num1
      else                                             {num2 > num1)}
         maxreal := num2;
   end;

  { ---------------------------------------------------------------  }

   function maxint (num1, num2: integer): integer;   {returns the larger of two entered ints}
                                                     {or the first if they are equal}
   begin
      if num1 >= num2 then
         maxint := num1
      else                                             {num2 > num1)}
         maxint := num2;
   end;

  { ---------------------------------------------------------------  }

   function maxlongint (num1, num2: longint): longint;     {returns the larger of two entered longints}
   begin
      if num1 >= num2 then
         maxlongint := num1
      else                                             {num2 > num1)}
         maxlongint := num2;
   end;

  { ---------------------------------------------------------------  }

   function minint (num1, num2: integer): integer;                        {returns the smaller of two entered integers}
                                                                               {or the first if they are equal}
   begin
      if num1 <= num2 then
         minint := num1
      else                                             {num2 < num1)}
         minint := num2;
   end;

  { ---------------------------------------------------------------  }

   function minlongint (num1, num2: longint): longint;   {returns the smaller of two entered integers}
                                                {or the first if they are equal}
   begin
      if num1 <= num2 then
         minlongint := num1
      else                                             {num2 < num1)}
         minlongint := num2;
   end;

  { ---------------------------------------------------------------  }

   function minreal (num1, num2: real): real;                          {returns the smaller of two entered reals}
                                                                               {or the first if they are equal}
   begin
      if num1 <= num2 then
         minreal := num1
      else                                             {num2 < num1)}
         minreal := num2;
   end;

  { ---------------------------------------------------------------  }

   procedure wait_seconds (time: integer);
         {time is number of seconds to wait; must be less than 60 to work}
      var
         hour, minute, second, sec100 : word;
         endsecond, endminute, endhour: integer;
         present : tdatetime;
   begin
      {In delphi function "time" returns current time.}
      present := now;
      DecodeTime (present, hour, minute, second, sec100);
      endsecond := (second mod 60) + time;
      endminute := minute;
      endhour := hour;
      if endsecond >= 60 then
         begin
            endsecond := endsecond - 60;
            endminute := minute + 1;
            if endminute >= 60 then
               begin
                  endminute := endminute - 60;
                  endhour := endhour + 1;
                  if endhour >= 24 then
                     begin
                        endhour := endhour - 24;
                     end;
               end;
         end;
      repeat
         begin
            present := now;
            DecodeTime (present, hour, minute, second, sec100);
         end
      until (second >= endsecond) and (minute >= endminute)
             and (hour >= endhour);
   end;

  { ---------------------------------------------------------------  }

   function read_csv_string (var afile : text) : string;
      {This will read a single continuous string value from a data file whether that data
       is comma separated or tab separated, or is in quotes.  It treats every character between
       the tabs/quotes/commas literally, so spaces will make it into the string.
       ** Spaces are no longer treated specially, and just get included in the string,
          and are not separators.  **  }
      var s : string;
          errorcode : integer;
          outval : longint;
          achar : char;
      begin
         s := '';
         {Previously, started by finding the first numeric/alphabetic character.  No longer.  }
         if (eoln(afile)) or (eof(afile)) then
            {never had a chance to read anything, it was already at end of line, leave blank}
            begin
            end
         else      {can read characters}
            begin
               read (afile, achar);
               {start by checking for immediate separator, which would mean no data.}
               if (achar=',') or (achar=chr(9)) then
                  begin   {do nothing, s will remain blank, it is an empty string}
                  end
               else
                  if (achar = '"') then    {this is string marked by a " }
                     begin
                        repeat
                           read (afile, achar);
                           {if not ((achar = '"') ) then}
                           if ((achar >= '0') and (achar <= '9')) or ((achar >= 'A') and (achar <= 'z'))
                              or (achar='_') or (achar='-') or (achar='.') or (achar=' ') or (achar=chr(39))
                              or (achar=',') or (achar='(') or (achar=')') then
                              s := s + achar;
                        until (achar = '"') or (eoln(afile)) or (eof(afile));
                        {But note:  if it starts and ends in a ", it should also have a trailing
                         comma or tab that must be read, unless at end of line.}
                        if not ((eoln(afile)) or (eof(afile))) then
                           begin
                              read (afile, achar);
                              if not ((achar = ',') or (achar = chr(9)) or (achar = chr(9)) ) then
                                 showmessage ('Error in read csv string in this data set.  Data delimited by quotes and not at eoln/eof is not followed by a separator.  String so far is "'+s+'"');
                           end;
                     end
               else
                  if (achar = chr(39)) then    {this is string marked by a '}
                     begin
                        repeat
                           read (afile, achar);
                           {if not  (achar = chr(39)) then }
                           {added , ( ) on 6/16/06}
                           if ((achar >= '0') and (achar <= '9')) or ((achar >= 'A') and (achar <= 'z'))
                              or (achar='_') or (achar='-') or (achar='.') or (achar=' ') or (achar='"')
                              or (achar=' ') or (achar=',') or (achar='(') or (achar=')') then
                              s := s + achar;
                        until (achar = chr(39)) or (eoln(afile)) or (eof(afile));
                        {But note:  if it starts and ends in a ", it should also have a trailing
                         comma or tab that must be read, unless at end of line.}
                        if not ((eoln(afile)) or (eof(afile))) then
                           begin
                              read (afile, achar);
                              if not ((achar = ',') or (achar = chr(9)) or (achar = chr(9))) then
                                 showmessage ('Error in read csv string in this data set.  Data delimited by single quote and not at eoln/eof is not followed by a separator.  String so far is "'+s+'"');
                           end;
                     end
               else   {first char of string was not a ' or ", and it's not an empty string}
                  begin    {now, read until see any separator, not looking for " or ' in particular}
                     s := s + achar;
                     {that's the 1st character, now read the rest, up to the next " , tab ' or space}
                     repeat
                        if (eoln(afile)) or (eof(afile)) then
                           begin
                              {if that 1st character was the end of the line, I'm done}
                           end
                        else  {there are more characters, so read until done}
                           begin
                              read (afile, achar);
                              {If not a separator character, then add to string}
                              {if not ((achar='"') or (achar=chr(39)) or (achar=',') or (achar=chr(9))) then}
                              {If an inrange character, then add to string}
                              if ((achar >= '0') and (achar <= '9')) or ((achar >= 'A') and (achar <= 'z'))
                                 or (achar='_') or (achar='-') or (achar='.') or (achar=' ')
                                 or (achar='(') or (achar=')') then
                                 s := s + achar;
                           end;
                     until ( (achar='"') or (achar=chr(39)) or (achar=',') or (achar=chr(9))
                             or (achar=#10) or (achar=#13) or (eof(afile)) or (eoln(afile)));
                  end;
            end;       {if not eof eoln at very start}
         read_csv_string := s;

      end;

  { ---------------------------------------------------------------  }

   function read_csv_int (var afile : text) : integer;
      {This will read a single integer value from a data file whether that data
       is comma separated, space separated, or tab separated, or is in quotes.}
      var s : string;
          errorcode : integer;
          outval : longint;
          achar : char;
      begin
         s := '';
         {need to start by finding the first numeric character.  This will
          skip over any initial , or " or '}
         repeat
            read (afile, achar);
         until ((achar >= '0') and (achar <= '9')) or (achar = '-') or eof(afile);
         s := s + achar;

         if eof(afile) then
            ShowMessage ('Error reading input file in read_csv int - read first numeric character but hit eof. Program error - notify programmer.');

         {that's the 1st character, now read the rest if not at eoln.}
         repeat
            if not eoln(afile) and not (eof(afile)) then
               begin
                  read (afile, achar);
                  if (achar >= '0') and (achar <= '9') then
                     s := s + achar
                  else if achar='.' then
                    begin
                       ShowMessage ('Error in function read_csv_int -- found a ".", indicating a real number, while trying to read an integer.  ');
                    end;
               end;
         until (achar='"') or (achar=',') or (achar=' ') or (achar=chr(9)) or (achar='.')
                or (achar=#13) or (achar=#10) or (eof(afile)) or (eoln(afile));
         {$R-}    {Turn off range checking to do a manual check}
         val (s, outval, errorcode);
         if errorcode > 0 then
            begin
               ShowMessage ('Integer not found in function read_csv_int.  Setting value to -9');
               read_csv_int := missing_value;
            end
         else
         if (outval > 32767) or (outval < -32768) then
            begin
               ShowMessage ('Value outside integer range found in read_csv_int.  Setting value to -9');
               read_csv_int := -9;
            end
         else
            begin   {OK value}
               read_csv_int := outval;
            end;
         {$R+}
      end;

  { ---------------------------------------------------------------  }

   function read_csv_boolean (var afile : text) : boolean;
      {This will read a single integer value from a data file to be 0 or 1 (boolean).}
      var s : string;
          errorcode : integer;
          outval : longint;
          achar : char;
      begin
         result := false;
         s := '';
         {need to start by finding the first numeric character.  This will
          skip over any initial , or " or '}
         repeat
            read (afile, achar);
         until ((achar >= '0') and (achar <= '9')) or (achar = '-') or eof(afile);
         s := s + achar;

         if eof(afile) then
            ShowMessage ('Error reading input file in read_csv int - read first numeric character but hit eof. Program error - notify programmer.');

         {that's the 1st character, now read the rest if not at eoln.}
         repeat
            if not eoln(afile) and not (eof(afile)) then
               begin
                  read (afile, achar);
                  if (achar >= '0') and (achar <= '9') then
                     s := s + achar
                  else if achar='.' then
                    begin
                       ShowMessage ('Error in function read_csv_boolean -- found a ".", indicating a real number, while trying to read a boolean.  ');
                    end;
               end;
         until (achar='"') or (achar=',') or (achar=' ') or (achar=chr(9)) or (achar='.')
                or (achar=#13) or (achar=#10) or (eof(afile)) or (eoln(afile));
         {$R-}    {Turn off range checking to do a manual check}
         val (s, outval, errorcode);
         if errorcode > 0 then
            begin
               ShowMessage ('Integer not found in function read_csv_boolean.  Setting value to 0 (false)');
               result := false;
            end
         else
         if (outval > 1) or (outval < 0) then
            begin
               ShowMessage ('Value outside boolean integer range (0 to 1) found in read_csv_boolean.  Setting value to 0 (false).');
               result := false;
            end
         else
            begin   {OK value}
               if outval = 0 then
                  result := false
               else if outval = 1 then
                  result := true;
            end;
         {$R+}
      end;

  { ---------------------------------------------------------------  }
     function read_csv_longint (var afile : text) : longint;
      {This will read a value from a data file whether that data is comma separated,
       space separated, or tab separated.}
      var s : string;
          errorcode : integer;
          outval : longint;
          achar : char;
      begin
         s := '';
         {need to start by finding the first numeric character.  This will
          skip over any initial , or " or '}
         repeat
            read (afile, achar);
         until ((achar >= '0') and (achar <= '9')) or (achar = '-') or eof(afile);
         s := s + achar;

         if eof(afile) then
            ShowMessage ('Error reading input file in read_csv int - read first numeric character but hit eof. Program error - notify programmer.');

         {that's the 1st character, now read the rest}
         repeat
            if not eoln(afile) and not (eof(afile)) then
               begin
                  read (afile, achar);
                  if ((achar >= '0') and (achar <= '9')) then
                     s := s + achar
                  else if achar='.' then
                    begin
                       ShowMessage ('Error in function read_csv_longint -- found a ".", indicating a real number, while trying to read a long integer.  ');
                    end;
               end;
         until (achar='"') or (achar=',') or (achar=' ') or (achar=chr(9)) or (achar='.')
                or (achar=#13) or (achar=#10) or (eof(afile)) or (eoln(afile));
         {$R-}    {Turn off range checking to do a manual check}
         val (s, outval, errorcode);
         if errorcode > 0 then
            begin
               ShowMessage ('Integer not found in function read_csv_longint.  Setting value to -9');
               read_csv_longint :=  missing_value;
            end
         else
            begin   {OK value}
               read_csv_longint := outval;
            end;
         {$R+}
      end;

  { ---------------------------------------------------------------  }

   function read_csv_real (var afile : text) : real;
      {This will read a real value from a data file whether that data is comma separated,
       space separated, or tab separated.  HOWEVER, it will only do so for numeric
       representations like 25.031.  It will NOT do 2.5031E+01.}
      var s : string;
          errorcode : integer;
          outval : real;
          achar : char;
      begin
         s := '';
         {need to start by finding the first numeric character.  This will
          skip over any initial , or " or '}
         repeat
            read (afile, achar);
         until ((achar >= '0') and (achar <= '9')) or (achar = '-') or (achar='.') or eof(afile);
         s := s + achar;

         if eof(afile) then
            ShowMessage ('Error reading input file in read_csv int - read first numeric character but hit eof. Program error - notify programmer.');

         {that's the 1st character, now read the rest}
         repeat
            if not eoln(afile) and not (eof(afile)) then
               begin
                  read (afile, achar);
                  if ((achar >= '0') and (achar <= '9')) or (achar='.') or (achar='-') then
                     s := s + achar
                  else if (achar='E') or (achar='+') then
                    begin
                       ShowMessage ('Error in function read_csv_real -- this program cannot ');
                       ShowMessage ('  read real number values in the form 2.5031E+01.  Convert data to 25.031 format.');
                    end;
               end;
         until (achar='"') or (achar=',') or (achar=' ') or (achar='E') or (achar='+') or
               (achar=chr(9)) or (achar=#13) or (achar=#10) or (eof(afile)) or (eoln(afile));
         {$R-}    {Turn off range checking to do a manual check}
         val (s, outval, errorcode);
         if errorcode > 0 then
            begin
               ShowMessage ('Integer not found in function read_csv_real.  Setting value to -9');
               read_csv_real := missing_value;
            end
         else
            begin   {OK value}
               read_csv_real := outval;
            end;
         {$R+}
      end;

  { ---------------------------------------------------------------  }

   procedure switchint (var num1, num2 : integer);
     var temp : integer;
     begin
        temp := num2;
        num2 := num1;
        num1 := temp;
     end;

  { ---------------------------------------------------------------  }

   function RadToDegr(radian_value : single) : single;
       {converts radians to degrees}
           begin
             { 57... is 180/pi}
             radToDegr := (57.2957795)*radian_value;
           end;

  { ---------------------------------------------------------------  }

   function DegrToRad(degree_value : single) : single;
      {converts degrees to radians}
           begin
             { 0.0174... is pi/180}
             DegrToRad := (0.01745329)*degree_value;
           end;

  { ---------------------------------------------------------------  }

   function arccos (x : single): single;        {x in radians}
      {1st line from delphi help, but help is incomplete.
       If value in (cos of y) is < 0, then need to add 180 degrees.}
           begin
              if x < 0 then arccos := (pi) + arctan (sqrt(1-sqr(x))/x)
              else arccos := arctan (sqrt(1-sqr(x))/x);
           end;

  { ---------------------------------------------------------------  }

   function realtostring (x : real; num_digits, num_decimals : integer): string;
     var astring : string;
     begin
        str(x:num_digits:num_decimals, astring);
        realtostring := astring;
     end;

{   function inttostr (anint : longint) : string;
     var astring : string;
     begin
        str(anint, astring);
        inttostr := astring;
     end;

  { ---------------------------------------------------------------  }

   function booltostring (x : boolean): string;
     begin
        case x of
          true : booltostring := '1';
          false : booltostring := '0';
        end;   {case}
     end;

  { ---------------------------------------------------------------  }


  end.                 {end of library/unit cmnprocs}










