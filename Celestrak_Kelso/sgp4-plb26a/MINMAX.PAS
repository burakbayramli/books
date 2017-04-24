Unit MinMax;
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 29 }
{ Current Revision:  1992 Sep 03 }
{          Version:  1.02 }
{        Copyright:  1992, All Rights Reserved }
{$N+}

INTERFACE

Function IMin(arg1,arg2 : integer) : integer;
Function IMax(arg1,arg2 : integer) : integer;
Function RMin(arg1,arg2 : real) : real;
Function RMax(arg1,arg2 : real) : real;
Function DMin(arg1,arg2 : double) : double;
Function DMax(arg1,arg2 : double) : double;

IMPLEMENTATION

Function IMin(arg1,arg2 : integer) : integer;
  begin
  if arg1 < arg2 then
    IMin := arg1
  else
    IMin := arg2;
  end; {Function IMin}

Function IMax(arg1,arg2 : integer) : integer;
  begin
  if arg1 > arg2 then
    IMax := arg1
  else
    IMax := arg2;
  end; {Function IMax}

Function RMin(arg1,arg2 : real) : real;
  begin
  if arg1 < arg2 then
    RMin := arg1
  else
    RMin := arg2;
  end; {Function RMin}

Function RMax(arg1,arg2 : real) : real;
  begin
  if arg1 > arg2 then
    RMax := arg1
  else
    RMax := arg2;
  end; {Function RMax}

Function DMin(arg1,arg2 : double) : double;
  begin
  if arg1 < arg2 then
    DMin := arg1
  else
    DMin := arg2;
  end; {Function DMin}

Function DMax(arg1,arg2 : double) : double;
  begin
  if arg1 > arg2 then
    DMax := arg1
  else
    DMax := arg2;
  end; {Function DMax}

end.
