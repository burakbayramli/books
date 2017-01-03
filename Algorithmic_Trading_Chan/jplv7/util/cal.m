function result = cal(begin_yr,begin_per,freq,obs)
% PURPOSE: create a time-series calendar structure variable that
%          associates a date with an observation #
% -----------------------------------------------------
% USAGE:       result = cal(begin_yr,begin_per,freq,obs)
%         or:  result = cal(cstruc,obs)
% where:    begin_yr  = beginning year, e.g., 1982
%           begin_per = beginning period, e.g., 3 
%               freq  = frequency, 1=annual,4=quarterly,12=monthly
%               obs   = optional argument for an observation #
%           cstruc    = a structure returned by cal()
% -----------------------------------------------------
% RETURNS: a structure:
%             result.beg_yr  = begin_yr
%             result.beg_per = begin_period
%             result.freq    = frequency
%             result.obs     = obs            (if input)
%             result.year    = year for obs   (if input)
%             result.period  = period for obs (if input)
% ------------------------------------------------------
% SEE ALSO: ical() an inverse function to find observation #
%           associated with a cal-structure date
%           tsdate() that returns a string for the date associated
%           with observation #
          
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 2 % case where user has input a structure from cal()
  if ~isstruct(begin_yr)
  error('cal requires a structure as input with 2 arguments');
  else
  obs = begin_per;
  begin_per = begin_yr.beg_per;
  freq = begin_yr.freq;
  begin_yr = begin_yr.beg_yr;
  end;
% error checking
% check that user input 4-digit year
    [junk strsize] = size(num2str(begin_yr));
    if strsize ~= 4; error('input a 4-digit year to cal'); end;
% check that begin_per not > freq or negative
if begin_per > freq
error('begin_per > freq in cal');
end;
if begin_per < 0
error('begin_per < 0 in cal');
end;
end;


if (nargin == 3) | (nargin == 4)
% error checking
% check that user input 4-digit year
    [junk strsize] = size(num2str(begin_yr));
    if strsize ~= 4; error('input a 4-digit year to cal'); end;
% check that begin_per not > freq or negative
if begin_per > freq
error('begin_per > freq in cal');
end;
if begin_per < 0
error('begin_per < 0 in cal');
end;
end;


switch nargin

case 4
result.beg_yr = begin_yr;
result.beg_per = begin_per;
result.freq = freq;
result.obs = obs;
%result.year = begin_yr + floor((obs-1)/freq);
result.year = begin_yr + fix((obs+begin_per-2)/freq);
% Wang Mingjian Guanghua School of Management Bejing  University,
% suggested this bug fix
tmp = rem((begin_per+obs-1),freq);
if tmp == 0
tmp = freq;
end;
result.period = tmp;

case 3
result.beg_yr = begin_yr;
result.beg_per = begin_per;
result.freq = freq;

case 2
result.beg_yr = begin_yr;
result.beg_per = begin_per;
result.freq = freq;
result.obs = obs;
result.year = begin_yr + fix((obs+begin_per-2)/freq);
tmp = rem((begin_per+obs-1),freq);
if tmp == 0
tmp = freq;
end;
result.period = tmp;


otherwise
error('wrong # of inputs to cal');
end;
