function obs = ical(year,period,cstructure)
% PURPOSE: finds observation # associated with a year,period 
%          given a cal() structure
% -----------------------------------------------------
% USAGE: obs = ical(year,period,c_str)
% where:  year   = year (4-digit)
%       period   = period ( <= frequency)
%         c_str  = a structure returned by cal() function
% -----------------------------------------------------
% RETURNS: obs = # of observation associated with year,period
% e.g., cstr = cal(1982,1,12) 
%        obs = ical(1986,1,cstr) would return 48
% e.g., cstr = cal(1982,1,12)
%        obs = ical(1982,1,cstr) would return 1
% ------------------------------------------------------
% SEE ALSO: cal() a function to set up a time-series calendar
%           that associates observation #'s with dates
%           tsdate() that returns a string for the date associated
%           with observation #
          
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin ~= 3
error('Wrong # of inputs to ical');
end;

% check that user input 4-digit year
    [junk strsize] = size(num2str(year));
    if strsize ~= 4; error('input a 4-digit year to ical'); end;
% check that begin_per not negative

if period < 0
error('period < 0 in ical');
end;

if isstruct(cstructure)

  begin_yr = cstructure.beg_yr;
  begin_per = cstructure.beg_per;
  freq = cstructure.freq;
   if period > freq % check that period not > freq
   error('period > freq in ical');
   end;
   if year < begin_yr % check that year not > beg_yr
   error('year > beg_yr in ical');
   end;
 if year > begin_yr
 obs1 = length(begin_per:freq);
 obs2 = length(begin_yr+1:year-1)*freq;
 obs3 = length(1:period);
 obs = obs1+obs2+obs3;
 else
 obs = length(begin_per:period);
 end;

else
error('ical requires a structure returned by cal');
end;

