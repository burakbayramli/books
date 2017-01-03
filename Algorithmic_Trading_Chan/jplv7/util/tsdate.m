function [out] = tsdate(beg_yr,beg_mth,freq,nobs)
% PURPOSE: produce a time-series date string for an observation #
%          given beginning year, beginning period and frequency of the data
% ---------------------------------------------------
% USAGE: out = tsdate(beg_yr,beg_period,freq,obsn);
%        or:   tsdate(beg_yr,beg_period,freq,obsn);
%        or:   tsdate(cal_struct,obsn);
% where: beg_yr     = beginning year, e.g., 1974
%        beg_period = beginning period, e.g., 4 for april
%        freq       = 1 for annual, 4 for quarterly 12 for monthly
%        obsn       = the observation #
%        cal_struct = a structure returned by cal()
%     
% e.g., tsdate(1974,1,12,13) would print: Jan75
%       tsdate(1974,1,4,13)  would print: Q1-77
%       tsdate(1974,1,1,13)  would print 1986
%       out = tsdate(1974,1,12,13) would return a string `Jan75'
%       
%       cstr = cal(1974,1,12)
%       tsdate(cstr,13)      would print Jan75
% ---------------------------------------------------
% RETURNS: a string, or: simply displays the date associated
%          with obsn (observation #)
% ---------------------------------------------------
% SEE ALSO: tsplot(), tsprint()
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

switch nargin

case 4   % case where a cal() structure is not used
% we need do nothing here

case 2   % case where a cal() structure is used
% error checking
if ~isstruct(beg_yr)
error('tsdate: one of the two inputs must be a structure from cal');
end;

nobs = beg_mth; % the second argument is really an observation #
% the first argument is really a structure returned by cal
freq = beg_yr.freq;
beg_mth = beg_yr.beg_per;
beg_yr = beg_yr.beg_yr;

otherwise
error('Wrong # of arguments to tsdate');
end; % end of switch on nargin


% error check for 4-digit years on input
    [junk strsize] = size(num2str(beg_yr));
    if strsize ~= 4; error('input a 4-digit year to tsdate'); end;


switch freq;

     case 1,      % case of annual series 
      if (beg_mth > 1); error('Wrong beg_per argument in tsdate');end;

  ydigit = 'yyyy';  
  d = datenum(beg_yr,12*nobs,1);
  if nargout == 0
   fprintf('%6s \n',datestr(d,ydigit));
  else
   out = datestr(d,ydigit);
  end;
  
     case 4,      % case of quarterly series
  if (beg_mth > 4); error('Wrong beg_per argument in tsdate');end;      
      
  ydigit = 'QQ-YY';  
  d = datenum(beg_yr,beg_mth*3+3*nobs-3,1);
  if nargout ==0
   fprintf('%6s \n',datestr(d,ydigit));
  else
   out = datestr(d,ydigit);
  end;

     case 12,      % case of monthly series
  if (beg_mth > 12); error('Wrong beg_per argument in tsdate');end;      
            
  ydigit = 'mmmyy';  
  d = datenum(beg_yr,beg_mth+nobs-1,1);
  if nargout == 0
   fprintf('%6s \n',datestr(d,ydigit));
  else
   out = datestr(d,ydigit);
  end;
  
      otherwise % how did we get here?
      disp('frequency unknown to tsdate');
end; % end of switch on freq








