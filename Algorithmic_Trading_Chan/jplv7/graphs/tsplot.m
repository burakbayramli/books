function tsplot(y,cstruc,varargin)
% PURPOSE: time-series plot with dates and labels
%---------------------------------------------------
% USAGE:     tsplot(y,cstruc,begp,endp,vnames) 
%        or: tsplot(y,cal_struc,vnames), which plots the entire series
%        or: tsplot(y,cal_struc), entire series no variable names
%
% where:  y      = matrix (or vector) of series to be plotted
%         cstruc = a structure returned by cal()
%         begp   = the beginning observation to plot
%         endp   = the ending observation to plot
%        vnames  = a string matrix of names for a legend (optional)
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];       strings
%---------------------------------------------------
% e.g.   cstr = cal(1970,1,12);
%        tsplot(y,cstr); would plot all data
%
%    or: tsplot(y,cstr,ical(1981,1,cstr),ical(1981,12,cstr)),
%         which would plot data for 1981 only
%---------------------------------------------------
% SEE ALSO: tsprint
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

nargs = length(varargin);

if nargin >= 2
[nobs nvar] = size(y);
   if ~isstruct(cstruc)
   error('tsplot: requires a cal() structure as input');
   end;
end;

if nargs == 0 % no user-supplied vnames or dates
begp = 1;
endp = nobs;
nflag = 0; 
elseif nargs == 1 % no dates but vnames
begp = 1;
endp = nobs;
vnames = varargin{1};
nflag = 1; % we have user-supplied variable names
elseif nargs == 2; % we have an error
error('Wrong # of arguments to tsplot');
elseif nargs == 3
nflag = 1;
begp = varargin{1};
endp = varargin{2};
vnames = varargin{3};
end;

if nflag == 0 % no variable names supplied, make some up
  vnames = [];
  for i=1:nvar
   if i < 10
     snames = 'series  ';
     name = [snames num2str(i)];
     vnames = [vnames
               name];
     else
     snames = 'series ';
     name = [snames num2str(i)];
     vnames = [vnames
               name];
     end;
    end;
[junk nsize] = size(vnames);
else
  [vsize nsize] = size(vnames); % error check vnames argument
  if vsize ~= nvar; error('Wrong # vnames in tsplot'); end;
end;


fsize = 9;             % font size
[nobs nvar] = size(y(begp:endp,:)); % find nobs, nvar

    
if nobs <=120; % provide a grid for small samples
grid = 'on';
else
grid = 'off';
end;

freq = cstruc.freq;
  
switch freq;

     case 1,      % case of annual series 
     out = cal(cstruc.beg_yr,cstruc.beg_per,cstruc.freq,begp);
     beg_yr = out.year;
  yr = beg_yr:beg_yr+nobs-1;
  yrs = yr';
  ydigit = 'yyyy';  
  plot(datenum(yrs,1,1),y(begp:endp,:));
      legend(vnames);
   
     case 4,      % case of quarterly series
      yrs = zeros(nobs,1);
      qtr = zeros(nobs,1);
   
      out = cal(cstruc.beg_yr,cstruc.beg_per,cstruc.freq,begp);
      beg_yr = out.year;
      %beg_qtr = out.period;
% BUG fix suggested by Stephen Burke
% PhD Student
% Faculty of Commerce and Bus. Admin, Dept. of Finance
% University of British Columbia
    if out.period == 1 
         beg_qtr = 1;
    elseif out.period == 2
         beg_qtr = 4;
    elseif out.period == 3
         beg_qtr = 7;
    else
         beg_qtr = 10;
    end;


      
      for i=1:nobs;
       yrs(i,1) = beg_yr;
       qtr(i,1) = beg_qtr;
       beg_qtr = beg_qtr+3;
       if beg_qtr > 12
        beg_yr = beg_yr+1;
        beg_qtr = 1;
       end;
      end;
  ydigit = 'QQ-YY';  
  plot(datenum(yrs,qtr,1),y(begp:endp,:));
      legend(vnames);

     case 12,      % case of monthly series
      yrs = zeros(nobs,1);
      mth = zeros(nobs,1);
      out = cal(cstruc.beg_yr,cstruc.beg_per,cstruc.freq,begp);
      beg_yr = out.year;
      beg_mth = out.period;
            
      for i=1:nobs;
       yrs(i,1) = beg_yr;
       mth(i,1) = beg_mth;
       beg_mth = beg_mth+1;
       if beg_mth > 12
        beg_yr = beg_yr+1;
        beg_mth = 1;
       end;
      end;
  ydigit = 'mmmyy';  
  plot(datenum(yrs,mth,1),y(begp:endp,:));
      legend(vnames);

      otherwise % how did we get here?
      disp('frequency unknown to tsplot');

end;


set(gca,'fontsize',fsize); 
set(gca,'tickdir','in');
datetick('x',ydigit);
set(gca,'GridLineStyle',':');
set(gca,'xgrid',grid);
set(gca,'xcolor','blue');


