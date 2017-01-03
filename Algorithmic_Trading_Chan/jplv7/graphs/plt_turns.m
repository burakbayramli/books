function plt_turns(results,cstruc,vname)
% PURPOSE: plot turning points in a time series
%          using results structure variable from fturns()
% -----------------------------------------------------
% USAGE:      plt_turns(results,cstruct,vname)
% where:    results = a results structure variable returned
%                     by the fturns() function
%           cstruct = (optional) calendar structure variable from cal()
%           vname   = name for time-series in plot legend
%           e.g. plt(fturns(y))
%                dates = cal(1982,1,12);
%                plt(fturns(y),dates);
%                plt(fturns(y),dates,'gdp-series');
% -----------------------------------------------------
% RETURNS:nothing, simply plots time-series with turns denoted
% ------------------------------------------------------
% SEE ALSO: plt, fturns, cal
          
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% error checking on inputs

tsflag = 0; vflag = 0; % defaults
if nargin == 1 % don't use tsplot
  if ~isstruct(results)
    error('plt_turns: requires a results structure from fturns');
  end;    
% use defaults
elseif nargin == 2 % use tsplot
  if ~isstruct(cstruc)
    error('plt_turns: requires a cal structure for 2nd argument');
  end;     
tsflag = 1;
elseif nargin == 3 % we have a vname
vflag = 1;
tsflag = 1;
else
error('Wrong # of arguments to plt_turns');
end;

y = results.y;
ut = results.ut;
dt = results.dt;
nobs = length(y);

yut = find(ut);
ydt = find(dt);

switch tsflag

case{0} % use plot
tt=1:nobs;
plot(tt,y,'r-',yut,y(yut,1),'k^',ydt,y(ydt,1),'kv');
if vflag == 0
 legend('time-series','upturn','downturn');
else
 legend(vname,'upturn','downturn');
end;


case{1} % use tsplot

begp = 1;
endp = nobs;
freq = cstruc.freq;
  
 switch freq;

     case 1,      % case of annual series 
     out = cal(cstruc.beg_yr,cstruc.beg_per,cstruc.freq,begp);
      yut = []; xut = [];
      ydt = []; xdt = [];     
     beg_yr = out.year;
  yr = beg_yr:beg_yr+nobs-1;
  yrs = yr';
  cntu = 0;
  cntd = 0;
        for i=1:nobs;
             if ut(i,1) == 1
            cntu = cntu+1;
                yut(cntu,1) = y(i,1);
                xut(cntu,1) = datenum(yrs(i,1),1,1);
            elseif dt(i,1) == 1
            cntd = cntd+1;
                ydt(cntd,1) = y(i,1);
                xdt(cntd,1) = datenum(yrs(i,1),1,1);
            end;           
      end;        
  ydigit = 'yyyy';  
  plot(datenum(yrs,1,1),y(begp:endp,:));
        hold on;
        plot(xut,yut,'^k');
        plot(xdt,ydt,'vk');
        if vflag == 0
         legend('time-series','upturn','downturn');
        else
         legend(vname,'upturn','downturn');
        end;  
        fsize = 9;             % font size
        datetick('x',ydigit);
        set(gca,'fontsize',fsize); 
        set(gca,'tickdir','in');          
        hold off;
   
     case 4,      % case of quarterly series
      yrs = zeros(nobs,1);
      qtr = zeros(nobs,1);
   
      out = cal(cstruc.beg_yr,cstruc.beg_per,cstruc.freq,begp);
      beg_yr = out.year;
      beg_qtr = out.period;
      yut = []; xut = [];
      ydt = []; xdt = [];
        cntu = 0;
        cntd = 0;

      for i=1:nobs;
       yrs(i,1) = beg_yr;
       qtr(i,1) = beg_qtr;
       beg_qtr = beg_qtr+1;
       if beg_qtr > 4
        beg_yr = beg_yr+1;
        beg_qtr = 1;
       end;
             if ut(i,1) == 1
            cntu = cntu+1;
                yut(cntu,1) = y(i,1);
                xut(cntu,1) = datenum(yrs(i,1),qtr(i,1),1);
            elseif dt(i,1) == 1
            cntd = cntd+1;
                ydt(cntd,1) = y(i,1);
                xdt(cntd,1) = datenum(yrs(i,1),qtr(i,1),1);
            end;           
      end;
  ydigit = 'QQ-YY';  
  plot(datenum(yrs,qtr,1),y(begp:endp,:));
        hold on;
        plot(xut,yut,'^k');
        plot(xdt,ydt,'vk');
        if vflag == 0
         legend('time-series','upturn','downturn');
        else
         legend(vname,'upturn','downturn');
        end; 
        fsize = 9;             % font size
        datetick('x',ydigit);
        set(gca,'fontsize',fsize); 
        set(gca,'tickdir','in');          
        hold off;

     case 12,      % case of monthly series
      yrs = zeros(nobs,1); yut = []; xut = [];
      mth = zeros(nobs,1); ydt = []; xdt = [];
      out = cal(cstruc.beg_yr,cstruc.beg_per,cstruc.freq,begp);
      beg_yr = out.year;
      beg_mth = out.period;
      cntu = 0;
      cntd = 0;
      for i=1:nobs;
       yrs(i,1) = beg_yr;
       mth(i,1) = beg_mth;
       beg_mth = beg_mth+1;
       if beg_mth > 12
        beg_yr = beg_yr+1;
        beg_mth = 1;
       end;
            if ut(i,1) == 1
            cntu = cntu+1;
                yut(cntu,1) = y(i,1);
                xut(cntu,1) = datenum(yrs(i,1),mth(i,1),1);
            elseif dt(i,1) == 1
            cntd = cntd+1;
                ydt(cntd,1) = y(i,1);
                xdt(cntd,1) = datenum(yrs(i,1),mth(i,1),1);
            end;
      end;
  ydigit = 'mmmyy';
  plot(datenum(yrs,mth,1),y);
        hold on;
        plot(xut,yut,'^k');
        plot(xdt,ydt,'vk');
        if vflag == 0
         legend('time-series','upturn','downturn');
        else
         legend(vname,'upturn','downturn');
        end;
        fsize = 9;             % font size
        datetick('x',ydigit);
        set(gca,'fontsize',fsize); 
        set(gca,'tickdir','in');        
        hold off;

      otherwise % how did we get here?
      disp('frequency unknown to tsplot');
 end; % end of switch freq

otherwise
 
end; % end of switch tsflag
