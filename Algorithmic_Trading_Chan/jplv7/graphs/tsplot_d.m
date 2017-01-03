% PURPOSE: demo of tsplot (time-series plot)
%          
% 
%---------------------------------------------------
% USAGE: tsplot_d
%---------------------------------------------------

% generate monthly data
n = 120;
y = randn(n,1);

cstr = cal(1970,1,12);

tsplot(y,cstr,'y-monthly');
pause;

% generate quarterly data
n = 80;
y = randn(n,1);

cstr = cal(1960,1,4);

% find 1970,1
begp = ical(1970,1,cstr);
% find 1979,4
endp = ical(1979,4,cstr);

tsplot(y,cstr,begp,endp,'y-quarterly');
pause;

% generate multiple time-series

n = 40;
y = randn(n,3);

vname = ['y1',
         'y2',
         'y3'];

cstr = cal(1980,1,12);
% plot the entire series
tsplot(y,cstr,vname);
pause;

% general annual time-series
n = 60;
y = randn(n,1);

cstr = cal(1940,1,1);
% find 1950
begp = ical(1950,1,cstr);
endp = 60;

tsplot(y,cstr,begp,endp,'y-annual');
pause;

