% PURPOSE: An example of using ical()                               
%          to find observation #'s
%          associated with time-series dates
%---------------------------------------------------
% USAGE: ical_d
%---------------------------------------------------


res = cal(1982,1,4,19);

ires = ical(1986,1,res);
disp('ical result structure');
ires

res = cal(1982,1,12);

ires = ical(1985,12,res);
disp('ical result structure');
ires

ires = ical(1982,1,cal(1982,1,4));
disp('ical result structure');
ires

ires = ical(1982,3,cal(1982,1,4));
disp('ical result structure');
ires
