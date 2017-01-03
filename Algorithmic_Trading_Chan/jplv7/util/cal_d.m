% PURPOSE: An example of using cal()                               
%          to associated time-series dates
%          with observation #'s
%---------------------------------------------------
% USAGE: cal_d
%---------------------------------------------------


res = cal(1982,1,4,19);
disp('cal result structure');
res

res = cal(1982,1,4,20);
disp('cal result structure');
res

dates = cal(1970,1,12);
% now find the date associated with obs=12
res = cal(dates,12);
disp('cal result structure');
res

res = cal(1982,1,12,48);
disp('cal result structure');
res

res = cal(1982,1,12,49);
disp('cal result structure');
res

res = cal(1982,1,1,9);
disp('cal result structure');
res

res = cal(1982,1,1,10);
disp('cal result structure');
res

res = cal(1982,1,1,11);
disp('cal result structure');
res
