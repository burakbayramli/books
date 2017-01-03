% PURPOSE: demo of rec_resid()
%          Recursive residuals
% 
%---------------------------------------------------
% USAGE: rec_residd
%---------------------------------------------------

n = 100; k = 4;
tt= 1:n;
ttp = tt';
t2 = ttp.*ttp;
x = [ones(n,1) ttp];
y = t2;

fprintf(1,'Regression of time2 on time trend \n');
prt(ols(y,x));
result = cusums(y,x);

plt(result);
title('cusums test for time2 on time regression');
