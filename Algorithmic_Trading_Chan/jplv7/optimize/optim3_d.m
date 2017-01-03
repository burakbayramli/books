% PURPOSE: An example of optimization
% solving a time-varying parameter model
% from Kim and Nelson pages 44-49
%                          
%---------------------------------------------------
% USAGE: optim3_d
%---------------------------------------------------

clear all;

load tvpmoney.data;

% column 1 = quarter index
%  2: m1===growth rate of quarterly average M1
%  3: dint=change in the lagged interest rate (3-month T-bill)
%  4: inf==lagged inflation
%  5: surpl==lagged full employment budget surplus
%  6: m1lag==lag of m1
%     1959.3--1987.4, 

y = tvpmoney(:,2);
n = length(y);
x = [ones(n,1) tvpmoney(:,3:6)];

nparm = 6; % # of parameters to be estimated
ntvp = 5;  % # of tvp parameters
% initial values
parm = [0.5
        0.1
        0.1
        0.1
        0.1
        0.1];
            

% solve using minz routine
infoz.call = 'other';
infoz.maxit = 500;
infoz.prt = 1;
result1 = maxlik('tvp_like1',parm,infoz,y,x); 
niter1   = result1.iter;
like1    = result1.f;
hess1    = result1.hess;
parm1 = result1.b;
  
% solve using frpr_min
info.maxit = 500;
info.prt = 1;
result2 = frpr_min('tvp_like1',parm,info,y,x); 
parm2 = result2.b;
niter2   = result2.iter;
like2    = result2.f;
hess2    = result2.hess;

% solve using pow_min routine for comparison
info.maxit = 1000;
info.prt = 1;
result3 = pow_min('tvp_like1',parm,info,y,x);
parm3 = result3.b;
niter3   = result3.iter;
like3    = result3.f;
hess3    = result3.hess;


% solve using dfp_min routine for comparison
info.maxit = 1000;
info.prt = 1;
result4 = dfp_min('tvp_like1',parm,info,y,x);
parm4 = result4.b;
niter4   = result4.iter;
like4    = result4.f;
hess4    = result4.hess;


[beta ferror] = tvp_beta(parm1,y,x);

% see Figures 3.9 - 3.14
tt=11:n;
plot(tt,beta(tt,1));
pause;
plot(tt,beta(tt,2));
pause;
plot(tt,beta(tt,3));
pause;
plot(tt,beta(tt,4));
pause;
plot(tt,beta(tt,5));
pause;

plot(tt,ferror(tt,1));
title('forecast error');
pause;

plot(tt,ferror(tt,2));
title('conditional variance');
pause;


% compare to the estimates reported in Table 3.2
in.fmt = '%9.3f'; fprintf(1,'estimates \n');
cnames = strvcat('maxlik','frpr_min','pow_min','dfp_min');
in.cnames = cnames;
mprint(abs([parm1 parm2 parm3 parm4]),in);
fprintf(1,'log likelihood function values \n');
mprint([like1 like2 like3 like4],in);
in2.fmt = '%12.3f';
fprintf(1,'hessians \n');
mprint(hess1,in2);
mprint(hess2,in2);
mprint(hess3,in2);
mprint(hess4,in2);

in.fmt = '%8d';
fprintf(1,'comparison of # of iterations \n');
mprint([niter1 niter2 niter3 niter4 ],in);

in.fmt = '%9.1f';

fprintf(1,'time taken \n');
mprint([result1.time result2.time result3.time result4.time],in);

