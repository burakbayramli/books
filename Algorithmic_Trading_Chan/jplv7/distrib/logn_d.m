% PURPOSE: demo of log-normal distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: logn_d
%---------------------------------------------------


n = 1000;
a = 10;
b = 2;

tic;
tst = logn_rnd(a,b,n,1);
toc;

ltst = log(tst);

% mean should equal a
fprintf('mean should = %16.8f \n',a);
fprintf('mean of draws = %16.8f \n',mean(ltst));

% variance should equal b
fprintf('variance should = %16.8f \n',b*b);
fprintf('variance of draws = %16.8f \n',std(ltst)*std(ltst));


tst = unif_rnd(n,0.1,3);
tsort = sort(tst);

pdf = logn_pdf(tsort);
plot(tsort,pdf);
title('log-normal pdf with mean=1, var=1');
pause;

cdf = logn_cdf(tsort);
plot(tsort,cdf);
title('log-normal cdf with mean=1, var=1');
pause;

tst = rand(n,1);
tsort = sort(tst);

x = logn_inv(tsort);
plot(tsort,x);
title('log-normal quantiles with mean=1, var=1');

