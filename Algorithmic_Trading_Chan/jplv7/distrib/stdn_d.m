% PURPOSE: demo of standard normal distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: stdn_d
%---------------------------------------------------

n = 1000;
a = 10;
b = 2;

tic;
tst = randn(n,1);
toc;


% mean should equal 0
fprintf('mean should = %16.8f \n',0);
fprintf('mean of draws = %16.8f \n',mean(tst));

% variance should equal 1
fprintf('variance should = %16.8f \n',1);
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));


tst = unif_rnd(n,-3,3);
tsort = sort(tst);

pdf = stdn_pdf(tsort);
plot(tsort,pdf);
title('std-normal pdf with mean=0, var=1');
pause;

cdf = stdn_cdf(tsort);
plot(tsort,cdf);
title('std-normal cdf with mean=0, var=1');
pause;

tst = rand(n,1);
tsort = sort(tst);

x = stdn_inv(tsort);
plot(tsort,x);
title('std-normal quantiles with mean=0, var=1');

