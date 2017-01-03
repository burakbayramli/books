% PURPOSE: demo of chis-squared distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: chis_d
%---------------------------------------------------

n = 1000;
a = 10;

tic;
tst = chis_rnd(n,a);
toc;

% mean should equal a
fprintf('mean should   = %16.8f \n',a);
fprintf('mean of draws = %16.8f \n',mean(tst));

% variance should equal 2*a)
fprintf('variance should   = %16.8f \n',2*a);
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));

tst = unif_rnd(n,0.1,30);
tsort = sort(tst);

pdf = chis_pdf(tsort,a);

plot(tsort,pdf);
title('chi-squared pdf dof = 10');
pause;

cdf = chis_cdf(tsort,a);
plot(tsort,cdf);
title('chi-squared cdf dof = 10');
pause;

tst = rand(n,1);
tsort = sort(tst);

x = chis_inv(tsort,a);
plot(tsort,x);
title('chi-squared quantiles dof = 10');

