% PURPOSE: demo of Student t-distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: tdis_d
%---------------------------------------------------

n = 1000;
a = 10;


tic;
tst = tdis_rnd(n,a);
toc;

% mean should equal 0 for large a
fprintf('mean should   = %16.8f \n',0);
fprintf('mean of draws = %16.8f \n',mean(tst));

% variance should equal (1/(a-2))*a;
fprintf('variance should   = %16.8f \n',(1/(a-2))*a);
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));


tst = unif_rnd(n,-3,3);
tsort = sort(tst);

pdf = tdis_pdf(tsort,a);
plot(tsort,pdf);
title('T pdf with a=10');
pause;

cdf = tdis_cdf(tsort,a);
plot(tsort,cdf);
title('T cdf with a=10');
pause;

tst = rand(n,1);
tsort = sort(tst);

x = tdis_inv(tsort,a);
plot(tsort,x);
title('T quantiles with a=10');

