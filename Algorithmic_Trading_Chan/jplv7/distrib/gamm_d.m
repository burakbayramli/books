% PURPOSE: demo of gamma distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: gamm_d
%---------------------------------------------------

n = 1000;
a = 10;
b = 1;

tic;
tst = b*gamm_rnd3(n,a);
toc;

% mean should equal a/b
fprintf('mean should   = %16.8f \n',a/b);
fprintf('mean of draws = %16.8f \n',mean(tst));

% variance should equal a/(b^2)
fprintf('variance should   = %16.8f \n',a/(b^2));
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));


tst = unif_rnd(n,0,30);
tsort = sort(tst);

pdf = gamm_pdf(tsort,a); % mean = a
plot(tsort,pdf);
title('gamma pdf with mean=10, variance=10');
pause;

cdf = gamm_cdf(tsort,a);
plot(tsort,cdf);
title('gamma cdf with mean=10, variance=10');
pause;

tst = rand(n,1);
tsort = sort(tst);

x = gamm_inv(tsort,a);
plot(tsort,x);
title('gamma quantiles with mean=10, variance=10');

