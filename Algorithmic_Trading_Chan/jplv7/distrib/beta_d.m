% PURPOSE: demo of beta distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: beta_d
%---------------------------------------------------

n = 1000;
a = 10;
b = 5;

tic;
tst = beta_rnd(n,a,b);
toc;

% mean should equal a/(a+b)
fprintf('mean should   = %16.8f \n',a/(a+b));
fprintf('mean of draws = %16.8f \n',mean(tst));

% variance should equal a*b/((a+b)*(a+b)*(a+b+1))
fprintf('variance should   = %16.8f \n',(a*b)/((a+b)*(a+b)*(a+b+1)));
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));

tst = rand(n,1);
tsort = sort(tst);

pdf = beta_pdf(tsort,a,b);

plot(tsort,pdf);
title('pdf of beta distribution');
pause;

cdf = beta_cdf(tsort,a,b);
plot(tsort,cdf);
title('cdf of beta distribution');
pause;

x = beta_inv(tsort,a,b);
plot(tsort,x);
title('inverse of beta distribution');


