% PURPOSE: demo of logistic distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: logt_d
%---------------------------------------------------



n = 1000;

tic;
tst = logt_rnd(n,1);
toc;


% mean should equal 0
fprintf('mean should = %16.8f \n',0);
fprintf('mean of draws = %16.8f \n',mean(tst));


tst = unif_rnd(n,-5,5);
tsort = sort(tst);

pdf = logt_pdf(tsort);
plot(tsort,pdf);
title('logistic pdf over -5 to 5');
pause;

cdf = logt_cdf(tsort);
plot(tsort,cdf);
title('logistic cdf over -5 to 5');
pause;

tst = rand(n,1);
tsort = sort(tst);

x = logt_inv(tsort);
plot(tsort,x);
title('logistic quantiles');

