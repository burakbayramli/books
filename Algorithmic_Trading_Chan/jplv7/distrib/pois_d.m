% PURPOSE: demo of poisson distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: pois_d
%---------------------------------------------------

n = 1000;
lam = 10;

tic;
tst = pois_rnd(n,lam);
toc;

% mean should equal lam
fprintf('mean should = %16.8f \n',lam);
fprintf('mean of draws = %16.8f \n',mean(tst));
fprintf('variance should = %16.8f \n',lam);
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));

tst = round(unif_rnd(n,1,20));
tsts = sort(tst);

pdf = pois_pdf(tsts,lam);
plot(tsts,pdf);
title('poisson pdf over 1 to 20 with mean=10');
pause;

cdf = pois_cdf(tsts,lam);
plot(tsts,cdf);
title('poisson cdf over 1 to 20 with mean=10');
pause;

pinv = pois_inv(cdf,lam);
plot(tsts,pinv);
title('poisson quantiles over 1 to 20 with mean=10');
