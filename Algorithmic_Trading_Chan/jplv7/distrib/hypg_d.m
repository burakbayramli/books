% PURPOSE: demo of Hypergeometric distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: hypg_d
%---------------------------------------------------

nobs = 1000;
n = 10;
K = 10;
N = 50; 

tic;
tst = hypg_rnd(nobs,n,K,N);
toc;


% mean should equal (n/N)*K
fprintf('mean should   = %16.8f \n',(n/N)*K);
fprintf('mean of draws = %16.8f \n',mean(tst));

hmean = (n/N)*K;

% variance should equal ((nK)/N^2*(N-1))*(N-K)*(N-n)
term1 = (n*K); term2 = (N*N)*(N-1);
term3 = (N-K)*(N-n);

fprintf('variance should   = %16.8f \n',(term1/term2)*term3);
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));

tsort = sort(tst);

pdf = hypg_pdf(tsort,n,K,N);

plot(tsort,pdf);
title(['hyperg pdf, mean=',num2str(hmean)]);
pause;

cdf = hypg_cdf(tsort,n,K,N);
plot(tsort,cdf);
title(['hyperg cdf,  mean=',num2str(hmean)]);
pause;

tst = rand(n,1);
tsort = sort(tst);

x = hypg_inv(tsort,n,K,N);
plot(tsort,x);
title(['hyperg quantiles, mean=',num2str(hmean)]);



