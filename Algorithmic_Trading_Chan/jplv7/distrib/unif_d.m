% PURPOSE: demo of uniform distribution functions
%          prints mean and variance of 1000 draws
%          plots pdf,cdf,inverse
% 
%---------------------------------------------------
% USAGE: unif_d
%---------------------------------------------------

n = 1000;
a = 5;
b = 10;

tic;
tst = unif_rnd(n,a,b);
toc;

% mean should equal (a+b)/2
fprintf('mean should = %16.8f \n',(a+b)/2);
fprintf('mean of draws = %16.8f \n',mean(tst));

% variance should equal (b-a)*(b-a)/12
fprintf('variance should = %16.8f \n',(b-a)*(b-a)/12);
fprintf('variance of draws = %16.8f \n',std(tst)*std(tst));


