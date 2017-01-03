% PURPOSE: demo using waldf() 
%                       
% computes Wald F-test for two regressions
%---------------------------------------------------
% USAGE: waldf_d
%---------------------------------------------------

clear all;

rand('seed',10);
n = 100; k=6;
x = randn(n,k);
e = randn(n,1);    
b = ones(k,1);
iota = ones(n,1);
x(:,1) = iota;
% generate y1 data vector using all 6 x's
y = x*b + e;
% create x-matrix based on only 4 x's
xsmall = x(:,1:4);

% unrestricted regression
resu = ols(y,x);

% restricted regression
resr = ols(y,xsmall);

% Wald-test of the restriction
% prints results to command window
waldf(resr,resu);

[fstat prob] = waldf(resr,resu);