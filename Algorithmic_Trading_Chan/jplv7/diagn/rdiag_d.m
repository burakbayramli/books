% PURPOSE: demo of rdiagn()
%          diagnostic plots of residuals
% 
%---------------------------------------------------
% USAGE: rdiagn_d
%---------------------------------------------------

% generate data set
n = 100;
k = 3;
x = randn(n,k);

beta = ones(k,1);

y = x*beta + randn(n,1);

rdiag(y,x);
pause;

% now add a few outliers
y(50,1) = 10.0;
y(70,1) = -10.0;

rdiag(y,x);

