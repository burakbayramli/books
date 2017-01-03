% PURPOSE: demo of arch() test for ARCH(p)
% 
%---------------------------------------------------
% USAGE: arch_d
%---------------------------------------------------

% generate heteroscedastic regression model
nobs = 100; nvar = 3;
x = randn(nobs,nvar);
b = ones(nvar,1);
tt = 1:nobs; 
tt = tt';
y = x*b + randn(nobs,1).*tt;

% do regression
result = ols(y,x);

% call arch() function using ols residuals
% and a vector of orders 1,2
orders = [1 2];
[archstat,pval] = arch(result.resid,orders);

% print out results
disp('Arch test results');
info.cnames = strvcat('Arch stats','p-values');
info.rnames = strvcat('Order','Order 1','Order 2');
mprint([archstat' pval'],info);
