% PURPOSE: An example using ridge(), bkw()
%                           prt_reg(),
%                           rtrace(),
% ridge regression, collinearity diagnostics
% and ridge trace
%---------------------------------------------------
% USAGE: ridge_d
%---------------------------------------------------

clear all;
% generate collinear data set
n = 100;
k = 3;
x = randn(n,k);
x(:,1) = ones(n,1);
x(:,3) = x(:,2) + randn(n,1)*0.05;

beta = ones(k,1);

y = x*beta + randn(n,1);


bkw(x);

vnames = ['y    ',
          'const',
          'x1   ',
          'x2   '];
          
          
res = ols(y,x);

prt(res,vnames);

rres = ridge(y,x);

prt(rres,vnames);

