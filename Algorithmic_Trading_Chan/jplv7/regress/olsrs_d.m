% PURPOSE: An example using olsrs(),
%                           prt(),
%                           plt(),
% restricted ols estimation
%---------------------------------------------------
% USAGE: olsrs_d
%---------------------------------------------------

clear all;
% generate data
nobs = 100;
nvar = 5;
beta = ones(nvar,1);
beta(nvar-1,1) = 0.6;
beta(nvar,1) = 0.4;

randn('state',0);

xmat = randn(nobs,nvar-1);

x = [ones(nobs,1) xmat];
evec = randn(nobs,1);

y = x*beta + evec;

% By some theoretical presumptions, 
%the coefficients on the x3 and x4 explanatory variables
% must sum up to one. 
% Given q = R*b, this translates into the following shape of R and q.
q = 1;
R = [0 0 0 1 1];
vnames = strvcat('y','constant','x1','x2','x3','x4');
results = olsrs(y,x,R,q);
prt_reg(results,vnames);


