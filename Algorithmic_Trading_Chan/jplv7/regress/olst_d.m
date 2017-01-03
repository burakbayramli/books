% PURPOSE: An example using olst(),
%                           prt_reg(),
%                           plt_reg(),
% ols with t-distributed errors estimation
%---------------------------------------------------
% USAGE: olst_d
%---------------------------------------------------

nobs = 100;
nvar = 5;
beta = ones(nvar,1);

randn('state',0);

xmat = randn(nobs,nvar-1);

x = [ones(nobs,1) xmat];
evec = tdis_rnd(nobs,2)*0.5; % generate t-distributed errors

y = x*beta + evec;


% do ols regression
result = ols(y,x); 
% print the output
prt_reg(result);

% do robust t-distributed errors regression
lresult = olst(y,x,1000,1e-4);
% print the output
prt_reg(lresult);


