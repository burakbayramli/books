% PURPOSE: An example using lad(),
%                           prt_reg(),
%                           plt_reg(),
% least-absolute deviations estimation
%---------------------------------------------------
% USAGE: lad_d
%---------------------------------------------------

nobs = 100;
nvar = 5;
beta = ones(nvar,1);

randn('state',0);

xmat = randn(nobs,nvar-1);

x = [ones(nobs,1) xmat];
evec = randn(nobs,1).^4; % create leptokurtic errors

y = x*beta + evec;


% do ols regression
result = ols(y,x); 
% print the output
prt_reg(result);

% do lad regression
lresult = lad(y,x);
% print the output
prt_reg(lresult);


