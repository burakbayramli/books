% PURPOSE: An example using robust(),
%                           prt_reg(),
%                           plt_reg(),
% Iteratively re-weighted least-squares robust regression
%---------------------------------------------------
% USAGE: robust_d
%---------------------------------------------------

% generate data with 2 outliers
clear all;
nobs = 100;
nvar = 3;

x = randn(nobs,nvar);

x(:,1) = ones(nobs,1);
beta = ones(nvar,1);
evec = randn(nobs,1);

y = x*beta + evec;

% put in 2 outliers
y(75,1) = 10.0;
y(90,1) = -10.0;

% get weighting parameter from OLS
% (of course you're free to do differently)
reso = ols(y,x);
sige = reso.sige;

% set up storage for bhat results
bsave = zeros(nvar,5);
bsave(:,1) = ones(nvar,1);

% loop over all methods producing estimates
for i=1:4;

wfunc = i;
wparm = 2*sige; % set weight to 2 sigma

res = robust(y,x,wfunc,wparm);

bsave(:,i+1) = res.beta;

end;

% add ols to bsave
res = ols(y,x);
bsave(:,6) = res.beta;

nstr0 = 'true beta';
nstr1 = 'Huber  t';
nstr2 = 'Ramsay ';
nstr3 = 'Andrews';
nstr4 = 'Tukey  ';
nstr5 = 'OLS    ';

cnames = strvcat(nstr0,nstr1,nstr2,nstr3,nstr4,nstr5);
in.fmt = '%12.4f';
in.cnames = cnames;
mprint(bsave,in);

% demonstrate prt_reg and plt_reg functions

res = robust(y,x,4,2);

prt(res);

plt(res);


