function results = ridge(y,x,theta)
% PURPOSE: computes Hoerl-Kennard Ridge Regression
%---------------------------------------------------
% USAGE: results = ridge(y,x,theta)
% where: y = dependent variable vector
%        x = independent variables matrix
%    theta = an optional ridge parameter
%            (default: best theta value ala Hoerl-Kennard)
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'ridge'
%        results.beta  = bhat
%        results.theta = theta (input or HK determined value)
%        results.tstat = t-stats
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.dw    = Durbin-Watson Statistic
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.y     = y data vector
% --------------------------------------------------
% SEE ALSO: rtrace, prt_reg, plt_reg
%---------------------------------------------------
% REFERENCES: David Birkes, Yadolah Dodge, 1993, Alternative Methods of Regression    
%              Hoerl, Kennard, Baldwin, 1975 `Ridge Regression: Some 
%              Simulations', Communcations in Statistics
  
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if (nargin > 3); 
error('Wrong # of arguments to ridge');
elseif (nargin < 2); error('Wrong # of arguments to ridge');
else
 [nobs nvar] = size(x); [nobs2 junk] = size(y);
 if (nobs ~= nobs2); error('x and y must have same # obs in ridge'); 
 end;
end;

[m,n] = size(x);

results.meth = 'ridge';
results.y = y;
results.nobs = m;
results.nvar = n;

if nargin == 2
dfs = m - n - 1;
b = zeros(n,1);
[q,r] = qr(x,0); 
xpxi = (r'*r)\eye(n);
b(:,1) = xpxi*(x'*y);
ridi = diag(diag(x'*x));
dif = x*b(:,1)-y;
ssqerr = dif'*dif;
theta = n*(ssqerr/dfs)/sum((ridi.^0.5*b(:,1)).^2);
else
ridi = eye(n);
end;
xpxi = inv(x'*x + ridi*theta);
b = xpxi*(x'*y);

results.beta = b;
results.theta = theta;
results.yhat = x*results.beta;
results.resid = y - results.yhat;
results.sige = (results.resid'*results.resid)/(m-n);
sigu = results.sige*(m-n);
tmp = (results.sige)*(diag(xpxi));
results.tstat = results.beta./sqrt(tmp);
ym = y - ones(m,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(m-n);
rsqr2 = rsqr2/(m-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
ediff = results.resid(2:m) - results.resid(1:m-1);
results.dw = (ediff'*ediff)/sigu; % durbin-watson


