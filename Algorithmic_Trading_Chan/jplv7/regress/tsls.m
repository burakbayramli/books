function results=tsls(y,y1,x1,xall)
% PURPOSE: computes Two-Stage Least-squares Regression
%---------------------------------------------------
% USAGE: results = tsls(y,yendog,xexog,xall)
% where: y      = dependent variable vector (nobs x 1)
%        yendog = endogenous variables matrix (nobs x g)
%        xexog  = exogenous variables matrix for this equation
%        xall   = all exogenous and lagged endogenous variables 
%                 in the system
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'tsls'
%        results.bhat  = bhat estimates
%        results.tstat = t-statistics
%        results.yhat  = yhat predicted values
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.dw    = Durbin-Watson Statistic
%        results.nobs  = nobs,
%        results.nendog = # of endogenous
%        results.nexog  = # of exogenous
%        results.nvar   = results.nendog + results.nexog
%        results.y      = y data vector
% --------------------------------------------------
% NOTE: you need to put a constant term in the x1 and xall matrices
% --------------------------------------------------
% SEE ALSO: prt_reg(results), plt_reg(results)
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if (nargin ~= 4); error('Wrong # of arguments to tsls'); end;
results.meth = 'tsls';
[nobs1 g] = size(y1);
[nobs2 k] = size(x1);
[nobs3 l] = size(xall);
results.nendog = g; results.nexog = k; results.nvar = k+g;
if nobs1 == nobs2;
 if nobs2 == nobs3
 nobs = nobs1;
 end;
else
error('tsls: # of observations in yendog, xexog, xall not the same');
end;
results.y = y; results.nobs = nobs;
% xall contains all explanatory variables
% x1 contains exogenous
% y1 contains endogenous
xapxa = inv(xall'*xall);
% form xpx
xpx = [y1'*xall*xapxa*xall'*y1     y1'*x1
       x1'*y1                      x1'*x1];
xpy = [y1'*xall*xapxa*xall'*y
       x1'*y                  ];
xpxi = inv(xpx);                
results.beta  = xpxi*xpy;             % bhat
results.yhat  = [y1 x1]*results.beta; % yhat
results.resid = y - results.yhat;     % residuals
sigu = results.resid'*results.resid;
results.sige = sigu/(nobs-k-g);       % sige
tmp = results.sige*(diag(xpxi));
results.tstat = results.beta./(sqrt(tmp));
ym = y - ones(nobs,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-k-g);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
ediff = results.resid(2:nobs) - results.resid(1:nobs-1);
results.dw = (ediff'*ediff)/sigu; % durbin-watson

