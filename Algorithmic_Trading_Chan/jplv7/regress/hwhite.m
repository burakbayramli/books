function results=hwhite(y,x)
% PURPOSE: computes White's adjusted heteroscedastic
%          consistent Least-squares Regression
%---------------------------------------------------
% USAGE: results = hwhite(y,x)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'ols'
%        results.beta  = bhat
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
% NOTES: uses function mcov() included in this file
% --------------------------------------------------
% SEE ALSO: hwhite_d, prt(results), plt(results)
%---------------------------------------------------
% References: H. White 1980, Econometrica Vol. 48 pp. 818-838. 
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if (nargin ~= 2); error('Wrong # of arguments to white'); end;

[nobs nvar] = size(x);

results.meth    = 'hwhite';
results.y       = y;
results.nobs    = nobs;
results.nvar    = nvar;

r = triu(qr(x,0));
xpxi = (r'*r)\eye(nvar);

results.beta    = xpxi*(x'*y);
results.yhat    = x*results.beta;
results.resid   = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige    = sigu/(nobs-nvar);

% perform White's correction
xuux = mcov(x,results.resid);
xpxia = xpxi*xuux*xpxi;
tmp = sqrt(diag(xpxia));
results.tstat = results.beta./tmp;
ym = y - ones(nobs,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
ediff = results.resid(2:nobs) - results.resid(1:nobs-1);
results.dw = diag((ediff'*ediff)./(sigu))'; % durbin-watson



function xuux = mcov(x,u)
% PURPOSE: computes x'*u*u'*x 
%----------------------------------------------------------------
% USAGE:   xuux = mcov(x,u);
% where: x = nobs x nvar explanatory variables matrix
%        u = nobs x 1 residuals
%----------------------------------------------------------------
% RETURNS: xuux such that xpx-inverse*xuux*xpx-inverse
%          represents a heteroscedasticity consistent vcv matrix
%----------------------------------------------------------------
% References: H. White 1980, Econometrica Vol. 48 pp. 818-838. 
%----------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if nargin ~= 2
error('Wrong # of arguments to mcov');
end;

[nobs nvar] = size(x);

xuux = zeros(nvar,nvar);

for i=1:nobs;
xp = x(i,:);
xpx = xp'*xp;
upu = u(i,1)*u(i,1);
xuux = xuux + upu*xpx;
end;

