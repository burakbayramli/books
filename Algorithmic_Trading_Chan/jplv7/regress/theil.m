function results=theil(y,x,rvec,rmat,v)
% PURPOSE: computes Theil-Goldberger mixed estimator
%          y = X B + E, E = N(0,sige*IN)
%          c = R B + U, U = N(0,v)
%---------------------------------------------------
% USAGE: results = theil(y,x,c,R,v)
% where: y    = dependent variable vector
%        x    = independent variables matrix of rank(k)
%        c    = a vector of prior mean values, (c above)
%        R    = a matrix of rank(r)            (R above)
%        v    = prior variance-covariance      (var-cov(U) above)
%---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'theil'
%          results.beta  = bhat estimates
%          results.tstat = t-statistics
%          results.pmean = prior means
%          results.pstd  = prior std deviations
%          results.yhat  = predicted values
%          results.resid = residuals
%          results.sige  = e'e/(n-k)
%          results.rsqr  = r-squared
%          results.rbar  = r-squared adjusted
%          results.dw    = Durbin Watson
%          results.nobs  = # of observations
%          results.nvar  = # of variables
%          results.y     = actual observations
% --------------------------------------------------
% SEE ALSO: prt, plt, ols_g
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if (nargin ~= 5); error('Wrong # of arguments to theil'); end;

[nobs nvar] = size(x);

results.meth = 'theil';
results.y = y;
results.nobs = nobs;
results.nvar = nvar;
[rsize junk] = size(rvec);
% fill in prior means and std deviations
results.pmean = rvec;
results.pstd  = sqrt(diag(v));

vi = inv(v);

% do ols to get sige estimate;
bols = (x'*x)\(x'*y);
sige = ((y - x*bols)'*(y-x*bols))/(nobs-nvar);
xpxi = inv(x'*x + sige*rmat'*vi*rmat);
xpy = (x'*y) + sige*rmat'*vi*rvec;
results.beta = xpxi*xpy;
results.yhat = x*results.beta;
results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = (sigu)/(nobs-nvar);
tmp = results.sige*diag(xpxi);
results.tstat = results.beta./sqrt(tmp);
ym = y - ones(nobs,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
ediff = results.resid(2:nobs) - results.resid(1:nobs-1);
results.dw = diag((ediff'*ediff)./(sigu))'; % durbin-watson


