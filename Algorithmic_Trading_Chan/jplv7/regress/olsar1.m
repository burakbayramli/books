function result = olsar1(y,x)
% PURPOSE: computes maximum likelihood ols regression for AR1 errors
%---------------------------------------------------
% USAGE: results = olsar1(y,x)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'olsar1'
%        results.beta  = bhat estimates 
%        results.rho   = rho estimate
%        results.tstat = t-stats
%        results.trho  = t-statistic for rho estimate
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = maximum likelihood sigma estimate
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.iter  = # of iterations
%        results.like  = log likelihood function value
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.y     = y data vector
%        results.time  = time (in seconds) for solution
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------

% do error checking on inputs
if (nargin ~= 2); error('Wrong # of arguments to olsar1'); end;

[nobs nvar] = size(x);
[nobs2 junk] = size(y);

if (nobs ~= nobs2); error('x and y must have same # obs in olsar1'); end;

xtmp = lag(x,1);
ytmp = lag(y,1);

% truncate 1st observation to feed the lag
xlag = xtmp(2:nobs,:);
ylag = ytmp(2:nobs,1);
yt = y(2:nobs,1);
xt = x(2:nobs,:);

% use cochrane-orcutt estimates as initial values
reso = olsc(y,x);

parm = zeros(nvar+2,1);
parm(1,1) = reso.rho;              % initial rho
parm(2:2+nvar-1,1) = reso.beta;    % intiial bhat's
parm(2+nvar,1) = reso.sige;        % initial sigma

oresult = maxlik('ar1_like',parm,[],y,x);
  niter = oresult.iter;
  llike = -oresult.f;
  beta = oresult.b;
  time = oresult.time;

% after convergence produce a final set of estimates using rho-value
rho = beta(1,1);
ys = y - rho*lag(y);
xs = x - rho*lag(x);
ys(1,1) = sqrt(1-rho*rho)*y(1,1);
xs(1,:) = sqrt(1-rho*rho)*x(1,:);

result = ols(ys,xs);
result.meth = 'olsar1';
% compute t-statistic for rho
varrho = (1-rho*rho)/(nobs-2);
result.trho = rho/sqrt(varrho);
result.rho = rho;
result.iter= niter;
result.like = llike;
result.time = time;
