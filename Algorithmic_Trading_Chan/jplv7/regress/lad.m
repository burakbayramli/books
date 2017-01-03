function results = lad(y,x,maxit,crit)
% PURPOSE: least absolute deviations regression
% --------------------------------------------------
% USAGE: results = lad(y,x,itmax,convg)
% where:       y = dependent variable vector (nobs x 1)
%              x = explanatory variables matrix (nobs x nvar)
%          itmax = maximum # of iterations (default=500)
%          convg = convergence criterion (default = 1e-15)
% --------------------------------------------------
% RETURNS: a structure
%        results.meth = 'lad'
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
%        results.iter  = # of iterations
%        results.conv  = convergence max(abs(bnew-bold))
% --------------------------------------------------
% NOTES: minimizes sum(abs(y - x*b)) using re-iterated weighted
%        least-squares where the weights are the inverse of 
%        the absolute values of the residuals
% --------------------------------------------------
% SEE ALSO: prt_reg(results), plt_reg(results)
%---------------------------------------------------
         
% Author: Ron Schoenberg rons@u.washington.edu
% Date: May 29, 1995
% converted from Gauss code to MATLAB by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 2
crit = 1e-15;
maxit = 500;
elseif nargin == 3
crit = 1e-15;
elseif nargin == 4
% do nothing
else
error('Wrong # of arguments to lad');
end;
[nobs nvar] = size(x);
      b_old = zeros(nvar,1); % starting values
      b_new = ones(nvar,1);
      iter = 0;
      w = x;
      conv = max(abs(b_new-b_old));
          while (conv > crit) & (iter <= maxit);
          b_old=b_new;
          b_new = invpd(w'*x)*(w'*y);
          resid = (abs(y-x*b_new));
          ind = find(resid < 0.00001);
          resid(ind) = 0.00001;   
          w = matdiv(x,resid);       
          iter = iter+1;
          conv = max(abs(b_new-b_old));
          end;
results.meth = 'lad';
results.beta = b_new;
results.y = y;
results.nobs = nobs;
results.nvar = nvar;
results.yhat = x*results.beta;
results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/(nobs-nvar);
tmp = (results.sige)*(diag(inv(w'*x)));
results.tstat = results.beta./(sqrt(tmp));
ym = y - ones(nobs,1)*mean(y);
rsqr1 = sigu;
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
ediff = results.resid(2:nobs) - results.resid(1:nobs-1);
results.dw = (ediff'*ediff)/sigu'; % durbin-watson
results.iter = iter;
results.conv = conv;
results.weight = ones(nobs,1)./resid;

