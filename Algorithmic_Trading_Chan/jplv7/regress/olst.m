function results = olst(y,x,maxit,crit)
% PURPOSE: ols with t-distributed errors
% --------------------------------------------------
% USAGE: results = olst(y,x,itmax,convg)
% where:       y = dependent variable vector (nobs x 1)
%              x = explanatory variables matrix (nobs x nvar)
%          itmax = maximum # of iterations (default=500)
%          convg = convergence criterion (default = 1e-8)
% --------------------------------------------------
% RETURNS: a structure
%        results.meth = 'olst'
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
% NOTES: uses iterated re-weighted least-squares 
%        to find maximum likelihood estimates
% --------------------------------------------------
% SEE ALSO: prt_reg(results), plt_reg(results)
%---------------------------------------------------
% REFERENCES: Section 22.3 Introduction to the Theory and Practice
%             of Econometrics, Judge, Hill, Griffiths, Lutkepohl, Lee

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 2
crit = 1e-8;
maxit = 500;
elseif nargin == 3
crit = 1e-8;
elseif nargin == 4
% do nothing
else
error('Wrong # of arguments to olst');
end;
[nobs nvar] = size(x);
      ores = ols(y,x);
      iota = ones(nobs,1);
      bmle = ores.beta; % use ols starting values
      iter = 1;
      nu = 1;
      conv = max(abs(bmle));
          while (conv > crit) & (iter <= maxit);
              emle = y - x*bmle;
              sige2 = (emle'*emle)/nobs;
              nusig2 = (nu+1)*sige2;
              wt = iota+(emle.*emle)/nusig2;
              wx = matdiv(x,wt);
              wy = y./wt;
              b_new = inv(x'*wx)*(x'*wy);
              conv = max(abs(bmle-b_new));
              bmle = b_new;
              iter = iter+1;
          end;

results.meth = 'olst';
results.beta = bmle;
results.weight = wt;
results.y = y;
results.nobs = nobs;
results.nvar = nvar;
results.yhat = x*results.beta;
results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/nobs;
covb = ((nu+3)*nusig2/(nu+1))*inv(x'*x);
tmp = diag(covb);
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

