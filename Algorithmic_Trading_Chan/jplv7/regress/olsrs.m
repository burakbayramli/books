function results = olsrs(y,x,R,q)
% PURPOSE: Restricted least-squares estimation
% y = Xb + e with the constraint that q = Rb
%---------------------------------------------------
% USAGE: results = olsrs(y,x,R,q)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%        R = restriction matrix (h x nvar) where h < nvar 
%            h corresponds to the number of linear restrictions
%        q = vector (h x 1);
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'olsrs'
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
%---------------------------------------------------
% SEE ALSO: prt(results), plt(results)
%---------------------------------------------------
% Reference: Greene, William H. (2000), page 281
%
% Daniel Gross
% University of Frankfurt
% Email: dgross@wiwi.uni-frankfurt.de
%

if (nargin ~= 4); 
   error('Wrong # of arguments to olsrs'); 
else
   [nobs nvar]  = size(x); 
   [nobs2 junk] = size(y);
   [roR colR]   = size(R);
   [roq colq]   = size(q);
   rankR        = rank(R);
   if (nobs ~= nobs2); 
      error('x and y must have same # obs in olsrs'); 
   end;
   if (roR >= nvar);
      error('Too much restrictions imposed');
   end;
   if (roq ~= roR);
      error('# of rows in q and R must be the same');
   end;
   if (rankR~=roR);
      error('R must have full rank !');
   end;   
end;

results.meth = 'olsrs';
results.y = y;
results.nobs = nobs;
results.nvar = nvar;

bols          = inv(x'*x)*x'*y;                                       % OlS estimator
results.beta  = bols + inv(x'*x)*R'*inv(R*inv(x'*x)*R')*(q - R*bols); % Restricted OLS estimator
results.yhat  = x*results.beta;
results.resid = y - results.yhat;
sigu          = results.resid'*results.resid;
results.sige  = sigu/(nobs-nvar);
bvar          = (results.sige)*inv(x'*x)*(eye(nvar)-R'*inv(R*inv(x'*x)*R')*R*inv(x'*x));
tmp           = diag(bvar);
results.stee  = sqrt(tmp);                                             % Standard Error of Estimator
results.tstat = results.beta./(sqrt(tmp));
ym            = y - mean(y);
rsqr1         = sigu;
rsqr2         = ym'*ym;
results.rsqr  = 1.0 - rsqr1/rsqr2;                                     % r-squared
rsqr1         = rsqr1/(nobs-nvar);
rsqr2         = rsqr2/(nobs-1.0);
results.rbar  = 1 - (rsqr1/rsqr2);                                     % rbar-squared
ediff         = results.resid(2:nobs) - results.resid(1:nobs-1);
results.dw    = (ediff'*ediff)/sigu;                                   % durbin-watson
results.R     = R;
results.q     = q;
