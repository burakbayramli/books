function results=robust(y,x,wfunc,wparm)
% PURPOSE: robust regression using iteratively reweighted
%          least-squares
%---------------------------------------------------
% USAGE: results = robust(y,x,wfunc,wparm)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%    wfunc = 1 for Huber's t function
%            2 for Ramsay's E function
%            3 for Andrew's wave function
%            4 for Tukey's biweight
%    wparm = weighting function parameter
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'robust'
%        results.beta  = bhat
%        results.tstat = t-stats
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.dw    = Durbin-Watson Statistic
%        results.iter  = # of iterations
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.y     = y data vector
%        results.wfunc = 'huber', 'ramsay', 'andrew', 'tukey'
%        results.wparm = wparm
%        results.weight = nobs - vector of weights
% --------------------------------------------------
% SEE ALSO: prt_reg(results), plt_reg(results)
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

if (nargin ~= 4); error('Wrong # of arguments to robust'); end;

[nobs nvar] = size(x);

results.meth = 'robust';
switch wfunc
case 1
results.wfunc = 'huber';
case 2
results.wfunc = 'ramsay';
case 3
results.wfunc = 'andrew';
case 4
results.wfunc = 'tukey';
otherwise
error('unknown weighting function');
end;

results.wparm = wparm;

results.y = y;
results.nobs = nobs;
results.nvar = nvar;

% find starting values
w = ones(nobs,1);
bhat = inv(x'*x)*x'*y;
yhat = x*bhat;
resid = y - yhat;

scale = median(abs(resid - median(resid)))/.6745;

convg = 1;
cnt = 0;

while convg > 0.00001

bhato = bhat;
resid = resid/scale;
cnt = cnt+1;

 switch wfunc
 case 1 % Huber's t function
 w = (wparm*ones(nobs,1)./abs(resid));
 wi = find(abs(resid) <= wparm);
 w(wi) = ones(length(wi),1);
 
 case 2 % Ramsay's E function
 w = exp(-wparm*abs(resid));
 wi = find(resid == 0);
 w(wi) = ones(length(wi),1);
 
 case 3 % Andrew's wave function
 w = sin(resid/wparm)./(resid/wparm);
 wi = find(resid == 0);
 w(wi) = ones(length(wi),1);
 wi = find(resid > pi*wparm);
 w(wi) = zeros(length(wi),1);
 
 case 4 % Tukey's biweight
 w = (1 - (resid/wparm).^2).^2;
 wi = find(resid == 0);
 w(wi) = ones(length(wi),1);
 wi = find(abs(resid) > wparm);
 w(wi) = zeros(length(wi),1);
 
 otherwise
 error('incorrect weight function option');
 
 end;
 
% do weighted least-squares
    ystar = y.*sqrt(w);
    xstar = matmul(x,sqrt(w));
    bhat = inv(xstar'*xstar)*xstar'*ystar;
    resid = y - x*bhat;
    
% check for convergence   
   convg = max(abs(bhat-bhato)./abs(bhato));
   

end; % end of while loop 

results.iter = cnt;
results.weight = w;
results.convg = convg;
results.beta = bhat;
results.yhat = x*results.beta;
results.resid = y - results.yhat;
sigu = results.resid'*results.resid;
results.sige = sigu/(nobs-nvar);
tmp = (results.sige)*(diag(inv(xstar'*xstar)));
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