function result = olsc(y,x)
% PURPOSE: computes Cochrane-Orcutt ols Regression for AR1 errors
%---------------------------------------------------
% USAGE: results = olsc(y,x)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'olsc'
%        results.beta  = bhat estimates
%        results.rho   = rho estimate
%        results.tstat = t-stats
%        results.trho  = t-statistic for rho estimate
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.iter  = niter x 3 matrix of [rho converg iteration#]
%        results.nobs  = nobs
%        results.nvar  = nvars
%        results.y     = y data vector
% --------------------------------------------------
% SEE ALSO: prt_reg(results), plt_reg(results)
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
%jlesage@spatial-econometrics.com
% do error checking on inputs
if (nargin ~= 2); error('Wrong # of arguments to olsc'); end;

[nobs nvar] = size(x);
[nobs2 junk] = size(y);

if (nobs ~= nobs2); error('x and y must have same # obs in olsc'); end;

% ----- setup parameters
ITERMAX = 100;
converg = 1.0;
rho = 0.0;
iter = 1;
xtmp = lag(x,1);
ytmp = lag(y,1);

% truncate 1st observation to feed the lag
xlag = xtmp(2:nobs,:);
ylag = ytmp(2:nobs,1);
yt = y(2:nobs,1);
xt = x(2:nobs,:);
       
% setup storage for iteration results
iterout = zeros(ITERMAX,3);

upperlimit = 1 - 1e-4;
while (converg > 0.0001) && (iter < ITERMAX)
% step 1, using intial rho = 0, do OLS to get bhat
 ystar = yt - rho*ylag;
 xstar = xt - rho*xlag;
 
res = ols(ystar,xstar);

e = y - x*res.beta;
elag = lag(e);

% truncate 1st observation to account for the lag
et = e(2:nobs,1);
elagt = elag(2:nobs,1);

% step 2, update estimate of rho using residuals
%         from step 1

res_rho = ols(et,elagt);
rho_last = rho;
rho = res_rho.beta(1);

if rho > upperlimit
    rho = rho_last;
    converg = abs(rho - rho_last);
    iterout(iter,1) = rho;
    iterout(iter,2) = converg;
    iterout(iter,3) = iter;
    break;
end

converg = abs(rho - rho_last);

iterout(iter,1) = rho;
iterout(iter,2) = converg;
iterout(iter,3) = iter;

iter = iter + 1;

end; % end of while loop

if iter == ITERMAX
 error('ols_corc did not converge in 100 iterations');
end;

result.iter= iterout(1:iter-1,:);

% after convergence produce a final set of estimates using rho-value
ystar = yt - rho*ylag;
xstar = xt - rho*xlag;

result = ols(ystar,xstar);
result.meth = 'olsc';
result.rho = rho;
result.iter = iterout(1:iter-1,:);
% compute t-statistic for rho
varrho = (1-rho*rho)/(nobs-2);
result.trho = rho/sqrt(varrho);




