function results = tobit(y,x,info)
% PURPOSE: computes Tobit Regression
%---------------------------------------------------
% USAGE: results = tobit(y,x,info)
% where: y = censored dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%     info = a structure with options:
%        info.trunc = 'left' or 'right' for censoring (default=left)
%        info.limit = value for censoring (default=0)
%        info.meth  = Hessian: ['dfp'], 'bfgs', 'gn', 'marq', 'sd'
%        info.btol  =  tolerance for b convergence (default = 1e-8)
%        info.ftol  = tolerance for FUN convergence (default = 1e-8)
%        info.maxit = maximum # of iterations (default = 500)
%        info.b     = starting values for parameters (default = ols)
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'tobit'
%        results.beta  = bhat
%        results.tstat = t-stats
%        results.yhat  = yhat
%        results.resid = residuals
%        results.sige  = e'*e/(n-k)
%        results.rsqr  = rsquared
%        results.rbar  = rbar-squared
%        results.lik   = Log-Likelihood value
%        results.iter  = # of iterations taken
%        results.grad  = gradient at solution
%        results.opt   = method of optimization used
%        results.nobs  = nobs
%        results.nobsc = # of censored observations
%        results.nvar  = nvars
%        results.y     = y data vector
% --------------------------------------------------
% SEE ALSO: maxlik, prt(results), plt(results), logit, probit
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


% default options
in.btol = 1e-6;
in.ftol = 1e-7; 
in.maxit = 1000;
in.hess = 'bfgs';
in.call = 'other';
[nobs nvar] = size(x);

if nargin == 3 % user supplied options
  if ~isstruct(info)
    error('tobit: options should be in a structure variable');
  end;

sflag = 0;
tflag = 0;
vflag = 0;
% parse options
fields = fieldnames(info);
nf = length(fields); 
  for i=1:nf
    if strcmp(fields{i},'maxit')
        in.maxit = info.maxit; 
    elseif strcmp(fields{i},'btol')
        in.btol = info.btol;
    elseif strcmp(fields{i},'ftol')
        in.ftol = info.ftol;
    elseif strcmp(fields{i},'meth')
        in.hess = info.meth;
     elseif strcmp(fields{i},'b');
      baug = b; sflag = 1;
     elseif strcmp(fields{i},'trunc');
      if strcmp(info.trunc,'left');
       tflag = 0;
      else
       tflag = 1;
      end;
      elseif strcmp(fields{i},'limit');
        vflag = info.limit;      
    end;
  end;

elseif nargin == 2 
sflag = 0; % use default options
in.call = 'other';
tflag = 0;
vflag = 0;
else
error('tobit: wrong # of arguments');
end;


[nobs nvar] = size(x);

% find # of censored observations
if tflag == 1
   results.nobsc = length(find(y >= vflag));
else
   results.nobsc = length(find(y <= vflag));
end;

   
if sflag == 0
% use ols starting values
res = ols(y,x);
b = res.beta;
sige = res.sige;
baug = [b
       sige];
    end;
 
% maximize the likelihood function
if tflag == 0 % case of left-truncation
 oresult = maxlik('to_llike',baug,in,y,x,vflag);
elseif tflag == 1
 oresult = maxlik('to_rlike',baug,in,y,x,vflag);
end; 

iter = oresult.iter;
llf = -oresult.f;
vcov = inv(oresult.hess);
grad = oresult.g; 
time = oresult.time;
beta = oresult.b;
 
if iter == in.maxit
fail = 1;
else
fail = 0;
end;

if fail == 1
error('optimization failed in tobit');
end;

if iter == in.maxit
warn(['no convergence in tobit in ' num2str(iter) ' iterations']);
end;

% now compute inference results

results.nobs = nobs;
results.nvar = nvar;
results.iter = iter;
results.beta = beta(1:nvar,1);
bhat = beta(1:nvar,1);
sig = beta(nvar+1,1);

results.sige = sig;
bcov = vcov(1:nvar,1:nvar);
stdb = sqrt(diag(bcov));
tstat = bhat./stdb;
results.tstat = tstat;
results.y = y;
yhat = x*bhat;
e = y - yhat;
sigu = e'*e;
results.yhat = yhat;
results.resid = e;
results.lik = llf;
ym = y - mean(y);
rsqr1 = sigu/(nobs-nvar);
rsqr2 = ym'*ym;
results.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(nobs-nvar);
rsqr2 = rsqr2/(nobs-1.0);
results.rbar = 1 - (rsqr1/rsqr2); % rbar-squared
results.meth = 'tobit';
results.opt = in.hess;
results.grad = grad;
results.time = time;
