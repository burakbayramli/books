function result = bvar(x,nlag,tight,weight,decay,xx);
% PURPOSE: Performs a Bayesian vector autoregression of order n
%---------------------------------------------------
% USAGE:  result = bvar(y,nlag,tight,weight,decay,x)
% where:    y    = an (nobs x neqs) matrix of y-vectors
%           nlag = the lag length
%          tight = Litterman's tightness hyperparameter
%         weight = Litterman's weight (matrix or scalar)
%          decay = Litterman's lag decay = lag^(-decay) 
%           x    = an optional (nobs x nx) matrix of variables
% NOTE:  constant vector automatically included
%---------------------------------------------------
% RETURNS: a structure:
% results.meth      = 'bvar'
% results.nobs      = nobs, # of observations
% results.neqs      = neqs, # of equations
% results.nlag      = nlag, # of lags
% results.nvar      = nlag*neqs+1+nx, # of variables per equation
% results.tight     = overall tightness hyperparameter
% results.weight    = weight scalar or matrix hyperparameter
% results.decay     = lag decay hyperparameter
% --- the following are referenced by equation # --- 
% results(eq).beta  = bhat for equation eq
% results(eq).tstat = t-statistics 
% results(eq).tprob = t-probabilities
% results(eq).resid = residuals 
% results(eq).yhat  = predicted values 
% results(eq).y     = actual values 
% results(eq).sige  = e'e/(n-k)
% results(eq).rsqr  = r-squared
% results(eq).rbar  = r-squared adjusted
% ---------------------------------------------------
% SEE ALSO:  bvarf, vare, ecm, rvar, plt_var, prt_var
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin < 5
error('Wrong # of arguments to bvar');
end;
if nargin > 6
error('Wrong # of arguments to bvar');
end;

if nlag < 1
error('Lag length less than 1 in bvar');
end;

[nobs nvar] = size(x);
if nlag > nobs
error('Lag length exceeds observations in bvar');
end;

if tight < 0.01
warning('Tightness less than 0.01 in bvar');
end;

if tight > 1.0
warning('Tightness greater than unity in bvar');
end;

if decay < 0
error('Negative lag decay in bvar');
end;

[wchk1 wchk2] = size(weight);
if (wchk1 ~= wchk2) 
 error('non-square weight matrix in bvar');
elseif wchk1 > 1
 if wchk1 ~= nvar
 error('wrong size weight matrix in bvar');
 end;
end;

% check for zeros in weight matrix
if wchk1 == 1
  if weight == 0
  error('bvar: must have weight > 0');
  end;
elseif wchk1 > 1
  zip = find(weight == 0);
 if length(zip) ~= 0
 error('bvar: must have weights > 0');
 end;
end;


[nobs neqs] = size(x);

nx = 0;
if nargin == 6 % we have deterministic variables
[nobs2 nx] = size(xx);
   if (nobs2 ~= nobs)
   error('X and Y-matrices in bvar have different # of obs');
   end;
end;

% adjust nobs to feed the lags
nobse = nobs - nlag;

% nvar adjusted for constant term
k = neqs*nlag + 1 + nx;
nvar = k;

% fill-in easy stuff
result.meth = 'bvar';
result.nlag = nlag;
result.nvar = nvar;
result.nobs = nobse;
result.neqs = neqs;
result.tight = tight;
result.decay = decay;
result.weight = weight;

% generate lagged rhs matrix
xlag = mlag(x,nlag);

% do scaling here using fuller y-vector information
% determine scale factors using univariate AR model

scale = zeros(neqs,1);
scale2 = zeros(neqs,neqs);

for j=1:neqs
   ytmp = x(1:nobs,j);
   if nx == 0
   scale(j,1) = scstd(ytmp,nobs,nlag);
   elseif nx > 0
   scale(j,1) = scstd(ytmp,nobs,nlag,xx);
   end;
end;

for j=1:neqs;
   for i=1:neqs;
   scale2(i,j) = scale(j)/scale(i);
   end;
end;

% form x-matrix only up to time begf-1
if nx
xmat = [xlag(nlag+1:nobs,:) xx(nlag+1:nobs,:) ones(nobs-nlag,1)];
else
xmat = [xlag(nlag+1:nobs,:) ones(nobs-nlag,1)];
end;

% pull out each y-vector and run Theil-Goldberger regressions

for j=1:neqs;

yvec = x(nlag+1:nobs,j);

bresult = theilbv(yvec,xmat,nlag,neqs,j,tight,weight,decay,scale2,scale,nx);

result(j).beta  = bresult.beta;         % bhats
result(j).tstat = bresult.tstat;        % t-stats
result(j).tprob = bresult.tprob;        % t-probs
result(j).resid = bresult.resid;        % residuals
result(j).yhat  = bresult.yhat;         % y-hats
resid = bresult.resid;                  % for use below
result(j).y     = yvec;                 % y-actuals
result(j).sige  = bresult.sige;         % e'e/(n-k)
result(j).rsqr = bresult.rsqr;          % r-squared
result(j).rbar = bresult.rbar;          % r-adjusted

end;

