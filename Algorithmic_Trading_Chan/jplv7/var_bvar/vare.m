function results = vare(y,nlag,x)
% PURPOSE: performs vector autogressive estimation
%---------------------------------------------------
% USAGE:  result = vare(y,nlag,x) 
% where:    y    = an (nobs x neqs) matrix of y-vectors
%           nlag = the lag length
%           x    = optional matrix of variables (nobs x nx)
%                 (NOTE: constant vector automatically included)
%---------------------------------------------------
% RETURNS a structure
% results.meth = 'vare'
% results.nobs = nobs, # of observations
% results.neqs = neqs, # of equations
% results.nlag = nlag, # of lags
% results.nvar = nlag*neqs+nx+1, # of variables per equation
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
% results(eq).boxq  = Box Q-statistics
% results(eq).ftest = Granger F-tests
% results(eq).fprob = Granger marginal probabilities
%---------------------------------------------------
% SEE ALSO: varf, prt_var, prt_granger, prt_ftests 
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% Stephan Siegel [ss1110@columbia.edu]
% provided a bug fix for the joint f-test part
% of this function July, 2004

[nobs neqs] = size(y);

results.meth = 'vare';

nx = 0;

if nargin == 3
[nobs2 nx] = size(x);
 if (nobs2 ~= nobs)
 error('var: nobs in x-matrix not the same as y-matrix');
 end;
end;

% adjust nobs to feed the lags
nobse = nobs - nlag;

% nvar adjusted for constant term 
 k = neqs*nlag + 1 + nx;
 nvar = k;

results.nvar = nvar;

xlag = mlag(y,nlag);

results.nobs = nobse;
results.neqs = neqs;
results.nlag = nlag;


% form x-matrix
if nx 
xmat = [xlag(nlag+1:nobs,:) x(nlag+1:nobs,:) ones(nobs-nlag,1)];
else
xmat = [xlag(nlag+1:nobs,:) ones(nobs-nlag,1)];
end;


% pull out each y-vector and run regressions
for j=1:neqs;

 yvec = y(nlag+1:nobs,j);
 res = ols(yvec,xmat);
 results(j).beta  = res.beta;      % bhats
 results(j).tstat = res.tstat;     % t-stats
 % compute t-probs
      tstat = zeros(nvar,1);
      tstat = res.tstat;
      tout = tdis_prb(tstat,nobse-nvar);
 results(j).tprob = tout;          % t-probs
 results(j).resid = res.resid;     % resids 
    sigu = res.resid'*res.resid;
 results(j).yhat = res.yhat;       % yhats
   results(j).y    = yvec;           % actual y
   results(j).rsqr = res.rsqr;       % r-squared
   results(j).rbar = res.rbar;       % r-adjusted
   results(j).sige = res.sige;


% do the Q-statistics
% use residuals to do Box-Pierce Q-stats
% use lags = nlag in the VAR
% NOTE: a rule of thumb is to use (1/6)*nobs
%       but this seems excessive to me
elag = mlag(res.resid,nlag);
% feed the lags
etrunc = elag(nlag+1:nobse,:);
rtrunc = res.resid(nlag+1:nobse,1);
qres   = ols(rtrunc,etrunc);
if nlag ~= 1
 boxq   = (qres.rsqr/(nlag-1))/((1-qres.rsqr)/(nobse-nlag));
else
 boxq   = (qres.rsqr/(nlag))/((1-qres.rsqr)/(nobse-nlag));
end;

results(j).boxq = boxq;

% form x matrices for joint F-tests
% exclude each variable from the model sequentially

for r=1:neqs;
xtmp = [];
for s=1:neqs;
 if s ~= r
   xlag = mlag(y(:,s),nlag);
 xtmp = [xtmp trimr(xlag,nlag,0)];
   end;
end;
% we have an xtmp matrix that excludes 1 variable
% add deterministic variables (if any) and constant term
if nx > 0
xtmp = [xtmp x(nlag+1:nobs,:) ones(nobse,1)];
else
xtmp = [xtmp ones(nobse,1)];
end;
% get ols residual vector
b = xtmp\yvec; % using Cholesky solution
etmp = yvec-xtmp*b;
sigr = etmp'*etmp;
% joint F-test for variables r
ftest(r,1) = ((sigr - sigu)/nlag)/(sigu/(nobse-k)); 
end;

results(j).ftest = ftest;     
results(j).fprob = fdis_prb(ftest,nlag,nobse-k);

end; 
% end of loop over equations 
 



