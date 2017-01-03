function results = ppooled(y,x)
% PURPOSE: performs Pooled Least Squares for Panel Data(for balanced or unbalanced data)
%----------------------------------------------------------------------------------------
% USAGE:  results = ppooled(y,x)
% where:    y     = a (nobs x neqs) matrix of all of the individual's observations 
%						 vertically concatenated. This matrix must include in the firt
%						 column the dependent variable, the independent variables must follow
%						 accordingly.	
%			    x    = optional matrix of exogenous variables, 
%						  dummy variables. 				
%						 (NOTE: constant vector automatically included)
%----------------------------------------------------------------------------------------
%RETURNS a structure
% results.meth = 'ppooled'
% results.nobs = nobs, # of observations
% results.nvar  = nvars, # of variables
% results.beta  = bhat 
% results.tstat = t-statistics 
% results.tprob = t-probabilities
% results.resid = residuals 
% results.yhat  = predicted values 
% results.y     = actual values 
% results.sige  = e'e/(n-k)
% results.rsqr  = r-squared
% results.rbar  = r-squared adjusted
% results.sige  = sigma^2 e
% results.xmat  = matrix of independent variables
% results.time  = time elapsed during the procedure
% results.crconst = correction of the constant term
%----------------------------------------------------------------------------------------

%Written by:
% Carlos Alberto Castro
% National Planning Department
% Bogota, Colombia
% Email: ccastro@dnp.gov.co 

t0 = clock;
     
results.meth = 'ppooled';

[nobs equ]= size(y);

nx = 0;

if nargin == 2 
[nobs2 nx] = size(x);
 if (nobs2 ~= nobs)
 error('nobs in x-matrix not the same as y-matrix');
 end;
end;

results.nobs = nobs;
results.crconst = 0;    %correction of the constant term


% form x-matrix
if nx 
xmat = [y(:,2:equ) x ones(nobs,1)];
else
xmat = [y(:,2:equ) ones(nobs,1)];
end;

[nobs3 nvars]= size(xmat); 
results.nvar  = nvars;
results.xmat  = xmat;

% run OLS

 res = ols(y(:,1),xmat);
 results.beta  = res.beta;      % bhats
 results.tstat = res.tstat;     % t-stats
% compute t-probs
      tstat = zeros(nvars,1);
      tstat = res.tstat;
      tout = tdis_prb(tstat,nobs-nvars);
 results.tprob = tout;          % t-probs
 results.resid = res.resid;     % resids 
    sigu = res.resid'*res.resid; %sse
 results.yhat = res.yhat;       % yhats
   results.y    = y(:,1);           % actual y
   results.rsqr = res.rsqr;       % r-squared
   results.rbar = res.rbar;       % r-adjusted
   results.sige = res.sige;       % sigma e
   results.time = etime(clock,t0); % time elapsed during the procedure

   



 



