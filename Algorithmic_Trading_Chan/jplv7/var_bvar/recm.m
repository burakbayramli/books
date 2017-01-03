function results = recm(y,nlag,w,freq,sig,tau,theta,r)
% PURPOSE: performs Bayesian error correction model estimation
%          using Random-walk averaging prior
%---------------------------------------------------
% USAGE: result = recm(y,nlag,w,freq,sig,tau,theta,r) 
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length    
%           w    = an (neqs x neqs) matrix containing prior means
%                  (rows should sum to unity, see below)
%           freq = 1 for annual, 4 for quarterly, 12 for monthly
%           sig  = prior variance hyperparameter (see below)
%           tau  = prior variance hyperparameter (see below)
%          theta = prior variance hyperparameter (see below)
%           x    = an (nobs x nx) matrix of deterministic variables
%                  (in any form, they are not altered during estimation)
%                  (constant term automatically included)
%           r    = # of cointegrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)                                      
% priors for important variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag
% priors for unimportant variables: N(w(i,j) ,theta*sig/k) for lag 1 
%                                   N(  0 ,theta*sig/k)    for lag k=2,...,nlag  
% e.g., if y1, y3, y4 are important variables in eq#1, y2 unimportant
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
% ---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms 
% because the prior means rely on common (growth-rate) scaling of variables
% hence the need for a freq argument input.
%        - constant term included automatically  
%        - x-matrix of exogenous variables not allowed
%        - error correction variables are automatically
%          constructed using output from Johansen's ML-estimator 
% ---------------------------------------------------
% RETURNS a structure
% results.meth  = 'recm'
% results.nobs  = nobs, # of observations
% results.neqs  = neqs, # of equations
% results.nlag  = nlag, # of lags
% results.nvar  = nlag*neqs+nx+1, # of variables per equation
% results.coint = # of co-integrating relations (or r if input)
% results.weight= weight matrix
% results.sig   = tightness hyperparameter
% results.tau   = tau hyperparameter
% results.theta = theta hyperparameter
% --- the following are referenced by equation # --- 
% results(eq).beta   = bhat for equation eq (includes ec-bhats)
% results(eq).tstat  = t-statistics 
% results(eq).tprob  = t-probabilities
% results(eq).resid  = residuals 
% results(eq).yhat   = predicted values (levels) (nlag+freq+1:nobs,1)
% results(eq).dyhat  = predicted values (growth rates) (nlag+freq+1:nobs,1)
% results(eq).y      = actual y-level values (nobs x 1)
% results(eq).dy     = actual y-growth rate values (nlag+freq+1:nobs,1)
% results(eq).sige   = e'e/(n-k)
% results(eq).rsqr   = r-squared
% results(eq).rbar   = r-squared adjusted
%---------------------------------------------------    
% SEE ALSO: recmf, becm, prt_var 
%---------------------------------------------------
% References: LeSage and Krivelyova (1998) 
% ``A Spatial Prior for Bayesian Vector Autoregressive Models'',
% forthcoming Journal of Regional Science, (on http://www.econ.utoledo.edu)
% and
% LeSage and Krivelova (1997) (on http://www.econ.utoledo.edu)
% ``A Random Walk Averaging Prior for Bayesian Vector Autoregressive Models''

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

nx = 0;

if nargin == 8 % user is specifying the # of error correction terms to
             % include -- get them using johansen()
 jres = johansen(y,0,nlag);
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind;
 % construct r-error correction variables
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x);
   
elseif nargin == 7 % we need to find r
 jres = johansen(y,0,nlag);
 % find r = # significant co-integrating relations using
 % the trace statistic output
 trstat = jres.lr1;
 tsignf = jres.cvt;
 r = 0;
 for i=1:neqs;
  if trstat(i,1) > tsignf(i,2)
   r = i;
  end;
 end;
 % recover error correction vectors
 ecvectors = jres.evec;
        index = jres.ind;
 % construct r error correction variables
if r > 0
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [junk nx] = size(x);    
end;
else
 error('Wrong # of arguments to recm');
end;

% call RVAR using co-integrating variables as x-matrix
% call depends on whether we have an x-matrix or not
if nx ~= 0 
results = rvar(y,nlag,w,freq,sig,tau,theta,x);
else
results = rvar(y,nlag,w,freq,sig,tau,theta);
end;

results(1).meth = 'recm';
results(1).coint = r;
results(1).sig = sig;
results(1).weight = w;
results(1).tau = tau;
results(1).theta = theta;
results(1).index = index;




