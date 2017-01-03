function results = rvar(y,nlag,w,freq,sig,tau,theta,x);
% PURPOSE: Estimates a Bayesian vector autoregressive model 
%          using the random-walk averaging prior 
%---------------------------------------------------
% USAGE:  result = rvar(y,nlag,w,freq,sig,tau,theta,x)
% where:    y    = an (nobs x neqs) matrix of y-vectors (in levels)
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
% priors for important variables:  N(w(i,j),sig) for 1st own lag
%                                  N(  0 ,tau*sig/k) for lag k=2,...,nlag               
% priors for unimportant variables: N(w(i,j) ,theta*sig/k) for lag 1 
%                                   N(  0 ,theta*sig/k)    for lag k=2,...,nlag  
% e.g., if y1, y3, y4 are important variables in eq#1, y2 unimportant
%  w(1,1) = 1/3, w(1,3) = 1/3, w(1,4) = 1/3, w(1,2) = 0                                              
% typical values would be: sig = .1-.3, tau = 4-8, theta = .5-1  
%---------------------------------------------------
% NOTES: - estimation is carried out in annualized growth terms 
% because the prior means rely on common (growth-rate) scaling of variables
%          hence the need for a freq argument input.
%        - constant term included automatically  
%---------------------------------------------------
% RETURNS: a structure
% results.meth = 'rvar'
% results.nobs = nobs, # of observations
% results.neqs = neqs, # of equations
% results.nlag = nlag, # of lags
% results.nvar = nlag*neqs+nx+1, # of variables per equation
% --- the following are referenced by equation # --- 
% results(eq).beta   = bhat for equation eq 
% results(eq).tstat  = t-statistics 
% results(eq).tprob  = t-probabilities
% results(eq).resid  = residuals (for growth rates regression)
% results(eq).yhat   = predicted values (levels) (nlag+freq+1:nobs,1)
% results(eq).dyhat  = predicted values (growth rates) (nlag+freq+1:nobs,1)
% results(eq).y      = actual y-level values (nobs x 1)
% results(eq).dy     = actual y-growth rate values (nlag+freq+1:nobs,1)
% results(eq).sige   = e'e/(n-k) (for growth rates regression)
% results(eq).rsqr   = r-squared (for growth rates regression)
% results(eq).rbar   = r-squared adjusted (for growth rates regression)
% ---------------------------------------------------    
% SEE ALSO: rvarf, var, bvar, ecm, becm, recm, prt_var 
% ---------------------------------------------------
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

results.meth = 'rvar';
results.sig = sig;
results.tau = tau;
results.theta = theta;
results.nobs = nobs;
results.neqs = neqs;
results.nlag = nlag;
results.weight = w;

nx = 0;

if nargin == 8 % user is specifying deterministic variables
   [nobs2 nx] = size(x);
   
elseif nargin == 7 % no deterministic variables
nx = 0;
else
 error('Wrong # of arguments to rvar');
end;

% transform y-levels to annualized growth rates
dy = growthr(y,freq);
dy = trimr(dy,freq,0);

% adjust nobs to account for seasonal differences and lags
nobse = nobs-freq-nlag;

% nvar 
 k = neqs*nlag+nx+1;
 nvar = k;
 
results.nvar = nvar;

y1 = mlag(dy,1);
y1 = trimr(y1,nlag,0);   % 1st own lags of the y-variables
xlag = nclag(dy,2,nlag); % lags 2 to nlag of the y-variables
xlag = trimr(xlag,nlag,0);
if nx > 0
x = trimr(x,nlag+freq,0); % truncate x variables for lags and diffs
end;
iota = ones(nobs,1);
iota = trimr(iota,nlag+freq,0);
dy = trimr(dy,nlag,0);    % truncate to feed lags

% form x-matrix of var plus deterministic variables
if nx ~= 0 
xmat = [xlag x iota];
else
xmat = [xlag iota];
end;

% form prior vector of means and matrix of variances
% for autoregressive parameters
% r = R beta + vmat

R = zeros(k,k);   
% only fill in 1's for lags, leave determininistic 
% and constant term elements set to zero
for i=1:neqs*nlag
 R(i,i) = 1.0;
end;

for j=1:neqs;    % ========> Equations loop

r = zeros(k,1);    % prior means 
vmat = eye(k)*100; % diffuse prior variance constant and deterministic

% set prior means for first lags  
% using weight matrix
for icnt = 1:neqs;
 r(icnt,1) = w(j,icnt);
end;
         
% use prior mean of zero  for lags 2 to nlag
% plus deterministic variables and constant
% already set by using r=zeros to start with

for ii=1:neqs;   % prior std deviations for 1st lags
 if w(j,ii) ~= 0
    vmat(ii,ii) = sig;
    else
    vmat(ii,ii) = theta*sig;
    end;
   end;
   
cnt = neqs+1;
for ii=1:neqs;   % prior std deviations for lags 2 to nlag
       if w(j,ii) ~= 0
 for kk=2:nlag;
    vmat(cnt,cnt) = tau*sig/kk;
    cnt = cnt + 1;
    end;
       else
 for kk=2:nlag;
    vmat(cnt,cnt) = theta*sig/kk;
    cnt = cnt + 1;
    end;       
       end;
end;

yvec = dy(:,j);
vmat = vmat.*vmat;       
res = theil(yvec,[y1 xmat],r,R,vmat);

% rearrange bhat parameters, t-statistics, tprobs in var order
bmat = zeros(k,1);
tmat = zeros(k,1);

% =====> rearrange bhat parameters in var order
cnt = 1;
 for i=1:nlag:k; % fills in lag 1 parameters
 bmat(i,1) = res.beta(cnt,1);
 tmat(i,1) = res.tstat(cnt,1);
 cnt = cnt + 1;
 end;

cnt = 2;
lcnt = 2;
 for i=1:k-nx-1-neqs; % fills in lag 2 to nlag parameters
 bmat(cnt,1) = res.beta(neqs+i,1);
 tmat(cnt,1) = res.tstat(neqs+i,1);
 cnt = cnt+1;
 lcnt = lcnt +1;
  if lcnt == nlag+1;
  cnt = cnt + 1;
  lcnt = 2;
  end;
 end;
for i=k-nx-1:k;
bmat(i,1) = res.beta(i,1);
tmat(i,1) = res.tstat(i,1);

end;

% find predicted values in levels form
ylag = lag(y(:,j),freq);
ylag = trimr(ylag,freq+nlag,0);
yhat = (iota + res.yhat).*ylag; % applied growth rate prediction to level
                                % from last year
 
 % put results in structure variables
 results(j).beta = bmat;
 results(j).tstat = tmat;
 results(j).resid = res.resid;
 results(j).dyhat = res.yhat;
    results(j).yhat = yhat;
 results(j).y = y(:,j);
 results(j).dy = dy(:,j);
 results(j).sige = res.sige;
 results(j).rsqr = res.rsqr;
 results(j).rbar = res.rbar;
end;
% end of for j loop



