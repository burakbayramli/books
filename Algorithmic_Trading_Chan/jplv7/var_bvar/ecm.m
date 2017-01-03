function results = ecm(y,nlag,r)
% PURPOSE: performs error correction model estimation
%---------------------------------------------------
% USAGE: result = ecm(y,nlag,r) 
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%           r    = # of cointegrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)                                    
% NOTES: constant vector automatically included
%         x-matrix of exogenous variables not allowed
%         error correction variables are automatically
%         constructed using output from Johansen's ML-estimator 
%---------------------------------------------------
% RETURNS a structure
% results.meth = 'ecm'
% results.nobs = nobs, # of observations
% results.neqs = neqs, # of equations
% results.nlag = nlag, # of lags
% results.nvar = nlag*neqs+nx+1, # of variables per equation
% results.coint= # of co-integrating relations (or r if input)
% results.index= index of co-integrating variables ranked by
%                size of eigenvalues large to small
% --- the following are referenced by equation # --- 
% results(eq).beta   = bhat for equation eq (includes ec-bhats)
% results(eq).tstat  = t-statistics 
% results(eq).tprob  = t-probabilities
% results(eq).resid  = residuals 
% results(eq).yhat   = predicted values (levels) (nlag+2:nobs,1)
% results(eq).dyhat  = predicted values (differenced) (nlag+2:nobs,1)
% results(eq).y      = actual y-level values (nobs x 1)
% results(eq).dy     = actual y-differenced values (nlag+2:nobs,1)
% results(eq).sige   = e'e/(n-k)
% results(eq).rsqr   = r-squared
% results(eq).rbar   = r-squared adjusted
% results(eq).ftest  = Granger F-tests
% results(eq).fprob  = Granger marginal probabilities
% ---------------------------------------------------    
% SEE ALSO: ecmf, becm, recm, prt_var 
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

nx = 0;

if nargin == 3 % user is specifying the # of error correction terms to
             % include -- get them using johansen()
 jres = johansen(y,0,nlag);
 % recover error correction vectors
 ecvectors = jres.evec;
   index = jres.ind;
 % construct r-error correction variables
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x);
   
elseif nargin == 2 % we need to find r
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
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [junk nx] = size(x);    
else
 error('Wrong # of arguments to ecm');
end;

% nvar adjusted for constant term 
 k = neqs*nlag+nx+1;
 nvar = k;

% transform to 1st difference form
dy = tdiff(y,1);
dy = trimr(dy,1,0); % account for differencing
x = trimr(x,1,0);   % account for differencing

% call VAR using 1st difference and co-integrating variables
% call depends on whether we have an x-matrix or not
if nx ~= 0 
results = vare(dy,nlag,x);
else
results = vare(dy,nlag);
end;

for j=1:neqs;
results(j).y = y(:,j);
results(j).dy = dy(:,j);
results(j).dyhat = results(j).yhat;
% find predicted values in levels form
ylag = lag(y(:,j),1);
ylag = trimr(ylag,nlag+1,0);
yhat = results(j).yhat + ylag;
results(j).yhat = yhat;
end;

results(1).meth = 'ecm';
results(1).coint = r;
results(1).index = index;




