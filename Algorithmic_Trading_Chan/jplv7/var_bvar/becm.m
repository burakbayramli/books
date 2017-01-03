function results = becm(y,nlag,tight,weight,decay,r)
% PURPOSE: performs Bayesian error correction model estimation
%          using Minnesota-type prior
%---------------------------------------------------
% USAGE: result = becm(y,nlag,tight,weight,deacy,r) 
% where:    y    = an (nobs x neqs) matrix of y-vectors in levels
%           nlag = the lag length
%          tight = Litterman's tightness hyperparameter
%         weight = Litterman's weight (matrix or scalar)
%          decay = Litterman's lag decay = lag^(-decay) 
%           r    = # of cointegrating relations to use
%                  (optional: this will be determined using
%                  Johansen's trace test at 95%-level if left blank)                                    
% NOTES: constant vector automatically included
%         x-matrix of exogenous variables not allowed
%         error correction variables are automatically
%         constructed using output from Johansen's ML-estimator 
%---------------------------------------------------
% RETURNS a structure
% results.meth  = 'becm'
% results.nobs  = nobs, # of observations
% results.neqs  = neqs, # of equations
% results.nlag  = nlag, # of lags
% results.nvar  = nlag*neqs+nx+1, # of variables per equation
% results.coint = # of co-integrating relations (or r if input)
% results.tight = tightness hyperparameter
% results.weight= weight matrix or scalar
% results.decay = lag decay hyperparameter
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
% ---------------------------------------------------    
%  SEE ALSO: becmf, ecm, recm, prt_var 
% ---------------------------------------------------
%  REFERENCES: James P. LeSage, 
% ``A Comparison of the Forecasting Ability of ECM and VAR Models'',
%  Review of Economics and Statistics,  1990, Vol 72, number 4, pp. 664-671.
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[nobs neqs] = size(y);

% do some error checking here so as not to confuse the user
% with error messages from bvar called below

if nlag < 1
error('Lag length less than 1 in becm');
end;

if nlag > nobs
error('Lag length exceeds observations in becm');
end;

if tight < 0.01
warning('Tightness less than 0.01 in becm');
end;

if tight > 1.0
warning('Tightness greater than unity in becm');
end;

if decay < 0
error('Negative lag decay in becm');
end;

[wchk1 wchk2] = size(weight);
if (wchk1 ~= wchk2) 
 error('non-square weight matrix in becm');
elseif wchk1 > 1
 if wchk1 ~= neqs
 error('wrong size weight matrix in becm');
 end;
end;

% check for zeros in weight matrix
if wchk1 == 1
  if weight == 0
  error('becm: must have weight > 0');
  end;
elseif wchk1 > 1
  zip = find(weight == 0);
 if length(zip) ~= 0
 error('becm: must have weights > 0');
 end;
end;


nx = 0;

if nargin == 6 % user is specifying the # of error correction terms to
             % include -- get them using johansen()
 jres = johansen(y,0,nlag);
 % recover error correction vectors
 ecvectors = jres.evec;
   index = jres.ind;
 % construct r-error correction variables
 x = mlag(y(:,index),1)*ecvectors(:,1:r); 
   [nobs2 nx] = size(x);
   
elseif nargin == 5 % we need to find r
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
 error('Wrong # of arguments to becm');
end;

% nvar adjusted for constant term 
 k = neqs*nlag+nx+1;
 nvar = k;

% transform to 1st difference form
dy = tdiff(y,1);
dy = trimr(dy,1,0); % account for differencing
x = trimr(x,1,0);   % account for differencing

% call BVAR using 1st difference and co-integrating variables
% call depends on whether we have an x-matrix or not
if nx ~= 0 
results = bvar(dy,nlag,tight,weight,decay,x);
else
results = bvar(dy,nlag,tight,weight,decay);
end;


results(1).meth = 'becm';
results(1).coint = r;
results(1).tight = tight;
results(1).weight = weight;
results(1).decay = decay;
results(1).index = index;

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




