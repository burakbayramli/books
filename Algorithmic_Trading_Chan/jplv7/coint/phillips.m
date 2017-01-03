function result = phillips(y,x,flag)
% PURPOSE: compute Phillips-Perron test of the unit-root hypothesis
%          based on a Dickey-Fuller/Augmented Dickey-Fuller regression
% -------------------------------------------------------------------
% USAGE: results = phillips(y,x,flag)
%  where:              y = dependent variable (time series vector in levels)
%                      x = explanatory variables matrix
%                   flag = 0 for DF regression (default)
%                        = nlag the # of lags for ADF regression 
% -------------------------------------------------------------------
% RETURNS: a structure variable result
%          results.meth = 'phillips'
%          results.pstat = Phillips-Peron statistic, autocorrelation/heteroskedasticity 
%                         corrected t-ratio for the unit-root coefficient based on
%                         a Dickey-Fuller or Augmented Dickey-Fuller regression
%          results.crit  = (6 x 1) vector of critical values
%                        [1% 5% 10% 90% 95% 99%] quintiles    
%          results.nlag  = nlag, (should equal flag input)  
%          results.alpha = autoregressive parameter estimate
% -------------------------------------------------------------------
% NOTES:    reject the null of a unit root, even in the presence of serial correlation
%           and/or heteroskedasticity, if result.pstat is statistically significant.
% -------------------------------------------------------------------
% REFERENCES:  Phillips, Peter (1987), "Time Series Regression with a Unit Root", 
% Econometrica, vol. 55, no. 2 (March), pp. 277-301
% Phillips, Peter & Pierre Perron (1988), "Testing for a Unit Root in Time Series
%    Regression", Biometrika, vol. 75, no. 2 (June), pp. 335-346
% -------------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com

% input checking
if nargin == 2, % use DF as default
flag = 0;
end;
if nargin < 2 | nargin > 3
error('Wrong # of arguments to phillips');
end;

result.meth = 'phillips';
nobs = length(y); 

if flag == 0, % use DF
     b = inv(x'*x)*x'*y;
     r       = y - x*b;
     dep     = tdiff(r,1);
     dep = trimr(dep,1,0);
     k       = 0     ;
     z       = trimr(lag(r,1),1,0) ;
   beta      = z\dep;

   %resid       = dep - z*beta;
     % BUG fix suggested by 
% Nick Firoozye
% Sanford C. Bernstein, Inc
% 767 Fifth Avenue, #21-49
% New York, NY 10153
     resid = detrend(dep,0)- detrend(z,0)*beta; 
     sigma      = (resid'*resid)/(rows(dep)-cols(z));
     var_cov = sigma*inv(z'*z) ;

else, % use ADF
     b = inv(x'*x)*x'*y;
     r       = y - x*b;
     dep     = tdiff(r,1);
     dep = trimr(dep,1,0);
     k       = 1     ;
     z       = trimr(lag(r,1),1,0) ;
l = flag;     
     while (k <= l)
           z = [z lag(dep,k)];
           k = k + 1 ;
          end;
     z       = trimr(z,l,0) ;
     dep     = trimr(dep,l,0) ;
     beta    = detrend(z,0)\detrend(dep,0) ;
     % resid     = dep - z*beta ;
     % BUG fix suggested by 
% Nick Firoozye
% Sanford C. Bernstein, Inc
% 767 Fifth Avenue, #21-49
% New York, NY 10153
     resid = detrend(dep,0)- detrend(z,0)*beta; 
     sigma      = (resid'*resid)/(rows(dep)-cols(z));
     var_cov = sigma*inv(z'*z) ;
end;

rss = resid'*resid;
stdb = sqrt(var_cov(1,1));
tstat = beta(1,1)/stdb;

% The number of autocovariances of the residuals to be used, 
% based on ad-hoc rule of thumb
covs = fix(4*(nobs/100)^0.25);

% Apply Newey & West (1987) adjustment:
nw = rss;
for j = 1 : covs
   nw = nw + 2*(1-j/(covs+1))*(resid(1:end-j)'*resid(j+1:end));
end
nw = nw/nobs;

% Obtain the Phillips-Perron statistic, which follows the
% tabulated Dickey-Fuller distribution, and obtain its statistical significance:
result.pstat = sqrt(rss/nobs / nw) * tstat - 1/2* (nw - rss/nobs)/sqrt(nw) * nobs*stdb/sigma;
result.crit = ztcrit(nobs,0);
result.alpha = beta(1,1);
result.nlag = flag;
