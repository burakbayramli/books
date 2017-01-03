function results = ar(y,nlag,info)
% PURPOSE: ols estimates for the AR(k) model 
%          y = b0 + y(t-1) b1 + y(t-2) b2 +,...,y(t-k) bk + E, 
%          E = N(0,sige*In) 
%---------------------------------------------------
% USAGE:    results = ar(y,nlag,info)
% where: y    = dependent variable vector
%        nlag = # of lagged values
%        info.const = (optional argument) 0 = no constant
%                     default = 1, a constant term
% ---------------------------------------------------
% RETURNS: a structure:
%          results.meth  = 'ar'
%          results.beta  = bhat estimates (nlag x 1) vector
%          results.sige  = sige estimate (a scalar)
%          results.yhat  = mean of posterior y-predicted values
%          results.nobs  = # of observations
%          results.nadj  = # of observations adjusted for feeding lags
%          results.nvar  = # of variables (including constant term)
%          results.neqs  = 1, # of equations in the model (must be 1)
%          results.y     = actual observations (nobs x 1)
%          results.x     = x-matrix of lagged values of y (nobs-nlag,nlag+const)
% --------------------------------------------------
% NOTES: a constant term is automatically included in the model
%        unless you set info.const = 0;
% --------------------------------------------------
% SEE ALSO: prt
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n junk] = size(y);
results.y = y;
results.neqs = 1;

% error checking on input
if nargin == 3
    if ~isstruct(info)
    error('ar: must supply the options as a structure variable');
    end;
% parse info structure
fields = fieldnames(info);
nf = length(fields);
const = 1; % a constant is the default
for i=1:nf
    if strcmp(fields{i},'const')
        const = info.const; 
    end;
end;
elseif nargin == 2
    const = 1;
else
error('Wrong # of arguments to ar');
end;

 if const == 1
 x = [ones(n,1) mlag(y,nlag)];
 else
 x = mlag(y,nlag);
 end;
x = trimr(x,nlag,0); % feed the lags
y = trimr(y,nlag,0); 
nadj = length(y);
b0 = (x'*x)\(x'*y);  % Find ols bhat estimates
k = nlag+const;
sige = (y-x*b0)'*(y-x*b0)/(nadj-k); 

results.meth  = 'ar';
results.beta = b0;
results.sige = sige;
results.yhat = x*b0;
results.nobs  = n;
results.nadj  = nadj;
results.nvar  = nlag+const;
results.x = x;
