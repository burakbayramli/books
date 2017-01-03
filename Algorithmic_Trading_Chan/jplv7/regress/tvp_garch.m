function result = tvp_garch(y,x,parm,info)
% PURPOSE: time-varying parameter estimation with garch(1,1) errors
%          y(t) = X(t)*B(t) + e(t), e(t) = N(0,h(t))
%          B(t) = B(t-1) + v(t),    v(t) = N(0,sigb^2)
%          h(t) = a0 + a1*e(t-1)^2 + a2*h(t-1) ARMA(1,1) error variances
% -------------------------------------------------------------------
% USAGE:     result = tvp_garch(y,x,parm,info);
%        or: result = tvp_garch(y,x,parm); for default options
% where: y = dependent variable vector
%        x = explanatory variable matrix
%     parm = (k+3)x1 vector of starting values
%             parm(1:k,1)   = sigb vector
%             parm(k+1,1)   = a0 
%             parm(k+2,1)   = a1
%             parm(k+3,1)   = a2
%   info = a structure variable containing optimization options
%   info.b0    = a (k+1) x 1 vector with initial b values (default: zeros(k+1,1))
%   info.v0    = a (k+1)x(k+1) matrix with prior for sigb 
%                (default: eye(k+1)*1e+5, a diffuse prior)
%   info.prt   = 1 for printing some intermediate results
%              = 2 for printing detailed results (default = 0)
%   info.delta = Increment in numerical derivs                [.000001]
%   info.hess  = Hessian: ['dfp'], 'bfgs', 'gn', 'marq', 'sd'
%   info.maxit = Maximium iterations                              [500]
%   info.lamda = Minimum eigenvalue of Hessian for Marquardt      [.01]
%   info.cond  = Tolerance level for condition of Hessian        [1000]
%   info.btol  = Tolerance for convergence of parm vector        [1e-4]
%   info.ftol  = Tolerance for convergence of objective function [sqrt(eps)] 
%   info.gtol  = Tolerance for convergence of gradient           [sqrt(eps)]
%   info.start = starting observation (default: 2*k+1)
% -------------------------------------------------------------------
% RETURNS: a result structure
%       result.meth   = 'tvp_garch'
%       result.sigb   = a (kx1) vector of sig beta estimates
%       result.ahat   = a (3x1) vector with a0,a1,a2 estimates
%       result.vcov   = a (k+3)x(k+3) var-cov matrix for the parameters       
%       result.tstat  = a (k+3) x 1 vector of t-stats based on vcov
%       result.stdhat = a (k+3) x 1 vector of estimated std deviations
%       result.beta   = a (start:n x k) matrix of time-varying beta hats
%       result.ferror = a (start:n x 1) vector of forecast errors
%       result.fvar   = a (start:n x 1) vector for conditional variances
%       result.sigt   = a (start:n x 1) vector of arch variances
%       result.rsqr   = R-squared
%       result.rbar   = R-bar squared
%       result.yhat   = predicted values
%       result.y      = actual values
%       result.like   = log likelihood (at solution values)
%       result.iter   = # of iterations taken
%       result.start  = # of starting observation
%       result.time   = time (in seconds) for solution
% -------------------------------------------------------------------
% NOTES: 1) to generate tvp betas based on max-lik parm vector
%           [beta ferror] = tvp_garch_filter(parm,y,x,start,b0,v0);
%        2) tvp_garch calls garch_trans(), maxlik(), tvp_garch_like, tvp_garch_filter
% -------------------------------------------------------------------
% SEE ALSO: prt(), plt(), tvp_garch_like, tvp_garch_filter
% -------------------------------------------------------------------
% REFERNCES: Kim and Nelson (1999)
% State-Space Models with Regime Switching
% -------------------------------------------------------------------


% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

infoz.maxit = 500;
[n k] = size(x);
start = 2*k+1;
priorv0 = eye(k+1)*1e+5;
priorb0 = zeros(k+1,1);

if nargin == 4 % we need to reset optimization defaults
if ~isstruct(info)
  error('tvp_garch: optimization options should be in a structure variable');
end;
% parse options
fields = fieldnames(info);
nf = length(fields); 
  for i=1:nf
    if strcmp(fields{i},'maxit')
        infoz.maxit = info.maxit; 
    elseif strcmp(fields{i},'btol')
        infoz.btol = info.btol;
    elseif strcmp(fields{i},'ftol')
        infoz.ftol = info.ftol;
    elseif strcmp(fields{i},'gtol')
        infoz.gtol = info.gtol;
    elseif strcmp(fields{i},'hess')
        infoz.hess = info.hess;
    elseif strcmp(fields{i},'cond')
        infoz.cond = info.cond;
    elseif strcmp(fields{i},'prt')
        infoz.prt = info.prt;
    elseif strcmp(fields{i},'delta')
        infoz.delta = info.delta;     
    elseif strcmp(fields{i},'lambda')
        infoz.lambda = info.lambda;  
    elseif strcmp(fields{i},'start')
        start = info.start;  
    elseif strcmp(fields{i},'v0')
        priorv0 = info.v0;  
    elseif strcmp(fields{i},'b0')
        priorb0 = info.b0;         
    end;
  end;
end;
         

% Do maximum likelihood estimation
oresult = maxlik('tvp_garch_like',parm,infoz,y,x,start,priorb0,priorv0);
parm1 = oresult.b;
% take absolute value of standard deviations
parm1(1:k,1) = abs(parm1(1:k,1));

niter = oresult.iter;
like = -oresult.f;
time = oresult.time;

% compute numerical hessian at the solution
cov0 = inv(fdhess('tvp_garch_like',parm1,y,x,start,priorb0,priorv0));
grad = fdjac('garch_trans',parm1);
vcov = grad*cov0*grad';
stdhat = sqrt(diag(vcov));

% produce tvp beta hats, 
% prediction errors and variance of forecast error,
% and garch(1,1) variance estimates
[beta ferror fvar sigt] = tvp_garch_filter(parm1,y,x,start,priorb0,priorv0);


% transform a0,a1,a2
parm1 = garch_trans(parm1);

yhat = zeros(n-start+1,1);
for i=start:n;
yhat(i-start+1,1) = x(i,:)*beta(i-start+1,:)';
end;

resid = y(start:n,1) - yhat;
sigu = resid'*resid;
tstat = parm./stdhat;

ym = y(start:n,1) - mean(y(start:n,1));
rsqr1 = sigu;
rsqr2 = ym'*ym;
result.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-start);
rsqr2 = rsqr2/(n-1.0);
result.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

% return results structure information
result.sigb = parm1(1:k,1);
result.ahat = parm1(k+1:k+3,1);
result.beta = beta;
result.ferror = ferror;
result.fvar = fvar;
result.sigt = sigt;
result.vcov = vcov;
result.yhat = yhat;
result.y = y;
result.resid = resid;
result.like = like;
result.time = time;
result.tstat = tstat;
result.stdhat = stdhat;
result.nobs = n;
result.nvar = k;
result.iter = niter;
result.meth = 'tvp_garch';
result.start = start;



function [betao, ferroro, fvaro, sigto] =  tvp_garch_filter(parm,y,x,start,priorb0,priorv0)
% PURPOSE: generate tvp_garch model betas, forecast errors, forecast variance
%          and garch(1,1) sigmas over time given maximum likelihood estimates
% -------------------------------------------------------------
% USAGE: [beta ferror fvar sigt ] = tvp_garch_filter(parm,y,x,start,priorb0,priorv0)
% where: parm = a vector of maximum likelihood estimates
%        y = data vector
%        x = data matrix
%    start = # of observation to start the filter
%            (default = 1)
%    priorb0 = a (k+1) x 1 vector with prior for b0        
%    priorv0 = a (k+1)x(k+1) matrix with prior for sigb
%            (default: eye(k+1)*1e+5, a diffuse prior)  
% -------------------------------------------------------------
% RETURNS:   beta = (Txk) matrix of tvp beta estimates
%          ferror = (Tx1) vector with forecast error and
%                              
%            fvar = (Tx1) vector with conditional variance
%            sigt = (Tx1) vector with garch variances               
% -------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


[n k] = size(x);

% transform parameters
parm = garch_trans(parm);

if nargin == 3
start = 1;
priorb0 = zeros(k+1,1);
priorv0 = eye(k+1)*1e+5;
elseif nargin == 4
priorb0 = zeros(k+1,1);
priorv0 = eye(k+1)*1e+5;
elseif nargin == 6
% do nothing
else
error('tvp_garch_filter: Wrong # of input arguments');
end;

beta = zeros(n,k);
ferror = zeros(n,1);
fvar = zeros(n,1);
sigt = zeros(n,1);

sigb = zeros(k,1);
for i=1:k;
sigb(i,1) = parm(i,1)*parm(i,1);
end;

a0 = parm(k+1,1);
a1 = parm(k+2,1);
a2 = parm(k+3,1);
ivar = a0/(1-a1-a2);

r = 0;

f = eye(k+1);
f(k+1,k+1) = 0;
g = eye(k+1);

cll = priorb0;
pll = priorv0;
pll(k+1,k+1) = ivar;
htl = ivar;


for iter = 1:n;

h = [x(iter,:) 1];

ht = a0 + a1*(cll(k+1,1)^2 + pll(6,6)) + a2*htl;

tmp = [sigb
       ht];

Q = diag(tmp);

ctl = f*cll;
ptl = f*pll*f' + g*Q*g';

vt = y(iter,1) - h*ctl; % prediction error
ft = h*ptl*h' + r;      % variance of forecast error

ctt = ctl + ptl*h'*(1/ft)*vt;
ptt = ptl - ptl*h'*(1/ft)*h*ptl;

beta(iter,:) = ctl(1:k,1)';
ferror(iter,1) = vt;
fvar(iter,1) = ft;
sigt(iter,1) = ht;

cll = ctt;
pll = ptt;
htl = ht;

end;

betao = beta(start:n,:);
ferroro = ferror(start:n,1);
fvaro = fvar(start:n,1);
sigto = sigt(start:n,1);




