function result = tvp(y,x,parm,info)
% PURPOSE: time-varying parameter maximum likelihood estimation
%          y(t) = X(t)*B(t) + e(t), e(t) = N(0,sige^2)
%          B(t) = B(t-1) + v(t),    v(t) = N(0,sigb^2)
%          optional model variant:  v(t) = N[0,delta*sige^2*inv(X'X)]
%          (Zellner's g-prior)          
% -------------------------------------------------------------------
% USAGE:     result = tvp(y,x,parm,info);
%        or: result = tvp(y,x,parm); for default options
% where: y = dependent variable vector
%        x = explanatory variable matrix
%     parm =  --- for normal model ---
%            (k+1)x1 vector of starting values 
%             parm(1,1)     = sige
%             parm(2:k+1,1) = sigb vector
%     parm =  --- for Zellner g-prior model ---
%             (2x1) vector of starting values
%             parm(1,1)     = sige
%             parm(2,1)     = delta
%   info = a structure with initial values and optimization options
%   info.b0 = a (kx1) vector with initial values for b0 (default = 0,diffuse)
%   info.v0 = a (kxk) matrix with initial matrix for sigb
%                (default = eye(k)*1e+5, a diffuse prior)
%   info.delta = delta (starting value for delta in Zellner g-prior model)
%                (same as parm(2,1) value)             
%   info.start = starting observation (default: 2*k+1)  
%   --- optimization options ---              
%   info.prt   = 1 for printing basic intermediate results (default = 0)
%              = 2 for printing detailed stuff
%   info.deriv = Increment in numerical derivs                [.000001]
%   info.hess  = Hessian method: ['dfp'], 'bfgs', 'gn', 'marq', 'sd'
%   info.maxit = Maximium iterations                              [500]
%   info.lamda = Minimum eigenvalue of Hessian for Marquardt      [.01]
%   info.cond  = Tolerance level for condition of Hessian        [1000]
%   info.btol  = Tolerance for convergence of parm vector        [1e-4]
%   info.ftol  = Tolerance for convergence of objective function [sqrt(eps)] 
%   info.gtol  = Tolerance for convergence of gradient           [sqrt(eps)]
% -------------------------------------------------------------------
% RETURNS: a result structure
%       result.meth   = 'tvp'
%       result.sige   = sige estimate
%       result.stds   = std of sige estimate
%       result.delta  = delta estimate (for g-prior model), 0 otherwise
%       result.stdd   = std of delta estimate (for g-prior model), 0 otherwise
%       result.sigb   = a (kx1) vector of sig beta estimates      
%       result.vcov   = a (kxk) var-cov matrix for the sig beta parameters 
%       result.stdb   = std deviation of sigb estimates      
%       result.tstat  = a (k+1) x 1 vector of t-stats for [sige sigb]'
%       result.beta   = a (start:n x k) matrix of time-varying beta hats
%       result.ferror = a (start:n x 1) vector of forecast errors
%       result.fvar   = a (start:n x 1) vector for conditional variances
%       result.rsqr   = R-squared
%       result.rbar   = R-bar squared
%       result.yhat   = a (start:n x 1) vector of predicted values
%       result.y      = (nx1) vector of actual values
%       result.like   = log likelihood (at solution values)
%       result.iter   = # of iterations taken
%       result.start  = # of starting observation
%       result.time   = time (in seconds) for solution
% -------------------------------------------------------------------
% NOTE: 1) to generate tvp betas based on max-lik parm vector
%          [beta ferror] = tvp_filter(parm,y,x,start,priorb0,priorv0);
%          The filter starts at obs=1, but only returns values for
%          start:n
%       2) calls tvp_like, maxlik, tvp_filter
% (tvp_filter   is included at the end of this function)
% (tvp_zgfilter is included at the end of this funtion)
% -------------------------------------------------------------------
% SEE ALSO: prt(), plt(), prt_tvp, plt_tvp, tvp_like, tvp_filter
% -------------------------------------------------------------------
% REFERENCES: Kim and Nelson (1999)
% State-Space Models with Regime Switching
% -------------------------------------------------------------------


% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


infoz.maxit = 500;
[n, k] = size(x);
start = 2*k+1;
priorb0 = zeros(k,1);
priorv0 = eye(k)*1e+5;
dflag = 0;

if nargin == 4 % we need to reset optimization defaults
if ~isstruct(info)
  error('tvp: options should be in a structure variable');
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
    elseif strcmp(fields{i},'deriv')
        infoz.delta = info.deriv;   
    elseif strcmp(fields{i},'lamda')
        infoz.lambda = info.lamda;  
    elseif strcmp(fields{i},'start')
        start = info.start;     
    elseif strcmp(fields{i},'b0')
        priorb0 = info.b0;
    elseif strcmp(fields{i},'v0')
        priorv0 = info.v0; 
    elseif strcmp(fields{i},'delta');
    delta = info.delta;
    dflag = 1;      
    end;
  end;
elseif nargin == 3
% use default options
else
error('tvp: Wrong # of input arguments');
end;

if dflag == 0         
chk = length(parm);
 if chk ~= k+1
 error('tvp: Wrong # of initial parameter values in parm');
 end;
elseif dflag == 1
chk = length(parm);
 if chk ~= 2
 error('tvp: Wrong # of initial parmaeter values in parm');
 end;
end;

% Do maximum likelihood estimation

if dflag == 1 % call likelihood function with delta argument
              % for Zellner g-prior
oresult = maxlik('tvp_zglike',parm,infoz,y,x,start,priorb0,priorv0); 
else          % call likelihood function without delta argument
oresult = maxlik('tvp_like',parm,infoz,y,x,start,priorb0,priorv0); 
end;
niter = oresult.iter;
like = -oresult.f;
% take absolute values for parm since they are variances
parm1 = abs(oresult.b);

if dflag == 0
vcovt = inv(oresult.hess);
stds = sqrt(vcovt(1,1));
tmp = sqrt(diag(vcovt(2:k+1,2:k+1)));
vcov = vcovt(2:k+1,2:k+1);
stdb = sqrt(diag(vcov));
tmp = [stds
       stdb];
tstat = parm1./tmp;
else
sige  = abs(oresult.b(1));
delta = abs(oresult.b(2));
tmp   = inv(oresult.hess);
stds = sqrt(tmp(1,1));
stdd = sqrt(tmp(2,2));
vcov = sige*sige*delta*delta*inv(x'*x);
sigb = sqrt(diag(vcov));
parm1 = zeros(k+1,1);
parm1(1,1) = sige;
parm1(2:k+1,1) = sigb;
parmt = [sige   % use numerical hessian to get var-cov estimates
         sigb]; % for std beta
vcovt = fdhess('tvp_like',parmt,y,x,start,priorb0,priorv0);
tmp = [sige
       sigb];
tstat = tmp./diag(sqrt(inv(vcovt)));
stdb = tmp(2:k+1,1)./tstat(2:k+1,1);
end;
time = oresult.time;
  
% produce tvp beta hats
if dflag == 1
[beta ferror] = tvp_zgfilter(parm1,y,x,start,priorb0,priorv0);
else
[beta ferror] = tvp_filter(parm1,y,x,start,priorb0,priorv0);
end;


yhat = zeros(n-start+1,1);
for i=start:n;
yhat(i-start+1,1) = x(i,:)*beta(i-start+1,:)';
end;

resid = y(start:n,1) - yhat;
sigu = resid'*resid;
ym = y(start:n,1) - mean(y(start:n,1));
rsqr1 = sigu;
rsqr2 = ym'*ym;
result.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-start-k);
rsqr2 = rsqr2/(n-1.0);
result.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

% return results structure information
result.sige = parm1(1,1);
result.stds = stds;
result.sigb = parm1(2:k+1,1);
result.stdb = stdb;
result.beta = beta;
result.ferror = ferror(:,1);
result.fvar = ferror(:,2);
result.vcov = vcov;
result.yhat = yhat;
result.y = y;
result.resid = resid;
result.like = like;
result.time = time;
result.tstat = tstat;
result.nobs = n;
result.nvar = k;
result.iter = niter;
result.meth = 'tvp';
result.start = start;
if dflag == 0
result.delta = 0;
result.stdd = 0;
else
result.delta = parm1(2,1)*parm1(2,1);
result.stdd = stdd;
end;



function [betao, ferroro] =  tvp_filter(parm,y,x,start,priorb0,priorv0)
% PURPOSE: generate tvp model filtered betas and forecast error variance
%          given maximum likelihood estimates
% -------------------------------------------------------------
% USAGE: [beta ferror] = tvp_filter(parm,y,x,start,priorb0,priorv0)
% where: parm = a vector of maximum likelihood estimates
%        y = (nx1) data vector
%        x = (nxk) data matrix
%    start = # of observation to start the filter (default = 1)
%    priorb0 = (k x 1) vector with prior b0 values 
%              (default = zeros(k,1), diffuse)
%    priorv0 = (k x k) matrix with prior variance for sigb
%              (default = eye(k)*1e+5, a diffuse prior)
% -------------------------------------------------------------
% NOTE: the filter starts at obs=1, but only returns information
%       from start onward            
% -------------------------------------------------------------
% RETURNS:   beta = (start:n x k) matrix of tvp beta estimates
%          ferror = (start:n x 2) matrix with forecast error and
%                         conditional variance        
% -------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(y);
k = length(parm) - 1;

if nargin == 3
start = 1;
priorb0 = zeros(k,1);
priorv0 = eye(k)*1e+5;
elseif nargin ==4
priorb0 = zeros(k,1);
priorv0 = eye(k)*1e+5;
elseif nargin == 6
% do nothing
else
error('tvp_filter: Wrong # of input arguments');
end;



beta = zeros(n,k);
ferror = zeros(n,2);

sige = parm(1);
sigb = zeros(k,1);
for i=1:k;
sigb(i,1) = parm(i+1,1);
end;

f = eye(k);
rr = sige.^2;
qq = diag(sigb.*sigb);
betall = priorb0;
pll = priorv0;

for iter=1:n;
xt = x(iter,:);
yt = y(iter,1);     
betatl = f*betall;
ptl = f*pll*f' + qq;
fcast = yt - xt*betatl;
ss = xt*ptl*xt' + rr;

betatt = betatl + (ptl*xt'/ss)*fcast;
ptt = (eye(k) - (ptl*xt'/ss)*xt)*ptl;
ferror(iter,:) = [fcast ss];
beta(iter,:) = betatl';

betall = betatt;
pll = ptt;

end;

betao = beta(start:n,:);
ferroro = ferror(start:n,:);


function [betao, ferroro] =  tvp_zgfilter(parm,y,x,start,priorb0,priorv0)
% PURPOSE: generate tvp model filtered betas and forecast error variance
%          given maximum likelihood estimates for Zellner g-prior tvp model
%          y(t) = X(t)*B(t) + e(t), e(t) = N(0,sige^2)
%          B(t) = B(t-1) + v(t),    v(t) = N[0,delta*sige*inv(X'X)]
%          (Zellner's g-prior)   
% -------------------------------------------------------------
% USAGE: [beta ferror] = tvp_zgfilter(parm,y,x,start,priorb0,priorv0)
% where: parm = a vector of maximum likelihood estimates
%               for sige, delta
%        y = (nx1) data vector
%        x = (nxk) data matrix
%    start = # of observation to start the filter (default = 1)
%    priorb0 = (k x 1) vector with prior b0 values 
%              (default = zeros(k,1), diffuse)
%    priorv0 = (k x k) matrix with prior variance for sigb
%              (default = eye(k)*1e+5, a diffuse prior)
% -------------------------------------------------------------
% NOTE: the filter starts at obs=1, but only returns information
%       from start onward            
% -------------------------------------------------------------
% RETURNS:   beta = (start:n x k) matrix of tvp beta estimates
%          ferror = (start:n x 2) matrix with forecast error and
%                         conditional variance        
% -------------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


[n k] = size(x);

if nargin == 3
start = 1;
priorb0 = zeros(k,1);
priorv0 = eye(k)*1e+5;
elseif nargin ==4
priorb0 = zeros(k,1);
priorv0 = eye(k)*1e+5;
elseif nargin == 6
% do nothing
else
error('tvp_filter: Wrong # of input arguments');
end;



beta = zeros(n,k);
ferror = zeros(n,2);

sige = parm(1,1);
delta = parm(2,1);
sigb = sige*sige*delta*delta*inv(x'*x);

f = eye(k);
rr = sige.^2;
qq = sigb;
betall = priorb0;
pll = priorv0;

for iter=1:n;
xt = x(iter,:);
yt = y(iter,1);     
betatl = f*betall;
ptl = f*pll*f' + qq;
fcast = yt - xt*betatl;
ss = xt*ptl*xt' + rr;

betatt = betatl + (ptl*xt'/ss)*fcast;
ptt = (eye(k) - (ptl*xt'/ss)*xt)*ptl;
ferror(iter,:) = [fcast ss];
beta(iter,:) = betatl';

betall = betatt;
pll = ptt;

end;

betao = beta(start:n,:);
ferroro = ferror(start:n,:);





