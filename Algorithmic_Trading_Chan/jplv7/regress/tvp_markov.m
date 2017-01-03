function result = tvp_markov(y,x,parm,info)
% PURPOSE: time-varying parameter model with Markov switching error variances
%          y(t) = X(t)*B(t) + e(t), e(t) = N(0,R_S1t)
%          B(t) = B(t-1) + v(t),    v(t) = N(0,Q_S2t)
%          Q_S2t = Q1*a1,1t + Q2*a2,1t + Q3*a3,1t + Q4*a4,1t + ... + Sm*am,1t
%          R_S1t = R1*b1,1t + R2*b2,2t + R3*b3,2t + R4*b4,2t + ... + Rm*bm,2t
%          where: ak,jt = 1 if S_kt = j, ak,jt = 0 if S_kt ~= j
%          k=1,2  j=1,...,m
%          S_1t, S_2t evolve according to a 1st-order Markov process          
% -------------------------------------------------------------------
% USAGE:     result = tvp_markov(y,x,parm,info);
%        or: result = tvp(y,x,parm); for default options
% where: y = dependent variable vector
%        x = explanatory variable matrix
%     parm = (k+4)x1 vector of starting values
%        parm(1,1) = p, Pr[St=1 | St-1=1]
%        parm(2,1) = q, Pr[St=0 | St-1=0]
%        parm(3:3+k-1,1) = diagonal transition equation std deviations
%        parm(3+k,1)     = noise std deviation in state 1
%        parm(4+k,1)     = noise std deviation in state 2
%   info = a structure variable containing optimization options
%   info.b0 = a (kx1) vector with initial period B0 (default: zeros(k,1))
%   info.v0 = a (kxk) matrix with initial period std(B0)
%                (default = eye(k)*1e+3, a relatively diffuse prior)
%   info.start = starting observation (default: 1)
%   --- optimization options ---                                 
%   info.prt   = 1 for printing basic intermediate results (default = 0)
%              = 2 for printing detailed stuff
%   info.delta = Increment in numerical derivs                [.000001]
%   info.hess  = Hessian method: ['dfp'], 'bfgs', 'gn', 'marq', 'sd'
%   info.maxit = Maximium iterations                              [100]
%   info.lamda = Minimum eigenvalue of Hessian for Marquardt      [.01]
%   info.cond  = Tolerance level for condition of Hessian        [1000]
%   info.btol  = Tolerance for convergence of parm vector        [1e-4]
%   info.ftol  = Tolerance for convergence of objective function [sqrt(eps)] 
%   info.gtol  = Tolerance for convergence of gradient           [sqrt(eps)]
% -------------------------------------------------------------------
% NOTES: only works for a 2-state model, i.e., m=2 (see above)
%        therefore the x-matrix should not contain lagged dependent variables
% -------------------------------------------------------------------
% RETURNS: a result structure
%       result.meth   = 'tvp_markov'
%       result.parm   = (k+4) x 1 vector of ML parameter estimates
%       result.vcov   = (k+4,k+4) matrix with variance-covariance of the ML estimates
%       result.stdhat = (k+4) x 1 vector with std deviation of estimates             
%       result.prob1  = (start:n x 1) vector with probability of state 1
%       result.prob2  = (start:n x 1) vector with probability of state 2
%       result.tstat  = a (k+4) x 1 vector of t-stats based on vcov
%       result.beta1  = an (start:n x k) matrix of time-varying beta hats for state 1
%       result.beta2  = an (start:n x k) matrix of time-varying beta hats for state 2
%       result.bvar1  = an (start:n x kxk) matrix of time-varying variances for beta1
%       result.bvar2  = an (start:n x kxk) matrix of time-varying variances for beta2
%       result.fvar   = an (start:n x 2) conditional variance decompostion
%                       column1 = forecast error variance due to beta variation, 
%                       column2 = forecast error variance due to heteroscedasticity
%       result.rsqr   = R-squared, based on yhat(start:n)
%       result.rbar   = R-bar squared, based on yhat(start:n)
%       result.yhat   = (start:n x 1) vector of proability weigthed predicted values
%       result.y      = (nx1) vector of actual values
%       result.like   = log likelihood (at solution values)
%       result.iter   = # of iterations taken
%       result.start  = # of starting observation       
%       result.time   = time (in seconds) for solution
% -------------------------------------------------------------------
% NOTE: to generate tvp betas, probs, etc., based on max-lik parm vector use:
%       [p0s,p1s,b0s,b1s,v0s,v1s,yhat] = tvp_markov_filter(parm,y,x,b0,v0)
% -------------------------------------------------------------------
% SEE ALSO: prt(), plt(), prt_tvp, plt_tvp, tvp_markov_like, tvp_markov_like
% -------------------------------------------------------------------
% REFERENCES: Kim and Nelson (1999)
% State-Space Models with Regime Switching
% -------------------------------------------------------------------


% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n k] = size(x);
IK = eye(k);

% default initial values
priorb0 = zeros(k,1);
priorv0 = IK*1000;
infoz.maxit = 500; % default optimization options
start = 1;

if nargin == 4 % we need to reset optimization defaults
if ~isstruct(info)
  error('tvp: optimization options should be in a structure variable');
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
    elseif strcmp(fields{i},'lamda')
        infoz.lambda = info.lamda;  
    elseif strcmp(fields{i},'start')
        start = info.start; 
    elseif strcmp(fields{i},'b0')
        priorb0 = info.b0;  
    elseif strcmp(fields{i},'v0')
        priorv0 = info.v0;      
    end;
  end;
end;

b0 = priorb0;
v0 = priorv0;
        
if length(parm) ~= 3+k+1
error('tvp_markov: Wrong # of initial parameter values entered');
end;

oresult = maxlik('tvp_markov_lik',parm,infoz,y,x,start,b0,v0);

niter = oresult.iter;
like = -oresult.f;
vcov = inv(oresult.hess);
time = oresult.time;
parm1 = abs(oresult.b);

% compute numerical hessian at the solution
cov0 = inv(fdhess('tvp_markov_lik',parm1,y,x,start,b0,v0));
grad = fdjac('ham_trans',parm1);
vcov = grad*cov0*grad';
stdhat = sqrt(diag(vcov));


result2 = tvp_markov_filter(parm1,y,x,start,b0,v0);

% transform parameters
parm = ham_trans(parm1);
tstat = parm./stdhat;


yhat = result2.yhat;
resid = y(start:n,1) - yhat;
sigu = resid'*resid;
ym = y(start:n,1) - mean(y(start:n,1));
rsqr1 = sigu;
rsqr2 = ym'*ym;
result.rsqr = 1.0 - rsqr1/rsqr2; % r-squared
rsqr1 = rsqr1/(n-k-start);
rsqr2 = rsqr2/(n-1.0);
result.rbar = 1 - (rsqr1/rsqr2); % rbar-squared

stdhat = sqrt(diag(vcov));
tstat = parm1./stdhat;

% return results structure information
result.parm = parm;
result.prob1 = result2.p0;
result.prob2 = result2.p1;
result.beta1 = result2.b0;
result.beta2 = result2.b1;
result.bvar1 = result2.v0;
result.bvar2 = result2.v1;
result.fvar  = result2.fvar;
result.vcov = vcov;
result.stdhat = stdhat;
result.tstat = tstat;
result.yhat = yhat;
result.y = y;
result.resid = resid;
result.like = like;
result.time = time;
result.nobs = n;
result.nvar = k;
result.iter = niter;
result.meth = 'tvp_markov';
result.start = start;


function result = tvp_markov_filter(parm,y,x,start,b0,v0)
% PURPOSE: Kalman filtering for Markov-switching TVP model 
%          y(t) = X(t)*B(t) + e(t), e(t) = N(0,R_S1t)
%          B(t) = B(t-1) + v(t),    v(t) = N(0,Q_S2t)
%          Q_S2t = Q1*a1,1t + Q2*a2,1t + Q3*a3,1t + Q4*a4,1t + ... + Sm*am,1t
%          R_S1t = R1*b1,1t + R2*b2,2t + R3*b3,2t + R4*b4,2t + ... + Rm*bm,2t
%          where: ak,jt = 1 if S_kt = j, ak,jt = 0 if S_kt ~= j
%          k=1,2  j=1,...,m
%          S_1t, S_2t evolve according to a 1st-order Markov process   
% -------------------------------------------------
% USAGE: result  = tvp_markov_filter(parm,y,x,start,b0,v0)
% where: parm = a vector with ML parameter estimates
%        parm(1,1) = p, Pr[St=1 | St-1=1]
%        parm(2,1) = q, Pr[St=0 | St-1=0]
%        parm(3:3+k-1,1) = diagonal transition equation variance
%        parm(3+k,1)   = noise std in state 1
%        parm(3+k+1,1) = noise std in state 2
%        y = dependent variable (nx1) vector
%        x = (nxk) matrix of explanatory variables
%    start = starting observation (default: 1)
%        b0 = prior b0 (default: diffuse = 0)
%        v0 = prior v0 (default: diffuse = 1000)
% -------------------------------------------------
% NOTES: 1) the filter starts at obs=1, but only returns information
%           from start onward     
%        2) only works for a 2-state model, i.e., m=2 (see above)
%           therefore the x-matrix should not contain lagged dependent variables         
% -------------------------------------------------
% RETURNS: result = a structure variable with fields:
%          result.p0 = (start:n x 1) probabilities for state0
%          result.p1 = (start:n x 1) probabilities for state1
%          result.b0 = (start:n x k) state0 TVP estimates  
%          result.b1 = (start:n x k) state1 TVP estimates
%          result.v0 = (start:n x kxk) state0 TVP variances
%          result.v1 = (start:n x kxk) state1 TVP variances
%       result.yhat = (start:n x 1) predicted values based on probabilities
%              yhat(i,1) = x(t,:)*b0*prob0 + x(t,:)*b1*prob1;
%       result.fvar = (start:n x 2) vector with forecast error variance
%       decomposition. Column 1 = conditional variance due to changing beta
%                      Column 2 = conditional variance due to heterscedasticity
%       Column 1 = x_t-1 *P_t|t-1 x'_t-1, 
%                  where: P_t|t_1 = probability weighted sum over both states, 
%       Column 2 =  sig = probability weighted sum over both states,
%                   
% -------------------------------------------------

% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[n k] = size(x);
IK = eye(k);

if nargin == 3
priorb0 = zeros(k,1);
priorv0 = IK*1000;
start = 1;
elseif nargin == 4
priorb0 = zeros(k,1);
priorv0 = IK*1000;
elseif nargin == 6
priorb0 = b0;
priorv0 = v0;
else
error('tvp_markov_filter: Wrong # of arguments');
end;


parm = ham_trans(parm); % 1/(1+exp(-parm))

ppr = parm(1,1); % Pr[St=1 | St-1=1]
qpr = parm(2,1); % Pr[St=0 | St-1=0]

aa = eye(k);

prob1 = (1-qpr)/(2-ppr-qpr); % Pr[St-1=1 | Yt-1], STEADY STATE PROB
prob0 = 1-prob1;             % Pr[ST-1=0 | Yt-1], STEADY STATE PROB

b0 = priorb0;
b1 = b0;
v0 = priorv0;
v1 = v0;

tmp = parm(3:3+k-1,1); % transition equation std deviations
tmp2 = tmp.*tmp;
qq = diag(tmp2);

sig0 = parm(3+k,1);
sig1 = parm(3+k+1,1);

like = 0;

% storage for return values
fcast = zeros(n,1);
b0s = zeros(n,k);
b1s = zeros(n,k);
v0s = zeros(n,k,k);
v1s = zeros(n,k,k);
p0s = zeros(n,1);
p1s = zeros(n,1);
fvar = zeros(n,2);
 
for i=1:n;

H = x(i,:);

% prediction
postb00 = aa*b0; % for St-1 = 0, St = 0
postb01 = aa*b0; % for St-1 = 0, St = 1
postb10 = aa*b1; % for St-1 = 1, St = 0
postb11 = aa*b1; % for St-1 = 1, St = 1

postv00 = aa*v0*aa' + qq;
postv01 = aa*v0*aa' + qq;
postv10 = aa*v1*aa' + qq;
postv11 = aa*v1*aa' + qq;

fcast00 = y(i,1) - H*postb00;
fcast01 = y(i,1) - H*postb01;
fcast10 = y(i,1) - H*postb10;
fcast11 = y(i,1) - H*postb11;

ss00 = H*postv00*H' + sig0*sig0;
ss01 = H*postv01*H' + sig1*sig1;
ss10 = H*postv10*H' + sig0*sig0;
ss11 = H*postv11*H' + sig1*sig1;

% DEBUG
if (ss00 <= 0 | ss01 <= 0 | ss10 <= 0 | ss11 <= 0)
        [ss00 ss01 ss10 ss11]
        pause;
end;


kg00 = postv00*H' / ss00;
kg01 = postv01*H' / ss01;
kg10 = postv10*H' / ss10;
kg11 = postv11*H' / ss11;

b00 = postb00 + kg00*fcast00;
b01 = postb01 + kg01*fcast01;
b10 = postb10 + kg10*fcast10;
b11 = postb11 + kg11*fcast11;

v00 = (IK - kg00*H)*postv00;
v01 = (IK - kg01*H)*postv01;
v10 = (IK - kg10*H)*postv10;
v11 = (IK - kg11*H)*postv11;

prv00 = vprob(fcast00,ss00)*qpr*prob0; % pr[St, Yt | Yt-1]
prv01 = vprob(fcast01,ss01)*(1-qpr)*prob0;
prv10 = vprob(fcast10,ss10)*(1-ppr)*prob1;
prv11 = vprob(fcast11,ss11)*ppr*prob1;


% step 3 denominator
prval = prv00+prv01+prv10+prv11; % pr[Yt | Yt-1]

% step 3
pr00 = prv00/prval; 
pr01 = prv01/prval;
pr10 = prv10/prval;
pr11 = prv11/prval;

% step 4
prob0 = pr00 + pr10; % pr[St=0 | Yt]
prob1 = pr01 + pr11; % pr[St=1 | Yt]

% avoid division by zero in cases
% where all probability weight gets placed on one state
if prob0 == 0
        prob0 = 0.001;
elseif prob0 == 1
        prob0 = 0.999;
end;
if prob1 == 0
        prob1 = 0.001;
elseif prob1 == 1
        prob1 = 0.999;
end;


% collapse terms eq 2.13' from Kim's article

b0 = (pr00*b00 + pr10*b10)/prob0;
b1 = (pr10*b10 + pr11*b11)/prob1;

% eq 2.14'
v0 = (pr00*(v00+(b0-b00)*(b0-b00)') ...
          +pr10*(v10+(b0-b10)*(b0-b10)'))/prob0;
v1 = (pr01*(v01+(b1-b01)*(b1-b01)') ...
          +pr11*(v11+(b1-b11)*(b1-b11)'))/prob1;


% compute forecast error decomposition
% see page 119 Kim-Nelson
fvar(i,1) = prob0*H*postv00*H' + prob1*H*postv11*H';
fvar(i,2) = fvar(i,1) + sig0*sig0*prob0 + sig1*sig1*prob1;

b0s(i,:) = b0';
b1s(i,:) = b1';
fcast(i,1) = H*b0*prob0 + H*b1*prob1;
p0s(i,1) = prob0;
p1s(i,1) = prob1;
v0s(i,:,:) = v0;
v1s(i,:,:) = v1;


end; % end of loop over observations


result.b0 = b0s(start:n,:);
result.b1 = b1s(start:n,:);
result.yhat = fcast(start:n,1);
result.p0 = p0s(start:n,1);
result.p1 = p1s(start:n,1);
result.v0 = v0s(start:n,1);
result.v1 = v1s(start:n,1);
result.fvar = fvar(start:n,:);
 

