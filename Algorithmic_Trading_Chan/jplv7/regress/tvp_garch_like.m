function llik = tvp_garch_like(parm,y,x,start,priorb0,priorv0)
% PURPOSE: log likelihood for tvp_garch model
% -------------------------------------------------------
% USAGE: llike = tvp_garch_like(parm,y,x,start,priorb0,priorv0)
% where: parm = a vector of parmaeters
%        parm(1) = sig beta 1
%        parm(2) = sig beta 2
%        .
%        .
%        .
%        parm(k) = sig beta k
%        parm(k+1) = a0
%        parm(k+2) = a1
%        parm(k+3) = a2
%        start     = # of observation to start at
%                    (default: 2*k+1)
%        priorb0 = a (k+1)x1 vector with prior for b0
%                (default: zeros(k+1,1), a diffuse prior)            
%        priorv0 = a (k+1)x(k+1) matrix with prior for sigb
%                (default: eye(k+1)*1e+5, a diffuse prior)                  
% ----------------------------------------------------
% RETURNS: -log likelihood function value (a scalar)                  
% ----------------------------------------------------
% REFERENCES: Kim and Nelson (1999)
% State-Space Models with Regime Switching
% ----------------------------------------------------

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
start = 2*k+1; % use initial observations for startup
priorv0 = eye(k+1)*1e+5;
priorb0 = zeros(k+1,1);
elseif nargin == 4
priorv0 = eye(k+1)*1e+5;
priorb0 = zeros(k+1,1);
elseif nargin == 6
% do nothing
else
error('tvp_garch_like: Wrong # of input arguments');
end;


sigb = zeros(k,1);
for i=1:k;
sigb(i,1) = parm(i,1)*parm(i,1);
end;
a0 = parm(k+1,1);
a1 = parm(k+2,1);
a2 = parm(k+3,1);

ivar = a0/(1-a1-a2); % initial variance

r = 0;

f = eye(k+1);
f(k+1,k+1) = 0;
g = eye(k+1);
cll = priorb0;
pll = priorv0; % initial var-cov for reg coef
pll(k+1,k+1) = ivar;
htl = ivar;


loglik = zeros(n,1);

for iter = 1:n;

h = [x(iter,:) 1];

ht = a0 + a1*(cll(k+1,1)*cll(k+1,1) + pll(k+1,k+1)) + a2*htl;

tmp = [sigb
       ht];

Q = diag(tmp);

ctl = f*cll;
ptl = f*pll*f' + g*Q*g';

vt = y(iter,1) - h*ctl; % prediction error
ft = h*ptl*h' + r;      % variance of forecast error

ctt = ctl + ptl*h'*(1/ft)*vt;
ptt = ptl - ptl*h'*(1/ft)*h*ptl;


lik = (1/sqrt(2*pi*abs(ft)))*exp(-0.5*vt'*(1/ft)*vt);

loglik(iter,1) = -log(lik);


cll = ctt;
pll = ptt;
htl = ht;

end;

llik = sum(loglik(start:n,1));

