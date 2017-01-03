function lik = tvp_like(parm,y,x,start,priorb0,priorv0)
% PURPOSE: returns -log likelihood function for tvp model
% -------------------------------------------------------
% USAGE: llike = tvp_like(parm,y,x,start)
% where: parm = a vector of parmaeters
%        parm(1) = sig epsilson
%        parm(2) = sig beta 1
%        parm(3) = sig beta 2
%        .
%        .
%        .
%        parm(k) = sig beta k
%        start   = # of observation to start at
%                  (default: 2*k+1)
%        priorb0   = (k x 1) vector with prior b0          
%        priorv0   = (k x k) matrix with prior variance
%                  for sigb                            
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


sige = parm(1);
k = length(parm) - 1;
n = length(y);

if nargin == 4
priorv0 = eye(k)*1e+5;
priorb0 = zeros(k,1);
end;

sigb = zeros(k,1);
for i=1:k;
sigb(i,1) = parm(i+1,1);
end;

f = eye(k);
rr = sige^2;
qq = diag(sigb.*sigb);
betall = priorb0;  % initial guess for betas
pll    = priorv0;  % prior uncertainty

lik = 0;
for iter = 1:n;
xt = x(iter,:);
yt = y(iter,1);

betatl = f*betall;
ptl = f*pll*f' + qq;
fcast = yt - xt*betatl;
ss = xt*ptl*xt' + rr;
betatt = betatl + (ptl*xt'/ss)*fcast;
ptt = (eye(k) - (ptl*xt'/ss)*xt)*ptl;
betall = betatt;
pll = ptt;

 if iter >= start
 lik = lik + 0.5*(log(2*pi*ss) + (fcast.^2)/ss);
 end;
end;

