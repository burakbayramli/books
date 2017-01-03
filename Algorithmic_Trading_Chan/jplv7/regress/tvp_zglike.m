function lik = tvp_zglike(parm,y,x,start,priorb0,priorv0)
% PURPOSE: returns -log likelihood function for tvp model with Zellner's g-prior
%          y(t) = X(t)*B(t) + e(t), e(t) = N(0,sige^2)
%          B(t) = B(t-1) + v(t),    v(t) = N[0,delta*sige*inv(X'X)]
%          (Zellner's g-prior)          
% -------------------------------------------------------
% USAGE: llike = tvp_zglike(parm,y,x,start,priorb0,priorv0)
% where: parm = a vector of parmaeters
%        parm(1) = sig epsilson
%        parm(2) = delta, Zellner's g-prior delta 
%        start   = # of observation to start at
%                  (default: 2*k+1)
%        priorb0   = (k+1) x 1 vector with prior b0          
%        priorv0   = (k+1)x(k+1) matrix with prior variance
%                  for sige, sigb  
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


sige = parm(1,1);
delta = parm(2,1);

[n k] = size(x);


if nargin == 4
priorv0 = eye(k)*1e+5;
priorb0 = zeros(k,1);
end;

xpxi = inv(x'*x);
sigb = sige*sige*delta*delta*xpxi;

f = eye(k);
rr = sige^2;
qq = sigb;
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

