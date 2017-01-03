function lik = tvp_like2(parm,y,x)
% PURPOSE: returns log likelihood function for tvp model
% -------------------------------------------------------
% USAGE: llike = tvp_like(parm,y,x)
% where: parm = a vector of parmaeters
%        parm(1) = sig epsilson
%        parm(2) = sig beta 1
%        parm(3) = sig beta 2
%        .
%        .
%        .
%        parm(k) = sig beta k

sige = parm(1);

k = length(parm) - 1;
n = length(y);
start = 2*k+1; % use initial observations for startup

sigb = zeros(k,1);
for i=1:k;
sigb(i,1) = parm(i+1,1);
end;

f = eye(k);
rr = sige^2;
qq = diag(sigb.*sigb);
betall = zeros(k,1); % initial guess for betas
pll = eye(k)*50; % diffuse prior uncertainty

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
% lik
 end;
end;

lik = -lik;
