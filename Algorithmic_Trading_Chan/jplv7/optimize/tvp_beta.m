function [beta, ferror] =  tvp_beta(parm,y,x)
% PURPOSE: generate tvp model betas and forecast error variance
%          given maximum likelihood estimates
% -------------------------------------------------------------
% USAGE: [beta ferror] = tvp_beta(parm,y,x)
% where: parm = a vector of maximum likelihood estimates
%        y = data vector
%        x = data matrix
% -------------------------------------------------------------
% RETURNS:   beta = (Txk) matrix of tvp beta estimates
%          ferror = (Tx2) matrix with forecast error and
%                         conditional variance        
% -------------------------------------------------------------

n = length(y);
k = length(parm) - 1;

start = 2*k+1;
lik = 0;

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
betall = zeros(k,1);
pll = eye(k)*50;

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

 if iter >= start
 lik = lik + 0.5*(log(2*pi*ss) + (fcast.^2)/ss);

 end;

betall = betatt;
pll = ptt;

end;

 lik

