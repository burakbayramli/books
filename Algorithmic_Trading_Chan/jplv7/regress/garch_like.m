function llik = garch_like(parm,y,x)
% PURPOSE: log likelihood for garch model
% -------------------------------------------------------
% USAGE: llike = garch_like(parm,y,x)
% where: parm = a vector of parmaeters
%        parm(1) = beta 1
%        parm(2) = beta 2
%        .
%        .
%        .
%        parm(k) = beta k
%        parm(k+1) = a0
%        parm(k+2) = a1
%        parm(k+3) = a2
% ----------------------------------------------------
% RETURNS: -log likelihood function value (a scalar)                  
% ----------------------------------------------------
% REFERENCES: Green (2000) Econometric Analysis
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometics.com


[n k] = size(x);

% transform parameters
parm = garch_trans(parm);

b = zeros(k,1);
for i=1:k;
b(i,1) = parm(i,1);
end;
a0 = parm(k+1,1);
a1 = parm(k+2,1);
a2 = parm(k+3,1);

tst = (1-a1-a2);
if tst ~= 0
ivar = a0/(1-a1-a2); % initial variance
else
ivar = 0.01;
end;

% compute residuals
e = y - x*b;
ss = (e'*e)/(n-k);

% generate sigt and log likelihood
sigt = zeros(n,1);
loglik = zeros(n,1);

for i = 1:n;
 if i == 1
 sigt(i,1) = a0 + a1*ss + a2*ss;
 else
 sigt(i,1) = a0 + a1*sigt(i-1,1) + a2*e(i-1,1)*e(i-1,1);
 end;

loglik(i,1) = -0.5*(log(2*pi) + log(sigt(i,1)) + (e(i,1)*e(i,1))/sigt(i,1));

end;

llik = -sum(loglik);
