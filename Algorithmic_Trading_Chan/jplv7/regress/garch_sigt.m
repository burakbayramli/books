function sigt =  garch_sigt(parm,y,x)
% PURPOSE: generate garch model sigmas over time 
%          given maximum likelihood estimates
% -------------------------------------------------------------
% USAGE: sigt = garch_sigt(parm,y,x)
% where: parm = a vector of maximum likelihood estimates
%        y = data vector
%        x = data matrix
% -------------------------------------------------------------
% RETURNS:   sigt = (Tx1) vector of sigma(t) estimates      
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

b = zeros(k,1);
for i=1:k;
b(i,1) = parm(i,1);
end;
a0 = parm(k+1,1);
a1 = parm(k+2,1);
a2 = parm(k+3,1);

sigt = zeros(n,1);
ivar = a0/(1-a1-a2); % initial variance

% compute residuals
e = y - x*b;

% generate sigt and log likelihood
sigt = zeros(n,1);

for i = 1:n;
 if i == 1
 sigt(i,1) = a0 + a1*ivar + a2*ivar;
 else
 sigt(i,1) = a0 + a1*sigt(i-1,1) + a2*e(i-1,1)*e(i-1,1);
 end;

end;

