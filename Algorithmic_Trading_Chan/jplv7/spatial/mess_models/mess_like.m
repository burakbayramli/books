function out = mess_like(parm,ys,xs,ymat);
% PURPOSE: evaluate the likelihood for the hessian function
%          MESS model
% ---------------------------------------------------
%  USAGE: out = mess_lik2(parm,y,x,ymat)
%  where:  parm = vector of parameters
%            [bhat   : k-vector of bhat
%             alpha  : matrix exponential alpha spatial parameter
%             sige]  : sigma estimate
%            y    = dependent variable vector
%            x    = explanatory variables matrix
%         ymat    = spatially transformed y from mess()
% ---------------------------------------------------
%  RETURNS: a log likelihood function value
%  NOTE: called only by mess
%  --------------------------------------------------
%  SEE ALSO: mess, mess_d
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


n = length(ys);
npar = length(parm);
k = npar - 2;
beta = parm(1:k,1);
alpha = parm(k+1,1);
sige = parm(npar,1);

[junk nq] = size(ymat);

W = (1./gamma(1:nq))';
v = ones(nq,1);
for i=2:nq;
v(i,1) = alpha.^(i-1);
end;
Sy = ymat*diag(W)*v;
e = Sy - xs*beta;
epe = e'*e;
out = (n/2)*log(pi) + (n/2)*log(sige) + epe/(2*sige);
