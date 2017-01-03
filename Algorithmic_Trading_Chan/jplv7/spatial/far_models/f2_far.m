function llike = f2_far(parm,y,W,detval)
% PURPOSE: evaluate the log-likelihood for ML rho,sigma values
% for the first-order spatial autoregressive model
% ---------------------------------------------------
%  USAGE: llike = f2_far(parm,y,W,ldet)
%  where: parm  = 2x1 vector with rho,sigma ML values
%          y    = dependent variable vector
%          W    = spatial weight matrix
%         ldet = matrix with [rho log determinant] values
%                computed in far.m using one of Kelley Pace's routines          
% ---------------------------------------------------
%  RETURNS: a scalar equal to minus the log-likelihood
%           function value at the parameters rho,sigma
%  --------------------------------------------------
%  NOTE: this is really two functions depending
%        on nargin = 3 or nargin = 4 (see the function)
% ---------------------------------------------------  
%  SEE ALSO: far, f2_sar, f2_sac, f2_sem
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial.econometrics.com

n = length(y);
rho = parm(1,1); sige = parm(2,1);

gsize = detval(2,1) - detval(1,1);

i1 = find(detval(:,1) <= rho + gsize);
i2 = find(detval(:,1) <= rho - gsize);
i1 = max(i1);
i2 = max(i2);
index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
detm = detval(index,2);

n = length(y);
e = (speye(n) - rho*sparse(W))*y;
epe = e'*e;
tmp = 1/(2*sige);
llike = -(n/2)*log(pi) - (n/2)*log(sige) - tmp*epe + detm;


