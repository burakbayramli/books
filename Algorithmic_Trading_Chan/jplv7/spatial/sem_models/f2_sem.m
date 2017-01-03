function llike = f2_sem(parm,y,x,W,detval);
% PURPOSE: evaluates SEM log-likelihood -- given ML parameters using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f2_sem(parm,y,X,W,detm)
%  where: parm = vector of maximum likelihood parameters
%                parm(1:k-2,1) = b, parm(k-1,1) = rho, parm(k,1) = sige
%         y    = dependent variable vector (n x 1)
%         X    = explanatory variables matrix (n x k)
%         W    = spatial weight matrix
%         ldet = matrix with [rho log determinant] values
%                computed in sem.m using one of Kelley Pace's routines
% ---------------------------------------------------                                           
%  NOTE: this is really two functions depending
%        on nargin = 3 or nargin = 4 (see the function)
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the ML parameters
%  --------------------------------------------------
%  SEE ALSO: sem, f2_sem2, f_sem
% ---------------------------------------------------

% written by: James P. LeSage 4/2002
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial.econometrics.com

n = length(y);
k = length(parm);
b = parm(1:k-2,1);
rho = parm(k-1,1);
sige = parm(k,1);


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

e = y - x*b;
ed = e - rho*W*e;
epe = ed'*ed;
tmp = 1/(2*sige);

llike =  detm - (n/2)*log(sige) - (n/2)*log(pi) - tmp*epe;
