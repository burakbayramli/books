function llike = f_far(rho,y,W,detval)
% PURPOSE: evaluate the concentrated log-likelihood 
%  1st order spatial autoregressive model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f_far(rho,y,W)
%  where: rho  = spatial autoregressive parameter
%         y    = dependent variable vector
%         W    = spatial weight matrix
%         ldet = matrix with [rho log determinant] values
%                computed in far.m using one of 
%                Pace and Barry's routines         
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the parameter rho
%  --------------------------------------------------
%  SEE ALSO: f_far2, far, f_sar, f_sac, f_sem
% ---------------------------------------------------

% written by: James P. LeSage 4/2002
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


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
e = (speye(n) - rho*sparse(W))*y;;
epe = e'*e;
llike = (n/2)*log(epe) - detm;
