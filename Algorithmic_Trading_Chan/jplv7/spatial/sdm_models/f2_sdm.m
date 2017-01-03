
function llike = f2_sdm(parm,y,x,W,detval)
% PURPOSE: evaluates full log-likelihood -- given ML estimates
%  spatial Durbin model using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f2_sdm(parm,y,X,W,ldet)
%  where: parm = vector of maximum likelihood parameters
%                parm(1:2*k-2,1) = b, parm(k-1,1) = rho, parm(k,1) = sige
%         y    = dependent variable vector (n x 1)
%         X    = explanatory variables matrix (n x 2*k+1)
%         W    = spatial weight matrix
%         ldet = matrix with [rho log determinant] values
%                computed in sar.m using one of Kelley Pace's routines  
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the ML parameters
% ---------------------------------------------------

% written by:
% James P. LeSage, last updated 3/2010
% Dept of Finance & Economics
% Texas State University-San Marcos
% 601 University Drive
% San Marcos, TX 78666
% jlesage@spatial-econometrics.com

n = length(y); 
k = length(parm);
rho = parm(1,1);
b = parm(2:k-1,1);
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

e = y-x*b-rho*sparse(W)*y;
epe = e'*e;
tmp2 = 1/(2*sige);
llike = -(n/2)*log(pi) - (n/2)*log(sige) + detm - tmp2*epe;


