function llike = f2_sac(parm,y,x,W1,W2,det1,det2)
% PURPOSE: evaluates log-likelihood for ML values general spatial model
%    y = rho*W1*y + X*b + u,   u = lam*W2*u + e,  
%        using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f2_sac(parm,y,x,W1,W2,det1,det2)
%  where: parm  = (beta,rho,lam,sige) ML values
%         y     = dependendent variable vector
%         x     = explanatory variables matrix
%         W1    = spatial lag weight matrix
%         W2    = spatial error weight matrix
%         det1  = matrix with [rho log determinant] values
%                 computed in sac.m using one of 
%                 Pace and Barry's routines 
%         det2  = matrix with [lam log determinant] values
%                 computed in sac.m using one of 
%                 Pace and Barry's routines 
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the ML parameters 
% --------------------------------------------------
%  NOTE: this is really two functions depending
%        on nargin = 5 or nargin = 7 (see the function)
%  --------------------------------------------------   
%  SEE ALSO: sac, f2_far, f2_sar, f2_sem
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

n = length(y);
k = length(parm); 
b = parm(1:k-3,1);
rho = parm(k-2,1); 
lam = parm(k-1,1); 
sige = parm(k,1);

 gsize = det1(2,1) - det1(1,1);
 i1 = find(det1(:,1) <= rho + gsize);
 i2 = find(det1(:,1) <= rho - gsize);
 i1 = max(i1);
 i2 = max(i2);
 index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
 detval1 = det1(index,2);

 gsize = det2(2,1) - det2(1,1);
 i1 = find(det2(:,1) <= lam + gsize);
 i2 = find(det2(:,1) <= lam - gsize);
 i1 = max(i1);
 i2 = max(i2);
 index = round((i1+i2)/2);
if isempty(index)
index = 1;
end;
 detval2 = det2(index,2);
 
In = speye(n);
Ay = (In - rho*W1)*y;

e = Ay - x*b;
ed = e - lam*W2*e;
epe = ed'*ed;
tmp = 1/(2*sige);

llike = -(n/2)*log(pi) - (n/2)*log(sige) + detval1 + detval2 - tmp*epe;
