function llike = darp_lik1(parm,y,x,xc,yc)
% PURPOSE: evaluate the log-likelihood for the DARP model
%          based on x-y expansion model
% ---------------------------------------------------
%  USAGE:llike = darp_lik1(parm,y,x,xc,yc)
%  where: parm  = a parameter vector containing:
%         parm(1)     = sige
%         parm(2)     = gamma
%         parm(3:k+2) = beta
%         y    = dependent variable vector
%         x    = explanatory variables matrix
%         xc   = x-coordinates vector
%         yc   = y-coordinates vector
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value given the parameters
%  --------------------------------------------------
%  SEE ALSO: darp, darp_lik2
% ---------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x);
evar = length(parm);
sige = parm(1,1); if sige < 0.001, sige = 0.001; end;
gamma1 = parm(2,1); gamma2 = parm(3,1); beta = parm(4:evar,1);
phi = exp(sige + gamma1*xc + gamma2*yc);
phii = ones(n,1)./phi;

temp = prod(phi);
if temp > 0.0001;
detphi = 0.5*log(temp);
else 
detphi = 0.0001;
end;
ys = sqrt(phii).*y;
% form expansion x-matrix
xt = x(:,2:k);
xx = matmul(xt,xc);
xy = matmul(xt,yc);
xmat = [x xx xy];
xs = matmul(xmat,sqrt(phii));
e = (ys-xs*beta);
epe = 0.5*(e'*e);
llike = (n/2)*log(2*pi) + epe + detphi;

