function llike = darp_lik2(parm,y,x,d)
% PURPOSE: evaluate the log-likelihood for the DARP model
%          based on distance expansion model
% ---------------------------------------------------
%  USAGE:llike = darp_lik2(parm,y,x,d)
%  where: parm  = a parameter vector containing:
%         parm(1,1)     = sige
%         parm(2,1)     = gamma
%         parm(3:k+2,1) = beta
%         y    = dependent variable vector
%         x    = explanatory variables matrix
%         d    = distance from central place
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value given the parameters
%  --------------------------------------------------
%  SEE ALSO: darp, darp_lik1
% ---------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x);
evar = length(parm);
sige = parm(1,1);
if sige < 0.0001; sige = 0.0001; end;
gamma = parm(2,1); beta = parm(3:evar,1);
phi = exp(sige + gamma*d);
phii = ones(n,1)./phi;
detphi = 0.5*log(prod(phi));
ys = sqrt(phii).*y;
% form expansion x-matrix
xt = x(:,2:k);
xx = matmul(xt,d);
xmat = [x xx];
xs = matmul(xmat,sqrt(phii));
e = (ys-xs*beta);
epe = (e'*e);
llike =   0.5*epe + detphi;

