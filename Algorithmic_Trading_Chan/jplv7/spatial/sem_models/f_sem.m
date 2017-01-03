function lik = f_sem(rho,e,W,detval)
% PURPOSE: evaluates SEM concentrated log-likelihood using sparse matrix algorithms
% ---------------------------------------------------
%  USAGE:llike = f_sem(lam,eD,W,detm)
%  where: rho  = spatial error parameter
%         e    = begls residuals
%         W    = spatial weight matrix
%         detm =  matrix with [rho log determinant] values
%                computed in sem.m using one of 
%                Kelley Pace's routines           
% ---------------------------------------------------
%  RETURNS: a  scalar equal to minus the log-likelihood
%           function value at the parameter rho
%  --------------------------------------------------
%  NOTE: this is really two functions depending
%        on nargin = 3 or nargin = 4 (see the function)
% ---------------------------------------------------        
%  SEE ALSO: sem, f_far, f_sac, f_sar
% ---------------------------------------------------

% written by: James P. LeSage 1/2000
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

ed = e - rho*W*e;
n = length(ed);
lik =  (n/2)*log(ed'*ed) - detm;

