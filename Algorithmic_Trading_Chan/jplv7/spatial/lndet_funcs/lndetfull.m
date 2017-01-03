function out=lndetfull(W,lmin,lmax)
% PURPOSE: computes Pace and Barry's grid for log det(I-rho*W) using sparse matrices
% -----------------------------------------------------------------------
% USAGE: out = lndetfull(W,lmin,lmax)
% where:    
%             W     = symmetric spatial weight matrix (standardized)
%             lmin  = lower bound on rho
%             lmax  = upper bound on rho
% -----------------------------------------------------------------------
% RETURNS: out = a structure variable
%          out.lndet = a vector of log determinants for 0 < rho < 1
%          out.rho   = a vector of rho values associated with lndet values
% -----------------------------------------------------------------------
% NOTES: should use 1/lambda(max) to 1/lambda(min) for all possible rho values
% -----------------------------------------------------------------------
% References: % R. Kelley Pace and  Ronald Barry. 1997. ``Quick
% Computation of Spatial Autoregressive Estimators'', Geographical Analysis
% -----------------------------------------------------------------------
 
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

rvec = lmin:.01:lmax;
spparms('tight'); 
[n junk] = size(W);
z = speye(n) - 0.1*sparse(W);
p = colamd(z);
niter = length(rvec);
dettmp = zeros(niter,2);
for i=1:niter;
    rho = rvec(i);
    z = speye(n) - rho*sparse(W);
    [l,u] = lu(z(:,p));
    dettmp(i,1) = rho;
    dettmp(i,2) = sum(log(abs(diag(u))));
end;

out.lndet = dettmp(:,2);
out.rho = dettmp(:,1);
