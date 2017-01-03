function result = lratios(y,x,W,res);
% PURPOSE: computes likelihood ratio test for spatial correlation
%          in the errors of a regression model
% ---------------------------------------------------
%  USAGE: result = lratios(y,x,W)
%     or: result = lratios(y,x,W,sem_result);
%  where:   y = dependent variable vector
%           x = independent variables matrix
%           W = contiguity matrix (standardized or unstandardized)
%  sem_result = a results structure from sem()
% ---------------------------------------------------
%  RETURNS: a  structure variable
%         result.meth   = 'lratios'
%         result.lratio = likelihood ratio statistic
%         result.chi1   = 6.635 (chi-squared 1 dof at 99% level)
%         result.prob   = marginal probability
%         result.nobs   = # of observations
%         result.nvar   = # of variables in x-matrix
% ---------------------------------------------------
% NOTES: lratio > 6.635, => small prob,
%                        => reject HO: of no spatial correlation
%        calling the function with a results structure from sem()
%        can save time for large models that have already been estimated                 
% ---------------------------------------------------
% See also: lmerror, walds, moran, lmsar
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


if nargin == 3
[n k] = size(x);
% do ols to get residuals
b = (x'*x)\(x'*y); e0 = y - x*b; epe0 = e0'*e0; sig0 = epe0/n;
% do sem to get residuals
res = sem(y,x,W); 

elseif nargin == 4
 if ~isstruct(res)
  error('lratios: requires results structure variable from sem');
 elseif ~strcmp(res.meth,'sem')
  error('lratios: requires results structure variable from sem');
 end;
[n k] = size(x);
b = (x'*x)\(x'*y); e0 = y - x*b; epe0 = e0'*e0; sig0 = epe0/n;
end;

sig1 = res.sige;
lam = res.rho;
% compute determinant of I-lam*W
spparms('tight'); 
z = speye(n) - 0.1*sparse(W); p = colmmd(z);
z = speye(n) - lam*sparse(W);
[l,u] = lu(z(:,p));
detval = sum(log(abs(diag(u))));

lratio = n*(log(sig0) - log(sig1)) + 2*detval;


result.meth = 'lratios';
result.nobs = n;
result.nvar = k;
result.lratio = lratio;
result.chi1   = 6.635;
result.prob   = 1-chis_prb(lratio,1);
