function result = walds(y,x,W);
% PURPOSE: Wald statistic for spatial autocorrelation in residuals
%          of a regression model
% ---------------------------------------------------
%  USAGE: result = walds(y,x,W)
%  where: y = dependent variable vector
%         x = independent variables matrix
%         W = contiguity matrix (standardized)
% ---------------------------------------------------
%  RETURNS: a structure variable
%         result.meth = 'walds'
%         result.wald = Wald statistic
%         result.prob = marginal probability
%         result.chi1 = 6.635 (chi-squared 1 dof at 99% level)
%         result.nobs = # of observations
%         result.nvar = # of variables
% ---------------------------------------------------
% NOTE: (wald > 6.635,  => small prob,
%                       => reject HO: of no spatial correlation
% ---------------------------------------------------
% See also:  lmerror, lratios, moran
% ---------------------------------------------------
% REFERENCES: Anselin (1988), pages 103-104.
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


if nargin ~= 3
error('Wrong # of arguments to walds');
end;

% get ML estimate of lambda
res = sem(y,x,W);
lam = res.rho;
[n k] = size(x);

spparms('tight'); 
z = speye(n) - 0.1*sparse(W);
p = colmmd(z);
z = speye(n) - lam*sparse(W);
zi = inv(z);
t1 = trace(W.*z);
t2 = trace(W*z)^2;
t3 = trace((W*z)'*(W*z));
walds = (lam^2) *(t2 + t3 - (1/n)*(t1*t1));
prob = 1-chis_prb(walds,1);

result.meth = 'walds';
result.wald = walds;
result.prob = prob;
result.chi1   = 6.635;
result.nobs = n;
result.nvar = k;
