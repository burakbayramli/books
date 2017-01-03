function result = lmerror(y,x,W);
% PURPOSE: LM error statistic for spatial correlation in residuals
%          of a regression model
% ---------------------------------------------------
%  USAGE: result = lmerror(y,x,W)
%  where: y = dependent variable vector
%         x = independent variables matrix
%         W = contiguity matrix (standardized)
% ---------------------------------------------------
%  RETURNS: a structure variable
%         result.meth = 'lmerror'
%         result.lm   = LM statistic
%         result.prob = marginal probability
%         result.chi1 = 6.635 (chi-squared 1 dof at 99% level)
%         result.nobs = # of observations
%         result.nvar = # of variables
% ---------------------------------------------------
% NOTE: lm > 6.635,  => small prob,
%                    => reject HO: of no spatial correlation
% ---------------------------------------------------
% See also:  walds, lratios, moran
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
error('Wrong # of arguments to lmerror');
end;
[n k] = size(x);
% do ols to get residuals
b = (x'*x)\(x'*y); e = y-x*b;
epe = (e'*e)/n;

t1 = trace((W+W')*W);
lm1 = (e'*W*e)/epe;
lmerr = (lm1*lm1)*(1/t1);
prob = 1-chis_prb(lmerr,1);

result.meth = 'lmerror';
result.lm = lmerr;
result.prob = prob;
result.chi1   = 17.611;
result.nobs = n;
result.nvar = k;
