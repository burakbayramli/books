function result=lmlag(y,x,W)
% PURPOSE: LM lag statistic for spatial correlation in dependent variable
%          of a regression model
% ---------------------------------------------------
%  USAGE: result = lmlag(y,x,W)
%  where: y = dependent variable vector
%         x = independent variables matrix
%         W = contiguity matrix (standardized)
% ---------------------------------------------------
%  RETURNS: a structure variable
%         result.meth = 'lmlag'
%         result.lm   = LM statistic
%         result.prob = marginal probability
%         result.chi1 = 6.635 (chi-squared 1 dof at 99% level)
%         result.nobs = # of observations
%         result.nvar = # of variables
% ---------------------------------------------------
% NOTE: lm > 6.635,  => small prob,
%                    => reject HO: of no spatial correlation
% ---------------------------------------------------
% See also:  walds, lratios, lmerror, moran
% ---------------------------------------------------
% REFERENCES: Anselin (1988), pages 103-104.
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
% modified by Shifeng Wang, Dept of Remote Sensing
% University of Freiburg 
% wangsf1013@gmail.com


if nargin ~= 3
error('Wrong # of arguments to lmlag');
end;
[n k] = size(x);
% do ols to get residuals
b = x\y; e = y-x*b;
sigma  = (e'*e)/(n-k);


epe = (e'*e)/n;
lm1 = (e'*W*y)/epe;

t1 = trace((W+W')*W);
D1=W*x*b;
M=eye(n)-x*inv((x'*x))*x';
D=(D1'*M*D1)*(1/sigma)+t1;


lmlag = (lm1*lm1)*(1/D);
prob = 1-chis_prb(lmlag,1);

result.meth = 'lmlag';
result.lm = lmlag;
result.prob = prob;
result.chi1   = 17.611;
result.nobs = n;
result.nvar = k;
