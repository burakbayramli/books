function result = moran(y,x,W);
% PURPOSE: computes Moran's I-statistic for spatial correlation
%          in the residuals of a regression model
% ---------------------------------------------------
%  USAGE: result = moran(y,x,W)
%  where: y = dependent variable vector
%         x = independent variables matrix
%         W = contiguity matrix (standardized or unstandardized)
% ---------------------------------------------------
%  RETURNS: a  structure variable
%         result.meth   = 'moran';
%         result.morani = e'*W*e/e'*e (I-statistic)
%         result.istat  = [i - E(i)]/std(i), standardized version
%         result.imean  = E(i),   expectation
%         result.ivar   = var(i), variance
%         result.prob   = std normal marginal probability
%         result.nobs   = # of observations
%         result.nvar   = # of variables in x-matrix
% ---------------------------------------------------
% NOTES: istat > 1.96, => small prob,
%                     => reject HO: of no spatial correlation
% This function standardizes the weight matrix and produces
% results based on a standardized weights matrix
% ---------------------------------------------------
% See also: lmerrs, walds, lratios
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


if nargin ~= 3
error('Wrong # of arguments to moran');
end;

[n k] = size(x);

% standardize the weight matrix
W = normw(W);

% do ols to get residuals
b = inv(x'*x)*x'*y;
e = y - x*b;
epe = e'*e;
mi = (e'*W*e)/epe;
M = eye(n) - x*(inv(x'*x))*x';
tmw = trace(M*W);

meani = tmw/(n-k);
vari =  trace((M*W)*(M*W')) + trace((M*W)*(M*W)) + tmw*tmw;
vari = vari/((n-k)*(n-k+2));
vari = vari - meani*meani;
mis = (mi-meani)/sqrt(vari);
prob = norm_prb(mis);

result.meth = 'moran';
result.nobs = n;
result.nvar = k;
result.morani = mi;
result.istat  = mis;
result.imean  = meani;
result.ivar   = vari;
result.prob   = prob;
