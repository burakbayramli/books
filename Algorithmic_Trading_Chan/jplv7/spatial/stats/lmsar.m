function result = lmsar(y,x,W1,W2,lmin,lmax);
% PURPOSE: LM error statistic for spatial correlation in residuals
%          of a spatial autoregressive model
% ---------------------------------------------------
%  USAGE: result = lmsar(y,x,W1,W2,rmin,rmax)
%  where: y = dependent variable vector
%         x = independent variables matrix
%        W1 = contiguity matrix for rho
%        W2 = contiguity matrix for lambda
%      rmin = (optional) minimum value of rho to use in search  
%      rmax = (optional) maximum value of rho to use in search   
% ---------------------------------------------------
%  RETURNS: a structure variable
%         result.meth = 'lmsar'
%         result.lm   = LM statistic
%         result.prob = marginal probability
%         result.chi1 = 6.635 (chi-squared 1 dof at 99% level)
%         result.nobs = # of observations
%         result.nvar = # of variables
% ---------------------------------------------------
% NOTE: lm > 6.635,  => small prob,
%                    => reject HO: of no spatial correlation
% ---------------------------------------------------
% See also:  walds, lratios, moran, lmerrors
% ---------------------------------------------------
% REFERENCES: Anselin (1988), pages 103-104.
% ---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if nargin == 4
rflag = 0;
elseif nargin == 6,
rflag = 1;
else,
error('Wrong # of arguments to lmsar');
end;

[n k] = size(x);
% do sar to get residuals
if rflag == 1
res = sar(y,x,W1,lmin,lmax);
elseif rflag == 0
res = sar(y,x,W1);
end;
e = res.resid;
rho = res.rho;
% recover variance of rho
rhot = res.tstat(k+1,1);
sige = res.sige;
stdt = rhot/rho;
stdt = 1/stdt;
varr = stdt*stdt;
A = speye(n) - rho*sparse(W1);
AI = inv(A);
W2 = sparse(W2);
T22 = trace(W2*W2 + W2'*W2);
T21 = trace(W2*W1*AI + W2'*W1*AI);
lm1 = (e'*W2*e)/sige;
Tterm = (T22 - T21*T21*varr);
TI = inv(Tterm);
lmerr = lm1*lm1*TI;
lmerr = full(lmerr);
prob = 1-chis_prb(lmerr,1);

result.meth = 'lmsar';
result.lm = lmerr;
result.prob = prob;
result.chi1   = 6.635;
result.nobs = n;
result.nvar = k;
