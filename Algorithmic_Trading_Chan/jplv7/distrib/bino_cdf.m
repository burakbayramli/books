function cdf = bino_cdf (x, n, p)
% PURPOSE: cdf at x of the binomial(n,p) distribution
%---------------------------------------------------
% USAGE: cdf = bino_cdf(x,n,p)
% where: p = the probability of success
%        n = number of trials
%        x = vector to be evaluated
% NOTE: mean [bino(n,p)] = n*p, variance = n*p(1-p)
%---------------------------------------------------
% RETURNS:
%        cdf = vector or scalar of cdf at x
%              equal to the number of successes in n-trials 
% --------------------------------------------------
% SEE ALSO: bino_d, bino_pdf, bino_rnd, bino_inv
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

[nobs nvar] = size(x);
if nargin ~= 3
error('Wrong # of arguments to bino_cdf');
elseif nvar ~= 1
error('bino_cdf: x-argument must be a vector');
elseif n <= 0
   error('Binomial denominator must be positive.')
elseif (p < 0) | (p > 1)
   error('Binomial probability must be >= 0 and <= 1.')
end;

cdf = zeros(nobs,1);
for i=1:nobs
 if x(i,1) >= n
   cdf(i,1) = 1;
 elseif x(i,1) < 0
   cdf(i,1) = 0;
 else
   cdf(i,1) = betainc( 1-p, n-x(i,1), x(i,1)+1 );
 end;
end
