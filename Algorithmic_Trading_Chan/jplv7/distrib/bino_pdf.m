function  pdf = bino_pdf(k,n,p)
% PURPOSE: pdf at x of the binomial(n,p) distribution
%---------------------------------------------------
% USAGE: pdf = bino_pdf(x,n,p)
% where: p = the probability of success
%        n = number of trials
%        x = vector to be evaluated
% NOTE: mean = n*p, variance = n*p(1-p)
%---------------------------------------------------
% RETURNS:
%        pdf = vector or scalar of cdf at x
%              equal to the number of successes in n-trials 
% --------------------------------------------------
% SEE ALSO: bino_d, bino_cdf, bino_rnd, bino_inv
%---------------------------------------------------

%       Anders Holtsberg, 16-03-95
%       Copyright (c) Anders Holtsberg
% modified by J.P. Lesage to fit the format of
% the econometrics toolbox

[nobs nvar] = size(k);
if nargin ~= 3
error('Wrong # of arguments to bino_pdf');
elseif nvar ~= 1
error('bino_pdf: x-argument must be a scalar');
elseif n <= 0
   error('bino_pdf: Binomial denominator must be positive.')
elseif (p < 0) | (p > 1)
   error('bino_pdf: Binomial probability must be >= 0 and <= 1.')
end;


pdf = bincoef(k,n) .* p.^k .* (1-p).^(n-k);
