function  k = hypg_inv(p,n,K,N)
% PURPOSE: hypergeometric inverse (quantile) function
%---------------------------------------------------
% USAGE:     k = hypg_inv(p,n,K,N)
% where:     p = a vector
%        n,K,N = parameters of the distribution
%---------------------------------------------------
% RETURNS:
%        The smallest integer k so that P(X <= k) >= p.
%        a vector of probabilities from the distribution      
% --------------------------------------------------
% SEE ALSO: hypg_d, hypg_cdf, hypg_rnd, hypg_inv
%---------------------------------------------------

%QHYPGEO  The hypergeometric inverse cdf
%
%        k = qhypg(p,n,K,N))
%
%        Gives the smallest integer k so that P(X <= k) >= p.

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation changed to match the econometrics toolbox
% by LeSage

% The algorithm contains a nice vectorization trick which
% relies on the fact that if two elements in a vector
% are exactely the same then matlab's routine SORT sorts them
% into the order they had. Do not change this, Mathworks!

if max([length(n) length(K) length(N)]) > 1
   error('Sorry, this is not implemented');
end
if any(any(abs(2*p-1)>1))
   error('A probability should be 0<=p<=1, please!')
end

lowerlim = max(0,n-(N-K));
upperlim = min(n,K);
kk = (lowerlim:upperlim)';
nk = length(kk);
cdf = max(0,min(1,cumsum(hypg_pdf(kk,n,K,N))));
cdf(length(cdf)) = 1;
[pp,J] = sort(p(:));
np = length(pp);
[S,I] = sort([pp;cdf]);
I = find(I<=np) - (1:np)' + lowerlim; 
J(J) = (1:np)';
p(:) = I(J);
k = p;
