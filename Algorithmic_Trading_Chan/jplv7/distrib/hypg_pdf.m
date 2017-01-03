function  p = hypg_pdf(k,n,K,N)
% PURPOSE: hypergeometric pdf function
%---------------------------------------------------
% USAGE: p = hypg_pdf(k,n,K,N)
% where: k,n,K,N are parameters 
% 
%---------------------------------------------------
% RETURNS:
%        a vector of probabilities from the distribution      
% --------------------------------------------------
% SEE ALSO: hypg_d, hypg_cdf, hypg_rnd, hypg_inv
%---------------------------------------------------


%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg

if any(any( n>N | K>N | K<0 ));
   error('hypg_cdf: Incompatible input arguments');
end

z = k<0 | n-k<0 | k>K | n-k>N-K;
I = find(~z);
if length(k)>1, k = k(I); end
if length(K)>1, K = K(I); end
if length(n)>1, n = n(I); end
if length(N)>1, N = N(I); end
pp = bincoef(k,K) .* bincoef(n-k,N-K) ./ bincoef(n,N);
p = z*0;
p(I) = pp(:);
