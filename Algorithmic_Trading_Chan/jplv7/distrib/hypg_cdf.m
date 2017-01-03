function  p = hypg_cdf(k,n,K,N)
% PURPOSE: hypergeometric cdf function
%---------------------------------------------------
% USAGE: p = hypg_cdf(k,n,K,N)
% where: k,n,K,N are parameters 
% 
%---------------------------------------------------
% RETURNS:
%        a vector of cumulative probabilities from the distribution      
% --------------------------------------------------
% SEE ALSO: hypg_d, hypg_cdf, hypg_rnd, 
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation changed to match the econometrics toolbox
% by LeSage

if max([length(n) length(K) length(N)]) > 1
   error('Sorry, this is not implemented');
end

kk = -1:n;
cdf = max(0,min(1,cumsum(hypg_pdf(kk,n,K,N))));
p = k;
p(:) = cdf(max(1,min(n+2,floor(k(:))+2)));

