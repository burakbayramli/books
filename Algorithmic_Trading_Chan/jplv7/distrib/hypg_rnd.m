function  x = hypg_rnd(num,n,K,N)
% PURPOSE: hypergeometric random draws
% prob(X=x) = (N)^-1 (K)(N-K)
%             (n)    (x)(n-x)
%---------------------------------------------------
% USAGE:     x = hypg_rnd(nobs,n,K,N)
% where:  nobs = number of draws
%        n,K,N = parameters of the distribution 
% NOTE: mean     = (n/N)*k
%       variance = [(n*K)/(N*N)*(N-1)]*(N-K)*(N-n)
%---------------------------------------------------
% RETURNS:
%        a vector of random draws from the distribution      
% --------------------------------------------------
% SEE ALSO: hypg_cdf, hypg_rnd, hypg_inv
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation changed to match the econometrics toolbox
% by LeSage

if length(num)==1
   num = [num 1];
end
x = hypg_inv(rand(num),n,K,N);


