function k = bincoef(n,N)
% PURPOSE: generate binomial coefficients
%
%         k = bincoef(n,N)

%       Anders Holtsberg, 13-05-94
%       Copyright (c) Anders Holtsberg

if all(all(round(n)==n)) & all(all(round(N)==N))
   k = round(exp(gammaln(N+1) - gammaln(n+1) - gammaln(N-n+1)));
else
   k = exp(gammaln(N+1) - gammaln(n+1) - gammaln(N-n+1));
end
