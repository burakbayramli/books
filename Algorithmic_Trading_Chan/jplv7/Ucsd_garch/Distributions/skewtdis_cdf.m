function cdf = skewtdis_cdf(x, nu, lambda)
% PURPOSE:
%     returns the cdf at x of Hansen's (1994) 'skewed t' distribution
% 
% USAGE:
%     cdf = skewtdis_cdf(x,nu,lambda)
% 
% INPUTS:
%     x  = a matrix, vector or scalar 
%     nu = a matrix or scalar degrees of freedom parameter 
%     lambda = a maxtrix or scalar skewness parameter 
% 
% OUTPUTS:
%     cdf = a matrix of pdf at each element of x of the distribution      
% 
% COMMENTS:
%     (Correcting mistake in Rockinger and Joneau, 2001)
% 
% Author: Andrew Patton
% Modified: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


[T,k] = size(x);
if size(nu,1)<T;
   nu = nu(1)*ones(T,1);
end
if size(lambda,1)<T;
   lambda = lambda(1)*ones(T,1);
end
c = gamma((nu+1)/2)./(sqrt(pi*(nu-2)).*gamma(nu/2));
a = 4*lambda.*c.*((nu-2)./(nu-1));
b = sqrt(1 + 3*lambda.^2 - a.^2);

y1 = (b.*x+a)./(1-lambda).*sqrt(nu./(nu-2));
y2 = (b.*x+a)./(1+lambda).*sqrt(nu./(nu-2));

cdf = (1-lambda).*tdis_cdf(y1,nu).*(x<-a./b);			% this method seems to cause error message - work it out later...
cdf = cdf + (x>=-a./b).*((1-lambda)/2 + (1+lambda).*(tdis_cdf(y2,nu)-0.5));

%cdf = -999.99*ones(T,1);
%for tt = 1:T;
%   if x(tt)<(-a(tt)/b(tt))
%      cdf(tt) = (1-lambda(tt)).*tdis_cdf(y1(tt),nu(tt));
%   else
%      cdf(tt) = ((1-lambda(tt))/2 + (1+lambda(tt)).*(tdis_cdf(y2(tt),nu(tt))-0.5));
%   end
%end

