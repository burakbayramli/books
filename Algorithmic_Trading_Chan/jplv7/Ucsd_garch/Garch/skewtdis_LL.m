function [LL, likelihoods] = skewtdis_LL(theta, x)
% PURPOSE:
%     Returns the log-likelihood at x of Hansen's (1994) 'skewed t' distribution
% 
% USAGE:
%     [LL, likelihoods] = skewtdis_LL(theta, x)
% 
% INPUTS:
%     x = a vector of data
%     theta = [nu;lambda]
%         nu = degrees of freedom parameter 
%         lambda = skewness parameter 
% 
% OUTPUTS:
%     LL - The log likelihoods
%     likelihoods - THe indiv log likelihoods
%     
% COMMENTS:
% 
% Author: Andrew Patton
% Modified: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


nu = theta(1);
lambda = theta(2);

[T,k] = size(x);

logc = gammaln((nu+1)/2) - gammaln(nu/2) - 0.5*log(pi*(nu-2));
c = exp(logc);

a = 4*lambda.*c.*((nu-2)./(nu-1));
logb = 0.5*log(1 + 3*lambda.^2 - a.^2);
b = exp(logb);


find1 = (x<(-a./b));
find2 = (x>=(-a./b));
LL1   = logb + logc - (nu+1)/2.*log(1+1./(nu-2).*((b.*x(find1)+a)./(1-lambda)).^2);
LL2   = logb + logc - (nu+1)/2.*log(1+1./(nu-2).*((b.*x(find2)+a)./(1+lambda)).^2);
LL    = sum(LL1) + sum(LL2);
LL    = -LL;

if nargout>1
     likelihoods=zeros(size(x));
     likelihoods(find1)=LL1;
     likelihoods(find2)=LL2;
end