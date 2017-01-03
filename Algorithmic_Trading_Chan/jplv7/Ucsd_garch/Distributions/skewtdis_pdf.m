function pdf = skewtdis_pdf(x, nu, lambda)
% PURPOSE:
% returns the pdf at x of Hansen's (1994) 'skewed t' distribution
% 
% USAGE:
% pdf = skewtdis_pdf(x,nu,lambda)
% 
% INPUTS:
% x  = a matrix, vector or scalar 
% nu = a matrix or scalar degrees of freedom parameter 
% lambda = a maxtrix or scalar skewness parameter 
% 
% OUTPUTS:
% pdf = a matrix of pdf at each element of x of the distribution      
% 
% 
% COMMENTS:
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

pdf1 = b.*c.*(1 + 1./(nu-2).*((b.*x+a)./(1-lambda)).^2).^(-(nu+1)/2);
pdf2 = b.*c.*(1 + 1./(nu-2).*((b.*x+a)./(1+lambda)).^2).^(-(nu+1)/2);
pdf  = pdf1.*(x<(-a./b)) + pdf2.*(x>=(-a./b));