function F = stdtdis_cdf (x, n)
% PURPOSE:
% returns cdf at x of the standardized to unit variance t(n) distribution
% 
% USAGE:
% cdf = stdtdis_cdf(x,n)
% 
% INPUTS:
% x = a vector 
% n = a scalar parameter with dof must be > 2
% 
% OUTPUTS:
% a vector of cdf at each element of x of the standardized t(n) distribution      
% 
% COMMENTS:
%      SEE ALSO: stdtdis_rnd, stdtdis_pdf
% 
% 
% Anders Holtsberg, 18-11-93
% Copyright (c) Anders Holtsberg
% modified by J.P. LeSage
% Modified: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

if nargin ~= 2
error('Wrong # of arguments to tdis_cdf');
end;

if any(any(n<=0))
   error('tdis_cdf dof is wrong');
end

x=x.*sqrt(n./(n-2));
[nobs junk] = size(x);
neg = x<0;
F = fdis_cdf(x.^2,1,n);
iota = ones(nobs,1);
out = iota-(iota-F)/2;
F = out + (iota-2*out).*neg;
    

