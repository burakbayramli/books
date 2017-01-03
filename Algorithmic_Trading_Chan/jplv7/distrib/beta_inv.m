function x = beta_inv(p, a, b)
% PURPOSE: inverse of the cdf (quantile) of the beta(a,b) distribution
%--------------------------------------------------------------
% USAGE: x = beta_inv(p,a,b)
% where:   p = vector of probabilities
%          a = beta distribution parameter, a = scalar
%          b = beta distribution parameter  b = scalar
% NOTE: mean [beta(a,b)] = a/(a+b), variance = ab/((a+b)*(a+b)*(a+b+1))
%--------------------------------------------------------------
% RETURNS: x at each element of p for the beta(a,b) distribution
%--------------------------------------------------------------
% SEE ALSO: beta_d, beta_pdf, beta_inv, beta_rnd
%--------------------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

if (nargin ~= 3)
    error('Wrong # of arguments to beta_inv');
end
 
if any(any((a<=0)|(b<=0)))
   error('beta_inv parameter a or b is nonpositive');
end
if any(any(abs(2*p-1)>1))
   error('beta_inv: A probability should be 0<=p<=1');
end

x = a ./ (a+b);
dx = 1;
while any(any(abs(dx)>256*eps*max(x,1)))
   dx = (betainc(x,a,b) - p) ./ beta_pdf(x,a,b);
   x = x - dx;
   x = x + (dx - x) / 2 .* (x<0);
end
    

 
