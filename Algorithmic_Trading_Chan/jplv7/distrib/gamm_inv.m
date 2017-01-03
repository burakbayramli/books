function x = gamm_inv(p,a)
% PURPOSE: returns the inverse of the cdf at p of the gamma(a) distribution
%---------------------------------------------------
% USAGE: x = gamm_inv(p,a)
% where: p = a vector of probabilities 
%        a = a scalar parameter gamma(a)
%---------------------------------------------------
% RETURNS:
%        a vector x of the quantile at each element of p of the gamma(a) distribution      
% --------------------------------------------------
% SEE ALSO: gamm_d, gamm_pdf, gamm_rnd, gamm_cdf
%---------------------------------------------------

%        Anders Holtsberg, 18-11-93
%        Copyright (c) Anders Holtsberg
% documentation modified by LeSage to fit the format
% of the econometrics toolbox

  if (nargin ~= 2)
    error('Wrong # of arguments to gamm_inv');
  end
  

if any(any(abs(2*p-1)>1))
   error('gamm_inv: a probability should be 0<=p<=1')
end
if any(any(a<=0))
   error('gamma_inv: parameter a is wrong')
end

x = max(a-1,0.1);
dx = 1;
while any(any(abs(dx)>256*eps*max(x,1)))
   dx = (gamm_cdf(x,a) - p) ./ gamm_pdf(x,a);
   x = x - dx;
   x = x + (dx - x) / 2 .* (x<0);
end

I0 = find(p==0);
x(I0) = zeros(size(I0));
I1 = find(p==1);
x(I1) = zeros(size(I0)) + Inf;
