function cdf = beta_cdf(x, a, b)
% PURPOSE: cdf of the beta distribution
%--------------------------------------------------------------
% USAGE: cdf = beta_cdf(x,a,b)
% where:   x = prob[beta(a,b) <= x], x = vector
%          a = beta distribution parameter, a = scalar 
%          b = beta distribution parameter  b = scalar 
% NOTE: mean [beta(a,b)], variance = ab/((a+b)*(a+b)*(a+b+1))
%--------------------------------------------------------------
% RETURNS: cdf at each element of x of the beta distribution
%--------------------------------------------------------------
% SEE ALSO: beta_d, beta_pdf, beta_inv, beta_rnd
%--------------------------------------------------------------
  
% written by:  Anders Holtsberg, 18-11-93
%              Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

if any(any((a<=0)|(b<=0)))
   error('Parameter a or b is nonpositive');
end

Ii = find(x>0&x<1);
Il = find(x<=0);
Iu = find(x>=1);
cdf = 0*(x+a+b); % Stupid allocation trick
cdf(Il) = 0*Il;
cdf(Iu) = 0*Iu + 1;
if length(x) > 0 
   cdf(Ii) = betainc(x(Ii),a,b);
end
