function pdf = beta_pdf(x, a, b)
% PURPOSE: pdf of the beta(a,b) distribution
%--------------------------------------------------------------
% USAGE: pdf = beta_pdf(x,a,b)
% where:   x = vector of components
%          a = beta distribution parameter, a = scalar
%          b = beta distribution parameter  b = scalar
% NOTE: mean[(beta(a,b)] = a/(a+b), variance = ab/((a+b)*(a+b)*(a+b+1))
%--------------------------------------------------------------
% RETURNS: pdf at each element of x of the beta(a,b) distribution
%--------------------------------------------------------------
% SEE ALSO: beta_d, beta_pdf, beta_inv, beta_rnd
%--------------------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox
  

if (nargin ~=3)
    error('Wrong # of arguments to beta_pdf');
end

if any(any((a<=0)|(b<=0)))
   error('Parameter a or b is nonpositive');
end

I = find((x<0)|(x>1));

pdf = x.^(a-1) .* (1-x).^(b-1) ./ beta(a,b);
pdf(I) = 0*I;
