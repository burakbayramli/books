function finv = fdis_inv(p,a,b)
% PURPOSE: returns inverse (quantile) at x of the F(a,b) distribution
%---------------------------------------------------
% USAGE: x = fdis_inv(p,a,b)
% where: p = a vector of probabilities
%        a = numerator dof
%        b = denominator dof
%---------------------------------------------------
% RETURNS:
%   a vector x at each element of p from F(a,b) distribution      
% --------------------------------------------------
% SEE ALSO: fdis_d, fdis_cdf, fdis_rnd, fdis_pdf, fdis_prb
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

finv = beta_inv(p,a/2,b/2);
finv = finv.*b./((1-finv).*a);

