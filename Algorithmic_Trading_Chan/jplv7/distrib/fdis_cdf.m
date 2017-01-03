function cdf = fdis_cdf(x,a,b)
% PURPOSE: returns cdf at x of the F(a,b) distribution
%---------------------------------------------------
% USAGE: cdf = fdis_cdf(x,a,b)
% where: x = a vector 
%        a = numerator dof
%        b = denominator dof
%---------------------------------------------------
% RETURNS:
%   a vector of cdf at each element of x of the F(a,b) distribution      
% --------------------------------------------------
% SEE ALSO: fdis_d, fdis_inv, fdis_rnd, fdis_pdf, fdis_prb
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

x = x./(x+b./a);
cdf = beta_cdf(x,a/2,b/2);

