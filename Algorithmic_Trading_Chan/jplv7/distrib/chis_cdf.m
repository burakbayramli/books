function F = chis_cdf (x, a)
% PURPOSE: returns the cdf at x of the chisquared(n) distribution
%---------------------------------------------------
% USAGE: cdf = chis_cdf(x,n)
% where: x = a vector
%        n = a scalar parameter
% NOTE: chis_cdf(x,n) = gamm_cdf(x/2,n/2)
%---------------------------------------------------
% RETURNS:
%        a vector pdf at each element of x from chisq(n) distribution      
% --------------------------------------------------
% SEE ALSO: chis_d, chis_pdf, chis_rnd, chis_inv
%---------------------------------------------------

%        Anders Holtsberg, 18-11-93
%        Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

if (nargin ~= 2)
    error ('Wrong # of arguments to chis_cdf');
end

if any(any(a<=0))
   error('chis_cdf: dof is wrong')
end

F = gamm_cdf(x/2,a*0.5);
