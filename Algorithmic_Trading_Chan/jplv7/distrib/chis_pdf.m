function f = chis_pdf (x, a)
% PURPOSE: returns the pdf at x of the chisquared(n) distribution
%---------------------------------------------------
% USAGE: pdf = chis_pdf(x,n)
% where: x = vector 
%        n = a scalar parameter
% NOTE: chis_pdf(x,n) = gamm_pdf(x/2,n/2)/2
%---------------------------------------------------
% RETURNS:
%        a vector of pdf at each element of x from chisq(n) distribution      
% --------------------------------------------------
% SEE ALSO: chis_d, chis_cdf, chis_rnd, chis_inv
%---------------------------------------------------

%        Anders Holtsberg, 18-11-93
%        Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox
 

if (nargin ~= 2)
    error ('Wrong # of arguments to chis_pdf');
end


if any(any(a<=0))
   error('chis_pdf: dof is wrong')
end

f = gamm_pdf(x/2,a*0.5)/2;
