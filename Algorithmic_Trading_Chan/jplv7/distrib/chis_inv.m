function x = chis_inv (p, a)
% PURPOSE: returns the inverse (quantile) at x of the chisq(n) distribution
%---------------------------------------------------
% USAGE: x = chis_inv(p,a)
% where: p = a vector of probabilities 
%        a = a scalar parameter
% NOTE: chis_inv(x,n) = gamm_inv(p,a/2)*2
%---------------------------------------------------
% RETURNS:
%        a vector x at each element of p from chisq(n) distribution      
% --------------------------------------------------
% SEE ALSO: chis_d, chis_cdf, chis_rnd, chis_pdf
%---------------------------------------------------

%        Anders Holtsberg, 18-11-93
%        Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox
   

if (nargin ~= 2)
    error ('Wrong # of arguments to chis_inv');
end


if any(any(abs(2*p-1)>1))
   error('chis_inv: a probability should be 0<=p<=1')
end
if any(any(a<=0))
   error('chis_inv: dof is wrong')
end

x = gamm_inv(p,a*0.5)*2;
