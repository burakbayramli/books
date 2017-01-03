function x = fdis_rnd(n,a,b)
% PURPOSE: returns random draws from the F(a,b) distribution
%---------------------------------------------------
% USAGE: rnd = fdis_rnd(n,a,b)
% where: n = size of vector 
%        a = scalar dof parameter
%        b = scalar dof parameter
%---------------------------------------------------
% RETURNS:
%        a vector of random draws from the F(a,b) distribution      
% --------------------------------------------------
% NOTES:
% mean should equal (b/a)*((a/2)/(b/2-1))
% --------------------------------------------------
% SEE ALSO: fdis_d, fdis_cdf, fdis_inv, fdis_pdf, fdis_prb
%---------------------------------------------------


%        Anders Holtsberg, 18-11-93
%        Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

x = beta_rnd(n,a/2,b/2);
x = x.*b./((1-x).*a);
