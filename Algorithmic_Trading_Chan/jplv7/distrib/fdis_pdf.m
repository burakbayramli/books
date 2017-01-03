function pdf = fdis_pdf(x,a,b)
% PURPOSE: returns pdf at x of the F(a,b) distribution
%---------------------------------------------------
% USAGE: pdf = fdis_pdf(x,a,b)
% where: x = a vector 
%        a = numerator dof
%        b = denominator dof
%---------------------------------------------------
% RETURNS:
%   a vector of pdf at each element of x of the F(a,b) distribution      
% --------------------------------------------------
% SEE ALSO: fdis_d, fdis_inv, fdis_rnd, fdis_cdf, fdis_prb
%---------------------------------------------------

%       Anders Holtsberg, 18-11-93
%       Copyright (c) Anders Holtsberg
% documentation modified by LeSage to
% match the format of the econometrics toolbox

if nargin ~= 3
error('Wrong # of arguments to fdis_pdf');
end;

c = b./a;
xx = x./(x+c);
pdf = beta_pdf(xx,a/2,b/2);
pdf = pdf./(x+c).^2.*c;

