function pdf = tdis_pdf(x, n)
% PURPOSE: returns the pdf at x of the t(n) distribution
%---------------------------------------------------
% USAGE: pdf = tdis_pdf(x,n)
% where: x = a matrix, vector or scalar 
%        n = a matrix or scalar parameter with dof
%---------------------------------------------------
% RETURNS:
%        a matrix of pdf at each element of x of the t(n) distribution      
% --------------------------------------------------
% SEE ALSO: tdis_cdf, tdis_rnd, tdis_inv, tdis_prb
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if (nargin ~= 2)
    error ('Wrong # of arguments to tdis_pdf');
end
  
if any(any(n<=0))
   error('dof in tdis_pdf is wrong')
end

pdf = gamma((n+1)/2)./sqrt(n*pi)./gamma(n/2).*(1+x.^2./n).^(-(n+1)/2);
     
