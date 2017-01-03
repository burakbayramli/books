function pdf = stdtdis_pdf(x, n)
% PURPOSE:
%     returns the pdf at x of the standardized t(n) distribution
% 
% USAGE:
%     pdf = stdtdis_pdf(x,n)
% 
% INPUTS:
%     x = a matrix, vector or scalar 
%     n = a matrix or scalar parameter with dof must be > 2
% 
% OUTPUTS:
%     a matrix of pdf at each element of x of the standardized t(n) distribution to have unit vairance
% 
% COMMENTS:
% 
% 
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
% 
% Modified: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
% 


x=x.*sqrt(n/(n-2));
if (nargin ~= 2)
    error ('Wrong # of arguments to tdis_pdf');
end
  
if any(any(n<=0))
   error('dof in tdis_pdf is wrong')
end

pdf = gamma((n+1)/2)./sqrt(n*pi)./gamma(n/2).*(1+x.^2./n).^(-(n+1)/2);
pdf=pdf.*sqrt(n/(n-2));
