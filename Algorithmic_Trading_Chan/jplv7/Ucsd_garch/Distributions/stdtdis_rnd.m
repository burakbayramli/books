function t = stdtdis_rnd(n,df)
% PURPOSE:
%     returns random draws from the standardized t(n) distribution with unit variance
%     rnd = stdtdis_rnd(n,df)
% 
% USAGE:
%     random = stdtdis_rnd(n,df)
% 
% INPUTS:
%     n = size of vector 
%     df = a scalar dof parameter must be > 2
% 
% OUTPUTS:
%     random = a vector of random draws from the standardized t(n) distribution      
% 
% 
% COMMENTS:
%     SEE ALSO: stdtdis_cdf, stdtdis_rnd, stdtdis_pdf, 
% 
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

  
if nargin ~= 2
error('Wrong # of arguments to tdis_rnd');
end;

if is_scalar(df)
 if (df<=0)
   error('tdis_rnd dof is wrong');
 end
 z = randn(n,1);
 x = chis_rnd(n,df);
 t = (z*sqrt(df))./sqrt(x);
else
 error('tdis_rnd: df must be a scalar');
end;
t=t./(sqrt(df/(df-2)));