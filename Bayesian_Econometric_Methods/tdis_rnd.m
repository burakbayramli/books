function t = tdis_rnd (n,df)
% PURPOSE: returns random draws from the t(n) distribution
%---------------------------------------------------
% USAGE: rnd = tdis_rnd(n,df)
% where: n = size of vector 
%        df = a scalar dof parameter
% NOTE:  mean=0, std=1
%---------------------------------------------------
% RETURNS:
%        a vector of random draws from the t(n) distribution      
% --------------------------------------------------
% SEE ALSO: tdis_cdf, tdis_rnd, tdis_pdf, tdis_prb
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

  
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