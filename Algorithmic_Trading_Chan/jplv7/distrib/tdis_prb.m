function y = tdis_prb(x,n)
% PURPOSE: calculates t-probabilities for elements in x-vector 
%---------------------------------------------------
% USAGE: y = tdis_prb(x,n)
% where: x = vector containing computed t-values
%        n = degrees of freedom parameter
%---------------------------------------------------
% RETURNS:
%        y = a vector of marginal probability levels
% --------------------------------------------------
% SEE ALSO: fdis_prb(), chis_prb
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


if nargin ~= 2; error('Wrong # of arguments to tdis_prb'); end;
if n <=0; error('dof is negative or zero in tdis_prb'); end;

x2 = n./(n+x.^2);
one = find(x2 >= 1);
if length(one) > 0
    x2(one,1) = 1-1e-12;
end;
zip = find(x2 <= 0);
if length(zip) > 0
    x2(zip,1) = 1e-12;
end;

tmp = 1.0 - 0.5*betainc(x2,0.5*n,0.5);
y = 2*(1-tmp);  
