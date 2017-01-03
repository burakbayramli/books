function m=cumprodc(x);
% PURPOSE: compute cumulative product of each column
% -----------------------------------------------------
% USAGE: m = cumprodc(x)
% where  x = input vector or matrix
% -----------------------------------------------------
% RETURNS: m = cumulative product of x elements
%              by columns
% -----------------------------------------------------
             
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


m=cumprod(x);
if size(m,2)>1;
   m=m';
end;
return;
