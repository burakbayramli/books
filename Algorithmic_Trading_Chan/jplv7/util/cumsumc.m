function m=cumsumc(x);
% PURPOSE: compute cumulative sum of each column
% -----------------------------------------------------
% USAGE: m = cumsumc(x)
% where  x = input vector or matrix
% -----------------------------------------------------
% RETURNS: m = cumulative sum of x elements
%              by columns
% -----------------------------------------------------
% NOTE: a Gauss compatability function
% -----------------------------------------------------
             
% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% get cumulative sum of each column
m=cumsum(x);
if size(m,2)>1;
   m=m';
end;
return;
