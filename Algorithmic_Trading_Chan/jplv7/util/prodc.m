function m=prodc(x);
% PURPOSE: compute product of each column
% -----------------------------------------
% USAGE: m = prodc(x)
% where:    x = input matrix (or vector) of length nobs
% -----------------------------------------
% RETURNS: m = matrix or vector containing products
%              of the columns
% -----------------------------------------
% NOTE: a Gauss compatibility function
% -----------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

m=prod(x);
if size(m,2)>1;
   m=m';
end;
return;
