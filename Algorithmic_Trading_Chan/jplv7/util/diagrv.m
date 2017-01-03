function y = diagrv(x,v)
% PURPOSE: replaces main diagonal of a square matrix
% -----------------------------------------
% USAGE: y - diagrv(x,v)
% where: x = input matrix
%        v = vector to replace main diagonal
% -----------------------------------------
% RETURNS: y = matrix x with v placed on main diagonal
% -----------------------------------------
% NOTE: a Gauss compatability function
% -----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
  
[r,c] = size(x);
if r ~= c;
  error('x matrix not square')
end;
if length(v) ~= r;
  error('v is not conformable with x')
end;
y = x - diag(diag(x)) + diag(v);
