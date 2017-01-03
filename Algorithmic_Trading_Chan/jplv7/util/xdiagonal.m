function A = xdiagonal(x)
% PURPOSE: spreads an nxk observation matrix x out on
%          a n*n x n*k diagonal matrix
% ----------------------------------------------------
% USAGE: a = xdiagonal(x)
% where: x = an nxk data matrix
% ----------------------------------------------------
% RETURNS: a = n*n x n*k matrix taking the form:
% a = [ x 0 ... 0]
%     [ 0 x 0 ..0]
%
%     [ 0 ....  x]
% ----------------------------------------------------

% written by: James P. LeSage 1/99
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

[n k] = size(x);
A = x;
for i=2:n
 [r,s] = size(A);
 A = [A zeros(r,k); zeros(n,s) x];
end;

