function M = dmult(A,B)
% PURPOSE: computes the product of diag(A) and B
% -----------------------------------------------------
% USAGE:     m = dmult(a,b)
%  where:    a = a matrix
%            b = a matrix
% -----------------------------------------------------
% RETURNS:  m = diag(A) times B
% -----------------------------------------------------             
% NOTE: a Gauss compatability function
% -----------------------------------------------------

% written by:
%  Gordon K Smyth, U of Queensland, Australia, gks@maths.uq.oz.au
% Nov 19, 1990.  Last revision Aug 29, 1995.

% documentation modifications made by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


[mb,nb] = size(B);
M=(A*ones(1,nb)).*B;