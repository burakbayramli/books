function z = trimc(x,n1,n2)
% PURPOSE: return a matrix (or vector) x stripped of the specified columns.
% -----------------------------------------------------
% USAGE: z = trimc(x,n1,n2)
% where: x = input matrix (or vector) (n x k)
%       n1 = first n1 columns to strip
%       n2 = last  n2 columns to strip
% NOTE: modeled after Gauss trimc function
% -----------------------------------------------------
% RETURNS: z = x(:,n1+1:n-n2)
% -----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

  z = trimr(x',n1,n2)';
  
