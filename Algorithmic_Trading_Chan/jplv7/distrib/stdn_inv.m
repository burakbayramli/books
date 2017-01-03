function ninv = stdn_inv (x)  
% PURPOSE: computes the quantile (inverse of the CDF) 
%          for each component of x with mean 0, variance 1
%---------------------------------------------------
% USAGE: ninv = stdn_inv(x)
% where: x = variable vector (nx1)
%---------------------------------------------------
% RETURNS: ninv = (nx1) vector containing quantiles at each x-element
%---------------------------------------------------

% Written by KH (Kurt.Hornik@ci.tuwien.ac.at) 
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu
  
  if (nargin ~= 1)
    error ('Wrong # of arguments to stdn_inv');
  end
  
  ninv = sqrt(2) * erfinv(2 * x - 1);
  
