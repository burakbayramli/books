function rnd = logt_rnd (r, c)
% PURPOSE: random draws from the logistic distribution
%---------------------------------------------------
% USAGE: rnd = logt_rnd(r,c)
% where: r,c = size of the matrix, vector or scalar of draws 
%---------------------------------------------------
% RETURNS:
%        rnd = a matrix of random numbers from the logistic distribution      
% --------------------------------------------------
% SEE ALSO: logt_cdf, logt_pdf, logt_inv
%---------------------------------------------------

% NOTE: Written by KH (Kurt.Hornik@ci.tuwien.ac.at)
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu
  
  if (nargin ~= 2)
    usage ('Wrong # of arguments to logt_rnd');
  end

  if ~(is_scalar(r) & (r > 0) & (r == round (r)))
    error ('logt_rnd:  r must be a positive integer');
  end
  if ~(is_scalar(c) & (c > 0) & (c == round (c)))
    error ('logt_rnd:  c must be a positive integer');
  end

  rnd = - log (ones(r,c) ./ rand(r, c) - ones(r,c));
  