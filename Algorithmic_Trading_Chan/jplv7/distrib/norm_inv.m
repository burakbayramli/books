function invp = norm_inv(x, m, v)
% PURPOSE: computes the quantile (inverse of the CDF) 
%          for each component of x with mean m, variance v
%---------------------------------------------------
% USAGE: invp = norm_inv(x,m,v)
% where: x = variable vector (nx1)
%        m = mean vector (default=0)
%        v = variance vector (default=1)
%---------------------------------------------------
% RETURNS: invp (nx1) vector
%---------------------------------------------------
% SEE ALSO: norm_d, norm_rnd, norm_inv, norm_cdf
%---------------------------------------------------

% Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Oct 26, 1994
% Copyright Dept of Probability Theory and Statistics TU Wien
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu

  if nargin > 3
    error ('Wrong # of arguments to norm_inv');
  end

  [r, c] = size (x);
  s = r * c;
  
  if (nargin == 1)
    m = zeros(1,s);
    v = ones(1,s);
  end



  x = reshape(x,1,s);
  m = reshape(m,1,s);
  v = reshape(v,1,s);

  invp = zeros (1,s);

    invp = m + sqrt(v) .* stdn_inv(x);

    invp = reshape (invp, r, c);
  
