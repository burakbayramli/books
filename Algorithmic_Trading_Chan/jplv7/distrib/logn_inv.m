function linv = logn_inv (x, a, v)
% PURPOSE: inverse cdf (quantile) of the lognormal distribution
%--------------------------------------------------------------
% USAGE: linv = logn_inv(x,a,v)
% where:   x = x = matrix or vector
%          a = mean, a = scalar or size(x),     default = 1
%          v = variance  b = scalar or size(x), default = 1
% NOTE: the logarithm of a lognormal random deviate is
%       normally distributed with mean = log(a) and variance (v)
%--------------------------------------------------------------
% RETURNS: quantile at each element of x of the lognormal distribution
%--------------------------------------------------------------

% Written by KH (Kurt.Hornik@ci.tuwien.ac.at) 
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu 
   

  if ~((nargin == 1) | (nargin == 3))
    error('Wrong # of arguments to logn_inv');
  end

  if (nargin == 1)
    a = 1;
    v = 1;
  end

% The following "straightforward" implementation unfortunately does
% not work (because exp (Inf) -> NaN):
% inv = exp (normal_inv (x, log (a), v));
% Hence ...
  
  [retval, x, a, v] = com_size (x, a, v);
  if (retval > 0)
    error('logn_inv: x, m and v must be of common size or scalars');
  end

  [r, c] = size(x);
  s = r * c;
  x = reshape(x, 1, s);
  a = reshape(a, 1, s);
  v = reshape(v, 1, s);
  linv = zeros(1, s);

  k = find (~(x >= 0) | ~(x <= 1) | ~(a > 0) | ~(a < Inf) ...
      | ~(v > 0) | ~(v < Inf)); 
  if any(k)
    linv(k) = NaN * ones(1, length(k));
  end
  
  k = find((x == 1) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if any(k)
    linv(k) = Inf * ones(1, length (k));
  end
  
  k = find ((x > 0) & (x < 1) & (a > 0) & (a < Inf) ...
      & (v > 0) & (v < Inf));
  if any(k)
    linv(k) = a(k) .* exp(sqrt(v(k)) .* stdn_inv(x(k)));
  end

  linv = reshape(linv, r, c);
  
