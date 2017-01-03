function cdf = logn_cdf (x, a, v)
% PURPOSE: cdf of the lognormal distribution
%--------------------------------------------------------------
% USAGE: cdf = logn_cdf(x,a,v)
% where:   x = x = matrix or vector
%          a = mean, a = scalar or size(x),     default = 1
%          v = variance  b = scalar or size(x), default = 1
% NOTE: the logarithm of a lognormal random deviate is
%       normally distributed with mean = log(a) and variance (v)
%--------------------------------------------------------------
% RETURNS: cdf at each element of x of the lognormal distribution
%--------------------------------------------------------------
% SEE ALSO: logn_d, logn_pdf, logn_inv, logn_rnd
% -------------------------------------------------------------

% Written by KH (Kurt.Hornik@ci.tuwien.ac.at) 
% Converted to MATLAB by JP LeSage, jlesage%spatial-econometrics.com
   

  if ~((nargin == 1) | (nargin == 3))
    error ('Wrong # of arguments to logn_cdf');
  end

  if (nargin == 1)
    a = 1;
    v = 1;
  end

% The following "straightforward" implementation unfortunately does
% not work (because exp (Inf) -> NaN etc):
% cdf = normal_cdf (log (x), log (a), v);
% Hence ...

  [retval, x, a, v] = com_size (x, a, v);
  if (retval > 0)
    error ('logn_cdf: x, m and v must be of common size or scalars');
  end

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  v = reshape (v, 1, s);
  cdf = zeros (1, s);
  
  k = find(isnan (x) | ~(a > 0) | ~(a < Inf) ...
      | ~(v > 0) | ~(v < Inf));
  if any(k)
    cdf(k) = NaN * ones(1, length(k));
  end
  
  k = find((x == Inf) & (a > 0) & (a < Inf) & (v > 0) & (v < Inf));
  if any(k)
    cdf(k) = ones(1, length(k));
  end
  
  k = find((x > 0) & (x < Inf) & (a > 0) & (a < Inf) ...
      & (v > 0) & (v < Inf));
  if any(k)
    cdf(k) = stdn_cdf((log(x(k)) - log(a(k))) ./ sqrt(v(k)));
  end
  
  cdf = reshape(cdf, r, c);
  
