function pdf = logn_pdf (x, a, v)
% PURPOSE: pdf of the lognormal distribution
%--------------------------------------------------------------
% USAGE: pdf = logn_pdf(x,a,v)
% where:   x = x = matrix or vector
%          a = mean, a = scalar or size(x),     default = 1
%          v = variance  b = scalar or size(x), default = 1
% NOTE: the logarithm of a lognormal random deviate is
%       normally distributed with mean = a and variance = v
%--------------------------------------------------------------
% RETURNS: pdf at each element of x of the lognormal distribution
%--------------------------------------------------------------
% SEE ALSO: logn_d, logn_pdf, logn_inv, logn_rnd
% -------------------------------------------------------------

% Written by KH (Kurt.Hornik@ci.tuwien.ac.at) 
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu 
   

  if ~((nargin == 1) | (nargin == 3))
    error('Wrong # of arguments to logn_pdf');
  end

  if (nargin == 1)
    a = 1;
    v = 1;
  end

% The following "straightforward" implementation unfortunately does
% not work for the special cases (Inf, ...)
% pdf = (x > 0) ./ x .* normal_pdf (log (x), log (a), v);
% Hence ...
  
  [retval, x, a, v] = com_size(x, a, v);
  if (retval > 0)
    error('logn_pdf: x, m and v must be of common size or scalars');
  end
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  a = reshape (a, 1, s);
  v = reshape (v, 1, s);
  pdf = zeros (1, s);

  k = find(isnan(x) | ~(a > 0) | ~(a < Inf) ...
      | ~(v > 0) | ~(v < Inf)); 
  if any(k)
    pdf(k) = NaN * ones(1, length (k));
  end
  
  k = find((x > 0) & (x < Inf) & (a > 0) & (a < Inf) ...
      & (v > 0) & (v < Inf));
  if any (k)
    pdf(k) = norm_pdf(log(x(k)), log(a(k)), v(k)) ./ x(k);
  end

  pdf = reshape(pdf, r, c);
  
