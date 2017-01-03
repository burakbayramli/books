function pinv = pois_inv(x,l)
% PURPOSE: computes the quantile (inverse of the cdf) at x
% of the Poisson distribution with parameter lambda
%---------------------------------------------------
% USAGE:    inv = pois_inv(x,lambda)
% where:    x   = variable vector (nx1)
%        lambda = value at which to evaluate pdf 
%---------------------------------------------------

% Author: Kurt Hornik
% <Kurt.Hornik@ci.tuwien.ac.at>
% Copyright Dept of Probability Theory and Statistics TU Wien
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu, 10/98


  if (nargin ~= 2)
    error('pois_inv: Wrong # of arguments');
  end;

  [retval, x, l] = com_size (x, l, l);
  if (retval > 0)
    error ('pois_inv: x and lambda must be of common size or scalar');
  end;
  
  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  l = reshape (l, 1, s);
  pinv = zeros (1, s);

  k = find ((x < 0) | (x > 1) | isnan (x) | ~(l > 0));
  if any (k)
    pinv(k) = NaN * ones (1, length (k));
  end;

  k = find ((x == 1) & (l > 0));
  if any (k)
    pinv(k) = Inf * ones (1, length (k));
  end;
  
  k = find ((x > 0) & (x < 1) & (l > 0));
  if any (k)
    cdf = exp (-l(k));
    while (1)
      m = find (cdf < x(k));
      if any (m)
 pinv(k(m)) = pinv(k(m)) + 1;
 cdf(m) = cdf(m) + pois_pdf (pinv(k(m)), l(k(m)));
      else
 break;
      end;
    end;
  end;
    
  pinv = reshape (pinv, r, c);
  
