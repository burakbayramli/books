function pdf = pois_pdf(x,l)
% PURPOSE: computes the probability density function at x
% of the Poisson distribution with parameter lambda
%---------------------------------------------------
% USAGE:    pdf = pois_pdf(x,lambda)
% where:    x   = variable vector (nx1)
%        lambda = value at which to evaluate pdf 
%---------------------------------------------------

% Author: Kurt Hornik
% <Kurt.Hornik@ci.tuwien.ac.at>
% Copyright Dept of Probability Theory and Statistics TU Wien
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu, 10/98


  if (nargin ~= 2)
    error('poisson_pdf: Wrong # of inputs');
  end;

  
  [retval, x, l, junk] = com_size (x, l, l);
  if (retval > 0)
    error ('pois_pdf: x and lambda must be of common size or scalar');
  end;

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  l = reshape (l, 1, s);
  pdf = zeros (1, s);

  k = find (~(l > 0) | isnan (x));
  if any (k)
    pdf(k) = NaN * ones (1, length (k));
  end;

  k = find ((x >= 0)  & (x < Inf) & (x == round (x)) & (l > 0));
  if any (k)
    pdf(k) = exp (x(k) .* log (l(k)) - l(k) - gammaln (x(k) + 1));
  end;

  pdf = reshape (pdf, r, c);
  

