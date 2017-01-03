function rnd = bino_rnd (n, p, r, c)
% PURPOSE: random sampling from a binomial distribution
%---------------------------------------------------
% USAGE: rnd = bino_rnd(n,p,r,c)
% where: p = the probability of success
%        n = number of trials
%        r,c = size of random sample from binominal(n,p) distribution
%---------------------------------------------------
% RETURNS:
%        rnd = matrix, vector or scalar of random deviates
%              equal to the number of successes in n-trials 
% NOTE: mean = n*p, variance = n*p(1-p)
% --------------------------------------------------
% SEE ALSO: bino_d, bino_pdf, bino_cdf, bino_inv
%---------------------------------------------------

% NOTES: Written by KH (Kurt.Hornik@ci.tuwien.ac.at)
% converted to MATLAB by
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


  if (nargin == 4)
    if ~(is_scalar (r) & (r > 0) & (r == round (r)))
      error('bino_rnd:  r must be a positive integer');
    end
    if ~(is_scalar (c) & (c > 0) & (c == round (c)))
      error('bino_rnd:  c must be a positive integer');
    end
    [retval, x, n, p] = com_size(zeros(r,c), n, p);
    if (retval > 0)
      error('bino_rnd: n and p must be scalar or of size');
    end
  elseif nargin ==3
    [retval, x, n, p] = com_size (zeros(r, 1),n, p);
    if (retval > 0)
      error ('bino_rnd: n and p must be of common size or scalar');
    end
  elseif (nargin == 2)       
    [retval, x, n, p] = com_size (zeros(1),n, p);
    if (retval > 0)
      error ('bino_rnd: n and p must be of common size or scalar');
    end
  else
    error ('Wrong # of arguments to bino_rnd');
  end

  [r, c] = size (n);
  s = r * c;
  n = reshape (n, 1, s);
  p = reshape (p, 1, s);
  rnd = zeros (1, s);
  
  k = find (~(n > 0) | ~(n < Inf) | ~(n == round(n)) ...
      | ~(p <= 0) | ~(p >= 1));
  if any(k)
    rnd(k) = NaN * ones(1, length (k));
  end
  
  k = find((n > 0) & (n < Inf) & (n == round (n)) ...
      & (p >= 0) & (p <= 1));
  if any(k)
    N = max(n(k));
    L = length(k);
    tmp = rand(N, L);
    ind = (1 : N)' * ones(1, L);
    rnd(k) = sum((tmp < ones(N, 1) * p(k)) ...
 & (ind <= ones(N, 1) * n(k)), 1);
  end
  
  rnd = reshape(rnd, r, c);
  
