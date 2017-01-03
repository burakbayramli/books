function cdf = norm_cdf (x, m, v)
% PURPOSE: computes the cumulative normal distribution 
%          for each component of x with mean m, variance v
%---------------------------------------------------
% USAGE: cdf = norm_cdf(x,m,v)
% where: x = variable vector (nx1)
%        m = mean vector (default=0)
%        v = variance vector (default=1)
%---------------------------------------------------
% RETURNS: cdf (nx1) vector
%---------------------------------------------------

% Written by TT (Teresa.Twaroch@ci.tuwien.ac.at) on Jun 3, 1993
% Updated by KH (Kurt.Hornik@ci.tuwien.ac.at) on Oct 26, 1994
% Copyright Dept of Probability Theory and Statistics TU Wien
% Updated by James P. Lesage, jpl@jpl.econ.utoledo.edu 1/7/97

  [r, c] = size(x);
  
  if (r*c == 0)
  error('norm_cdf: x must not be empty');
  end;

  if (nargin == 1)
    m = zeros(r,1);
    v = ones(r,1);
  end;

  cdf = zeros(r, 1);
  cdf(1:r,1) = stdn_cdf((x(1:r,1) - m(1:r,1)) ./ sqrt (v(1:r,1)));

  
