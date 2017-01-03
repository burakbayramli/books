function t = studentize(x) 
% PURPOSE: If x is a vector, subtract its mean and divide
%          by its standard deviation. If x is a matrix, 
%          do the above for each column.
%---------------------------------------------------
% USAGE:   out = studentize(x)
% where:     x = a vector or matrix
%---------------------------------------------------
% RETURNS:
%          out = transformed matrix
% --------------------------------------------------

% Written by KH (Kurt.Hornik@ci.tuwien.ac.at)  
% Copyright Dept of Probability Theory and Statistics TU Wien
% Documentation and code adapted from Octave
% to matlab by J. LeSage
  
  
  [nobs nvar] = size(x);
  
  if nvar == 1
    if (std(x) == 0)
      t = zeros(size(x));
    else
      t = (x - mean(x)) / std(x);
    end
  elseif nvar > 1
    l = ones (rows(x), 1);
    t = x - l * mean(x);
    tmp = [std(t)
           ~any(t)];
    t = t ./ (l * max (tmp));
  else
    error('studentize:  x must be a vector or a matrix');
  end

