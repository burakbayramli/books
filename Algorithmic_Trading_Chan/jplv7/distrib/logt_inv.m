function inv = logt_inv (x)
% PURPOSE: inv of the logistic distribution
%---------------------------------------------------
% USAGE: cdf = logt_inv(x)
% where: x = a vector or scalar argument 
%---------------------------------------------------
% RETURNS:
%        inv = the inverse (quantile) of the logistic distribution      
% --------------------------------------------------
% SEE ALSO: logt_cdf, logt_pdf, logt_rnd
%---------------------------------------------------

% NOTE: Written by KH (Kurt.Hornik@ci.tuwien.ac.at)
% Converted to MATLAB by JP LeSage, jpl@jpl.econ.utoledo.edu
    

  if (nargin ~= 1)
    error('Wrong # of arguments to logt_inv');
  end;

  [r, c] = size (x);
  s = r * c;
  x = reshape (x, 1, s);
  inv = zeros (1, s);

  k = find ((x < 0) | (x > 1) | isnan (x));
  if any (k)
    inv(k) = NaN * ones (1, length (k));
  end;

  k = find (x == 0);
  if any (k)
    inv(k) = (-Inf) * ones (1, length (k));
  end;

  k = find (x == 1);
  if any (k)
    inv(k) = Inf * ones (1, length (k));
  end;
  
  k = find ((x > 0) & (x < 1));
  if any (k)
    inv (k) = - log (1 ./ x(k) - 1);
  end;
  
  inv = reshape (inv, r, c);
  
