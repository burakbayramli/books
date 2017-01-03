function t = unstudentize(xin,xraw) 
% PURPOSE: returns reverse studentized vector 
%          given xin a studentized vector, 
%          and xraw the vector in raw form 
% (adds back the mean and multiplies by its standard deviation) 
%          If x is a matrix, do the above for each column.
%---------------------------------------------------
% USAGE:   out = unstudentize(xin,xraw)
% where:     xin  = a vector or matrix in studentized form
%            xraw = a vector or matrix in raw form 
%---------------------------------------------------
% RETURNS:
%          out = untransformed matrix or vector
% --------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com

  
  [nobs nvar] = size(xin);
  meanx = mean(xraw);
  stdx = std(xraw);
  
  if nvar == 1
    if (stdx == 0)
      t = zeros(size(xin));
 else
  tmp = xin*stdx;
      t = tmp + meanx;
    end
  elseif nvar > 1
    l = ones (rows(xin), 1);
    t = matmul(xin,stdx);
    t = matadd(t,meanx);
  else
    error('unstudentize:  x must be a vector or a matrix');
  end

