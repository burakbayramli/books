function v = vecr(x)
% PURPOSE: creates a column vector by stacking rows of x
%----------------------------------------------------------
% USAGE:  v = vecr(x)
% where:  x = an input matrix
%---------------------------------------------------------
% RETURNS:
%         v = output vector containing stacked rows of x
%----------------------------------------------------------

% written by: James P. LeSage 2/98
% University of Toledo
% Department of Economics
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% Thanks to psummers@clyde.its.unimelb.edu.au
% for suggesting an improvement in this function


  if (nargin ~= 1)
  error('Wrong # of arguments to vecr'); 
  end 
  
xt = x';
v = xt(:);

 
  
