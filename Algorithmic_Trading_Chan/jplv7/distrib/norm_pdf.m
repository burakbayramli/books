function pdf = norm_pdf (x, m, v)
% PURPOSE: computes the normal probability density function 
%          for each component of x with mean m, variance v
%---------------------------------------------------
% USAGE: pdf = norm_pdf(x,m,v)
% where: x = variable vector (nx1)
%        m = mean vector (default=0)
%        v = variance vector (default=1)
%---------------------------------------------------
% RETURNS: pdf (nx1) vector
%---------------------------------------------------
% SEE ALSO: norm_d, norm_rnd, norm_inv, norm_cdf
%---------------------------------------------------

% Updated by James P. Lesage, 
% jlesage@spatial-econometrics.com 3/2009


  if ~((nargin == 1) | (nargin == 3))
    error('Wrong # of arguments to norm_pdf');
  
  elseif (nargin == 1) % we have a standard normal with mean 0, variance 1   
      
  pdf = stdn_pdf(x);
  
  else
  [r, c] = size (x);    
  pdf = zeros (r,1);

    pdf(1:r,1) = stdn_pdf((x(1:r,1) - m(1:r,1)) ./ sqrt (v(1:r,1))) ...
 ./ sqrt (v(1:r,1)); 
  end
  
