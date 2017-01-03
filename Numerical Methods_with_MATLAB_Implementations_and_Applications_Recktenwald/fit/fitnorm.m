function [c,R2,rout] = fitnorm(x,y,basefun)
% fitnorm   Least-squares fit via solution to the normal equations
%           Given ordered pairs of data, (x_i,y_i), i=1,...,m, fitnorm
%           returns the vector of coefficients, c_1,...,c_n, such that
%               F(x) = c_1*f_1(x) + c_2*f_2(x) + ... + c_n*f_n(x)
%           minimizes the L2 norm of y_i - F(x_i).
%
% Synopsis:  c       = fitnorm(x,y,basefun)
%           [c,R2]   = fitnorm(x,y,basefun)
%           [c,R2,r] = fitnorm(x,y,basefun)
%
% Input:   x,y     = vectors of data to be fit
%          basefun = (string) name of user-supplied m-file that computes
%                    matrix A.  The columns of A are the values of the
%                    basis functions evaluated at the x data points.
%
% Output:  c = vector of coefficients obtained from the fit
%          R2 = (optional) adjusted coefficient of determination; 0 <= R2 <= 1
%               R2 close to 1 indicates a strong relationship between y and x
%          r  = (optional) residuals of the fit

if length(y)~= length(x);  error('x and y are not compatible');  end

A = feval(basefun,x(:)); %  Coefficient matrix of overdetermined system
c = (A'*A)\(A'*y(:));    %  Solve normal equations, y(:) is always a column
if nargout>1
  r = y - A*c;           %  Residuals at data points used to obtain the fit
  [m,n] = size(A);
  R2 = 1 - (m-1)/(m-n-1)*(norm(r)/norm(y-mean(y)))^2;
  if nargout>2,  rout = r;  end
end
