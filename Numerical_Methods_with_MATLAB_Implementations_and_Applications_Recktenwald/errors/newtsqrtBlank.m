function r = newtsqrt(x,delta,maxit)
% newtsqrt   Use Newton's method to compute the square root of a number
%
% Synopsis:  r = newtsqrt(x,delta,maxit)
%
% Input:  x     = number for which the square root is desired
%         delta = (optional) convergence tolerance.  Default: delta = 5e-9
%         maxit = (optional) maximum number of iterations.  Default: maxit = 25
%
% Output:    r = square root of x to within delta/2

if x<0,  error('Negative input to newtsqrt not allowed');  end
if x==0,      r=x;  return;   end
if nargin<2,  delta = 5e-9;   end
if nargin<3,  maxit=25;       end

r = x/2;  rold = x;   %  Initialize, make sure convergence test fails on first try
it = 0;
while NOT_CONVERGED & it<maxit     %  Convergence test
   rold = r;                       %  Save old value for next convergence test
   r = 0.5*(rold + x/rold);        %  Update the guess
   it = it + 1;
end
