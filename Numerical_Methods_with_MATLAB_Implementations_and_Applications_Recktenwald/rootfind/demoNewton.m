function x = demoNewton(x0,n)
% demoNewton  Use Newton's method to find the root of f(x) = x - x^(1/3) - 2
%
% Synopsis:  x = demoNewton(x0)
%            x = demoNewton(x0,n)
%
%  Input:    x0 = initial guess
%            n = (optional) number of iterations, Default: n = 5
%
%  Output    x = estimate of the root

if nargin<2, n=5; end       %  Default number of iterations
x = x0;                     %  Initial guess

fprintf('  k       f(x)          dfdx           x(k+1)\n');
for k=1:n
  f = x - x.^(1/3) - 2;
  dfdx = 1 - (1/3)*x.^(-2/3);
  x = x - f/dfdx;
  fprintf('%3d  %12.3e  %12.3e   %18.15f\n',k-1,f,dfdx,x);
end
