function Isum = quadToInfinity(fun,a,dx0,tol,method)
% quadToInfinity  Integral from a to infinity evaluated as sum of integrals
%                 Size of subintervals increases geometrically.  Sum is
%                 terminated when change in integral is less than tolerance
%
% Synopsis:  I = quadToInfinity(fun)
%            I = quadToInfinity(fun,a)
%            I = quadToInfinity(fun,a,dx0)
%            I = quadToInfinity(fun,a,dx0,tol)
%            I = quadToInfinity(fun,a,dx0,tol,method)
%
% Input:  fun = (string) name of m-file that evaluates the integrand, f(x)
%         a   = (optional) lower limit of the integral.  Default:  a = 0
%         dx0 = (optional) size of first interval of x axis used to
%               evaluate the partial integrals.  Default:  x = 0.5
%         tol = (optional) relative tolerance.  Sum is terminated when
%               abs(I_{k+1}/I_k) < tol, where I_k = sum(I_j), j=1,...,k.
%               and I_j is integral over subinterval j.  Default: tol=5e-4
%         method = (optional) integral rule used for subintervals. method=1
%               for composite Gauss-Legendre quadrature with 8 panels and 6
%               nodes per panel; method=2 for built-in quad; method=4 for
%               built-in quad8.   Default:  method = 1
%
% Output:  I = estimate of the integral of f(x) dx from x=a to x=infinty
if nargin<2,      a = 0;  end
if nargin<3,  dx0 = 0.5;  end
if nargin<4, tol = 5e-4;  end
if nargin<5, method = 1;  end
j = 0;  dx = dx0;   Isum = 0;   x1 = a;   maxint = 35;   %  Initialize
 
fprintf('\n    j       dx       x2       I_j           Isum\n');
while j<maxint
  x2 = x1 + dx;
  switch method
    case 1,     I = gaussQuad(fun,x1,x2,8,6);
    case 2,     I = quad(fun,x1,x2);
    case 3,     I = quad8(fun,x1,x2);
    otherwise,  error(sprintf('method = %d not allowed',method));
  end
  Isum = Isum + I;
  fprintf(' %4d %8.1f %8.1f  %12.8f   %12.8f\n',j,dx,x2,I,Isum);
  if j>5 & abs(I/Isum) < tol,  break;  end
  j = j + 1;  x1 = x2;   dx = 2*dx;  %  prepare for next interval
end
