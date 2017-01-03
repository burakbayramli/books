function xm = demoBisect(xleft,xright,n)
% demoBisect  Use bisection to find the root of x - x^(1/3) - 2
%
% Synopsis: x = demoBisect(xleft,xright)
%           x = demoBisect(xleft,xright,n)
%
%  Input:   xleft,xright = left and right brackets of the root
%           n = (optional) number of iterations;  default:  n = 15
%
%  Output:  x = estimate of the root

if nargin<3, n=15;  end    %  Default number of iterations
a = xleft;   b = xright;   %  Copy original bracket to local variables
fa = a - a^(1/3) - 2;      %  Initial values of f(a) and f(b)
fb = b - b^(1/3) - 2;  
fprintf('  k       a            xmid          b             f(xmid)\n');

for k=1:n
  xm = a + 0.5*(b-a);      %  Minimize roundoff in computing the midpoint
  fm = xm - xm^(1/3) - 2;  %  f(x) at midpoint
  fprintf('%3d  %12.8f  %12.8f  %12.8f  %12.3e\n',k,a,xm,b,fm);
  if sign(fm)==sign(fa)    %  Root lies in interval [xm,b], replace a
    a = xm;
    fa = fm;
  else                     %  Root lies in interval [a,xm], replace b
    b = xm;
    fb = fm;
  end
end
