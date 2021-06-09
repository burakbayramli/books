function demoTaylor(x0,dx)
% demoTaylor  Taylor Series approximations for f(x) = 1/(1-x)
%          
% Synopsis:  demoTaylor
%            demoTaylor(x0,dx)
%
% Input:  x0 = (optional) point about which the Taylor Series expansion is
%              made.  Default:  x0 = 1.6;
%         dx = (optional) size of neighborhood over which the expansion
%              is evaluated.  Default:  dx = 0.8
%
%  Output:  a plot of f(x) and its Taylor Series approximations

if nargin<2,  x0 = 1.6;  dx = 0.8;  end

x = linspace(x0-dx/2,x0+dx/2,20);   %  x-values at which f(x) is evaluated
fx = 1./(1-x);                      %  Exact f(x); notice the array operator

h = x - x0;                      %  Avoid recomputing intermediate values,
t = 1/(1-x0);                    %    h and t
p1x = t*ones(size(x)) + h*t^2;   %  First order Taylor polynomial
p2x = p1x + (h.^2)*t^3;          %  Second order "  "  "
p3x = p2x + (h.^3)*t^4;          %  Third

plot(x,fx,'-',x,p1x,'o-',x,p2x,'^-',x,p3x,'s-');
legend('exact  ','P_1(x)','P_2(x)','P_3(x)',4);
xlabel('x');    ylabel('Approximations to f(x) = 1/(1-x)');
