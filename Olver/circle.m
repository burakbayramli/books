function z = circle(v,r,c,n)
%
%  circle(v,r,n)
%
%   Plot a circle
%
%     v - center (either 2 or 3 dimensional vector)
%     r - radius; default = 1
%     c - color; default = white
%     n - number of plot points; default = 20
%
%   See also TRIANGLE

if nargin < 2, r = 1; end
if nargin < 3, c = 'w'; end
if nargin < 4, n = 20; end


th = linspace(0,2*pi,n);
rr = r * ones(1,n);
[xx,yy] = pol2cart(th,rr);

switch size(v,2)
 case 2,
  x = xx + v(1); y = yy + v(2);
  z = patch(x,y,c);
 case 3,
  x = xx + v(1); y = yy + v(2) - yy; z = yy + v(3);
  z = patch(x,y,z,c);
 otherwise error('vector must be two or three-dimensional')
end
