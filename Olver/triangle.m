function triangle(v,r,c)
%
%  triangle(v,r,c)
%
%   Plot an equilateral triangle 
%
%     v - center (either 2 or 3 dimensional vector)
%     r - size; default = 1
%     c - color; default = white
%
%   See also CIRCLE

if nargin < 2, r = 1; end
if nargin < 3, c = 'w'; end

d = r;

switch size(v,2)
 case 2,
   x = [v(1), v(1) + d, v(1) - d];
   y = [v(2) + 2*d/sqrt(3), v(2) - d/sqrt(3),  v(2) - d/sqrt(3)];
   patch(x,y,c)
 case 3,
   x = [v(1), v(1) + d, v(1) - d];
   y = [v(2),v(2),v(2)];
   z = [v(3) + 2*d/sqrt(3), v(3) - d/sqrt(3),  v(3) - d/sqrt(3)];
   patch(x,y,z,c)
 otherwise error('vector must be two or three-dimensional')
end
