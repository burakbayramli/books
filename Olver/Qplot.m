function Qplot(A,b,c,r,n)
%
%  Plots graph of quadratic function
%     Q(v) = v' * A * v - v * b + c
%  where  v = [ x y]
%  A is a 2 by 2 matrix, b a vector and c a scalar
%  optional arguments:
%  r - range of plot -r <= x & y <=r
%       default = 5
%  n - number of mesh points
%       default = 25
%


if nargin < 4, r = 5; end
if nargin < 5, n = 25; end

x = linspace(-r,r,n);
y = x;

max = -10000; min = 10000;
for i=1:n,
for j=1:n,
v = [x(j),y(i)];
t = (1/2)* v * A * v'  - v * b + c;
if t > max 
   max = t;
end
if t < min 
   min = t;
end

z(i,j) = t;
end
end

surf(x,y,z);
colormap hsv;
shading interp;

if abs(det(A)) > .01
   del = (max - min)/7;
   x0 = A\b;

   l = line([x0(1) x0(1)], [x0(2) x0(2)], [min-del,max+del]);
   set(l,'color',[1 0 1]);
else
'Matrix close to singular'
end
