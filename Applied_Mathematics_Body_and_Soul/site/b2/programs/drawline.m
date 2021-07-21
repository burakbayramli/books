function drawline(p1, p2, color)
% drawline(p1, p2, color)
%
% p1 and p2 are points, color is a vector. Draws a line between p1 and
% p2 with the color color. The color argument is optional.



  X = [p1(1) p2(1)];
  Y = [p1(2) p2(2)];
  Z = [p1(3) p2(3)];

l = line(X, Y, Z);
if(nargin > 2)
  set(l, 'color', color);
end