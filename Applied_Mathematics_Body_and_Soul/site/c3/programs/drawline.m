function drawline(p1, p2, color)

  X = [p1(1) p2(1)];
  Y = [p1(2) p2(2)];
  Z = [p1(3) p2(3)];

l = line(X, Y, Z);
if(nargin > 2)
  set(l, 'color', color);
end