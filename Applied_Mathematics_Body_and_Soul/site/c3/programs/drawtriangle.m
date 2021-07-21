function h = drawtriangle(v1, v2, v3, color)

  X = [v1(1) v2(1) v3(1)];
  Y = [v1(2) v2(2) v3(2)];
  Z = [v1(3) v2(3) v3(3)];

if(nargin > 3)
  h = patch(X, Y, Z, color);
else
  h = patch(X, Y, Z, 'r');
end
  
set(h, 'FaceAlpha', 0.3);
set(h, 'LineStyle', 'none');
