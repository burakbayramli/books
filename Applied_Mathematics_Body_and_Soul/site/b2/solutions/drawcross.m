function drawcross(v1, v2)
% drawcross(v1, v2)
%
% v1 and v2 are vectors. Draws v1, v2 and v1 x v2. Draws a triangle
% with corners 0, v1 and v2.

  c = cross(v1, v2);

  initgraphics;
  cleargraphics;
  drawaxes;
  drawvector(v1);
  drawvector(v2);
  drawtriangle([0, 0, 0], v1, v2);
  drawvector(c);
