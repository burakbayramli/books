function drawcross(v1, v2)

  c = cross(v1, v2);

  initgraphics;
  cleargraphics;
  drawaxes;
  drawvector(v1);
  drawvector(v2);
  drawtriangle([0, 0, 0], v1, v2);
  drawvector(c);
