function drawprojectionpl(p1, d, p2)

  pp2 = projectionpl(p1, d, p2);

  initgraphics;
  cleargraphics;
  drawaxes;
  drawlinepointdirection(p1, d);
  drawarrow(p1, p2);
  drawarrow(p1, pp2);
