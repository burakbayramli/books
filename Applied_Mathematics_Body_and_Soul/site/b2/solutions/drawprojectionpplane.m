function drawprojectionpplane(p1, n, p2)
% drawprojectionpplane(p1, n, p2)
%
% p1, n and p2 are vectors. Visualizes the projection of p2 onto the \
% plane represented by p1 and n.
 


  initgraphics;
  cleargraphics;
  drawaxes;

  pp2 = projectionpplane(p1, n, p2);

  drawplanepointnormal(p1, n, [1, 0, 0]);
  drawarrow(p1, p2);
  drawarrow(p1, pp2);
