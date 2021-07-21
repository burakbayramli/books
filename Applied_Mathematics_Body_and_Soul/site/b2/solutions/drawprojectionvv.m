function drawprojectionvv(v1, v2)
% drawprojectionvv(v1, v2)
%
% v1 and v2 are vectors. Visualizes the projection of v2 onto v1.


  pv2 = projectionvv(v1, v2);

  initgraphics;
  cleargraphics;
  drawaxes;
  drawvector(v1);
  drawvector(v2);
  drawvector(pv2);
