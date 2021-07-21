function drawprojectionvv(v1, v2)

  pv2 = projectionvv(v1, v2);

  initgraphics;
  cleargraphics;
  drawaxes;
  drawvector(v1);
  drawvector(v2);
  drawvector(pv2);
