function drawvolume(v1, v2, v3)

  initgraphics;
  cleargraphics;
  drawaxes;
  drawvector(v1);
  drawvector(v2);
  drawvector(v3);

  drawline(v1, v2);
  drawline(v2, v3);
  drawline(v1, v3);


  drawtriangle([0, 0, 0], v1, v2);
  drawtriangle([0, 0, 0], v2, v3);
  drawtriangle([0, 0, 0], v1, v3);
  drawtriangle(v1, v2, v3);
