function drawaxes()
% Draws an x, y and z axis in the viewing volume.

  drawline([-2, 0, 0], [2, 0, 0], [0.8, 0.6, 0.6]);
  drawline([0, -2, 0], [0, 2, 0], [0.6, 0.8, 0.6]);
  drawline([0, 0, -2], [0, 0, 2], [0.6, 0.6, 0.8]);

