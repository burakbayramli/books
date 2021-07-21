function drawlinepointdirection(point, direction)

  ndir = direction / norm(direction);

  x1 = point + 10 * ndir;
  x2 = point - 10 * ndir;

  drawline(x1, x2);

