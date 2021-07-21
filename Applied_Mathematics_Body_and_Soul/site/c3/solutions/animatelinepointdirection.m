function animatelinepointdirection(point, direction)

  ndir = direction / norm(direction);

  x1 = point + 10 * ndir;
  x2 = point - 10 * ndir;

  drawline(x1, x2);

  while(1)

    for i = -2:0.1:2
      xi = point + i * ndir;
      drawaxes;
      drawline(x1, x2);
      drawvector(xi);
      cleargraphics;
    end
  end
