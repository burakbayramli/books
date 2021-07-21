function drawlinepointdirection(p, d)
% drawlinepointdirection(p, d)
%
% p and d are vectors. Draws a line in the viewing volume represented
% by the point p and the direction d.


  ndir = d / norm(d);

  p1 = p + 10 * ndir;
  p2 = p - 10 * ndir;

  drawline(p1, p2);
