function Z = cmeshgrid (ll, ur, n)
  x = linspace (real (ll), real (ur), n);
  y = linspace (imag (ll), imag (ur), n);
  [X, Y] = meshgrid (x, y);
  Z = complex (X, Y);
