function streamlines_W (ll, ur, W, v)
  Z = cmeshgrid (ll, ur, 998);
  contour (real (Z), imag (Z), imag (W (Z)), v)
  axis ('image')
