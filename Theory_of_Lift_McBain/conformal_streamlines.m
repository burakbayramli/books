function conformal_streamlines (ll, ur, W1, W2, v)
  Z = cmeshgrid (ll, ur, 1e3);
  subplot (1, 2, 1)
  contour (real (Z), imag (Z), imag (W1 (Z)), v)
  axis ('image')
  subplot (1, 2, 2)
  contour (real (Z), imag (Z), imag (W2 (Z)), v)
  axis ('image')
