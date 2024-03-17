function zeta = joukowsky_inverse (z, c)
  zeta = (z - c/2 + sqrt (z .* (z - c))) / 2;
  flip = abs (zeta) < c/4;
  zeta(flip) = c^2 ./ (16 * zeta(flip));
