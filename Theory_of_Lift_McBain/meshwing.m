function [x, y, z] = meshwing (AR, nchord, nspan, sweep, ...
			       dihedral, taper, twist, ...
			       camber)
  z = linspace (0.5, -0.5, nspan + 1);
  lex = tan (sweep) * abs (z);
  ley = tan (dihedral) * abs (z);
  xi = linspace (0, 1, nchord + 1)';
  meanline = [xi, camber(xi)];
  c = 2 * (1 - 2 * abs (z) * (1 - taper)) / (1 + taper) / AR;
  twists = 2 * twist * abs (z);
  dx = meanline * ([c; c] .* [cos(twists); -sin(twists)]);
  dy = meanline * ([c; c] .* [sin(twists); +cos(twists)]);
  x = repmat (lex, size (xi)) + dx;
  y = repmat (ley, size (xi)) + dy;
  z = repmat (z, size (xi));
