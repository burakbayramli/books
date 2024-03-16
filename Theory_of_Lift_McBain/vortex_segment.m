function q = vortex_segment (r, r1, r2)
  lhat = normalize (r2 - r1);
  h = cross (lhat, r - r1);
  normh2 = dot (h, h);
  q = repmat (dot (lhat / 4 / pi, ...
		   normalize(r - r1) - normalize(r - r2)) ...
	      ./ normh2, [3, 1, 1] ) .* h;
  q(repmat (normh2 < eps, [3, 1, 1])) = 0; % r collinear
