function direction (ll, ur, wf)
  Z = cmeshgrid (ll, ur, 22);
  w = wf (Z);
  s = w ./ abs (w);
  quiver (real (Z), imag (Z), real (s), -imag (s))
  axis ([real(ll), real(ur), imag(ll), imag(ur)], ...
	'off', 'equal')
  box ('on')
