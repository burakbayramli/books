x = [60, 10, 5, 4, 2, 1, 0.2, -[0.2, 1:3]];
r = [x; zeros(size (x)); -4*ones(size (x))];
psi = pi ./ [6, 4, 3];
r2 = [tan(psi); repmat([0; 1], [1, length(psi)])];
q = vortex_horseshoe (r, -r2, r2, ...
		      repmat ([1; 0; 0], [1, size(r2, 2)]));
reshape (4 * pi * q(2,:)', size (r, 2), length (psi))
